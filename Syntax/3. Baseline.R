rm(list = ls())

library(tidyverse)
library(glue)
library(gtsummary)
library(labelled)
library(gt)

date <- "20250717"
working_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Data/")
output_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Output/")
dir.create(output_dir, showWarnings = F)


# Load data
baseline_gps <- readRDS(file = file.path(working_dir, "Processed", "baseline_gps.Rds"))
baseline_consultations <- readRDS(file = file.path(working_dir, "Processed", "baseline_consultations.Rds"))
baseline_interviews <- readRDS(file = file.path(working_dir, "Processed", "baseline_interviews.Rds"))
baseline_experience <- readRDS(
  file = file.path(working_dir, "Processed", "outcomes_experience.Rds")
)

baseline_utaut <- readRDS(file = file.path(working_dir, "Processed", "outcomes_utaut.Rds")) %>%
  filter(period == "Baseline") %>%
  select(-period)


baseline_gps <- left_join(baseline_gps, baseline_utaut, by="participant")

var_label(baseline_gps) <- c(
  "Participant ID",
  "Job",
  "Type",
  "List size",
  "Location",
  "Patients from a deprived area",
  "Female",
  "Age (years)",
  "Experience as a GP (years)",
  "Direct link between tool and EHR",
  "General digital skills",
  "EHR skills",
  "Skills in using various programs",
  "Skills for safe internet use",
  "Consultations observed",
  "Outcome expectation",
  "Effort expectation",
  "Social influence",
  "Facilitating conditions",
  "Intention to use"
)

gp_table <- baseline_gps %>%
  relocate(gp_age, gp_gender, gp_experience, starts_with("selfscan"), outcome_expectation,
           effort_expectation, social_influence, facilitating_conditions, intentions) %>%
  tbl_summary(include = c(-participant, -consultations, -gp_job),
              type = list(gp_age ~ "continuous",
                          practice_list_size ~ "continuous",
                          deprived_area ~ "continuous",
                          gp_experience ~ "continuous",
                          selfscan_general ~ "continuous",
                          selfscan_ehr ~ "continuous",
                          selfscan_programs ~ "continuous",
                          selfscan_internet ~ "continuous",
                          outcome_expectation ~ "continuous",
                          effort_expectation ~ "continuous",
                          social_influence ~ "continuous",
                          facilitating_conditions ~ "continuous",
                          intentions ~ "continuous"),
              value = list(gp_gender ~ "Female"),
              statistic = list(deprived_area ~ "{median}% ({p25}%, {p75}%)")) %>%
  add_variable_group_header("GP characteristics", c(starts_with("gp_"))) %>%
  add_variable_group_header("Practice characteristics", c(starts_with("practice"), deprived_area, ehr_api)) %>%
  add_variable_group_header("Digital skills", c(starts_with("selfscan"))) %>%
  add_variable_group_header(
    "UTAUT",
    c(
      outcome_expectation,
      effort_expectation,
      social_influence,
      facilitating_conditions,
      intentions
    )
  ) %>%
  modify_column_indent(
    column = "label",
    rows = row_type == "level",
    indent = 8L
  ) %>%
  modify_column_indent(
    column = "label",
    rows = row_type == "missing",
    indent = 8L
  ) %>%
  modify_abbreviation(
    "GP=general practitioner"
  ) %>%
  modify_abbreviation(
    "EHR=electronic health record"
  ) %>%
  modify_abbreviation(
    "UTAUT=Unified Theory of Acceptance and Use of Technology"
  )

gp_table

gp_table %>% as_gt() %>% gtsave(paste0(output_dir, "baseline_gp_table.rtf"))


# Consultations
baseline_consultations_table <- baseline_consultations %>% 
  rowwise() %>%
  mutate(questions = sum(!is.na(c_across(icpc1:icpc6)))) %>%
  ungroup() %>%
  select(
    period,
    consultation_planned_time,
    consultation_used,
    consultation_used_why,
    observation_patient_age,
    observation_patient_gender,
    questions
  )

number_consult_period <- baseline_consultations_table %>%
  group_by(period) %>%
  summarise(consultations = n())

surveys <- baseline_experience %>%
  mutate(informed_consent = if_else(is.na(informed_consent), 0, informed_consent)) %>%
  group_by(period) %>%
  summarise(sent = n(), filled_in = sum(informed_consent)) %>%
  left_join(number_consult_period, by="period") %>%
  rowwise() %>%
  mutate(filled_in = paste0(filled_in, " (", round(filled_in/sent*100, 0), "%)")) %>%
  mutate(sent = paste0(sent, " (", round(sent/consultations*100, 0), "%)"))

var_label(baseline_consultations_table) <- c(
  "Period",
  "Planned consultation time",
  "Tool used",
  "Reason tool not used",
  "Patient age (years)",
  "Female",
  "Number of complaints"
)

consultation_table <- baseline_consultations_table %>%
  relocate(observation_patient_age, observation_patient_gender, questions) %>%
  tbl_summary(by=period,
              type=list(questions="continuous"),
              statistic = list(questions ~ "{median} ({p25}, {p75})"),
              value = list(observation_patient_gender ~ "Female")) %>%
  remove_row_type(variables=c(consultation_used_why), type="missing") %>%
  modify_table_body(~ .x %>% mutate(stat_1=if_else(stat_1=="0 (NA%)", "-", stat_1))) %>%
  modify_table_body(
    ~ .x %>% add_row(
      variable = "survey_sent",
      var_type = "continuous",
      row_type = "label",
      var_label = "Surveys consented/sent",
      label = "Surveys consented/sent",
      stat_1 = surveys %>% filter(period == "Baseline") %>% pull(sent),
      stat_2 = surveys %>% filter(period == "Intervention") %>% pull(sent)
    )
  ) %>%
  modify_table_body(
    ~ .x %>% add_row(
      variable = "survey_filled",
      var_type = "continuous",
      row_type = "label",
      var_label = "Surveys answered",
      label = "Surveys answered",
      stat_1 = surveys %>% filter(period == "Baseline") %>% pull(filled_in),
      stat_2 = surveys %>% filter(period == "Intervention") %>% pull(filled_in)
    )
  )


consultation_table

consultation_table %>% as_gt() %>% gtsave(filename = file.path(output_dir, "consultation_table.rtf"))

baseline_icpc_table <- baseline_consultations %>%
  select(period, starts_with("icpc")) %>%
  mutate(across(starts_with("icpc"), ~ substr(.x, 1, 1))) %>%
  pivot_longer(-period, names_to="number", values_to="icpc") %>%
  filter(!is.na(icpc)) %>%
  select(-number) %>%
  mutate(icpc = case_when(
    icpc == "A" ~ "General/non-specified",
    icpc == "B" ~ "Haematological",
    icpc == "D" ~ "Digestive tract",
    icpc == "F" ~ "Eye",
    icpc == "H" ~ "Ear",
    icpc == "K" ~ "Circulatory tract",
    icpc == "L" ~ "Musculoskeletal system",
    icpc == "N" ~ "Nervous system",
    icpc == "P" ~ "Psychological problems",
    icpc == "R" ~ "Respiratory tract",
    icpc == "S" ~ "Skin",
    icpc == "T" ~ "Endocrine tract/metabolism",
    icpc == "U" ~ "Urinary tract",
    icpc == "W" ~ "Pregnancy/birth/contraception",
    icpc == "X" ~ "Female genitalia and breasts",
    icpc == "Y" ~ "Male genitalia and breasts",
    icpc == "Z" ~ "Social problems"
  ))

var_label(baseline_icpc_table) <- c(
  "Period",
  "ICPC code"
)



icpc_table <- baseline_icpc_table %>%
  tbl_summary(by = period) %>%
  modify_abbreviation("ICPC=International Classification of Primary Care")  %>%
  # Add the absolute difference between columns
  modify_table_body( ~ .x %>% mutate(
    stat_3 = paste0(
      ((
        str_extract(stat_2, "(\\d+(\\.\\d+)?%)") %>% gsub("%", "", .) %>% as.numeric()
      ) - (
        str_extract(stat_1, "(\\d+(\\.\\d+)?%)") %>% gsub("%", "", .) %>% as.numeric()
      )) %>% round(2)
  , "%") %>% if_else(. == "NA%", "", .))) %>%
  modify_header(stat_3 = "**Difference**") %>%
  modify_footnote_header(
    footnote = "Absolute difference in percentage between periods",
    columns = stat_3,
    replace = F
  )

icpc_table

icpc_table %>% as_gt() %>% gtsave(filename = file.path(output_dir, "icpc_table.rtf"))

baseline_interviews <- baseline_interviews %>%
  relocate(
    participant,
    interview_patient_age,
    interview_patient_gender,
    interview_patient_education,
    interview_patient_language,
    migration_background,
    interview_patient_chronic
  )


var_label(baseline_interviews) <- c(
  "Participant ID",
  "Age (years)",
  "Female",
  "Education level",
  "Dutch proficiency",
  "Migration background",
  "Self-reported chronic disease"
)

interview_table <- baseline_interviews %>%
  tbl_summary(include = c(-participant),
              value = list(interview_patient_gender ~ "Female"))

interview_table

interview_table %>% as_gt() %>% gtsave(paste0(output_dir, "interview_table.rtf"))  