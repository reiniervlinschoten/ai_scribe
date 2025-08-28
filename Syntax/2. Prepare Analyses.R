rm(list = ls())

library(tidyverse)
library(glue)

date <- "20250717"
working_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Data/")

dataset <- readRDS(file = file.path(working_dir, "Processed", "cleaned_data.Rds"))

# Prepare baseline table files
# For GPs
baseline_gps <- dataset$study  %>%
  # Add number of consultations
  left_join(
    dataset$reports$consultations %>% group_by(`Participant Id`) %>% summarise(consultations = n()),
    by="Participant Id"
  ) %>%
  rename("participant" = "Participant Id") %>%
  select(
    -starts_with("Participant", ignore.case=F),
    -`Site Abbreviation`,
    -juvoly_experience,
    -informed_consent_date
  ) %>%
  mutate(deprived_area = if_else(deprived_area < 0, NA_integer_, deprived_area))

saveRDS(baseline_gps,
        file = file.path(working_dir, "Processed", "baseline_gps.Rds"))

# For interviews
baseline_interviews <- dataset$reports$patient_interview %>%
  rename("participant" = "Participant Id") %>%
  select("participant", starts_with("interview")) %>%
  # set comorbidity/chronic conditions to 0 or 1
  mutate(
    interview_patient_chronic = case_when(
      is.na(interview_patient_chronic) ~ NA_integer_,
      interview_patient_chronic == "n.v.t." ~ FALSE,
      interview_patient_chronic == "N.v.t." ~ FALSE,
      interview_patient_chronic == "nee" ~ FALSE,
      interview_patient_chronic == "nvt" ~ FALSE,
      interview_patient_chronic == "Nvt" ~ FALSE,
      interview_patient_chronic == "NVT" ~ FALSE,
      TRUE ~ TRUE
    )
  ) %>%
  # Set education to 3 levels
  mutate(interview_patient_education = factor(
    case_when(
      interview_patient_education == "Geen school of opleiding afgemaakt" ~ "Low",
      interview_patient_education == "Lagere school of basisschool" ~ "Low",
      interview_patient_education == "Vmbo, mbo niveau 1, mavo, huishoudschool, vbo, lbo, lts, leao, lhno, mulo, ivo" ~ "Low",
      interview_patient_education == "Vwo, atheneum, gymnasium, havo, mbo niveau 2-4, mts, meao, mhno, inas, intas, hbs, mms" ~ "Middle",
      interview_patient_education == "1-jarig hbo, mbo specialistenopleiding" ~ "Middle",
      interview_patient_education == "Associate degree (2-3 jarig hbo)" ~ "High",
      interview_patient_education == "Hbo-bachelor, wo-bachelor, hts, heao of hhno" ~ "High",
      interview_patient_education == "Wo-master, hbo-master" ~ "High",
      interview_patient_education == "Doctoraat" ~ "High",
      interview_patient_education == "Geen school of opleiding afgemaakt" ~ "Low",
      interview_patient_education == "##USER_MISSING_97##" ~ NA_character_,
      is.na(interview_patient_education) ~ NA_character_,
      TRUE ~ "Error"
    ),
    levels = c("Low", "Middle", "High")
  )) %>%
  mutate(interview_patient_language = factor(interview_patient_language, levels=c("Poor", "Fair", "Good"))) %>%
  mutate(across(
    starts_with("interview_patient_migration"),
    .fns = ~ case_when(
      .x == "Africa" ~ "non-western ",
      .x == "America/Oceania" ~ "western ",
      .x == "Asia" ~ "non-western ",
      .x == "Europe, not the Netherlands" ~ "western ",
      .x == "Indonesia" ~ "western ",
      .x == "Netherlands" ~ "Netherlands",
      .x == "Suriname" ~ "non-western ",
      .x == "Turkey" ~ "non-western "
    )
  )) %>%
  mutate(
    migration = case_when(
      (
        interview_patient_migration_mother != "Netherlands" |
          interview_patient_migration_father != "Netherlands"
      ) &
        interview_patient_migration != "Netherlands" ~ "First generation ",
      (
        interview_patient_migration_mother != "Netherlands" |
          interview_patient_migration_father != "Netherlands"
      ) &
        interview_patient_migration == "Netherlands" ~ "Second generation ",
      (
        interview_patient_migration_mother == "Netherlands" |
          interview_patient_migration_father == "Netherlands"
      ) ~ "",
      TRUE ~ "Error"
    )
  ) %>% mutate(
    background = case_when(
      migration == "" ~ "Dutch ",
      migration == "First generation " ~ paste0(interview_patient_migration, "migration "),
      migration == "Second generation " &
        interview_patient_migration_mother != "Netherlands" ~ paste0(interview_patient_migration_mother, "migration "),
      migration == "Second generation " &
        interview_patient_migration_mother == "Netherlands" ~ paste0(interview_patient_migration_father, "migration "),
      TRUE ~ "ERROR"
    )
  ) %>%
  mutate(migration_background = paste0(migration, background, "background")) %>%
  select(-starts_with("interview_patient_migration"), -migration, -background)

# Test if all comorbidities were checked
baseline_interviews %>%
  select(interview_patient_chronic) %>%
  mutate(interview_patient_chronic = interview_patient_chronic < 2) %>%
  all(na.rm = T)

# Test if all education levels are correct
baseline_interviews %>%
  select(interview_patient_education) %>%
  mutate(interview_patient_education = interview_patient_education != "Error") %>%
  all(na.rm = T)

saveRDS(baseline_interviews,
        file = file.path(working_dir, "Processed", "baseline_interviews.Rds"))

# For consultations
baseline_consultations <- dataset$reports$consultations %>%
  rename("participant" = "Participant Id",
         "period" = "Repeating data Parent") %>%
  select(
    -`Participant Status`,
    -starts_with("Repeating Data"),
    -consultation_date,
    -starts_with("time_"),
    -starts_with("note_"),
    `Repeating data Name Custom`
  ) %>%
  # Split ICPC codes in multiple columns
  # Remove leading and trailing whitespace
  mutate(observation_patient_diagnosis = trimws(observation_patient_diagnosis)) %>%
  # Make sure they are only linked with +
  mutate(observation_patient_diagnosis = gsub("en", "+", observation_patient_diagnosis)) %>%
  mutate(observation_patient_diagnosis = gsub(",", "+", observation_patient_diagnosis)) %>%
  mutate(observation_patient_diagnosis = gsub(" ", "+", observation_patient_diagnosis)) %>%
  # Replace multiple + with a single
  mutate(observation_patient_diagnosis = gsub("(\\+)+", "+", observation_patient_diagnosis)) %>%
  # Set to caps
  mutate(observation_patient_diagnosis = toupper(observation_patient_diagnosis)) %>%
  # Split on  +
  separate_wider_delim(
    observation_patient_diagnosis,
    "+",
    names = c("icpc1", "icpc2", "icpc3", "icpc4", "icpc5", "icpc6"),
    too_few = "align_start"
  ) %>%
  # Remove whitespace
  mutate(across(starts_with("icpc"), ~ if_else(.x == "", NA_character_, .x))) %>%
  # Fix icpc codes where dot is missing, add dot in the middle
  mutate(across(
    starts_with("icpc"),
    ~ gsub("([0-9]{2})([0-9]{2})", "\\1\\.\\2", .x)
  )) %>%
  mutate(consultation_used_why = case_when(
    consultation_used == "Yes" ~ NA_character_,
    is.na(consultation_used_why) ~ consultation_used_why,
    consultation_used_why == "Baseline period" ~ NA_character_,
    consultation_used_why == "GP decision" ~ consultation_used_why,
    consultation_used_why == "Technical difficulties" ~ consultation_used_why,
    consultation_used_why == "Technical difficulties" ~ consultation_used_why, 
    consultation_used_why_other == "vergeten aan te zetten" ~ "Forgotten",
    consultation_used_why_other == "HA vergeten Juvoly aan te zetten" ~ "Forgotten",
    consultation_used_why_other == "Huisarts vergeten aan te zetten" ~ "Forgotten",
    consultation_used_why_other == "Vergeten aan te zetten" ~ "Forgotten",
    consultation_used_why_other == "vergeten aan te zetten " ~ "Forgotten",
    consultation_used_why_other == "Interventie" ~ "Intervention",
    consultation_used_why_other == "Interventie andere kamer" ~ "Intervention",
    consultation_used_why_other == "Interventie, juvoly geen meerwaarde" ~ "Intervention",
    consultation_used_why_other == "Interventie, sv niet bruikbaar" ~ "Intervention",
    consultation_used_why_other == "Tool stond nog op nederlandstalig bij engelstalige pt" ~ "Wrong language",
    TRUE ~ "Error"
  )) %>%
  select(-consultation_used_why_other)

# Test if all other reasons are formatted correct
baseline_consultations %>%
  select(consultation_used_why) %>%
  mutate(consultation_used_why = consultation_used_why != "Error") %>%
  all(na.rm = T)

# Test if all ICPC codes are syntactically valid
baseline_consultations %>%
  select(starts_with("icpc")) %>%
  mutate(across(everything(), ~ nchar(.x) == 3 |
                  nchar(.x) == 6)) %>%
  all(na.rm = T)

# Prepare analysis files for statistical analysis

# Consult outcomes
outcomes_consultations <- dataset$reports$consultations %>%
  rename(participant = `Participant Id`, period = `Repeating data Parent`) %>%
  select(
    -`Participant Status`,
    -starts_with("Repeating Data"),
    -consultation_date,
    -starts_with("consultation_used_"),
    -starts_with("observation_"),
    -time_total_calc,
    `Repeating data Name Custom`
  ) %>%
  mutate(time_total_documentation = time_clinical_documentation + time_waiting + time_copypaste) %>%
  mutate(time_non_documentation = time_total - time_total_documentation) %>%
  full_join(baseline_consultations %>% select(participant, starts_with("icpc"), `Repeating data Name Custom`),
            by=c("participant", "Repeating data Name Custom")) %>%
  select(-`Repeating data Name Custom`) %>%
  mutate(re_icpc = icpc1) %>%
  mutate(across(starts_with("icpc"), function(x) !is.na(x))) %>%
  rowwise() %>%
  mutate(re_icpc = if_else(sum(icpc1, icpc2, icpc3, icpc4, icpc5, icpc6) > 1,
                           as.character(sum(icpc1, icpc2, icpc3, icpc4, icpc5, icpc6)),
                           substring(re_icpc, 1, 1))) %>%
  ungroup() %>%
  select(-starts_with("icpc"))

saveRDS(
  outcomes_consultations,
  file = file.path(working_dir, "Processed", "outcomes_consultations.Rds")
)


saveRDS(
  baseline_consultations %>% select(-"Repeating data Name Custom"),
  file = file.path(working_dir, "Processed", "baseline_consultations.Rds")
)

# Experience outcome
outcomes_experience <- dataset$surveys$experiences %>%
  # Link period
  left_join(
    dataset$reports$consultations %>% select(
      `Participant Id`,
      `Repeating data Name Custom`,
      `Repeating data Parent`
    ),
    by = c("Castor Participant ID" = "Participant Id", "Survey Parent" =
             "Repeating data Name Custom")
  ) %>%
  rename("period" = "Repeating data Parent", "participant" = "Castor Participant ID") %>%
  select(-starts_with("Survey"),
         -starts_with("Castor"),
         -store_data,
         -contact_again) %>%
  # Make the numbers numbers, they were made lists by combining enlgish with dutch
  mutate(across(c(starts_with("peq_"), informed_consent), as.integer)) %>%
  # Switch outcomes where high is now negative to be in line with rest
  mutate(across(c(peq_connect, peq_smalltalk, peq_questions, peq_decisions, peq_overheard, peq_crowd), ~ 6 - .x)) %>%
  # Combine in average
  mutate(peq_outcome = (peq_todo + peq_expect + peq_handle + peq_prevent)/4) %>%
  mutate(peq_communication = (peq_talk + peq_reassured + peq_understood + peq_care)/4) %>%
  mutate(peq_barriers = (peq_connect + peq_smalltalk + peq_questions + peq_decisions)/4) %>%
  mutate(peq_aux = (peq_overheard + peq_crowd)/2) %>%
  mutate(peq_emotions = (peq_relieved + peq_cheerful + peq_strengthened + peq_relaxed)/4) %>%
  dplyr::select(participant, period, informed_consent, peq_outcome, peq_communication, peq_barriers, peq_aux, peq_emotions)

saveRDS(
  outcomes_experience,
  file = file.path(working_dir, "Processed", "outcomes_experience.Rds")
)

outcomes_utaut <- dataset$surveys$utaut %>%
  rename("period" = "Survey Parent", "participant" = "Castor Participant ID") %>%
  select(-starts_with("Survey"),
         -starts_with("Castor")) %>%
  # By translating was turned into a list
  mutate(across(c(starts_with("utaut_")), as.integer)) %>%
  rowwise() %>%
  mutate(outcome_expectation = mean(c(utaut_useful, utaut_quick, utaut_productive, utaut_raise))) %>%
  mutate(effort_expectation = mean(c(utaut_clear, utaut_training, utaut_easy, utaut_learning))) %>%
  mutate(social_influence = mean(c(utaut_influence, utaut_important, utaut_management, utaut_organisation))) %>%
  mutate(facilitating_conditions = mean(c(utaut_resources, utaut_knowledge, utaut_compatible, utaut_assistance))) %>%
  mutate(intentions = mean(c(utaut_intend, utaut_predict, utaut_plan))) %>%
  ungroup() %>%
  select(-starts_with("utaut_"))
  

saveRDS(
  outcomes_utaut,
  file = file.path(working_dir, "Processed", "outcomes_utaut.Rds")
)
