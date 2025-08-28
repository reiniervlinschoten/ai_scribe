rm(list=ls())

library(tidyverse)
library(gt)
library(glue)

rm(list = ls())

date <- "20250717"
working_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Data/")
output_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Output/")

options(scipen=999)

# CHANGE
result_table <- readRDS(file = file.path(output_dir, "analyses_complete.Rds"))

result_table <- result_table %>%
  mutate(`95% CI` = glue("[{round(periodIntervention[,1], 2)}; {round(periodIntervention[,2], 2)}]")) %>%
  rename(`p-value` = `Pr(>|z|)`) %>%
  mutate(`p-value` = if_else(is.na(`p-value`), `Pr(>|t|)`, `p-value`)) %>%
  select(-`Pr(>|t|)`, -periodIntervention) %>%
  mutate(Estimate = round(Estimate, 2)) %>%
  mutate(`p-value` = format.pval(`p-value`, digits = 1, eps = 0.0001, nsmall=0)) %>%
  relocate(Name, Estimate, `95% CI`, `p-value`)

result_main <- result_table %>%
  filter(!grepl("sensitivity", Name) & !grepl("protocol", Name)& !grepl("icpc", Name)) %>%
  mutate(Name = case_when(
    Name == "time_total_documentation" ~ "Documentation time",
    Name == "time_total" ~ "Consultation time",
    Name == "note_length_s" ~ "Subjective",
    Name == "note_length_o" ~ "Objective",
    Name == "note_length_a" ~ "Assessment",
    Name == "note_length_p" ~ "Plan",
    Name == "note_number_signs" ~ "Signs",
    Name == "note_number_context" ~ "Context",
    Name == "note_number_symptoms" ~ "Symptoms",
    Name == "note_number_diagnosis" ~ "Diagnosis",
    Name == "note_number_plan" ~ "Plan ",
    Name == "peq_outcome" ~ "Outcome of the consultation",
    Name == "peq_communication" ~ "Communication experiences",
    Name == "peq_barriers" ~ "Absence of communication barriers",
    Name == "peq_aux" ~ "Staff relationship",
    Name == "peq_emotions" ~ "Emotions after consultation"
  )) %>%
  gt() %>%
  tab_row_group(label="Patient experience", rows = c(12:16)) %>%
  tab_row_group(label="Number of variables in note", rows = c(7:11)) %>%
  tab_row_group(label="Note length", rows = c(3:6)) %>%
  tab_row_group(label="Time outcomes", rows = c(1:2)) %>%
  tab_footnote("Difference on an additive scale", locations = cells_row_groups(groups = c("Time outcomes", "Patient experience"))) %>%
  tab_footnote("Difference on a multiplicative scale", locations = cells_row_groups(groups = c("Note length", "Number of variables in note"))) %>%
  tab_footnote(
    "CI=confidence interval"
  )

result_main

result_main %>% gtsave(paste0(output_dir, "main_result_table.rtf")) 


result_per_protocol <- result_table %>%
  filter(!grepl("sensitivity", Name) & grepl("protocol", Name)) %>%
  mutate(Name = case_when(
    Name == "time_total_documentation_per_protocol" ~ "Documentation time",
    Name == "time_total_per_protocol" ~ "Consultation time",
    Name == "note_length_s_per_protocol" ~ "Subjective",
    Name == "note_length_o_per_protocol" ~ "Objective",
    Name == "note_length_a_per_protocol" ~ "Assessment",
    Name == "note_length_p_per_protocol" ~ "Plan",
    Name == "note_number_signs_per_protocol" ~ "Signs",
    Name == "note_number_context_per_protocol" ~ "Context",
    Name == "note_number_symptoms_per_protocol" ~ "Symptoms",
    Name == "note_number_diagnosis_per_protocol" ~ "Diagnosis",
    Name == "note_number_plan_per_protocol" ~ "Plan "
  )) %>%
  gt() %>%
  tab_row_group(label="Number of variables in note", rows = c(7:11)) %>%
  tab_row_group(label="Note length", rows = c(3:6)) %>%
  tab_row_group(label="Time outcomes", rows = c(1:2)) %>%
  tab_footnote("Difference on an additive scale", locations = cells_row_groups(groups = c("Time outcomes"))) %>%
  tab_footnote("Difference on a multiplicative scale", locations = cells_row_groups(groups = c("Note length", "Number of variables in note"))) %>%
  tab_footnote(
    "CI=confidence interval"
  )

result_per_protocol

result_per_protocol %>% gtsave(paste0(output_dir, "per_protocol_result_table.rtf")) 


result_sensitivity <- result_table %>%
  filter(grepl("sensitivity", Name) & !grepl("protocol", Name)) %>%
  mutate(Name = case_when(
    Name == "time_total_documentation - sensitivity" ~ "Documentation time",
    Name == "time_total - sensitivity" ~ "Consultation time",
    Name == "note_length_s - sensitivity" ~ "Subjective",
    Name == "note_length_o - sensitivity" ~ "Objective",
    Name == "note_length_a - sensitivity" ~ "Assessment",
    Name == "note_length_p - sensitivity" ~ "Plan",
    Name == "note_number_signs - sensitivity" ~ "Signs",
    Name == "note_number_context - sensitivity" ~ "Context",
    Name == "note_number_symptoms - sensitivity" ~ "Symptoms",
    Name == "note_number_diagnosis - sensitivity" ~ "Diagnosis",
    Name == "note_number_plan - sensitivity" ~ "Plan "
  )) %>%
  gt() %>%
  tab_row_group(label="Number of variables in note", rows = c(6:10)) %>%
  tab_row_group(label="Note length", rows = c(2:5)) %>%
  tab_row_group(label="Time outcomes", rows = c(1)) %>%
  tab_footnote("Difference on an additive scale", locations = cells_row_groups(groups = c("Time outcomes"))) %>%
  tab_footnote("Difference on a multiplicative scale", locations = cells_row_groups(groups = c("Note length", "Number of variables in note"))) %>%
  tab_footnote(
    "CI=confidence interval"
  )

result_sensitivity

result_sensitivity %>% gtsave(paste0(output_dir, "sensitivity_result_table.rtf")) 

result_icpc <- result_table %>%
  filter(grepl("icpc", Name)) %>%
  mutate(Name = case_when(
    Name == "time_total_documentation- icpc" ~ "Documentation time",
    Name == "time_total- icpc" ~ "Consultation time"
  )) %>%
  gt() %>%
  tab_row_group(label="Time outcomes", rows = c(1:2)) %>%
  tab_footnote("Difference on an additive scale", locations = cells_row_groups(groups = c("Time outcomes"))) %>%
  tab_footnote(
    "CI=confidence interval"
  )

result_icpc

result_icpc %>% gtsave(paste0(output_dir, "icpc_result_table.rtf")) 
