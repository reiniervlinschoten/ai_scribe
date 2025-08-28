rm(list = ls())

library(glue)
library(lme4)
library(lmerTest)
library(tidyverse)
library(DHARMa)
library(gtable)

date <- "20250717"
working_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Data/")
output_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Output/")
dir.create(output_dir, showWarnings = F)
residuals_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Output/residuals/")
dir.create(residuals_dir, showWarnings = F)

# Load data
outcomes_consultations <- readRDS(file = file.path(working_dir, "Processed", "outcomes_consultations.Rds"))
outcomes_experience <- readRDS(file = file.path(working_dir, "Processed", "outcomes_experience.Rds"))
outcomes_utaut <- readRDS(file = file.path(working_dir, "Processed", "outcomes_utaut.Rds"))

# Per protocol
outcomes_consultations_pp <- outcomes_consultations %>%
  filter(period == "Baseline" | consultation_used == "Yes")

result_table <- tibble()

analysis_function <- function(outcome, dataset, name) {
  # First normal analysis
  analysis <- lmer(as.formula(
    paste(
      outcome,
      "~ period + consultation_planned_time + (1 | participant)"
    )
  ), data = dataset)
  analysis_outcome <- as_tibble_row(summary(analysis)$coefficients["periodIntervention", c("Estimate", "Pr(>|t|)")])
  analysis_confint <- as_tibble_row(confint(
    analysis,
    parm = c("periodIntervention"),
    method = "boot",
    .progress = "txt",
    seed = "2025"
  ))
  analysis_results <- bind_cols("Name" = outcome, analysis_outcome, analysis_confint)
  result_table <- analysis_results
  
  png(file = paste0(residuals_dir, "residuals_", outcome, ".png"))
  simulateResiduals(analysis, plot = T)
  dev.off()
  
  # Sensitivity but not when total time
  if (outcome != "time_total" & !grepl("per_protocol", name)) {
    analysis_sens <- lmer(as.formula(
      paste(
        outcome,
        "~ period + consultation_planned_time + I(time_non_documentation/100) + (1 | participant)"
      )
    ), data = dataset)
    analysis_outcome_sens <- as_tibble_row(summary(analysis_sens)$coefficients["periodIntervention", c("Estimate", "Pr(>|t|)")])
    analysis_confint_sens <- as_tibble_row(confint(
      analysis_sens,
      parm = c("periodIntervention"),
      method = "boot",
      .progress = "txt",
      seed = "2025"
    ))
    analysis_results_sens <- bind_cols(
      "Name" = paste(outcome, "- sensitivity"),
      analysis_outcome_sens,
      analysis_confint_sens
    )
    result_table <- bind_rows(result_table, analysis_results_sens)
    
    png(file = paste0(residuals_dir, "residuals_", outcome, "_sens.png"))
    simulateResiduals(analysis_sens, plot = T)
    dev.off()
  }
  return(result_table)
}


analysis_function_nb <- function(outcome, dataset, name) {
  print(outcome)
  # First normal analysis
  analysis <- glmer.nb(as.formula(
    paste(
      outcome,
      "~ period + consultation_planned_time + (1 | participant)"
    )
  ), data = dataset)
  analysis_outcome <- as_tibble_row(summary(analysis)$coefficients["periodIntervention", c("Estimate", "Pr(>|z|)")])
  analysis_confint <- as_tibble_row(confint(
    analysis,
    parm = c("periodIntervention"),
    method = "boot",
    .progress = "txt",
    seed = "2025"
  ))
  analysis_results <- bind_cols("Name" = outcome, analysis_outcome, analysis_confint)
  result_table <- analysis_results
  
  png(file = paste0(residuals_dir, "residuals_", outcome, ".png"))
  simulateResiduals(analysis, plot = T)
  dev.off()
  
  # Sensitivity but not when total time
  if (outcome != "time_total"  & !grepl("per_protocol", name)) {
    print("sens")
    analysis_sens <- glmer.nb(as.formula(
      paste(
        outcome,
        "~ period + consultation_planned_time + I(time_non_documentation/100) + (1 | participant)"
      )
    ), data = dataset)
    analysis_outcome_sens <- as_tibble_row(summary(analysis_sens)$coefficients["periodIntervention", c("Estimate", "Pr(>|z|)")])
    analysis_confint_sens <- as_tibble_row(confint(
      analysis_sens,
      parm = c("periodIntervention"),
      method = "boot",
      .progress = "txt",
      seed = "2025"
    ))
    analysis_results_sens <- bind_cols(
      "Name" = paste(outcome, "- sensitivity"),
      analysis_outcome_sens,
      analysis_confint_sens
    )
    result_table <- bind_rows(result_table, analysis_results_sens)
    
    png(file = paste0(residuals_dir, "residuals_", outcome, "_sens.png"))
    simulateResiduals(analysis_sens, plot = T)
    dev.off()
  }
  
  result_table <- result_table %>%
    mutate(across(c(-Name, -`Pr(>|z|)`), ~ exp(.x)))
  return(result_table)
}


analysis_function_experience <- function(outcome, dataset) {
  print(outcome)
  # First normal analysis
  analysis <- lmer(as.formula(
    paste(
      outcome,
      "~ period + (1 | participant)"
    )
  ), data = dataset)
  analysis_outcome <- as_tibble_row(summary(analysis)$coefficients["periodIntervention", c("Estimate", "Pr(>|t|)")])
  analysis_confint <- as_tibble_row(confint(
    analysis,
    parm = c("periodIntervention"),
    method = "boot",
    .progress = "txt",
    seed = "2025"
  ))
  analysis_results <- bind_cols("Name" = outcome, analysis_outcome, analysis_confint)
  result_table <- analysis_results
  
  png(file = paste0(residuals_dir, "residuals_", outcome, ".png"))
  simulateResiduals(analysis, plot = T)
  dev.off()
  
  return(result_table)
}

analysis_function_icpc <- function(outcome, dataset, name) {
  # First normal analysis
  analysis <- lmer(as.formula(
    paste(
      outcome,
      "~ period + consultation_planned_time + (1 | participant) + (1 | re_icpc)"
    )
  ), data = dataset)
  analysis_outcome <- as_tibble_row(summary(analysis)$coefficients["periodIntervention", c("Estimate", "Pr(>|t|)")])
  analysis_confint <- as_tibble_row(confint(
    analysis,
    parm = c("periodIntervention"),
    method = "boot",
    .progress = "txt",
    seed = "2025"
  ))
  analysis_results <- bind_cols("Name" = paste0(outcome, "- icpc"), analysis_outcome, analysis_confint)
  result_table <- analysis_results
  
  png(file = paste0(residuals_dir, "residuals_", outcome, "_icpc.png"))
  simulateResiduals(analysis, plot = T)
  dev.off()
  
  return(result_table)
}

outcomes <- names(outcomes_consultations %>% dplyr::select(time_total_documentation, time_total))

outcomes_nb <- names(outcomes_consultations %>% dplyr::select(starts_with("note_")))

outcomes_exp <- names(outcomes_experience %>% dplyr::select(starts_with("peq_")))

# Intention to treat
for (outcome in outcomes) {
  result_table <- bind_rows(result_table, analysis_function(outcome, outcomes_consultations, "intention_to_treat"))
}

saveRDS(result_table,
        file = file.path(output_dir, "analyses_itt.Rds"))

for (outcome in outcomes_nb) {
  result_table <- bind_rows(result_table, analysis_function_nb(outcome, outcomes_consultations, "intention_to_treat"))
}

saveRDS(result_table,
        file = file.path(output_dir, "analyses_itt_nb.Rds"))

# Per protocol
for (outcome in outcomes) {
  result_table <- bind_rows(result_table, analysis_function(outcome, outcomes_consultations_pp, "per_protocol") %>%
                              mutate(Name = paste0(Name, "_per_protocol")))
}

saveRDS(result_table,
        file = file.path(output_dir, "analyses_itt_nb_pp.Rds"))

for (outcome in outcomes_nb) {
  result_table <- bind_rows(result_table, analysis_function_nb(outcome, outcomes_consultations_pp, "per_protocol") %>%
                              mutate(Name = paste0(Name, "_per_protocol")))
}

saveRDS(result_table,
        file = file.path(output_dir, "analyses_itt_nb_pp_nb.Rds"))

# patient experience
for (outcome in outcomes_exp) {
  result_table <- bind_rows(result_table, analysis_function_experience(outcome, outcomes_experience))
}

saveRDS(result_table,
        file = file.path(output_dir, "analyses_itt_nb_pp_nb_outcomes.Rds"))

# icpc adjusted
for (outcome in outcomes) {
  result_table <- bind_rows(result_table, analysis_function_icpc(outcome, outcomes_consultations, "_icpc"))
}

saveRDS(result_table,
        file = file.path(output_dir, "analyses_complete.Rds"))


