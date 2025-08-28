rm(list = ls())

library(tidyverse)
library(glue)

date <- "20250717"
working_dir <- glue("C:/Users/r.vanlinschoten/Documents/AI Scribe/{date} Data/")

dataset <- list()
dataset[["reports"]][["consultations"]] <- read_delim(glue("{working_dir}AI_Scribe_Consultation_export_{date}.csv"),
                                                      show_col_types = F)
dataset[["surveys"]][["experiences_dutch"]] = read_delim(
  glue(
    "{working_dir}AI_Scribe_Ervaringen_vragenlijst_export_{date}.csv"
  ),
  show_col_types = F
)
dataset[["surveys"]][["experiences_eng"]] <- read_delim(
  glue(
    "{working_dir}AI_Scribe_Experience_questionnaire_export_{date}.csv"
  ),
  show_col_types = F
)
dataset[["study"]] <- read_delim(glue("{working_dir}AI_Scribe_export_{date}.csv"),
                                 show_col_types = F)
dataset[["reports"]][["patient_interview"]] = read_delim(
  glue(
    "{working_dir}AI_Scribe_Patient_interview_export_{date}.csv"
  ),
  show_col_types = F
)
dataset[["surveys"]][["utaut"]] <- read_delim(
  glue(
    "{working_dir}AI_Scribe_UTAUT_vragenlijst_export_{date}.csv"
  ),
  show_col_types = F
)

dataset[["info"]][["field_options"]] <- read_delim(glue("{working_dir}field_options.csv"), show_col_types =
                                                     F)
dataset[["info"]][["survey_vars"]] <- read_delim(glue("{working_dir}survey_variablelist.csv"),
                                                 show_col_types = F)
dataset[["info"]][["study_vars"]] <- read_delim(glue("{working_dir}study_variablelist.csv"),
                                                show_col_types = F)
dataset[["info"]][["report_vars"]] <- read_delim(glue("{working_dir}survey_variablelist.csv"),
                                                 show_col_types = F)


translate_function <- function(column, language) {
  # Only translate character/factor columns
  if (is.numeric(column)) {
    return(column)
  }
  
  # Get option groups
  eng_options <- dataset$info$survey_vars %>%
    filter(`Variable name` == cur_column()) %>%
    pull(`Optiongroup name`)
  
  dutch_options <- dataset$info$survey_vars %>%
    filter(`Variable name` == sub("eng_", "", cur_column())) %>%
    pull(`Optiongroup name`)
  
  # Get Dutch label to value
  dutch_keys <- dataset$info$field_options %>%
    filter(`Option group name` == dutch_options)
  
  # Get English label to value
  eng_keys <- dataset$info$field_options %>%
    filter(`Option group name` == eng_options)
  
  # Link English to Dutch labels
  key_linked <- full_join(
    dutch_keys,
    eng_keys,
    suffix = c("_nl", "_eng"),
    by = c("Option value")
  )
  
  # Translate the column
  new_column <- sapply(as.vector(column), function(x)
    if (is.na(x)) {
      NA_character_
    }
    else if (language == "nl")
    {
      key_linked %>% filter(`Option name_nl` == x) %>% pull(`Option value`)
    }
    else if (language == "eng") {
      key_linked %>% filter(`Option name_eng` == x) %>% pull(`Option value`)
    })
  return(new_column)
}

# Translate english questionnaire
experiences_eng_new <- dataset$surveys$experiences_eng %>%
  mutate(across(starts_with("eng"), function(x)
    translate_function(x, "eng"))) %>%
  rename_with(function(x)
    gsub("eng_", "", x))

# Translate dutch questionnaire
experiences_dutch_new <- dataset$surveys$experiences_dutch %>%
  mutate(
    across(starts_with("peq") |
             informed_consent |
             store_data |
             contact_again, function(x)
               translate_function(x, "nl"))
  )

# Create one experience questionnaire
dataset$surveys$experiences <- experiences_dutch_new %>%
  bind_rows(experiences_eng_new)

# Remove old questionnaires
dataset$surveys$experiences_dutch <- NULL
dataset$surveys$experiences_eng <- NULL

# Translate utaut to number
dataset$surveys$utaut <- dataset$surveys$utaut %>%
  mutate(
    across(starts_with("utaut"), function(x) translate_function(x, "nl"))
  )

# Set to tibble
dataset$study <- dataset$study %>% as_tibble()
dataset$surveys <- lapply(dataset$surveys, as_tibble)
dataset$reports <- lapply(dataset$reports, as_tibble)

# Save data
dir.create(file.path(working_dir, "Processed"), showWarnings = F)
saveRDS(dataset, file = file.path(working_dir, "Processed", "cleaned_data.Rds"))
