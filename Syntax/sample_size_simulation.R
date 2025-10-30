library(tidyverse)
library(lme4)
library(lmerTest)
library(DHARMa)
set.seed(2025)

consultations <- seq(15, 75, by=15)
gps <- seq(2, 40, by=2)
consultation <- list(c(16, 7))
admin <- list(c(2.7, 1.5))
gp_re_doc <- c(0.5)
gp_re_total <- c(2)
effect <- c(0.75, 1)
min_time <- c(1/6)

options <- expand_grid(consultations, gps, consultation, admin, gp_re_doc, gp_re_total, effect)

create_data <-
  function(no_consultations,
           no_gp,
           admin_mean,
           admin_sd,
           total_mean,
           total_sd,
           gp_re_doc,
           gp_re_total,
           effect) {
    gp <- rep(1:no_gp, each = no_consultations)
    gp_res <- rep(rnorm(no_gp, 0, 1), each = no_consultations)
    
    df_base <-
      bind_cols(list(
        "base_doc_time" = rnorm(no_consultations * no_gp, admin_mean, admin_sd),
        "base_total_time" = rnorm(no_consultations * no_gp, total_mean, total_sd),
        "gp" = gp,
        "gp_time" = gp_res
      ))     %>%
      mutate(doc_time = pmax(base_doc_time + gp_re_doc, min_time),
             total_time = pmax(base_total_time + gp_re_total, min_time),
             exposure = 0)
    
    df_inter <-
      bind_cols(list(
        "base_doc_time" = rnorm(no_consultations * no_gp, admin_mean, admin_sd),
        "base_total_time" = rnorm(no_consultations * no_gp, total_mean, total_sd),
        "gp" = gp,
        "gp_time" = gp_res
      )) %>%
      mutate(doc_time = pmax(base_doc_time + gp_re_doc - effect, min_time),
             total_time = pmax(base_total_time + gp_re_total - effect, min_time),
             exposure = 1)
    
    return(bind_rows (df_base, df_inter))
  }

simulations <- 1000

total_results <- tibble()

for (r in 1:nrow(options)) {
  results_doc <- tibble()
  results_total <- tibble()
  row <- options[r, ]
  for (s in 1:simulations) {
    print(paste0(r, " - ", s))
    data <-
      create_data(row$consultations, 
                  row$gps,
                  row$admin[[1]][[1]],
                  row$admin[[1]][[2]],
                  row$consultation[[1]][[1]],
                  row$consultation[[1]][[2]],
                  row$gp_re_doc,
                  row$gp_re_total,
                  row$effect)
    analysis_doc <- lmer(doc_time ~ exposure + (1|gp), data = data)
    analysis_total <- lmer(total_time ~ exposure + (1|gp), data = data)
    results_doc <- bind_rows(results_doc, summary(analysis_doc)$coefficients["exposure",])
    results_total <- bind_rows(results_total, summary(analysis_total)$coefficients["exposure",])
  }
  results <- results_doc %>% mutate(type = "doc") %>%
    bind_rows(results_total %>% mutate(type = "total"))
  
  results <- results %>% group_by(type) %>%
    summarise(power = mean(`Pr(>|t|)` < 0.05),
              estimate = mean(Estimate))
  combined_results <- bind_cols(row, results)
  total_results <- bind_rows(total_results, combined_results)
}

ggplot(data=total_results, aes(x=consultations, y=power, colour=as.factor(gps))) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(type, effect))

saveRDS(total_results, "total_results.Rds")