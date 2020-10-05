# @@@@@@@@@@@@@
# Load data----
# @@@@@@@@@@@@@


us_scripts <- list(
  "libraries.R",
  "functions_analysis.R",
  "us_preprocess.R"
)

library(here)
# Ode to the here https://github.com/jennybc/here_here

# Load libraries, get data and preprocess
lapply(us_scripts, function(x) source(here("code", "scripts", x)))

# Run analyses
to_run <- grep("secondary|strata", list.files(here("code", "scripts")), value = TRUE)
lapply(to_run, function(x) source(here("code", "scripts", x)))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# all set 15 analyses: medically selected primary ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

MRR_all_selected <- MRR_strata_selected_3_today %>%
  select(-weighted_variance) %>%
  bind_rows(MMR_secondary_selected) %>%
  unite(id, c("type_pp", "adjustment", "nyc_removed"), remove = FALSE)

print(MRR_all_selected)

MRR_date <- MRR_strata_selected_3 %>%
  filter(analysis_group == "dates") %>%
  mutate(date = ymd(date))

print(MRR_date)

MRR_cases <- MRR_strata_selected_3 %>%
  filter(analysis_group == "cases")

print(MRR_cases)

# @@@@@@@@@@@@@@@@@@
# all secondary ----
# @@@@@@@@@@@@@@@@@@

MRR_all_original <- MRR_strata_original_3_today %>%
  select(-weighted_variance) %>%
  bind_rows(MMR_secondary_original) %>%
  filter(type_pp != "multivar") %>%
  unite(id, c("type_pp", "adjustment", "nyc_removed"), remove = FALSE)

MRR_all_rf_pars <- MRR_strata_rf_3_today_pars %>%
  select(-weighted_variance) %>%
  # filter(analysis_group != "dates") %>%
  bind_rows(MMR_secondary_rf_pars) %>%
  filter(type_pp != "multivar") %>%
  unite(id, c("type_pp", "adjustment", "nyc_removed"), remove = FALSE)

MRR_all_rf_inclus <- MRR_strata_rf_3_today_inclus %>%
  select(-weighted_variance) %>%
  bind_rows(MMR_secondary_rf_inclus) %>%
  filter(type_pp != "multivar") %>%
  unite(id, c("type_pp", "adjustment", "nyc_removed"), remove = FALSE)


all_secondary <- MRR_all_original %>%
  mutate(set = "Knowledge-based (set 40)") %>%  
  
  bind_rows(MRR_all_rf_pars %>%
              mutate(set = "Agnostic (set 12)")) %>% 
  
  bind_rows(MRR_all_rf_inclus %>% 
              mutate(set = "Agnostic (set 38)")) %>% 
  
  mutate(set = factor(set, levels = c("Knowledge-based (set 40)", "Agnostic (set 12)", "Agnostic (set 38)"))) %>% 
  
  mutate(
    type_pp = factor(type_pp, levels = c("continuous", "quintile", "tertile", "strata_3")),
    adjustment = factor(adjustment, levels = c("state", "divname")),
    nyc_removed = factor(as.character(nyc_removed), levels = c("TRUE", "FALSE"))
  )

print(all_secondary)

# @@@@@@@@@
# RMSE ----
# @@@@@@@@@

source(here("code", "scripts", "extra_analyses.R"))

print(all_results_folds)
