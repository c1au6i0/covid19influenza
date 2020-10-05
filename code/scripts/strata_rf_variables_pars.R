# us_scripts <- list(
#   "libraries.R",
#   "functions_analysis.R",
#   "functions_graph.R",
#   "us_preprocess.R"
# )
#
#
# # Ode to the here https://github.com/jennybc/here_here
# library(here)
# lapply(us_scripts, function(x) source(here("code", "county_level", "scripts", x)))


mess <- "\nAnalyzing Strata rf!\n"
sep_mess <- paste(rep.int("=", nchar(mess)), collapse = "")

message(paste0(sep_mess, mess, sep_mess))


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set some general parameters and dataframe to use---------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data_to_use <- dat_rf_pars

date_tostudy <- max(data_to_use$date)
to_filt_cases <- c(1:20, seq(30, 100, 10))
XX <- grep("XX", names(data_to_use), value = TRUE)
min_filt <- 1
variables_used <- "rf_pars"

# Dry? no, definitely WET :(

# @@@@@@@@@@@@@@@@@@@@@@@@
# TERTILE ---------------
# @@@@@@@@@@@@@@@@@@@@@@@

# formula for propensity score
form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

number_strata_tertiles <- 3 # tertiles

# @@@@@@
# state
# @@@@@@

form_state <- as.formula(YY_deaths ~ ZZ_perc_imm65 + PP + state)

strata_states_rf_3_pars <- strata_glm_pp(
  dat = data_to_use,
  date_ts = date_tostudy,
  filt_c = 1,
  form_ps = form_ps,
  form_glm = form_state,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "state_dates")


# @@@@@@@@@@@@@@@
# state w/o NYC
# @@@@@@@@@@@@@@

strata_states_rf_nonyc_3_pars <- strata_glm_pp(
  dat = data_to_use,
  date_ts = date_tostudy,
  form_ps = form_ps,
  form_glm = form_state,
  filt_c = 1,
  nonyc = TRUE,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "state_nonyc")


# @@@@@@@@
# divname
# @@@@@@@

form_divname <- as.formula(YY_deaths ~ ZZ_perc_imm65 + PP + divname)

strata_divname_rf_3_pars <- strata_glm_pp(
  dat = data_to_use,
  date_ts = date_tostudy,
  filt_c = 1,
  form_ps = form_ps,
  form_glm = form_divname,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "divname_dates")


# @@@@@@@@@@@@@@@@@
# divname w/o NYC
# @@@@@@@@@@@@@@@@@

strata_divname_rf_nonyc_3_pars <- strata_glm_pp(
  dat = data_to_use,
  date_ts = date_tostudy,
  filt_c = 1,
  form_ps = form_ps,
  form_glm = form_divname,
  number_strata = number_strata_tertiles,
  nonyc = TRUE
) %>%
  mutate(analysis = "divname_nonyc")

# @@@@@@@@@@@@@@@@@
# NO adjustment
# @@@@@@@@@@@@@@@@@

form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

form_x <- as.formula(YY_deaths ~ ZZ_perc_imm65 + PP)

strata_noadj_rf_3_pars <- strata_glm_pp(
  dat = data_to_use,
  date_ts = date_tostudy,
  form_ps = form_ps,
  form_glm = form_x,
  filt_c = 1,
  nonyc = FALSE,
  number_strata = 3
) %>%
  mutate(analysis = "state_nonyc")

strata_weights_MRR(strata_noadj_rf_3_pars)

# @@@@@@@@@@@@@@
# Calculate MRR-
# @@@@@@@@@@@@@@

strata_rf_results_3_pars <- bind_rows(
  strata_states_rf_3_pars,
  strata_states_rf_nonyc_3_pars,
  strata_divname_rf_3_pars,
  strata_divname_rf_nonyc_3_pars
)

strata_rf_results_splitted_3_pars <- strata_rf_results_3_pars %>%
  unite("date_filt_analysis", c("date", "filt_cases_c", "analysis")) %>%
  select(-model, -data) %>%
  split.data.frame(., .$date_filt_analysis)


MRR_strata_rf_3_pars <- map_dfr(strata_rf_results_splitted_3_pars, strata_weights_MRR) %>%
  relocate(c("date_filt_analysis", "term"), .before = estimate) %>%
  separate(date_filt_analysis, into = c("date", "filt_cases_c", "adjustment", "analysis_group"), sep = "_") %>%
  mutate(filt_cases_c = as.numeric(filt_cases_c)) %>%
  mutate(nyc_removed = if_else(analysis_group == "nonyc", TRUE, FALSE)) %>%
  relocate(nyc_removed, .before = term) %>%
  mutate(variables_used = !!variables_used) %>%
  relocate(variables_used, .after = date) %>%
  relocate(type_pp, .before = term)


## The last date with one case
MRR_strata_rf_3_today_pars <- MRR_strata_rf_3_pars %>%
  filter(date == max(date)) %>%
  filter(filt_cases_c == 1)
