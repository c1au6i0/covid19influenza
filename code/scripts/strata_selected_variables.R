mess <- "\nAnalyzing Strata selected!\n"
sep_mess <- paste(rep.int("=", nchar(mess)), collapse = "")

message(paste0(sep_mess, mess, sep_mess))


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set some general parameters and dataframe to use---------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data_to_use <- dat_selected

dates_tostudy <- sort(unique(data_to_use$date))
dates_tostudy <- dates_tostudy[(length(dates_tostudy) - 60):length(dates_tostudy)]
to_filt_cases <- 1:25
XX <- grep("XX", names(data_to_use), value = TRUE)
min_filt <- 1
variables_used <- "selected"

# Dry? no, definitely WET :(

# @@@@@@@@@@@@@@@@@@@@@@@
# TERTILE ---------------
# @@@@@@@@@@@@@@@@@@@@@@@

# formula for propensity score
form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

number_strata_tertiles <- 3 # tertiles

# @@@@@@@@@@@@@@@
# state by date
# @@@@@@@@@@@@@@

form_state <- as.formula(YY_deaths ~ ZZ_perc_imm65 + PP + state)

strata_states_selected_dates_3 <- map_dfr(dates_tostudy, strata_glm_pp,
  dat = data_to_use,
  filt_c = 1,
  form_ps = form_ps,
  form_glm = form_state,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "state_dates")


# @@@@@@@@@@@@@@@
# state by case
# @@@@@@@@@@@@@@

strata_states_selected_cases_3 <- map_dfr(to_filt_cases, strata_glm_pp,
  dat = data_to_use,
  date_ts = max(dates_tostudy),
  form_ps = form_ps,
  form_glm = form_state,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "state_cases")


# @@@@@@@@@@@@@@@
# state w/o NYC
# @@@@@@@@@@@@@@

strata_states_selected_nonyc_3 <- strata_glm_pp(
  dat = data_to_use,
  date_ts = max(dates_tostudy),
  form_ps = form_ps,
  form_glm = form_state,
  number_strata = number_strata_tertiles,
  filt_c = 1,
  nonyc = TRUE
) %>%
  mutate(analysis = "state_nonyc")


# @@@@@@@@@@@@@@@@@
# divname by date
# @@@@@@@@@@@@@@@@@

form_divname <- as.formula(YY_deaths ~ ZZ_perc_imm65 + PP + divname)

strata_divname_selected_dates_3 <- map_dfr(dates_tostudy, strata_glm_pp,
  dat = data_to_use,
  filt_c = 1,
  form_ps = form_ps,
  form_glm = form_divname,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "divname_dates")


# @@@@@@@@@@@@@@@@@
# divname by case
# @@@@@@@@@@@@@@@@@

strata_divname_selected_cases_3 <- map_dfr(to_filt_cases, strata_glm_pp,
  dat = data_to_use,
  date_ts = max(dates_tostudy),
  form_ps = form_ps,
  form_glm = form_divname,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "divname_cases")


# @@@@@@@@@@@@@@@@@
# divname w/o NYC
# @@@@@@@@@@@@@@@@@

strata_divname_selected_nonyc_3 <- strata_glm_pp(
  dat = data_to_use,
  date_ts = max(dates_tostudy),
  form_ps = form_ps,
  form_glm = form_divname,
  filt_c = 1,
  nonyc = TRUE,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "divname_nonyc")

# @@@@@@@@@@@@@@@@@
# NO adjustment
# @@@@@@@@@@@@@@@@@

form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

form_x <- as.formula(YY_deaths ~ ZZ_perc_imm65 + PP)

strata_noadj_original_3 <- strata_glm_pp(
  dat = data_to_use,
  date_ts = max(dates_tostudy),
  form_ps = form_ps,
  form_glm = form_x,
  filt_c = 1,
  nonyc = FALSE,
  number_strata = 3
) %>%
  mutate(analysis = "state_nonyc")

strata_weights_MRR(strata_noadj_original_3)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Results in individual strata 1 case and last date (divname)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

strata_divname_selected_1case_3 <- map_dfr(1, strata_glm_pp,
  dat = data_to_use,
  date_ts = max(dates_tostudy),
  form_ps = form_ps,
  form_glm = form_divname,
  number_strata = number_strata_tertiles
) %>%
  mutate(analysis = "divname_1case")


strata_divname_selected_1case_3[, c("data", "model")] <- NULL

individual_strata <- strata_divname_selected_1case_3 %>%
  filter(term %in% c("(Intercept)", "ZZ_perc_imm65", "PP"))

individual_strata[, c(1, 8:10)] <- NULL

# @@@@@@@@@@@@@@@@@
# Calculate MRR
# @@@@@@@@@@@@@@@@@

strata_selected_results_3 <- bind_rows(
  strata_states_selected_dates_3,
  strata_states_selected_cases_3,
  strata_states_selected_nonyc_3,
  strata_divname_selected_dates_3,
  strata_divname_selected_cases_3,
  strata_divname_selected_nonyc_3
)

strata_selected_results_splitted_3 <- strata_selected_results_3 %>%
  unite("date_filt_analysis", c("date", "filt_cases_c", "analysis")) %>%
  select(-model, -data) %>%
  split.data.frame(., .$date_filt_analysis)


MRR_strata_selected_3 <- map_dfr(strata_selected_results_splitted_3, strata_weights_MRR) %>%
  relocate(c("date_filt_analysis", "term"), .before = estimate) %>%
  separate(date_filt_analysis, into = c("date", "filt", "adjustment", "analysis_group"), sep = "_") %>%
  mutate(filt = as.numeric(filt)) %>%
  mutate(nyc_removed = if_else(analysis_group == "nonyc", TRUE, FALSE)) %>%
  relocate(nyc_removed, .before = term) %>%
  mutate(variables_used = !!variables_used) %>%
  relocate(variables_used, .after = date) %>%
  relocate(type_pp, .before = term)

## The last date with one case
MRR_strata_selected_3_today <- MRR_strata_selected_3 %>%
  filter(analysis_group != "dates") %>%
  filter(date == max(date)) %>%
  filter(filt == 1)
