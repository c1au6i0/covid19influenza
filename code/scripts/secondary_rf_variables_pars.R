# us_scripts <- list(
#   "libraries.R",
#   "functions_analysis.R",
#   "functions_graph.R",
#   "us_preprocess.R"
# )
# #
# # # Ode to the here https://github.com/jennybc/here_here
# library(here)
# lapply(us_scripts, function(x) source(here("code", "scripts","county_level", x)))

mess <- "\nPerforming secondary analyses on rf!\n"
sep_mess <- paste(rep.int("=", nchar(mess)), collapse = "")

message(paste0(sep_mess, mess, sep_mess))


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set some general parameters and dataframe to use---------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# dataframe to use
data_to_use <- dat_rf_pars

# dates
date_tostudy <- max(data_to_use$date)

# variables used
variables_used <- "rf_Pars"

# variables
XX <- grep("XX", names(data_to_use), value = TRUE)
# XX <- XX[XX != "XX_total_beds"]

# formula for propensity score
form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")

# min number of cases to be included
min_filt <- 1

# Dry? no, definitely WET :(

# For the Propensity score
rf_today_1case_pars <- data_to_use %>%
  filter(date == max(date)) %>%
  filter(NC_cases >= min_filt)

form_ps <- reformulate(termlabels = XX, response = "logitZZ_perc_imm65")
prop_scores_rf_pars <- lm(form_ps, data = rf_today_1case_pars)
tidy(prop_scores_rf_pars)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# US: Quintiles
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

pp_split_quint <- 5

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Quintile divname-----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

quint_divname_rf_pars <- glm_pp(
  dat = data_to_use,
  dates_tostudy = date_tostudy,
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  nation = "us",
  pp_split = pp_split_quint,
  inter = FALSE,
  adjust = c("divname")
)

quint_divname_rf_pars <- extract_model(quint_divname_rf_pars, filt_cases = min_filt)
tidy(quint_divname_rf_pars)

quint_divname_rf_pars <- calculate_MRR(tidy(quint_divname_rf_pars), 10) %>%
  mutate(adjustment = "divname", nyc_removed = FALSE, type_pp = "quintile")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Quintile state-----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

quint_state_rf_pars <- glm_pp(
  dat = data_to_use,
  dates_tostudy = date_tostudy,
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  nation = "us",
  pp_split = pp_split_quint,
  inter = FALSE,
  adjust = c("state")
)

quint_state_rf_pars <- extract_model(quint_state_rf_pars, filt_cases = min_filt)

tidy(quint_state_rf_pars)

quint_state_rf_pars <- calculate_MRR(tidy(quint_state_rf_pars), 10) %>%
  mutate(adjustment = "state", nyc_removed = FALSE, type_pp = "quintile")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Quintile divname no NYC -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

quint_divname_rf_nonyc_pars <-

  data_to_use %>%
  filter(!fips %in% !!fips_nyc) %>%
  glm_pp(
    dat = .,
    dates_tostudy = date_tostudy,
    filt_cases = min_filt,
    form = form_ps,
    offset_f = "log(NP_total_pop)",
    var_dep = "YY_deaths",
    var_int = "ZZ_perc_imm65",
    include_model = TRUE,
    verbose = TRUE,
    nation = "us",
    pp_split = pp_split_quint,
    inter = FALSE,
    adjust = c("divname")
  )


quint_divname_rf_nonyc_pars <- extract_model(quint_divname_rf_nonyc_pars, filt_cases = min_filt)

quint_divname_rf_nonyc_pars <- calculate_MRR(tidy(quint_divname_rf_nonyc_pars), 10) %>%
  mutate(adjustment = "divname", nyc_removed = TRUE, type_pp = "quintile")

# lincom(us_perc_imm65_deahts_pop_quint_nonyc, "ZZ_perc_imm65", eform = TRUE)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Quintile state no NYC -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

quint_state_rf_nonyc_pars <-

  data_to_use %>%
  filter(!fips %in% !!fips_nyc) %>%
  glm_pp(
    dat = .,
    dates_tostudy = date_tostudy,
    filt_cases = min_filt,
    form = form_ps,
    offset_f = "log(NP_total_pop)",
    var_dep = "YY_deaths",
    var_int = "ZZ_perc_imm65",
    include_model = TRUE,
    verbose = TRUE,
    nation = "us",
    pp_split = pp_split_quint,
    inter = FALSE,
    adjust = c("state")
  )


quint_state_rf_nonyc_pars <- extract_model(quint_state_rf_nonyc_pars, filt_cases = min_filt)

quint_state_rf_nonyc_pars <- calculate_MRR(tidy(quint_state_rf_nonyc_pars), 10) %>%
  mutate(adjustment = "state", nyc_removed = TRUE, type_pp = "quintile")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# US: Tertiles
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

pp_split_tert <- 3

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# tertile divname-----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

tert_divname_rf_pars <- glm_pp(
  dat = data_to_use,
  dates_tostudy = date_tostudy,
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  nation = "us",
  pp_split = pp_split_tert,
  inter = FALSE,
  adjust = c("divname")
)

tert_divname_rf_pars <- extract_model(tert_divname_rf_pars, filt_cases = min_filt)
tidy(tert_divname_rf_pars)

tert_divname_rf_pars <- calculate_MRR(tidy(tert_divname_rf_pars), 10) %>%
  mutate(adjustment = "divname", nyc_removed = FALSE, type_pp = "tertile")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# tertile state-----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

tert_state_rf_pars <- glm_pp(
  dat = data_to_use,
  dates_tostudy = date_tostudy,
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  nation = "us",
  pp_split = pp_split_tert,
  inter = FALSE,
  adjust = c("state")
)

tert_state_rf_pars <- extract_model(tert_state_rf_pars, filt_cases = min_filt)

tert_state_rf_pars <- calculate_MRR(tidy(tert_state_rf_pars), 10) %>%
  mutate(adjustment = "state", nyc_removed = FALSE, type_pp = "tertile")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# tertile divname no NYC -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

tert_divname_rf_nonyc_pars <-

  data_to_use %>%
  filter(!fips %in% !!fips_nyc) %>%
  glm_pp(
    dat = .,
    dates_tostudy = date_tostudy,
    filt_cases = min_filt,
    form = form_ps,
    offset_f = "log(NP_total_pop)",
    var_dep = "YY_deaths",
    var_int = "ZZ_perc_imm65",
    include_model = TRUE,
    verbose = TRUE,
    nation = "us",
    pp_split = pp_split_tert,
    inter = FALSE,
    adjust = c("divname")
  )


tert_divname_rf_nonyc_pars <- extract_model(tert_divname_rf_nonyc_pars, filt_cases = min_filt)

tert_divname_rf_nonyc_pars <- calculate_MRR(tidy(tert_divname_rf_nonyc_pars), 10) %>%
  mutate(adjustment = "divname", nyc_removed = TRUE, type_pp = "tertile")

# lincom(us_perc_imm65_deahts_pop_tert_nonyc, "ZZ_perc_imm65", eform = TRUE)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# tertile state no NYC -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

tert_state_rf_nonyc_pars <-

  data_to_use %>%
  filter(!fips %in% !!fips_nyc) %>%
  glm_pp(
    dat = .,
    dates_tostudy = date_tostudy,
    filt_cases = min_filt,
    form = form_ps,
    offset_f = "log(NP_total_pop)",
    var_dep = "YY_deaths",
    var_int = "ZZ_perc_imm65",
    include_model = TRUE,
    verbose = TRUE,
    nation = "us",
    pp_split = pp_split_tert,
    inter = FALSE,
    adjust = c("state")
  )


tert_state_rf_nonyc_pars <- extract_model(tert_state_rf_nonyc_pars, filt_cases = min_filt)

tert_state_rf_nonyc_pars <- calculate_MRR(tidy(tert_state_rf_nonyc_pars), 10) %>%
  mutate(adjustment = "state", nyc_removed = TRUE, type_pp = "tertile")


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# US: Continuous
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

pp_split_cont <- 0


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# PS contineous divname----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

cont_divname_rf_pars <- glm_pp(
  dat = data_to_use,
  dates_tostudy = date_tostudy,
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  pp_split = pp_split_cont,
  nation = "us",
  adjust = c("divname")
)

cont_divname_rf_pars <- extract_model(cont_divname_rf_pars, filt_cases = min_filt)

cont_divname_rf_pars <- calculate_MRR(tidy(cont_divname_rf_pars), 10) %>%
  mutate(adjustment = "divname", nyc_removed = FALSE, type_pp = "continuous")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# PS contineous divname with interaction----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

cont_divname_rf_inter_pars <- glm_pp(
  dat = data_to_use,
  dates_tostudy = max(date_tostudy),
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  pp_split = pp_split_cont,
  nation = "us",
  adjust = c("divname"),
  inter = TRUE
)

cont_divname_rf_inter_pars <- extract_model(cont_divname_rf_inter_pars, filt_cases = min_filt)
summary(cont_divname_rf_inter_pars)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# PS contineous state----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

cont_state_rf_pars <- glm_pp(
  dat = data_to_use,
  dates_tostudy = date_tostudy,
  filt_cases = min_filt,
  form = form_ps,
  offset_f = "log(NP_total_pop)",
  var_dep = "YY_deaths",
  var_int = "ZZ_perc_imm65",
  include_model = TRUE,
  verbose = TRUE,
  pp_split = pp_split_cont,
  nation = "us",
  adjust = c("state")
)

cont_state_rf_pars <- extract_model(cont_state_rf_pars, filt_cases = min_filt)

cont_state_rf_pars <- calculate_MRR(tidy(cont_state_rf_pars), 10) %>%
  mutate(adjustment = "state", nyc_removed = FALSE, type_pp = "continuous")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# PS contineous divname Without NYC----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

cont_divname_rf_nonyc_pars <-
  data_to_use %>%
  filter(!fips %in% !!fips_nyc) %>%
  glm_pp(
    dat = .,
    dates_tostudy = date_tostudy,
    filt_cases = min_filt,
    form = form_ps,
    offset_f = "log(NP_total_pop)",
    var_dep = "YY_deaths",
    var_int = "ZZ_perc_imm65",
    include_model = TRUE,
    verbose = TRUE,
    pp_split = pp_split_cont,
    nation = "us",
    adjust = c("divname")
  )

cont_divname_rf_nonyc_pars <- extract_model(cont_divname_rf_nonyc_pars, filt_cases = min_filt)

cont_divname_rf_nonyc_pars <- calculate_MRR(tidy(cont_divname_rf_nonyc_pars), 10) %>%
  mutate(adjustment = "divname", nyc_removed = TRUE, type_pp = "continuous")

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# PS contineous state Without NYC----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

cont_state_rf_nonyc_pars <-
  data_to_use %>%
  filter(!fips %in% !!fips_nyc) %>%
  glm_pp(
    dat = .,
    dates_tostudy = date_tostudy,
    filt_cases = min_filt,
    form = form_ps,
    offset_f = "log(NP_total_pop)",
    var_dep = "YY_deaths",
    var_int = "ZZ_perc_imm65",
    include_model = TRUE,
    verbose = TRUE,
    pp_split = pp_split_cont,
    nation = "us",
    adjust = c("state")
  )

cont_state_rf_nonyc_pars <- extract_model(cont_state_rf_nonyc_pars, filt_cases = min_filt)

cont_state_rf_nonyc_pars <- calculate_MRR(tidy(cont_state_rf_nonyc_pars), 10) %>%
  mutate(adjustment = "state", nyc_removed = TRUE, type_pp = "continuous")

MMR_secondary_rf_binded_pars <- bind_rows(
  quint_divname_rf_pars,
  quint_state_rf_pars,
  quint_divname_rf_nonyc_pars,
  quint_state_rf_nonyc_pars,

  cont_divname_rf_pars,
  cont_state_rf_pars,
  cont_divname_rf_nonyc_pars,
  cont_state_rf_nonyc_pars,
)

MMR_secondary_rf_pars <- MMR_secondary_rf_binded_pars %>%
  filter(term == "ZZ_perc_imm65") %>%
  mutate(
    date = !!date_freeze,
    variables_used = !!variables_used,
    filt = !!min_filt,
    analysis_group = "secondary"
  ) %>%
  relocate(c("date", "variables_used", "filt", "adjustment", "analysis_group", "nyc_removed", "type_pp"),
    .before = "term"
  )


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# MRR only done for tertiles (but if we add here we have the rest)

MMR_secondary_rf_binded_pars <- bind_rows(
  quint_divname_rf_pars,
  quint_divname_rf_nonyc_pars,
  quint_state_rf_pars,
  quint_state_rf_nonyc_pars,

  tert_divname_rf_pars,
  tert_divname_rf_nonyc_pars,
  tert_state_rf_pars,
  tert_state_rf_nonyc_pars,

  cont_divname_rf_pars,
  cont_divname_rf_nonyc_pars,
  cont_state_rf_pars,
  cont_state_rf_nonyc_pars,
)

MMR_secondary_rf_pars <- MMR_secondary_rf_binded_pars %>%
  filter(term == "ZZ_perc_imm65") %>%
  mutate(
    date = !!date_freeze,
    variables_used = !!variables_used,
    filt = !!min_filt,
    analysis_group = "secondary"
  ) %>%
  relocate(c("date", "variables_used", "filt", "adjustment", "analysis_group", "nyc_removed", "type_pp"),
    .before = "term"
  )
