#' @@@@@@@@@@@@@@@@@@@@@@@
#' US: preprocessing-----
#' @@@@@@@@@@@@@@@@@@@@@@@
# us_scripts <- list(
#   "libraries.R",
#   "functions_analysis.R",
#   "functions_graph.R"
# )
#
# library(here)
# lapply(us_scripts, function(x) source(here("code", "county_level", "scripts", x)))

min_filt <- 1
date_freeze <- "2020-09-20"

fips_nyc <- c(36085, 36061, 36081, 36047, 36005)

# These are the variable that will be selected. Original preprint and second preprint
to_select <- c(
  "date", "county", "state", "fips", "cases", "deaths", "total_pop", "perc_families",
  "perc_family_only_onep", "perc_edu_bachelor", "perc_withinternet", "perc_imm65", "total_beds",
  "ratio_beds", "perc_alzheimer_dementia", "perc_asthma", "perc_atrial_fibrillation",
  "perc_cancer_breast", "perc_cancer_colorectal", "perc_cancer_lung", "perc_ch_obstructive_pulm",
  "perc_chronic_kidney_disease", "perc_depression", "perc_diabetes", "perc_heart_failure",
  "perc_hypertension", "perc_ischemic_heart_disease", "perc_obesity", "perc_rheumatoid_arthritis",
  "perc_stroke", "perc_tobacco_use", "median_income", "pm2.5", "summer_temp", "summer_hum",
  "winter_temp", "winter_hum", "perc_age65_over", "median_age", "sex_ratio", "child_dependency",
  "perc_black", "perc_lat", "perc_white", "perc_asian", "perc_island", "perc_other",
  "perc_two_more_races", "days_f0", "perc_imm65"
)

to_select_2 <- c(
  "date", "county", "state", "fips", "cases", "deaths", "total_pop",
  "perc_imm65",
  ## COVID19 and state related
  "days_f0", # "pop_dens", #"total_tests",
  ## Family and Household related variables
  "perc_withinternet", # "perc_families",
  ## Socioeconomic
  "median_income",
  ## Healthcare related variables
  "annual_wellness_visit", # "ratio_beds",
  ## Education related variables
  "perc_edu_bachelor_higher",
  ## Race related variables
  "perc_black", "perc_lat", "perc_white", # "perc_asian",  "perc_native",
  # "perc_pacific_islander","perc_other_race", "perc_two_more_races",
  ## Demographic variables
  "median_age", # "perc_over65",  "child_dependency", #"sex_ratio",
  ## Medical conditions or diseases: mental health
  # "perc_alzheimer_dementia", #"perc_depression",
  ## Medical conditions or diseases: respiratory
  "perc_ch_obstructive_pulm", # "perc_asthma", # "perc_tobacco_use",
  ## Medical conditions or diseases: heart
  "perc_hypertension", # "perc_ischemic_heart_disease",
  # "perc_atrial_fibrillation",	#"perc_heart_failure",
  ## Medical conditions or diseases: cancer
  # "perc_cancer_all", #"perc_cancer_breast", "perc_cancer_colorectal", "perc_cancer_lung",
  ## Medical conditions or diseases: metabolic
  "perc_diabetes", # "perc_obesity",
  ## Medical conditions or diseases: kidney
  # "perc_chronic_kidney_disease",
  ## Medical conditions or diseases: immunological
  # "perc_rheumatoid_arthritis",
  ## Environmental variables:
  "pm2.5",
  "winter_temp", "winter_hum" # ,"summer_temp", "summer_hum"
)


region_compass_divnumber_divname_ls <- list(
  "Region1_Northeast_Division1_NewEngland" = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont"),
  "Region1_Northeast_Division2_MidAtlantic" = c("New Jersey", "New York", "Pennsylvania"),
  "Region2_Midwest_Division3_EastNorthCentral" = c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin"),
  "Region2_Midwest_Division4_WestNorthCentral" = c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"),
  "Region3_South_Division5_SouthAtlantic" = c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "District of Columbia", "West Virginia"),
  "Region3_South_Division6_EastSouthCentral" = c("Alabama", "Kentucky", "Mississippi", "Tennessee"),
  "Region3_South_Division7_WestSouthCentral" = c("Arkansas", "Louisiana", "Oklahoma", "Texas"),
  "Region4_West_Division8_Mountain" = c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming"),
  "Region4_West_Division9_Pacific" = c("Alaska", "California", "Hawaii", "Oregon", "Washington")
)

census_regions <- purrr::map_df(region_compass_divnumber_divname_ls, ~ as.data.frame(.x), .id = "region_compass_divnumber_divname") %>%
  rename(state = .x) %>%
  separate(region_compass_divnumber_divname, into = c("region", "compass", "divnumber", "divname"))


# get the data
df1_us_jhu <- getus_all() %>%

  # Rode Islande has not deaths at the county level:
  # https://coronavirus.jhu.edu/us-map-faq
  # we are going to remove RI
  filter(state != "Rhode Island")

# date_freeze  <- max(df1_us_jhu$date)

# we get RI from the NYT repository
df1_us_nyt <- getus_all(repo = "nyt") %>%
  # fips 0 is for the state unassigned deaths
  filter(state == "Rhode Island")

# row bind and filter for till data freeze
df1_us <- bind_rows(df1_us_jhu, df1_us_nyt) %>%
  filter(date <= !!date_freeze)

# some cleaning
suppressWarnings(
  df2 <-
    df1_us %>%
    # calculate age65_over
    mutate(perc_age65_over = `perc_65_69` + `perc_70_74` + `perc_75_79` + `perc_80_84` + perc_85_over) %>%
    mutate(urban = if_else(urban == "Urban", 1, 0)) %>%

    mutate(total_tests = positive + negative) %>%
    # total hospital beds normalized per population
    mutate(ratio_beds = total_beds / total_pop) %>%

    # calculate day since first case
    # 0/0 generate warning
    mutate(f_date = case_when(cases >= 1 ~ date)) %>%
    group_by(fips) %>%
    mutate(f_date = min(f_date, na.rm = TRUE), days_f0 = as.numeric(date - f_date)) %>%
    ungroup() %>%
    mutate(days_f0 = if_else(is.finite(days_f0), days_f0, NA_real_)) %>%


    # perc races
    # mutate_at and mutate(across) crashes so I have to repeat code
    mutate(perc_black = total_black / total_pop * 100) %>%
    mutate(perc_white = total_white / total_pop * 100) %>%
    mutate(perc_lat = total_latino / total_pop * 100) %>%
    mutate(perc_asian = total_asian / total_pop * 100) %>%
    mutate(perc_island = total_pacific_islander / total_pop * 100) %>%
    mutate(perc_native = total_native / total_pop * 100) %>%
    mutate(perc_other = total_other_race / total_pop * 100) %>%
    mutate(perc_two_more_races = total_two_more_races / total_pop * 100) %>%


    # perc divided by 100
    mutate_at(vars(starts_with("perc")), function(x) x / 100) %>%

    # family with one parent together
    mutate(perc_family_only_onep = perc_families_only_female + perc_families_only_male) %>%

    # add Distric of Columbia to Maryland
    mutate(state = replace(state, state == "District of Columbia", "Maryland"))
)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# dat_original ORIGINAL VARIABLE (as in the preprint) -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# function dat_select_ZZ_XX defined in analysis_functions.R
#  Function that selects variables from dataframe (df2), join the dataframe with the one containing census regions,
# rename variables applying prefix XX, ZZ and calculate logit of ZZ

dat_original <- dat_select_ZZ_XX(dat = df2, var_select = to_select, census_r = census_regions)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# dat_selected SELECTED VARIABLE -------------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# function dat_select_ZZ_XX defined in analysis_functions.R
#  Function that selects variables from dataframe (df2), join the dataframe with the one containing census regions,
# rename variables applying prefix XX, ZZ and calculate logit of ZZ

dat_selected <- dat_select_ZZ_XX(dat = df2, var_select = to_select_2, census_r = census_regions)


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# dat_rf_inclus & dat_rf_pars VARIABLES RANDOM FOREST -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# NOTE that dat_rf contains data only of the last day
# the code below is to select variables based on RandomForest Selection
# the code was slightly altered from the one provided by Elisabeth
# 2 set of variables are selected:
#   parsimonious: 12 vars
#   inclusive: 38 vars

# all variables today filt == 1
today_filt_rf <- df2 %>%
  mutate(logit_perc_imm65 = logit(perc_imm65)) %>%
  filter(date == max(date), cases >= !!min_filt) %>%
  filter(!county %in% c("Unassigned", "Out of", "Unknown"))


# vars_el <- c(
#   "perc_families", "perc_family_only_onep",
#   "perc_withinternet", "perc_edu_9grade",
#   "perc_edu_nodiploma", "perc_edu_highschool",
#   "perc_edu_somecollege", "perc_edu_associate",
#   "perc_edu_bachelor", "perc_edu_gradprofess",
#   "perc_edu_highschool_higher", "perc_edu_bachelor_higher",
#   "perc_age65_over", "median_age", "sex_ratio",
#   "age_dependency", "child_dependency",
#   # "ratio_beds",
#   "perc_acute_myocardial_infarction", "perc_alzheimer_dementia",
#   "perc_asthma", "perc_atrial_fibrillation",
#   "perc_cancer_breast", "perc_cancer_colorectal",
#   "perc_cancer_lung", "perc_cancer_all",
#   "perc_ch_obstructive_pulm", "perc_chronic_kidney_disease",
#   "perc_depression", "perc_diabetes",
#   "perc_heart_failure", "perc_hypertension",
#   "perc_ischemic_heart_disease", "perc_obesity",
#   "perc_osteoporosis", "perc_rheumatoid_arthritis",
#   "perc_schizophrenia_psychotic_dis", "perc_stroke",
#   "perc_tobacco_use", "annual_wellness_visit",
#   "perc_poverty",
#   "median_income", "pm2.5",
#   "summer_temp", "summer_hum",
#   "winter_temp", "winter_hum",
#   "urban", "perc_black",
#   "perc_lat", "perc_white",
#   "perc_asian", "perc_island", "perc_native",
#   "perc_other", "perc_two_more_races", "logit_perc_imm65"
# )

# we excluded all counties with at least 500 missing values
to_exclude <- c(
  "fips",
  "county",
  "state",
  "date",
  "deaths",
  "cases",
  "cmr",
  "total_pop",
  "perc_imm65",
  "perc_pneumococcal_vaccine",
  "f_date", # actual date in which the first case was recorded

  "perc_poverty_pacificisland",
  "dex_a",
  "pending",
  "hospitalized_cumul",
  "icu_curr",
  "icu_cumul",
  "ventilator_curr",
  "ventilator_cumul",
  "recovered",
  "ratio_beds",
  "total_beds"
)

# we do not include variables that start with `total`, becouse redundant, with the exception
# of "total_pop", "total_tests"
total_vars <- grep("^total", names(today_filt_rf), value = TRUE)
total_exclude <- total_vars[!total_vars %in% c("total_pop", "total_tests")]

to_exclude_all <- c(to_exclude, total_exclude)

# dataset without variables with NAs or Totals
dat_rf_exclusions <- today_filt_rf %>%
  filter(state != "Puerto Rico") %>%
  select(-!!to_exclude_all)

# y_var <- dat_rf_exclusions$logit_perc_imm65
# x_var <- dat_rf_exclusions[, names(dat_rf_exclusions) != "logit_perc_imm65"]

set.seed(615)

mess <- "\nRunning Random Forest to select variables!\n"
sep_mess <- paste(rep.int("=", nchar(mess)), collapse = "")

message(paste0(sep_mess, mess, sep_mess))

rf <- randomForest(logit_perc_imm65 ~ .,
  mtry = 5, ntree = 500,
  importance = TRUE,
  na.action = na.roughfix,
  data = dat_rf_exclusions
)

# png(filename = here("figs", "rfImportance.png"), width = 2000, height = 2000, res = 300)
varImpPlot(rf, n.var = 203, type = 2, cex = 0.4)
abline(v = 3.5, col = "red")
abline(v = 6.5, col = "blue")
# dev.off()

# x = x_var, y = y_var,
# select the first
confounders <- rf$importance %>%
  as.data.frame() %>%
  mutate(var_select = row.names(.)) %>%
  slice_max(order_by = IncNodePurity, n = 200) %>%
  select(var_select) %>%
  unlist()

# select 4 and 22

# we also need other vars and the identifiers
other_vars <- c(
  "date", "county", "state", "fips", "cases", "deaths", "total_pop",
  "perc_imm65"
)

# These are the variables that will be selected
to_select_rf_pars <- as.character(unlist(c(other_vars, confounders[1:12])))

to_select_rf_inclus <- as.character(unlist(c(other_vars, confounders[1:38])))


# function dat_select_ZZ_XX defined in analysis_functions.R
#  Function that selects variables from dataframe (df2), join the dataframe with the one containing census regions,
# rename variables applying prefix XX, ZZ and calculate logit of ZZ

# dat_rf data with variables selected with RandomForest
dat_rf_pars <- dat_select_ZZ_XX(dat = df2, var_select = to_select_rf_pars, census_r = census_regions) %>%
  filter(date == max(date), NC_cases >= min_filt)


dat_rf_inclus <- dat_select_ZZ_XX(dat = df2, var_select = to_select_rf_inclus, census_r = census_regions) %>%
  filter(date == max(date), NC_cases >= min_filt)


