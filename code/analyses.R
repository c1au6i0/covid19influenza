# @@@@@@@@@@@@@
# Load data----
# @@@@@@@@@@@@@

us_scripts <- list(
  "libraries.R",
  "functions_analysis.R",
  "us_preprocess.R",
  "secondary_selected_variables.R",
  "strata_selected_variables.R"
)

library(here)
# Ode to the here https://github.com/jennybc/here_here
lapply(us_scripts, function(x) source(here("code", "scripts", x)))



# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Results strata, quintiles, tertiles and continuous
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


MRR_all_selected <- MRR_strata_selected_3_today %>%
  select(-weighted_variance) %>%
  bind_rows(MMR_secondary_selected) %>%
  unite(id, c("type_pp", "adjustment", "nyc_removed"), remove = FALSE)


MRR_all_selected %>%
  mutate(
    type_pp = factor(type_pp, levels = c("continuous", "quintile", "tertile", "strata_3")),
    adjustment = factor(adjustment, levels = c("state", "divname")),
    nyc_removed = factor(as.character(nyc_removed), levels = c("TRUE", "FALSE"))
  ) 



# @@@@@@@@@@@@@@@@@@@@@@@@@@
# Results stability analyses
# @@@@@@@@@@@@@@@@@@@@@@@@@@

# this assumed that you run the analysis_strata_selected_variable.R script


MRR_strata_selected_3 %>%
  filter(str_detect(analysis_group, "dates|cases")) %>%
  mutate(date = ymd(date)) 

# @@@@@@@@@@@@@@@@@@@@@
# Demographic table----
# @@@@@@@@@@@@@@@@@@@@@

# @@@@@@@@@
# Parameters
# @@@@@@@@@

min_filt <- 1
data_table <- dat_selected

today_us_filt_preprint <- data_table %>%
  filter(NC_cases >= !!min_filt, date == max(date))


# @@@@@@@@@@@@@
# var to select
# @@@@@@@@@@@@@

# variables
new_order <- c(
  # COVID-19
  "n",
  "death_rate",
  "case_ratio",
  "XX_days_f0",
  
  # Socioeconomic factors
  "XX_perc_edu_bachelor_higher",
  "XX_perc_withinternet",
  "XX_median_income",
  
  # Health factors
  "XX_annual_wellness_visit",
  # # "XX_ratio_beds",
  # # "XX_perc_alzheimer_dementia",
  # # "XX_perc_asthma",
  "XX_perc_atrial_fibrillation",
  # "XX_perc_cancer_breast",
  # "XX_perc_cancer_colorectal",
  # "XX_perc_cancer_lung",
  "XX_perc_ch_obstructive_pulm",
  # "XX_perc_chronic_kidney_disease",
  # "XX_perc_depression",
  "XX_perc_diabetes",
  # "XX_perc_heart_failure",
  "XX_perc_hypertension",
  # "XX_perc_ischemic_heart_disease",
  # "XX_perc_obesity",
  # "XX_perc_osteoporosis",
  # "XX_perc_rheumatoid_arthritis",
  # "XX_perc_stroke",
  # "XX_perc_tobacco_use",
  
  # Environmental factors
  "XX_pm2.5",
  "XX_winter_temp",
  "XX_winter_hum",
  
  # Population demographics
  "XX_median_age",
  
  # 7 Race
  "XX_perc_black",
  "XX_perc_lat",
  "XX_perc_white"
)

median_value <- median(today_us_filt_preprint$ZZ_perc_imm65) * 100

# @@@@@@@
# tab flu
# @@@@@@@

tab_us_flu <-
  today_us_filt_preprint %>%
  mutate(median = median(ZZ_perc_imm65)) %>%
  select(matches("ZZ|XX|YY|NP|NC")) %>%
  select(
    -logitZZ_perc_imm65
  ) %>%
  mutate_at(vars(matches("perc")), ~ . * 100) %>%
  mutate(ZZ_perc_imm65 = if_else(ZZ_perc_imm65 > median_value, "high", "low")) %>%
  mutate(
    death_rate = YY_deaths / NP_total_pop * 100000,
    case_ratio = NC_cases / NP_total_pop * 100000,
    XX_median_income = XX_median_income / 1000
  ) %>%
  group_by(ZZ_perc_imm65) %>%
  mutate(n = n()) %>%
  summarize_if(is.numeric, tibble::lst(mean, sd)) %>%
  pivot_longer(-ZZ_perc_imm65) %>%
  separate(name, c("var", "metric"), "_(?=(mean)|(sd))") %>%
  unite(imm65, ZZ_perc_imm65, metric) %>%
  pivot_wider(names_from = imm65, values_from = value) %>%
  mutate_if(is.numeric, round, 1) %>%
  mutate(
    "\u2264median (St.Dev.)" = paste0(low_mean, " (", low_sd, ")"),
    ">median (St.Dev.)" = paste0(high_mean, " (", high_sd, ")")
  ) %>%
  select(var, "\u2264median (St.Dev.)", ">median (St.Dev.)") %>%
  filter(!var %in% c("NP_total_pop", "YY_deaths", "NC_cases")) %>%
  arrange(match(var, !!new_order)) %>%
  rename(Variable = var) %>%
  mutate(Variable = recode(Variable,
                           "n" = "Number of counties",
                           
                           "death_rate" = "Death rate (per 100,000 people)",
                           "case_ratio" = "Confirmed-case rate (per 100,000 people)",
                           "XX_days_f0" =  "Number of days since first case",
                           
                           # 3: Socioeconomic factors
                           "XX_perc_edu_bachelor_higher" = "% with bachelor or higher degree",
                           "XX_perc_withinternet" = "% with internet",
                           "XX_median_income" = "Median income ($1,000)",
                           
                           # 5: Health factors
                           "XX_annual_wellness_visit" = "Rate annual wellness visits (per 1.000)",
                           # "XX_perc_alzheimer_dementia" = "% with Alzheimer's disease",
                           # "XX_perc_asthma" = "% with asthma",
                           # "XX_perc_atrial_fibrillation" = "% with atrial fibrillation",
                           # "XX_perc_cancer_breast" = "% with breast cancer",
                           # "XX_perc_cancer_colorectal" = "% with colorectal cancer",
                           # "XX_perc_cancer_lung" = "% with lung cancer",
                           "XX_perc_ch_obstructive_pulm" = "% with obstructive pulmonary disease",
                           # "XX_perc_chronic_kidney_disease" = "% with chronic kidney disease",
                           # "XX_perc_depression" = "% with depression",
                           "XX_perc_diabetes" = "% with diabetes",
                           # "XX_perc_heart_failure" = "% with heart failure",
                           "XX_perc_hypertension" = "% with hypertension",
                           # "XX_perc_ischemic_heart_disease" = "% with ischemic heart disease",
                           # "XX_perc_obesity" = "% with obesity",
                           # "XX_perc_rheumatoid_arthritis" = "% with rheumatoid arthritis",
                           # "XX_perc_stroke" = "% stroke transient ischemic attack",
                           # "XX_perc_tobacco_use" =  "% using tobacco",
                           
                           # Environmental
                           "XX_pm2.5" = "Average PM2.5 (microg/m3)",
                           # "XX_summer_temp" = "Summer temperature (K)",
                           # "XX_summer_hum" =  "% summer humidity",
                           "XX_winter_temp" = "Winter temperature (K)",
                           "XX_winter_hum" = "Winter humidity (%)",
                           
                           "XX_median_age" = "Median age",
                           
                           # "XX_perc_age65_over" = "% 65 years or more of age",
                           # "XX_sex_ratio" = "Sex ratio",
                           # "XX_child_dependency" = "Child dependency ratio",
                           
                           "XX_perc_black" =   "% Blacks",
                           "XX_perc_lat" =  "% Latinos",
                           "XX_perc_white" = "% Whites",
                           # "XX_perc_asian" = "% asian",
                           # "XX_perc_island" = "% island native",
                           # "XX_perc_other" =   "% other",
                           # "XX_perc_two_more_races" = "% two more races"
  ))


# @@@@@@@
# tab tot
# @@@@@@@

tab_us_tot <-
  today_us_filt_preprint %>%
  mutate(median = median(ZZ_perc_imm65)) %>%
  select(matches("ZZ|XX|YY|NP|NC")) %>%
  select(
    -logitZZ_perc_imm65
  ) %>%
  mutate_at(vars(matches("perc")), ~ . * 100) %>%
  mutate(ZZ_perc_imm65 = if_else(ZZ_perc_imm65 > 45, "high", "low")) %>%
  mutate(
    death_rate = YY_deaths / NP_total_pop * 100000,
    case_ratio = NC_cases / NP_total_pop * 100000,
    # XX_urban = XX_urban * 100,
    XX_median_income = XX_median_income / 1000,
    all_d = "all_d"
  ) %>%
  group_by(all_d) %>%
  mutate(n = n()) %>%
  summarize_if(is.numeric, funs(mean, sd)) %>%
  pivot_longer(-all_d) %>%
  separate(name, c("var", "metric"), "_(?=(mean)|(sd))") %>%
  unite(all_d, all_d, metric) %>%
  pivot_wider(names_from = all_d, values_from = value) %>%
  mutate_if(is.numeric, round, 1) %>%
  mutate(
    "All (St.Dev.)" = paste0(all_d_mean, " (", all_d_sd, ")")
  ) %>%
  select(var, "All (St.Dev.)") %>%
  mutate_if(is.numeric, round, 1) %>%
  filter(!var %in% c("NP_total_pop", "YY_deaths", "NC_cases")) %>%
  arrange(match(var, !!new_order)) %>%
  rename(Variable = var) %>%
  mutate(Variable = recode(Variable,
                           "n" = "Number of counties",
                           
                           "death_rate" = "Death rate (per 100,000 people)",
                           "case_ratio" = "Confirmed-case rate (per 100,000 people)",
                           "XX_days_f0" =  "Number of days since first case",
                           
                           # 3: Socioeconomic factors
                           "XX_perc_edu_bachelor_higher" = "% with bachelor or higher degree",
                           "XX_perc_withinternet" = "% with internet",
                           "XX_median_income" = "Median income ($1,000)",
                           
                           # 4: Health factors
                           "XX_annual_wellness_visit" = "Rate annual wellness visits (per 1.000)",
                           # "XX_perc_alzheimer_dementia" = "% with Alzheimer's disease",
                           # "XX_perc_asthma" = "% with asthma",
                           # "XX_perc_atrial_fibrillation" = "% with atrial fibrillation",
                           # "XX_perc_cancer_breast" = "% with breast cancer",
                           # "XX_perc_cancer_colorectal" = "% with colorectal cancer",
                           # "XX_perc_cancer_lung" = "% with lung cancer",
                           "XX_perc_ch_obstructive_pulm" = "% with obstructive pulmonary disease",
                           # "XX_perc_chronic_kidney_disease" = "% with chronic kidney disease",
                           # "XX_perc_depression" = "% with depression",
                           "XX_perc_diabetes" = "% with diabetes",
                           # "XX_perc_heart_failure" = "% with heart failure",
                           "XX_perc_hypertension" = "% with hypertension",
                           # "XX_perc_ischemic_heart_disease" = "% with ischemic heart disease",
                           # "XX_perc_obesity" = "% with obesity",
                           # "XX_perc_rheumatoid_arthritis" = "% with rheumatoid arthritis",
                           # "XX_perc_stroke" = "% stroke transient ischemic attack",
                           # "XX_perc_tobacco_use" =  "% using tobacco",
                           
                           # Environmental
                           "XX_pm2.5" = "Average PM2.5 (microg/m3)",
                           # "XX_summer_temp" = "Summer temperature (K)",
                           # "XX_summer_hum" =  "% summer humidity",
                           "XX_winter_temp" = "Winter temperature (K)",
                           "XX_winter_hum" = "Winter humidity (%)",
                           
                           "XX_median_age" = "Median age",
                           
                           # "XX_perc_age65_over" = "% 65 years or more of age",
                           # "XX_sex_ratio" = "Sex ratio",
                           # "XX_child_dependency" = "Child dependency ratio",
                           
                           "XX_perc_black" =   "% Blacks",
                           "XX_perc_lat" =  "% Latinos",
                           "XX_perc_white" = "% Whites",
                           # "XX_perc_asian" = "% asian",
                           # "XX_perc_island" = "% island native",
                           # "XX_perc_other" =   "% other",
                           # "XX_perc_two_more_races" = "% two more races"
  ))

# @@@@@@@@@@@@
# tab together
# @@@@@@@@@@@@

tab_us_all <- inner_join(tab_us_tot, tab_us_flu, by = "Variable")

tab_us_clean <- tab_us_all %>%
  mutate(
    Category =
      c(
        "",
        rep("COVID-19", 3),
        rep("Socioeconomic factors", 3),
        rep("Health factors", 4),
        rep("Environmental factors", 3),
        rep("Population demographics", 1),
        rep("Race", 3)
      )
  ) %>%
  tibble() %>%
  relocate(Category, .before = Variable) %>%
  rename(All = "All (St.Dev.)", "\u2265Median" = "≤median (St.Dev.)", "<Median" = ">median (St.Dev.)")


# remove ( 0 ) from first row
first_row <- sub("\\s\\(0)", "", tab_us_clean[1, ])
names(first_row) <- names(tab_us_clean)

tab_us_clean <- tab_us_clean %>%
  filter(Variable != "Number of counties")

tab_us_clean <- bind_rows(first_row, tab_us_clean)


us_data <- kable(tab_us_clean, "latex", longtable = T, booktabs = T, digits = 2) %>%
  add_header_above(c(" ", " ", "Vaccination Coverage" = 3), bold = TRUE) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, bold = TRUE) %>%
  collapse_rows(columns = 1, latex_hline = "major", valign = "middle")






