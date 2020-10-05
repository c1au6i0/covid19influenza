# us_scripts <- list(
#   "libraries.R",
#   "functions_analysis.R",
#   "functions_graph.R",
#   "us_preprocess.R"
# )
#
# set.seed(615)
# # Ode to the here https://github.com/jennybc/here_here
# library(here)
# lapply(us_scripts, function(x) source(here("code", "scripts", x)))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# imm65 ~ .in a 10 cross-validation ------
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Code written by MO
# For each set of variables we run a lm (we don't run rf anymore)
# We then compare the RMSE. Is it worth to have more variables?

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# medically_parsimonious aka knowledege based (kb): selected data set15
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Filter data
selected_today_1case <- dat_selected %>%
  filter(date == max(date)) %>%
  filter(NC_cases >= min_filt)

# variables
XX <- grep("XX", names(selected_today_1case), value = TRUE)

data_train_kb_set15 <- selected_today_1case[, c("logitZZ_perc_imm65", XX)]
rownames(data_train_kb_set15) <- selected_today_1case$fips

# @@@@@@@@
# 10-fold cross validation
control <- trainControl(method = "repeatedcv", number = 10, classProbs = FALSE, repeats = 5)

## Fit the model

fit_lm_kb_set15 <- train(logitZZ_perc_imm65 ~ ., data = data_train_kb_set15, method = "lm", trControl = control)
# fit_rf_set15 <- train(logitZZ_perc_imm65 ~ ., data=data_train_set15, method="rf", trControl=control)
# fit_rf_set15 <- readRDS(here("objs", "fit_rf_set15.RDS"))


# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# medically_inclusive: original set41
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Filter data
original_today_1case <- dat_original %>%
  filter(date == max(date)) %>%
  filter(NC_cases >= min_filt)

# variables
XX <- grep("XX", names(original_today_1case), value = TRUE)

data_train_kb_set41 <- original_today_1case[, c("logitZZ_perc_imm65", XX)]
rownames(data_train_kb_set41) <- original_today_1case$fips

## Fit the model

fit_lm_kb_set41 <- train(logitZZ_perc_imm65 ~ ., data = data_train_kb_set41, method = "lm", trControl = control)
# fit_rf_set41 <- train(logitZZ_perc_imm65 ~ ., data=data_train_set41, method="rf", trControl=control, verboseIter = TRUE)
# fit_rf_set41 <- readRDS(here("objs", "fit_rf_set41.RDS"))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# rf aka black-box aka bb: parsimonious set12
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# name of the dataset: dat_rd_pars

# variables
XX <- grep("XX", names(dat_rf_pars), value = TRUE)

data_train_bb_set12 <- dat_rf_pars[, c("logitZZ_perc_imm65", XX)]
rownames(data_train_bb_set12) <- dat_rf_pars$fips

## Fit the  model
fit_lm_bb_set12 <- train(logitZZ_perc_imm65 ~ ., data = data_train_bb_set12, method = "lm", trControl = control)
# fit_rf_set_rf <- train(logitZZ_perc_imm65 ~ ., data=data_train_set_rf, method = "rf", trControl=control, verboseIter = TRUE)
# fit_rf_set_rf <- readRDS(here("objs", "fit_rf_set_rf"))

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# rf aka black-box aka bb: inclusive set38
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# name of the dataset: dat_rf_inclus

# variables
XX <- grep("XX", names(dat_rf_inclus), value = TRUE)

data_train_bb_set38 <- dat_rf_inclus[, c("logitZZ_perc_imm65", XX)]
rownames(data_train_bb_set38) <- dat_rf_inclus$fips

## Fit the  model
fit_lm_bb_set38 <- train(logitZZ_perc_imm65 ~ ., data = data_train_bb_set38, method = "lm", trControl = control)

# @@@@@@@@@@@@
# all together
# @@@@@@@@@@@@

## Compare the RMSE between the different sets of variables
all_results_folds <- data.frame(
  small_kb_set15 = fit_lm_kb_set15$resample$RMSE,
  big_kb_set41 = fit_lm_kb_set41$resample$RMSE,
  small_bb_set12 = fit_lm_bb_set12$resample$RMSE,
  big_bb_set38 = fit_lm_bb_set38$resample$RMSE
)

