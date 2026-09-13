library(dplyr)
library(tidyr)
library(purrr)
library(readr)

############################################################
################### 0. Working directory ###################
############################################################

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")) {
  setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")) {
  setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
} else {
  stop("Working directory not found.")
}

############################################################
################### 1. Load imputations ####################
############################################################

data_imputed <- lapply(1:20, function(i) {
  read.csv(paste0("data_imp_scale", i, ".csv"), header = TRUE)
})

if (length(data_imputed) != 20) {
  stop("The number of imputed datasets is not equal to 20.")
}

############################################################
################### 2. User settings #######################
############################################################

group_var <- "sex"

group_labels <- c(
  "0" = "boys",
  "1" = "girls"
)

vars_descriptives <- c(
  "Adhd_3y.z", "Adhd_5y.z", "Adhd_9y.z", "Adhd_15y.z",
  "Ext_3y.z",  "Ext_5y.z",  "Ext_9y.z",  "Ext_15y.z",
  "Int_3y.z",  "Int_5y.z",  "Int_9y.z",  "Int_15y.z",
  "early.deprivation.3y.z", "early.threat.3y.z", "early.stochasticity.3y.z", "early.volatility.3y.z",
  "depr5_resid", "threat5_resid", "stoch5_resid", "vol5_resid",
  "age.1st.date.z", "num.date.z", "age.1st.sex.z", "num.sex.z",
  "age.1st.cig.z", "freq.smoke.month.z", "age.1st.drank.z", "freq.alc.month.z"
)

# Same variable set used for pooled correlation tables
vars_correlations <- vars_descriptives

output_csv <- "descriptive_statistics_by_group_pooled_imputations.csv"
output_csv_wide <- "descriptive_statistics_by_group_pooled_imputations_wide.csv"

output_cor_csv_all <- "correlation_matrix_pooled_imputations_all.csv"
output_cor_csv_boys <- "correlation_matrix_pooled_imputations_boys.csv"
output_cor_csv_girls <- "correlation_matrix_pooled_imputations_girls.csv"

############################################################
############### 3. Basic checks and helpers ################
############################################################

missing_vars <- setdiff(c(group_var, vars_descriptives), names(data_imputed[[1]]))
if (length(missing_vars) > 0) {
  stop(
    "These variables are missing from the imputed datasets: ",
    paste(missing_vars, collapse = ", ")
  )
}

same_names <- sapply(data_imputed, function(x) identical(names(x), names(data_imputed[[1]])))
if (!all(same_names)) {
  stop("Variable names are not identical across the 20 imputed datasets.")
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x)
}

safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_cor_mat <- function(dat, vars) {
  x <- dat[, vars, drop = FALSE]
  x[] <- lapply(x, function(col) suppressWarnings(as.numeric(col)))
  stats::cor(x, use = "pairwise.complete.obs")
}

############################################################
########## 4. Descriptives within each imputation ##########
############################################################

desc_each_imp <- purrr::imap_dfr(
  data_imputed,
  function(dat, imp_id) {
    
    dat %>%
      dplyr::select(dplyr::all_of(c(group_var, vars_descriptives))) %>%
      dplyr::mutate(
        !!group_var := as.character(.data[[group_var]])
      ) %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(vars_descriptives),
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::group_by(.data[[group_var]], variable) %>%
      dplyr::summarise(
        n = sum(!is.na(value)),
        mean = safe_mean(value),
        sd = safe_sd(value),
        min = safe_min(value),
        max = safe_max(value),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        imputation = as.integer(imp_id)
      ) %>%
      dplyr::rename(group = !!group_var)
  }
)

############################################################
########### 5. Pool descriptives across imputations ########
############################################################

desc_pooled <- desc_each_imp %>%
  dplyr::group_by(group, variable) %>%
  dplyr::summarise(
    n_mean = mean(n, na.rm = TRUE),
    mean = mean(mean, na.rm = TRUE),
    sd = mean(sd, na.rm = TRUE),
    min = mean(min, na.rm = TRUE),
    max = mean(max, na.rm = TRUE),
    mean_between_imp_sd = sd(mean, na.rm = TRUE),
    sd_between_imp_sd = sd(sd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    group = dplyr::recode(group, !!!group_labels)
  )

############################################################
################### 6. Format outputs ######################
############################################################

desc_pooled_b <- desc_pooled %>%
  dplyr::mutate(
    n_mean = round(n_mean, 1),
    mean = round(mean, 3),
    sd = round(sd, 3),
    min = round(min, 3),
    max = round(max, 3),
    mean_between_imp_sd = round(mean_between_imp_sd, 4),
    sd_between_imp_sd = round(sd_between_imp_sd, 4),
    mean_sd = paste0(mean, " (", sd, ")")
  ) %>%
  dplyr::arrange(variable, group)

desc_pooled_wide <- desc_pooled_b %>%
  dplyr::select(variable, group, n_mean, mean, sd, mean_sd, min, max) %>%
  tidyr::pivot_wider(
    names_from = group,
    values_from = c(n_mean, mean, sd, mean_sd, min, max),
    names_sep = "_"
  ) %>%
  dplyr::arrange(variable)

############################################################
########## 7. Correlation matrices across imputations ######
############################################################

# Overall pooled correlation matrix
cor_list_all <- lapply(data_imputed, function(dat) {
  safe_cor_mat(dat, vars_correlations)
})

cor_pooled_all <- Reduce("+", cor_list_all) / length(cor_list_all)
cor_pooled_all <- round(cor_pooled_all, 3)

# Boys pooled correlation matrix
cor_list_boys <- lapply(data_imputed, function(dat) {
  dat_sub <- dat[dat[[group_var]] == 0, , drop = FALSE]
  safe_cor_mat(dat_sub, vars_correlations)
})

cor_pooled_boys <- Reduce("+", cor_list_boys) / length(cor_list_boys)
cor_pooled_boys <- round(cor_pooled_boys, 3)

# Girls pooled correlation matrix
cor_list_girls <- lapply(data_imputed, function(dat) {
  dat_sub <- dat[dat[[group_var]] == 1, , drop = FALSE]
  safe_cor_mat(dat_sub, vars_correlations)
})

cor_pooled_girls <- Reduce("+", cor_list_girls) / length(cor_list_girls)
cor_pooled_girls <- round(cor_pooled_girls, 3)

# Convert to exportable data frames with variable names kept as first column
cor_pooled_all_df <- data.frame(variable = rownames(cor_pooled_all), cor_pooled_all, row.names = NULL, check.names = FALSE)
cor_pooled_boys_df <- data.frame(variable = rownames(cor_pooled_boys), cor_pooled_boys, row.names = NULL, check.names = FALSE)
cor_pooled_girls_df <- data.frame(variable = rownames(cor_pooled_girls), cor_pooled_girls, row.names = NULL, check.names = FALSE)

############################################################
###################### 8. Export files #####################
############################################################

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")) {
  setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")) {
  setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")
} else {
  stop("Working directory not found.")
}

write.csv2(desc_pooled_wide, output_csv_wide, row.names = FALSE)

write.csv2(cor_pooled_all_df, output_cor_csv_all, row.names = FALSE)
write.csv2(cor_pooled_boys_df, output_cor_csv_boys, row.names = FALSE)
write.csv2(cor_pooled_girls_df, output_cor_csv_girls, row.names = FALSE)

############################################################
######################## 9. Print ##########################
############################################################

cat("\n=== Long pooled descriptives ===\n")
print(desc_pooled_b, n = Inf)

cat("\n=== Wide pooled descriptives ===\n")
print(desc_pooled_wide, n = Inf)

cat("\n=== Pooled correlation matrix: all participants ===\n")
print(cor_pooled_all)

cat("\n=== Pooled correlation matrix: boys ===\n")
print(cor_pooled_boys)

cat("\n=== Pooled correlation matrix: girls ===\n")
print(cor_pooled_girls)
