library(blavaan)
library(lavaan)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)
library(clue)

############################################################
################### 0. Working directories #################
############################################################

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/data")) {
  data_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/data"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/data")) {
  data_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/data"
} else {
  stop("Data directory not found.")
}

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Tables")) {
  table_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Tables"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Tables")) {
  table_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Tables"
} else {
  stop("Results_Tables directory not found.")
}

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Objects")) {
  fit_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Objects"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Objects")) {
  fit_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Objects"
} else {
  fit_dir <- file.path(dirname(table_dir), "Fit_Objects")
  dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)
}

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Figures")) {
  fig_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Figures"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Figures")) {
  fig_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Model Fit_Figures"
} else {
  dir.create(file.path(dirname(table_dir), "Results_Figures"), recursive = TRUE, showWarnings = FALSE)
  fig_dir <- file.path(dirname(table_dir), "Results_Figures")
}

options(mc.cores = parallel::detectCores())

############################################################
######################## 1. Settings #######################
############################################################

n_imp <- 20
model_id <- "model_2"
output_prefix <- "model_2_RI_CLPM"

loaded_fit_objects <- ls(envir = .GlobalEnv, pattern = "^model_2_fit_imp_[0-9]+$")

if (length(loaded_fit_objects) > 0) {
  cat("Removing already loaded fit objects from memory:\n")
  print(loaded_fit_objects)
  rm(list = loaded_fit_objects, envir = .GlobalEnv)
  gc()
}

############################################################
########### 2. Detect Stan parameter families ##############
############################################################

fit_ref_file <- file.path(fit_dir, "model_2_fit_imp_1.rds")

if (!file.exists(fit_ref_file)) {
  stop("Reference fit file not found: ", fit_ref_file)
}

cat("\nREADING reference fit from:\n", fit_ref_file, "\n", sep = "")
fit_ref <- readRDS(fit_ref_file)

stan_sum_ref <- summary(fit_ref@external[["mcmcout"]])$summary
stan_ref_names <- rownames(stan_sum_ref)

stan_prefixes <- unique(
  sub("\\[.*", "", stan_ref_names[grepl("\\[", stan_ref_names)])
)

cat("\nDetected Stan prefixes:\n")
print(sort(stan_prefixes))

rm(fit_ref, stan_sum_ref, stan_ref_names)
gc()

beta_family    <- "bet_sign"
nu_family      <- "Nu_free"
theta_family   <- "Theta_var"
psi_cov_family <- "Psi_cov"
psi_var_family <- "Psi_var"

families_to_map <- c(
  beta_family,
  nu_family,
  theta_family,
  psi_cov_family,
  psi_var_family
)

families_to_map <- families_to_map[families_to_map %in% stan_prefixes]

cat("\nFamilies to map empirically:\n")
print(families_to_map)

if (!beta_family %in% families_to_map) {
  stop("bet_sign was not found among Stan prefixes. Regression parameters cannot be mapped.")
}

stan_regex <- paste0("^(", paste(families_to_map, collapse = "|"), ")\\[")

############################################################
############### 3. Empirical mapping functions ############
############################################################

family_label_from_stan <- function(family_name) {
  case_when(
    family_name == "bet_sign"  ~ "beta",
    family_name == "Nu_free"   ~ "nu",
    family_name == "Theta_var" ~ "theta",
    family_name == "Psi_cov"   ~ "psi",
    family_name == "Psi_var"   ~ "psi",
    TRUE ~ family_name
  )
}

make_parameter_label <- function(lhs, op, rhs) {
  case_when(
    op == "~"  ~ paste0(lhs, " ~ ", rhs),
    op == "~~" ~ paste0(lhs, " ~~ ", rhs),
    op == "~1" ~ paste0(lhs, " ~1"),
    TRUE ~ paste(lhs, op, rhs)
  )
}

make_target_table <- function(fit, family_name) {
  
  pt <- lavaan::parTable(fit)
  
  if (!"label" %in% names(pt)) {
    pt$label <- NA_character_
  }
  
  pt <- pt %>%
    mutate(
      rhs = ifelse(is.na(rhs), "", rhs),
      label = ifelse(is.na(label), "", label)
    )
  
  pe_raw <- parameterEstimates(fit, ci = TRUE)
  
  if (!"label" %in% names(pe_raw)) {
    pe_raw$label <- NA_character_
  }
  
  pe_raw <- pe_raw %>%
    mutate(
      rhs = ifelse(is.na(rhs), "", rhs),
      label = ifelse(is.na(label), "", label)
    )
  
  if (family_name == "bet_sign") {
    
    target <- pt %>%
      filter(op == "~", free > 0)
    
  } else if (family_name == "Nu_free") {
    
    target <- pt %>%
      filter(op == "~1", free > 0)
    
  } else if (family_name == "Theta_var") {
    
    target <- pt %>%
      filter(
        mat == "theta",
        op == "~~",
        lhs == rhs,
        free > 0
      )
    
  } else if (family_name == "Psi_cov") {
    
    target <- pt %>%
      filter(
        mat == "psi",
        op == "~~",
        lhs != rhs,
        free > 0
      )
    
  } else if (family_name == "Psi_var") {
    
    target <- pt %>%
      filter(
        mat == "psi",
        op == "~~",
        lhs == rhs,
        free > 0
      )
    
  } else {
    
    stop("Unknown family: ", family_name)
  }
  
  target <- target %>%
    select(any_of(c("id", "group", "lhs", "op", "rhs", "label", "free", "mat"))) %>%
    distinct()
  
  pe_keep <- pe_raw %>%
    select(any_of(c("group", "lhs", "op", "rhs", "est", "ci.lower", "ci.upper")))
  
  target <- target %>%
    left_join(
      pe_keep,
      by = c("group", "lhs", "op", "rhs")
    )
  
  if (any(is.na(target$est))) {
    cat("\nUnmatched target parameters for family:", family_name, "\n")
    print(target %>% filter(is.na(est)))
    stop("Some target parameters have no matching estimate for family: ", family_name)
  }
  
  target
}

make_empirical_family_map <- function(fit, imp_id, draws_i, family_name) {
  
  draws_family <- draws_i %>%
    filter(
      imputation == imp_id,
      grepl(paste0("^", family_name, "\\["), stan_name)
    ) %>%
    group_by(stan_name) %>%
    summarise(
      draw_mean = mean(value, na.rm = TRUE),
      draw_q2.5 = quantile(value, 0.025, na.rm = TRUE),
      draw_q97.5 = quantile(value, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      stan_index = as.numeric(sub(".*\\[([0-9]+)\\]", "\\1", stan_name))
    ) %>%
    arrange(stan_index)
  
  target <- make_target_table(fit, family_name)
  
  if (nrow(draws_family) == 0) {
    stop("No Stan draws found for family ", family_name, " in imputation ", imp_id)
  }
  
  if (nrow(target) == 0) {
    stop("No lavaan target parameters found for family ", family_name, " in imputation ", imp_id)
  }
  
  if (nrow(draws_family) > nrow(target)) {
    cat("\nFamily:", family_name, "\n")
    cat("Imputation:", imp_id, "\n")
    cat("Stan parameters:", nrow(draws_family), "\n")
    cat("lavaan candidate target parameters:", nrow(target), "\n")
    stop("There are more Stan parameters than lavaan candidate parameters.")
  }
  
  if (nrow(draws_family) != nrow(target)) {
    cat(
      "\nNote: family ", family_name,
      ", imputation ", imp_id,
      ": selecting ", nrow(draws_family),
      " Stan parameters among ", nrow(target),
      " lavaan candidate parameters.\n",
      sep = ""
    )
  }
  
  cost <- outer(
    draws_family$draw_mean,
    target$est,
    function(x, y) abs(x - y)
  )
  
  assignment <- as.integer(clue::solve_LSAP(cost))
  
  mapped <- bind_cols(
    draws_family,
    target[assignment, ]
  ) %>%
    mutate(
      imputation = imp_id,
      family = family_name,
      family_label = family_label_from_stan(family_name),
      index = stan_index,
      parameter = make_parameter_label(lhs, op, rhs),
      match_cost = cost[cbind(seq_len(nrow(draws_family)), assignment)],
      abs_diff_mean = abs(draw_mean - est)
    ) %>%
    select(any_of(c(
      "imputation",
      "stan_name",
      "stan_index",
      "index",
      "family",
      "family_label",
      "group",
      "lhs",
      "op",
      "rhs",
      "label",
      "free",
      "mat",
      "parameter",
      "est",
      "draw_mean",
      "draw_q2.5",
      "draw_q97.5",
      "abs_diff_mean",
      "match_cost"
    )))
  
  if (max(mapped$abs_diff_mean, na.rm = TRUE) > 1e-6) {
    cat(
      "\nWarning: imperfect empirical mapping for family ",
      family_name,
      ", imputation ",
      imp_id,
      ". Max abs diff = ",
      max(mapped$abs_diff_mean, na.rm = TRUE),
      "\n",
      sep = ""
    )
    
    print(
      mapped %>%
        arrange(desc(abs_diff_mean)) %>%
        select(
          imputation, family, stan_name, group, lhs, op, rhs,
          draw_mean, est, abs_diff_mean
        ) %>%
        head(20)
    )
  }
  
  mapped
}

############################################################
########### 4. Extract draws, diagnostics, mappings ########
###########    one .rds at a time ##########################
############################################################

draws_list <- vector("list", n_imp)
diag_list <- vector("list", n_imp)
param_map_emp_list <- vector("list", n_imp)

for (i in seq_len(n_imp)) {
  
  cat("\n============================================================\n")
  cat("Processing imputation ", i, " / ", n_imp, "\n", sep = "")
  cat("============================================================\n")
  
  fit_file_i <- file.path(
    fit_dir,
    paste0(model_id, "_fit_imp_", i, ".rds")
  )
  
  if (!file.exists(fit_file_i)) {
    stop("Fit file not found: ", fit_file_i)
  }
  
  cat("READING blavaan object:\n", fit_file_i, "\n", sep = "")
  fit_i <- readRDS(fit_file_i)
  cat("Loaded imputation ", i, ".\n", sep = "")
  
  cat("Extracting raw Stan draws...\n")
  post_i <- as.data.frame(fit_i@external[["mcmcout"]])
  
  cat("Extracting diagnostics...\n")
  stan_sum_i <- summary(fit_i@external[["mcmcout"]])$summary
  
  diag_i <- data.frame(
    stan_name = rownames(stan_sum_i),
    rhat = if ("Rhat" %in% colnames(stan_sum_i)) stan_sum_i[, "Rhat"] else NA_real_,
    n_eff = if ("n_eff" %in% colnames(stan_sum_i)) stan_sum_i[, "n_eff"] else NA_real_,
    imputation = i,
    stringsAsFactors = FALSE
  ) %>%
    filter(grepl(stan_regex, stan_name))
  
  pars_i <- grep(stan_regex, names(post_i), value = TRUE)
  
  if (length(pars_i) == 0) {
    stop("No Stan parameters matched stan_regex for imputation ", i)
  }
  
  draws_i <- post_i %>%
    select(all_of(pars_i)) %>%
    mutate(
      draw = row_number(),
      imputation = i
    ) %>%
    pivot_longer(
      cols = all_of(pars_i),
      names_to = "stan_name",
      values_to = "value"
    )
  
  cat("Building empirical parameter mapping...\n")
  map_i <- purrr::map_dfr(
    families_to_map,
    ~ make_empirical_family_map(
      fit = fit_i,
      imp_id = i,
      draws_i = draws_i,
      family_name = .x
    )
  )
  
  draws_list[[i]] <- draws_i
  diag_list[[i]] <- diag_i
  param_map_emp_list[[i]] <- map_i
  
  cat("Finished imputation ", i, ". Removing heavy object from memory.\n", sep = "")
  
  rm(fit_i, post_i, stan_sum_i, diag_i, pars_i, draws_i, map_i)
  gc()
}

all_draws <- bind_rows(draws_list)
all_diag <- bind_rows(diag_list)
param_map_emp_all <- bind_rows(param_map_emp_list)

rm(draws_list, diag_list, param_map_emp_list)
gc()

cat("\nFinished sequential extraction.\n")
cat("Rows in all_draws         :", nrow(all_draws), "\n")
cat("Rows in all_diag          :", nrow(all_diag), "\n")
cat("Rows in param_map_emp_all :", nrow(param_map_emp_all), "\n")

############################################################
################ 5. Mapping diagnostics ####################
############################################################

mapping_checks <- param_map_emp_all %>%
  group_by(imputation, family) %>%
  summarise(
    n = n(),
    mean_abs_diff = mean(abs_diff_mean, na.rm = TRUE),
    median_abs_diff = median(abs_diff_mean, na.rm = TRUE),
    max_abs_diff = max(abs_diff_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(family, imputation)

cat("\n--- empirical mapping checks by family and imputation ---\n")
print(mapping_checks, n = Inf)

problematic_mapping <- mapping_checks %>%
  filter(!is.na(max_abs_diff), max_abs_diff > 1e-6)

cat("\n--- problematic empirical mappings, threshold max_abs_diff > 1e-6 ---\n")
print(problematic_mapping, n = Inf)

if (nrow(problematic_mapping) > 0) {
  warning(
    "Some empirical mappings have max_abs_diff > 1e-6. Inspect problematic_mapping before interpreting results."
  )
}

saveRDS(
  param_map_emp_all,
  file.path(fit_dir, paste0(model_id, "_empirical_mapping_all_imputations.rds")),
  compress = "xz"
)

############################################################
############ 6. Map raw draws to readable parameters #######
############################################################

all_draws_mapped <- all_draws %>%
  left_join(
    param_map_emp_all %>%
      select(
        imputation,
        stan_name,
        stan_index,
        index,
        family,
        family_label,
        group,
        lhs,
        op,
        rhs,
        label,
        free,
        mat,
        parameter
      ),
    by = c("imputation", "stan_name")
  )

cat("\n--- draw mapping check ---\n")
print(
  all_draws_mapped %>%
    summarise(
      n_rows = n(),
      n_missing_lhs = sum(is.na(lhs)),
      prop_missing_lhs = mean(is.na(lhs)),
      n_imp = n_distinct(imputation)
    )
)

############################################################
################ 7. Summarize diagnostics ##################
############################################################

diag_mapped <- all_diag %>%
  left_join(
    param_map_emp_all %>%
      select(
        imputation,
        stan_name,
        family,
        family_label,
        group,
        lhs,
        op,
        rhs,
        parameter
      ),
    by = c("imputation", "stan_name")
  )

diag_summary <- diag_mapped %>%
  group_by(family, family_label, group, lhs, op, rhs, parameter) %>%
  summarise(
    mean_rhat = if (all(is.na(rhat))) NA_real_ else mean(rhat, na.rm = TRUE),
    max_rhat  = if (all(is.na(rhat))) NA_real_ else max(rhat, na.rm = TRUE),
    mean_neff = if (all(is.na(n_eff))) NA_real_ else mean(n_eff, na.rm = TRUE),
    min_neff  = if (all(is.na(n_eff))) NA_real_ else min(n_eff, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
######## 8. Posterior summary from raw mapped draws ########
############################################################

summary_table <- all_draws_mapped %>%
  filter(!is.na(lhs)) %>%
  group_by(family, family_label, group, lhs, op, rhs, label, mat, parameter) %>%
  summarise(
    mean  = mean(value, na.rm = TRUE),
    sd    = sd(value, na.rm = TRUE),
    q2.5  = quantile(value, 0.025, na.rm = TRUE),
    q97.5 = quantile(value, 0.975, na.rm = TRUE),
    n_draws = n(),
    n_imp = n_distinct(imputation),
    .groups = "drop"
  ) %>%
  left_join(
    diag_summary,
    by = c("family", "family_label", "group", "lhs", "op", "rhs", "parameter")
  ) %>%
  mutate(
    family_label = factor(
      family_label,
      levels = c("beta", "theta", "psi", "nu")
    ),
    estimate_ci = sprintf("%.3f [%.3f; %.3f]", mean, q2.5, q97.5)
  ) %>%
  arrange(family_label, group, lhs, op, rhs)

############################################################
############ 9. Convenient subtables #######################
############################################################

results_beta <- summary_table %>%
  filter(family_label == "beta") %>%
  arrange(group, lhs, rhs)

results_theta <- summary_table %>%
  filter(family_label == "theta") %>%
  arrange(group, lhs, rhs)

results_psi <- summary_table %>%
  filter(family_label == "psi") %>%
  arrange(group, lhs, rhs)

results_nu <- summary_table %>%
  filter(family_label == "nu") %>%
  arrange(group, lhs)

results_ar <- results_beta %>%
  filter(
    (lhs == "wAdhd_5"  & rhs == "wAdhd_3")  |
      (lhs == "wAdhd_9"  & rhs == "wAdhd_5")  |
      (lhs == "wAdhd_15" & rhs == "wAdhd_9")  |
      (lhs == "wExt_5"   & rhs == "wExt_3")   |
      (lhs == "wExt_9"   & rhs == "wExt_5")   |
      (lhs == "wExt_15"  & rhs == "wExt_9")   |
      (lhs == "wInt_5"   & rhs == "wInt_3")   |
      (lhs == "wInt_9"   & rhs == "wInt_5")   |
      (lhs == "wInt_15"  & rhs == "wInt_9")
  )

results_cl <- results_beta %>%
  filter(
    (lhs == "wExt_5"   & rhs == "wAdhd_3") |
      (lhs == "wExt_9"   & rhs == "wAdhd_5") |
      (lhs == "wExt_15"  & rhs == "wAdhd_9") |
      (lhs == "wInt_5"   & rhs == "wAdhd_3") |
      (lhs == "wInt_9"   & rhs == "wAdhd_5") |
      (lhs == "wInt_15"  & rhs == "wAdhd_9") |
      (lhs == "wAdhd_5"  & rhs == "wExt_3")  |
      (lhs == "wAdhd_9"  & rhs == "wExt_5")  |
      (lhs == "wAdhd_15" & rhs == "wExt_9")  |
      (lhs == "wAdhd_5"  & rhs == "wInt_3")  |
      (lhs == "wAdhd_9"  & rhs == "wInt_5")  |
      (lhs == "wAdhd_15" & rhs == "wInt_9")  |
      (lhs == "wExt_5"   & rhs == "wInt_3")  |
      (lhs == "wExt_9"   & rhs == "wInt_5")  |
      (lhs == "wExt_15"  & rhs == "wInt_9")  |
      (lhs == "wInt_5"   & rhs == "wExt_3")  |
      (lhs == "wInt_9"   & rhs == "wExt_5")  |
      (lhs == "wInt_15"  & rhs == "wExt_9")
  )

results_adversity_paths <- results_beta %>%
  filter(rhs %in% c(
    "early.stochasticity.3y.z",
    "early.volatility.3y.z",
    "early.threat.3y.z",
    "early.deprivation.3y.z",
    "stoch5_resid",
    "vol5_resid",
    "threat5_resid",
    "depr5_resid"
  ))

cat("Rows in results_beta      :", nrow(results_beta), "\n")
cat("Rows in results_theta     :", nrow(results_theta), "\n")
cat("Rows in results_psi       :", nrow(results_psi), "\n")
cat("Rows in results_nu        :", nrow(results_nu), "\n")
cat("Rows in results_ar        :", nrow(results_ar), "\n")
cat("Rows in results_cl        :", nrow(results_cl), "\n")
cat("Rows in results_adversity :", nrow(results_adversity_paths), "\n")

cat("\nsummary_table by family and group:\n")
print(summary_table %>% count(family_label, group))

cat("\nresults_nu by group:\n")
print(results_nu %>% count(group))

cat("\nresults_theta by group:\n")
print(results_theta %>% count(group))

cat("\nresults_psi by group:\n")
print(results_psi %>% count(group))

############################################################
#################### 10. Save outputs ######################
############################################################

fit_files_index <- data.frame(
  imputation = seq_len(n_imp),
  fit_file = file.path(
    fit_dir,
    paste0(model_id, "_fit_imp_", seq_len(n_imp), ".rds")
  ),
  file_exists = file.exists(
    file.path(
      fit_dir,
      paste0(model_id, "_fit_imp_", seq_len(n_imp), ".rds")
    )
  ),
  stringsAsFactors = FALSE
)

write.csv(
  fit_files_index,
  file.path(table_dir, paste0(output_prefix, "_fit_files_index.csv")),
  row.names = FALSE
)

write.csv(
  param_map_emp_all,
  file.path(table_dir, paste0(output_prefix, "_empirical_mapping_all_imputations.csv")),
  row.names = FALSE
)

write.csv(
  mapping_checks,
  file.path(table_dir, paste0(output_prefix, "_empirical_mapping_checks.csv")),
  row.names = FALSE
)

write.csv(
  summary_table,
  file.path(table_dir, paste0(output_prefix, "_posterior_summary_all_parameters_readable.csv")),
  row.names = FALSE
)

write.csv(
  results_beta,
  file.path(table_dir, paste0(output_prefix, "_regression_results.csv")),
  row.names = FALSE
)

write.csv(
  results_theta,
  file.path(table_dir, paste0(output_prefix, "_theta_results.csv")),
  row.names = FALSE
)

write.csv(
  results_psi,
  file.path(table_dir, paste0(output_prefix, "_var_covar_results.csv")),
  row.names = FALSE
)

write.csv(
  results_nu,
  file.path(table_dir, paste0(output_prefix, "_intercepts_results.csv")),
  row.names = FALSE
)

write.csv(
  results_ar,
  file.path(table_dir, paste0(output_prefix, "_AR_results.csv")),
  row.names = FALSE
)

write.csv(
  results_cl,
  file.path(table_dir, paste0(output_prefix, "_CL_results.csv")),
  row.names = FALSE
)

write.csv(
  results_adversity_paths,
  file.path(table_dir, paste0(output_prefix, "_adversity_paths_results.csv")),
  row.names = FALSE
)

write.csv(
  all_draws,
  file.path(table_dir, paste0(output_prefix, "_posterior_draws_all_parameters.csv")),
  row.names = FALSE
)

write.csv(
  all_draws_mapped,
  file.path(table_dir, paste0(output_prefix, "_posterior_draws_all_parameters_mapped.csv")),
  row.names = FALSE
)

write.csv(
  all_diag,
  file.path(table_dir, paste0(output_prefix, "_diagnostics_all_parameters_by_imputation.csv")),
  row.names = FALSE
)

write.csv(
  diag_summary,
  file.path(table_dir, paste0(output_prefix, "_diagnostics_summary.csv")),
  row.names = FALSE
)

############################################################
################ 11. Basic sanity checks ###################
############################################################

cat("\n--- checks ---\n")
cat("Stan families mapped:", paste(families_to_map, collapse = ", "), "\n")
cat("Number of imputations in draws:", length(unique(all_draws$imputation)), "\n")
cat("Imputation ids in draws:", paste(sort(unique(all_draws$imputation)), collapse = ", "), "\n")
cat("Number of unique raw Stan parameters:", length(unique(all_draws$stan_name)), "\n")

cat("\nRaw Stan parameter families:\n")
print(table(sub("\\[.*", "", unique(all_draws$stan_name))))

cat("\nRows in param_map_emp_all :", nrow(param_map_emp_all), "\n")
cat("Rows in all_draws         :", nrow(all_draws), "\n")
cat("Rows in all_draws_mapped  :", nrow(all_draws_mapped), "\n")
cat("Rows in summary_table     :", nrow(summary_table), "\n")
cat("Rows in results_beta      :", nrow(results_beta), "\n")
cat("Rows in results_theta     :", nrow(results_theta), "\n")
cat("Rows in results_psi       :", nrow(results_psi), "\n")
cat("Rows in results_nu        :", nrow(results_nu), "\n")

cat("\n--- mapping max_abs_diff summary ---\n")
print(
  mapping_checks %>%
    group_by(family) %>%
    summarise(
      max_abs_diff_overall = max(max_abs_diff, na.rm = TRUE),
      .groups = "drop"
    )
)

cat("\nHead of results_ar:\n")
print(head(results_ar, 20))

cat("\nHead of results_cl:\n")
print(head(results_cl, 20))

cat("\nHead of results_adversity_paths:\n")
print(head(results_adversity_paths, 20))

############################################################
############################################################
######################## Visualization #####################
############################################################
############################################################

############################################################
#################### 12. Minimal cleaning ##################
############################################################

clean_results_df <- function(dat) {
  dat %>%
    mutate(
      family = trimws(as.character(family)),
      family_label = if ("family_label" %in% names(.)) trimws(as.character(family_label)) else NA_character_,
      lhs = trimws(as.character(lhs)),
      rhs = trimws(as.character(rhs)),
      op = trimws(as.character(op)),
      label = trimws(as.character(label)),
      parameter = trimws(as.character(parameter)),
      mean = as.numeric(mean),
      sd = as.numeric(sd),
      q2.5 = as.numeric(q2.5),
      q97.5 = as.numeric(q97.5),
      group = as.numeric(group)
    )
}

summary_table <- clean_results_df(summary_table)
results_beta <- clean_results_df(results_beta)
results_theta <- clean_results_df(results_theta)
results_psi <- clean_results_df(results_psi)
results_nu <- clean_results_df(results_nu)
results_ar <- clean_results_df(results_ar)
results_cl <- clean_results_df(results_cl)
results_adversity_paths <- clean_results_df(results_adversity_paths)

all_draws <- all_draws %>%
  mutate(
    stan_name = trimws(as.character(stan_name)),
    value = as.numeric(value),
    imputation = as.numeric(imputation),
    draw = as.numeric(draw)
  )

all_draws_mapped <- all_draws_mapped %>%
  mutate(
    stan_name = trimws(as.character(stan_name)),
    family = trimws(as.character(family)),
    family_label = trimws(as.character(family_label)),
    lhs = trimws(as.character(lhs)),
    rhs = trimws(as.character(rhs)),
    op = trimws(as.character(op)),
    parameter = trimws(as.character(parameter)),
    value = as.numeric(value),
    imputation = as.numeric(imputation),
    draw = as.numeric(draw),
    group = as.numeric(group),
    index = as.numeric(index)
  )

############################################################
################### 13. Global settings ####################
############################################################

rope_limit <- 0.05
group_levels <- c("boys", "girls")
lag_levels <- c("3 -> 5", "5 -> 9", "9 -> 15")
phase_levels <- c("dynamic", "3y baseline", "5y residual")

class_levels <- c(
  "ADHD AR",
  "EXT AR",
  "INT AR",
  "adhd -> ext",
  "adhd -> int",
  "ext -> adhd",
  "int -> adhd",
  "ext -> int",
  "int -> ext",
  "stoch -> adhd",
  "vol -> adhd",
  "threat -> adhd",
  "depr -> adhd",
  "stoch -> ext",
  "vol -> ext",
  "threat -> ext",
  "depr -> ext",
  "stoch -> int",
  "vol -> int",
  "threat -> int",
  "depr -> int"
)

theme_results <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.2, colour = "grey88"),
      panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey90"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = base_size + 2, face = "bold"),
      plot.subtitle = element_text(size = base_size - 2),
      axis.text.x = element_text(size = base_size - 1),
      axis.text.y = element_text(size = base_size - 1),
      legend.position = "top"
    )
}

save_plot <- function(plot, filename, width, height, save_in_wd = TRUE) {
  
  fig_png <- file.path(fig_dir, paste0(filename, ".png"))
  fig_pdf <- file.path(fig_dir, paste0(filename, ".pdf"))
  
  ggsave(fig_png, plot, width = width, height = height, dpi = 250)
  ggsave(fig_pdf, plot, width = width, height = height)
  
  if (isTRUE(save_in_wd)) {
    wd_png <- file.path(getwd(), paste0(filename, ".png"))
    wd_pdf <- file.path(getwd(), paste0(filename, ".pdf"))
    
    ggsave(wd_png, plot, width = width, height = height, dpi = 250)
    ggsave(wd_pdf, plot, width = width, height = height)
  }
}

file_stub_fun <- function(cl) {
  case_when(
    cl == "ADHD AR" ~ "adhd_ar",
    cl == "EXT AR" ~ "ext_ar",
    cl == "INT AR" ~ "int_ar",
    cl == "adhd -> ext" ~ "adhd_to_ext",
    cl == "adhd -> int" ~ "adhd_to_int",
    cl == "ext -> adhd" ~ "ext_to_adhd",
    cl == "int -> adhd" ~ "int_to_adhd",
    cl == "ext -> int" ~ "ext_to_int",
    cl == "int -> ext" ~ "int_to_ext",
    cl == "stoch -> adhd" ~ "stoch_to_adhd",
    cl == "vol -> adhd" ~ "vol_to_adhd",
    cl == "threat -> adhd" ~ "threat_to_adhd",
    cl == "depr -> adhd" ~ "depr_to_adhd",
    cl == "stoch -> ext" ~ "stoch_to_ext",
    cl == "vol -> ext" ~ "vol_to_ext",
    cl == "threat -> ext" ~ "threat_to_ext",
    cl == "depr -> ext" ~ "depr_to_ext",
    cl == "stoch -> int" ~ "stoch_to_int",
    cl == "vol -> int" ~ "vol_to_int",
    cl == "threat -> int" ~ "threat_to_int",
    cl == "depr -> int" ~ "depr_to_int",
    TRUE ~ "other"
  )
}

make_group_label <- function(x) {
  case_when(
    x == 1 ~ "boys",
    x == 2 ~ "girls",
    TRUE ~ as.character(x)
  )
}

############################################################
############### Figure sampling settings ###################
############################################################

set.seed(1234)

figure_draw_prop <- 0.10
min_draws_per_parameter <- 200
max_draws_per_parameter <- 1000

sample_draws_for_plot <- function(dat, prop = figure_draw_prop) {
  
  dat %>%
    group_by(stan_name, imputation) %>%
    group_modify(~ {
      
      n_available <- nrow(.x)
      
      n_keep <- ceiling(n_available * prop)
      n_keep <- max(n_keep, min_draws_per_parameter)
      n_keep <- min(n_keep, max_draws_per_parameter)
      n_keep <- min(n_keep, n_available)
      
      dplyr::slice_sample(.x, n = n_keep, replace = FALSE)
    }) %>%
    ungroup()
}

############################################################
############### 14. Build plotting table ###################
############################################################

results_beta_plot <- results_beta %>%
  mutate(
    pretty_param = paste0(lhs, " ~ ", rhs),
    group_label = make_group_label(group),
    class = case_when(
      lhs == "wAdhd_5"  & rhs == "wAdhd_3"  ~ "ADHD AR",
      lhs == "wAdhd_9"  & rhs == "wAdhd_5"  ~ "ADHD AR",
      lhs == "wAdhd_15" & rhs == "wAdhd_9"  ~ "ADHD AR",
      
      lhs == "wExt_5"   & rhs == "wExt_3"   ~ "EXT AR",
      lhs == "wExt_9"   & rhs == "wExt_5"   ~ "EXT AR",
      lhs == "wExt_15"  & rhs == "wExt_9"   ~ "EXT AR",
      
      lhs == "wInt_5"   & rhs == "wInt_3"   ~ "INT AR",
      lhs == "wInt_9"   & rhs == "wInt_5"   ~ "INT AR",
      lhs == "wInt_15"  & rhs == "wInt_9"   ~ "INT AR",
      
      lhs == "wExt_5"   & rhs == "wAdhd_3"  ~ "adhd -> ext",
      lhs == "wExt_9"   & rhs == "wAdhd_5"  ~ "adhd -> ext",
      lhs == "wExt_15"  & rhs == "wAdhd_9"  ~ "adhd -> ext",
      
      lhs == "wInt_5"   & rhs == "wAdhd_3"  ~ "adhd -> int",
      lhs == "wInt_9"   & rhs == "wAdhd_5"  ~ "adhd -> int",
      lhs == "wInt_15"  & rhs == "wAdhd_9"  ~ "adhd -> int",
      
      lhs == "wAdhd_5"  & rhs == "wExt_3"   ~ "ext -> adhd",
      lhs == "wAdhd_9"  & rhs == "wExt_5"   ~ "ext -> adhd",
      lhs == "wAdhd_15" & rhs == "wExt_9"   ~ "ext -> adhd",
      
      lhs == "wAdhd_5"  & rhs == "wInt_3"   ~ "int -> adhd",
      lhs == "wAdhd_9"  & rhs == "wInt_5"   ~ "int -> adhd",
      lhs == "wAdhd_15" & rhs == "wInt_9"   ~ "int -> adhd",
      
      lhs == "wExt_5"   & rhs == "wInt_3"   ~ "int -> ext",
      lhs == "wExt_9"   & rhs == "wInt_5"   ~ "int -> ext",
      lhs == "wExt_15"  & rhs == "wInt_9"   ~ "int -> ext",
      
      lhs == "wInt_5"   & rhs == "wExt_3"   ~ "ext -> int",
      lhs == "wInt_9"   & rhs == "wExt_5"   ~ "ext -> int",
      lhs == "wInt_15"  & rhs == "wExt_9"   ~ "ext -> int",
      
      lhs %in% c("wAdhd_3", "wAdhd_5") & rhs %in% c("early.stochasticity.3y.z", "stoch5_resid") ~ "stoch -> adhd",
      lhs %in% c("wAdhd_3", "wAdhd_5") & rhs %in% c("early.volatility.3y.z", "vol5_resid") ~ "vol -> adhd",
      lhs %in% c("wAdhd_3", "wAdhd_5") & rhs %in% c("early.threat.3y.z", "threat5_resid") ~ "threat -> adhd",
      lhs %in% c("wAdhd_3", "wAdhd_5") & rhs %in% c("early.deprivation.3y.z", "depr5_resid") ~ "depr -> adhd",
      
      lhs %in% c("wExt_3", "wExt_5") & rhs %in% c("early.stochasticity.3y.z", "stoch5_resid") ~ "stoch -> ext",
      lhs %in% c("wExt_3", "wExt_5") & rhs %in% c("early.volatility.3y.z", "vol5_resid") ~ "vol -> ext",
      lhs %in% c("wExt_3", "wExt_5") & rhs %in% c("early.threat.3y.z", "threat5_resid") ~ "threat -> ext",
      lhs %in% c("wExt_3", "wExt_5") & rhs %in% c("early.deprivation.3y.z", "depr5_resid") ~ "depr -> ext",
      
      lhs %in% c("wInt_3", "wInt_5") & rhs %in% c("early.stochasticity.3y.z", "stoch5_resid") ~ "stoch -> int",
      lhs %in% c("wInt_3", "wInt_5") & rhs %in% c("early.volatility.3y.z", "vol5_resid") ~ "vol -> int",
      lhs %in% c("wInt_3", "wInt_5") & rhs %in% c("early.threat.3y.z", "threat5_resid") ~ "threat -> int",
      lhs %in% c("wInt_3", "wInt_5") & rhs %in% c("early.deprivation.3y.z", "depr5_resid") ~ "depr -> int",
      
      TRUE ~ "other"
    ),
    lag = case_when(
      lhs %in% c("wAdhd_5", "wExt_5", "wInt_5") ~ "3 -> 5",
      lhs %in% c("wAdhd_9", "wExt_9", "wInt_9") ~ "5 -> 9",
      lhs %in% c("wAdhd_15", "wExt_15", "wInt_15") ~ "9 -> 15",
      TRUE ~ NA_character_
    ),
    phase = case_when(
      lhs %in% c("wAdhd_3", "wExt_3", "wInt_3") &
        rhs %in% c(
          "early.stochasticity.3y.z",
          "early.volatility.3y.z",
          "early.threat.3y.z",
          "early.deprivation.3y.z"
        ) ~ "3y baseline",
      
      lhs %in% c("wAdhd_5", "wExt_5", "wInt_5") &
        rhs %in% c(
          "stoch5_resid",
          "vol5_resid",
          "threat5_resid",
          "depr5_resid"
        ) ~ "5y residual",
      
      TRUE ~ "dynamic"
    ),
    effect_class = case_when(
      q2.5 > 0 ~ "positive",
      q97.5 < 0 ~ "negative",
      TRUE ~ "includes zero"
    ),
    rope_class = case_when(
      q2.5 >= -rope_limit & q97.5 <= rope_limit ~ "inside ROPE",
      TRUE ~ "outside ROPE"
    ),
    class = factor(class, levels = class_levels),
    lag = factor(lag, levels = lag_levels),
    phase = factor(phase, levels = phase_levels),
    group_label = factor(group_label, levels = group_levels)
  ) %>%
  filter(class %in% class_levels) %>%
  distinct(group, lhs, rhs, .keep_all = TRUE)

############################################################
########### 15. Density plots by parameter class ###########
########### memory-safe version: one class at a time #######
############################################################

if (exists("all_draws_beta_plot")) {
  rm(all_draws_beta_plot)
  gc()
}

plots_density <- list()

for (cl in class_levels) {
  
  cat("\nPreparing beta density plot for class: ", cl, "\n", sep = "")
  
  meta_cl <- results_beta_plot %>%
    filter(class == cl) %>%
    select(
      group,
      lhs,
      rhs,
      class,
      lag,
      phase,
      group_label,
      pretty_param
    ) %>%
    distinct()
  
  if (nrow(meta_cl) == 0) {
    cat("No metadata for class: ", cl, ". Skipping.\n", sep = "")
    next
  }
  
  dat_cl <- all_draws_mapped %>%
    filter(
      as.character(family_label) == "beta",
      is.finite(value)
    ) %>%
    semi_join(
      meta_cl,
      by = c("group", "lhs", "rhs")
    ) %>%
    select(
      stan_name,
      imputation,
      value,
      group,
      lhs,
      rhs
    ) %>%
    left_join(
      meta_cl,
      by = c("group", "lhs", "rhs")
    ) %>%
    filter(!is.na(class))
  
  if (nrow(dat_cl) == 0) {
    cat("No draws for class: ", cl, ". Skipping.\n", sep = "")
    rm(meta_cl, dat_cl)
    gc()
    next
  }
  
  dat_cl <- dat_cl %>%
    sample_draws_for_plot() %>%
    arrange(group, lhs, rhs) %>%
    mutate(
      facet_label = paste0(group_label, " | ", pretty_param),
      facet_label = factor(facet_label, levels = unique(facet_label))
    )
  
  if (nrow(dat_cl) == 0) {
    cat("No sampled draws for class: ", cl, ". Skipping.\n", sep = "")
    rm(meta_cl, dat_cl)
    gc()
    next
  }
  
  if (n_distinct(dat_cl$value) < 2) {
    cat("Not enough variation for class: ", cl, ". Skipping.\n", sep = "")
    rm(meta_cl, dat_cl)
    gc()
    next
  }
  
  p_density <- ggplot(dat_cl, aes(x = value)) +
    geom_density(
      fill = NA,
      colour = "#14D9BB",
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.35,
      colour = "grey50"
    ) +
    facet_wrap(~ facet_label, scales = "free_y", ncol = 3) +
    coord_cartesian(xlim = c(-1, 1)) +
    labs(
      title = paste0("Posterior distributions - ", cl),
      x = "Parameter value",
      y = "Density"
    ) +
    theme_results(12)
  
  plots_density[[cl]] <- p_density
  
  file_stub <- file_stub_fun(cl)
  
  save_plot(
    p_density,
    paste0(model_id, "_density_", file_stub, "_mi"),
    12,
    8
  )
  
  print(p_density)
  
  rm(meta_cl, dat_cl, p_density)
  gc()
}

############################################################
############ 16. Forest plots by parameter class ###########
############################################################

plots_forest <- list()

for (cl in class_levels) {
  
  dat_cl <- results_beta_plot %>%
    filter(class == cl) %>%
    arrange(group, lhs, rhs) %>%
    mutate(
      facet_y = paste0(group_label, " | ", pretty_param),
      facet_y = factor(facet_y, levels = rev(unique(facet_y)))
    )
  
  if (nrow(dat_cl) == 0) next
  
  p_forest <- ggplot(dat_cl, aes(x = mean, y = facet_y)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35, colour = "grey50") +
    geom_errorbar(
      aes(xmin = q2.5, xmax = q97.5, colour = effect_class),
      orientation = "y",
      linewidth = 0.8
    ) +
    geom_point(
      aes(colour = effect_class),
      size = 2.8
    ) +
    scale_color_manual(
      values = c(
        "positive" = "#14D9BB",
        "negative" = "#F58518",
        "includes zero" = "grey40"
      )
    ) +
    scale_shape_manual(
      values = c(
        "inside ROPE" = 21,
        "outside ROPE" = 19
      )
    ) +
    coord_cartesian(xlim = c(-1, 1)) +
    scale_x_continuous(
      breaks = seq(-1, 1, by = 0.25),
      labels = function(x) sprintf("%.2f", x)
    ) +
    labs(
      title = paste0("Posterior estimates - ", cl),
      x = "Posterior estimate",
      y = NULL,
      colour = NULL,
      shape = NULL
    ) +
    theme_results(12)
  
  plots_forest[[cl]] <- p_forest
  
  file_stub <- file_stub_fun(cl)
  
  save_plot(
    p_forest,
    paste0(model_id, "_forest_", file_stub, "_mi"),
    10,
    6
  )
}

if (Sys.getenv("RSTUDIO") == "1") {
  options(device = "RStudioGD")
}

for (cl in names(plots_forest)) {
  print(plots_forest[[cl]])
}

############################################################
########## 17. Forest plots for non-regression params ######
########## split by family subtype and sex #################
############################################################

make_nonreg_forest_plot <- function(dat, title, filename, width = 10, height = 7) {
  
  if (nrow(dat) == 0) {
    cat("No data for:", filename, "\n")
    return(NULL)
  }
  
  dat <- dat %>%
    filter(is.finite(mean), is.finite(q2.5), is.finite(q97.5)) %>%
    arrange(group_label, pretty_param) %>%
    mutate(
      pretty_param = factor(pretty_param, levels = rev(unique(pretty_param)))
    )
  
  if (nrow(dat) == 0) {
    cat("No finite data for:", filename, "\n")
    return(NULL)
  }
  
  p <- ggplot(dat, aes(x = mean, y = pretty_param)) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.35,
      colour = "grey50"
    ) +
    geom_errorbar(
      aes(xmin = q2.5, xmax = q97.5),
      orientation = "y",
      linewidth = 0.8,
      colour = "grey35"
    ) +
    geom_point(size = 2.5, colour = "black") +
    coord_cartesian(xlim = c(-1, 1)) +
    scale_x_continuous(
      breaks = seq(-1, 1, by = 0.25),
      labels = function(x) sprintf("%.2f", x)
    ) +
    labs(
      title = title,
      x = "Posterior estimate",
      y = NULL
    ) +
    theme_results(11)
  
  print(p)
  save_plot(p, filename, width, height)
  
  p
}

############################################################
#################### 17a. nu: by sex #######################
############################################################

nu_plot <- results_nu %>%
  mutate(
    pretty_param = paste0(lhs, " ~1"),
    group_label = factor(make_group_label(group), levels = group_levels)
  )

plots_nu_forest <- list()

for (sex_i in group_levels) {
  
  dat_i <- nu_plot %>%
    filter(group_label == sex_i)
  
  plots_nu_forest[[sex_i]] <- make_nonreg_forest_plot(
    dat = dat_i,
    title = paste0("Posterior estimates - nu parameters - ", sex_i),
    filename = paste0(model_id, "_forest_nu_", sex_i, "_mi"),
    width = 10,
    height = 7
  )
}

############################################################
################### 17b. theta: by sex #####################
############################################################

theta_plot <- results_theta %>%
  mutate(
    pretty_param = paste0(lhs, " ~~ ", rhs),
    group_label = factor(make_group_label(group), levels = group_levels)
  )

plots_theta_forest <- list()

for (sex_i in group_levels) {
  
  dat_i <- theta_plot %>%
    filter(group_label == sex_i)
  
  plots_theta_forest[[sex_i]] <- make_nonreg_forest_plot(
    dat = dat_i,
    title = paste0("Posterior estimates - theta parameters - ", sex_i),
    filename = paste0(model_id, "_forest_theta_", sex_i, "_mi"),
    width = 10,
    height = 8
  )
}

############################################################
############### 17c. psi variances: by sex #################
############################################################

psi_var_plot <- results_psi %>%
  filter(lhs == rhs) %>%
  mutate(
    pretty_param = paste0(lhs, " ~~ ", rhs),
    group_label = factor(make_group_label(group), levels = group_levels)
  )

plots_psi_var_forest <- list()

for (sex_i in group_levels) {
  
  dat_i <- psi_var_plot %>%
    filter(group_label == sex_i)
  
  plots_psi_var_forest[[sex_i]] <- make_nonreg_forest_plot(
    dat = dat_i,
    title = paste0("Posterior estimates - psi variances - ", sex_i),
    filename = paste0(model_id, "_forest_psi_variances_", sex_i, "_mi"),
    width = 10,
    height = 8
  )
}

############################################################
############## 17d. psi covariances: by sex ################
############################################################

psi_cov_plot <- results_psi %>%
  filter(lhs != rhs) %>%
  mutate(
    pretty_param = paste0(lhs, " ~~ ", rhs),
    group_label = factor(make_group_label(group), levels = group_levels)
  )

plots_psi_cov_forest <- list()

for (sex_i in group_levels) {
  
  dat_i <- psi_cov_plot %>%
    filter(group_label == sex_i)
  
  plots_psi_cov_forest[[sex_i]] <- make_nonreg_forest_plot(
    dat = dat_i,
    title = paste0("Posterior estimates - psi covariances - ", sex_i),
    filename = paste0(model_id, "_forest_psi_covariances_", sex_i, "_mi"),
    width = 11,
    height = 10
  )
}

############################################################
###### 18. Density plots for non-regression params #########
###### memory-safe version: one family x sex at a time #####
############################################################

if (exists("all_draws_other_plot")) {
  rm(all_draws_other_plot)
  gc()
}

plot_specs_other <- tibble::tribble(
  ~family_label, ~plot_title,                                        ~file_stub,           ~width, ~height, ~ncol,
  "nu",          "Posterior distributions for intercept parameters", "nu_parameters",      11,     8,       4,
  "theta",       "Posterior distributions for theta parameters",     "theta_parameters",   11,     9,       4,
  "psi",         "Posterior distributions for psi parameters",       "psi_parameters",     12,     10,      4
)

plots_other_density <- list()

for (i in seq_len(nrow(plot_specs_other))) {
  
  fam_i <- plot_specs_other$family_label[i]
  title_i <- plot_specs_other$plot_title[i]
  stub_i <- plot_specs_other$file_stub[i]
  width_i <- plot_specs_other$width[i]
  height_i <- plot_specs_other$height[i]
  ncol_i <- plot_specs_other$ncol[i]
  
  for (sex_i in group_levels) {
    
    cat("\nPreparing density plot for ", fam_i, " - ", sex_i, "\n", sep = "")
    
    group_i <- case_when(
      sex_i == "boys"  ~ 1,
      sex_i == "girls" ~ 2,
      TRUE ~ NA_real_
    )
    
    meta_i <- summary_table %>%
      filter(
        as.character(family_label) == fam_i,
        group == group_i
      ) %>%
      mutate(
        pretty_param = case_when(
          op == "~~" ~ paste0(lhs, " ~~ ", rhs),
          op == "~1" ~ paste0(lhs, " ~1"),
          op == "~"  ~ paste0(lhs, " ~ ", rhs),
          TRUE ~ parameter
        )
      ) %>%
      select(
        family_label,
        group,
        lhs,
        rhs,
        op,
        parameter,
        pretty_param
      ) %>%
      distinct()
    
    if (nrow(meta_i) == 0) {
      cat("No metadata for ", fam_i, " - ", sex_i, ". Skipping.\n", sep = "")
      next
    }
    
    dat_i <- all_draws_mapped %>%
      filter(
        as.character(family_label) == fam_i,
        group == group_i,
        is.finite(value)
      ) %>%
      select(
        stan_name,
        imputation,
        value,
        family_label,
        group,
        lhs,
        rhs,
        op,
        parameter
      ) %>%
      left_join(
        meta_i,
        by = c(
          "family_label",
          "group",
          "lhs",
          "rhs",
          "op",
          "parameter"
        )
      ) %>%
      filter(!is.na(pretty_param))
    
    if (nrow(dat_i) == 0) {
      cat("No draws for ", fam_i, " - ", sex_i, ". Skipping.\n", sep = "")
      rm(dat_i, meta_i)
      gc()
      next
    }
    
    dat_i <- dat_i %>%
      sample_draws_for_plot() %>%
      mutate(
        group_label = sex_i,
        facet_label = factor(pretty_param, levels = unique(pretty_param))
      )
    
    if (nrow(dat_i) == 0) {
      cat("No sampled draws for ", fam_i, " - ", sex_i, ". Skipping.\n", sep = "")
      rm(dat_i, meta_i)
      gc()
      next
    }
    
    if (n_distinct(dat_i$facet_label) == 0 || n_distinct(dat_i$value) < 2) {
      cat("Not enough variation for ", fam_i, " - ", sex_i, ". Skipping.\n", sep = "")
      rm(dat_i, meta_i)
      gc()
      next
    }
    
    p_i <- ggplot(dat_i, aes(x = value)) +
      geom_density(
        fill = NA,
        colour = "#14D9BB",
        linewidth = 0.6,
        na.rm = TRUE,
        adjust = 1
      ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        linewidth = 0.3,
        colour = "grey50"
      ) +
      facet_wrap(~ facet_label, scales = "free_y", ncol = ncol_i) +
      coord_cartesian(xlim = c(-1, 1)) +
      scale_x_continuous(
        breaks = seq(-1, 1, by = 0.25),
        labels = function(x) sprintf("%.2f", x)
      ) +
      labs(
        title = paste0(title_i, " - ", sex_i),
        x = "Parameter value",
        y = "Density"
      ) +
      theme_results(9) +
      theme(strip.text = element_text(size = 8))
    
    plots_other_density[[paste(fam_i, sex_i, sep = "_")]] <- p_i
    
    save_plot(
      p_i,
      paste0(model_id, "_density_", stub_i, "_", sex_i, "_mi"),
      width_i,
      height_i
    )
    
    print(p_i)
    
    rm(dat_i, meta_i, p_i)
    gc()
  }
}

############################################################
########### 19. Robust vs uncertain effects summary ########
############################################################

plot_robust <- results_beta_plot %>%
  mutate(
    robustness = case_when(
      q2.5 > 0 ~ "robust positive",
      q97.5 < 0 ~ "robust negative",
      TRUE ~ "uncertain"
    )
  ) %>%
  count(class, group_label, robustness) %>%
  group_by(class, group_label) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

plot_robust_boys <- plot_robust %>%
  filter(group_label == "boys")

p_robust_boys <- ggplot(plot_robust_boys, aes(x = class, y = prop, fill = robustness)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c(
      "robust positive" = "#14D9BB",
      "robust negative" = "#F58518",
      "uncertain" = "grey70"
    )
  ) +
  labs(
    title = "Robust vs uncertain effects across parameter classes - boys",
    subtitle = "Classification based on 95% credible intervals",
    x = NULL,
    y = "Proportion of parameters",
    fill = NULL
  ) +
  theme_results(12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

print(p_robust_boys)
save_plot(p_robust_boys, paste0(model_id, "_robustness_summary_boys_mi"), 11, 7)

plot_robust_girls <- plot_robust %>%
  filter(group_label == "girls")

p_robust_girls <- ggplot(plot_robust_girls, aes(x = class, y = prop, fill = robustness)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(
    values = c(
      "robust positive" = "#14D9BB",
      "robust negative" = "#F58518",
      "uncertain" = "grey70"
    )
  ) +
  labs(
    title = "Robust vs uncertain effects across parameter classes - girls",
    subtitle = "Classification based on 95% credible intervals",
    x = NULL,
    y = "Proportion of parameters",
    fill = NULL
  ) +
  theme_results(12) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

print(p_robust_girls)
save_plot(p_robust_girls, paste0(model_id, "_robustness_summary_girls_mi"), 11, 7)

############################################################
############## 20. Heatmap of mean effects #################
############################################################

heatmap_data <- results_beta_plot %>%
  mutate(
    x_axis = case_when(
      phase == "dynamic" ~ as.character(lag),
      TRUE ~ as.character(phase)
    )
  ) %>%
  group_by(class, x_axis, group_label) %>%
  summarise(
    mean_effect = mean(mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    x_axis = factor(
      x_axis,
      levels = c(
        "3y baseline",
        "5y residual",
        "3 -> 5",
        "5 -> 9",
        "9 -> 15"
      )
    )
  )

p_heatmap <- ggplot(heatmap_data, aes(x = x_axis, y = class, fill = mean_effect)) +
  geom_tile(colour = "white") +
  facet_wrap(~ group_label) +
  scale_fill_gradient2(
    low = "#F58518",
    mid = "#F7F7F7",
    high = "#14D9BB",
    midpoint = 0
  ) +
  labs(
    title = "Mean effects across parameter classes",
    subtitle = "Average posterior means by phase or lag and sex",
    x = "Phase or developmental interval",
    y = "Effect class",
    fill = "Mean effect"
  ) +
  theme_results(12) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))

print(p_heatmap)
save_plot(p_heatmap, paste0(model_id, "_heatmap_effects_mi"), 14, 8)

############################################################
########## 21. Developmental variation in AR paths #########
############################################################

trajectory_ar <- results_beta_plot %>%
  filter(class %in% c("ADHD AR", "EXT AR", "INT AR"), !is.na(lag))

p_trajectory_ar <- ggplot(
  trajectory_ar,
  aes(x = lag, y = mean, group = class, colour = class)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.08, linewidth = 0.5) +
  facet_wrap(~ group_label) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_colour_manual(
    values = c(
      "ADHD AR" = "#14D9BB",
      "EXT AR"  = "#F58518",
      "INT AR"  = "#D4B24C"
    )
  ) +
  labs(
    title = "Developmental variation in autoregressive effects",
    x = "Developmental interval",
    y = "Posterior estimate",
    colour = "Parameter class"
  ) +
  theme_results(12) +
  theme(legend.position = "bottom")

print(p_trajectory_ar)
save_plot(p_trajectory_ar, paste0(model_id, "_trajectory_ar_mi"), 11, 6.5)

############################################################
######## 22. Forward cross-lagged developmental plot #######
############################################################

trajectory_forward <- results_beta_plot %>%
  filter(class %in% c("adhd -> ext", "adhd -> int"), !is.na(lag))

p_trajectory_forward <- ggplot(
  trajectory_forward,
  aes(x = lag, y = mean, group = class, colour = class)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.08, linewidth = 0.5) +
  facet_wrap(~ group_label) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_colour_manual(
    values = c(
      "adhd -> ext" = "#14D9BB",
      "adhd -> int" = "#F58518"
    )
  ) +
  labs(
    title = "Developmental variation in forward cross-lagged effects",
    x = "Developmental interval",
    y = "Posterior estimate",
    colour = "Parameter class"
  ) +
  theme_results(12) +
  theme(legend.position = "bottom")

print(p_trajectory_forward)
save_plot(p_trajectory_forward, paste0(model_id, "_trajectory_forward_crosslag_mi"), 11, 6.5)

############################################################
####### 23. Backward cross-lagged developmental plot #######
############################################################

trajectory_backward <- results_beta_plot %>%
  filter(class %in% c("ext -> adhd", "int -> adhd"), !is.na(lag))

p_trajectory_backward <- ggplot(
  trajectory_backward,
  aes(x = lag, y = mean, group = class, colour = class)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.08, linewidth = 0.5) +
  facet_wrap(~ group_label) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_colour_manual(
    values = c(
      "ext -> adhd" = "#14D9BB",
      "int -> adhd" = "#F58518"
    )
  ) +
  labs(
    title = "Developmental variation in backward cross-lagged effects",
    x = "Developmental interval",
    y = "Posterior estimate",
    colour = "Parameter class"
  ) +
  theme_results(12) +
  theme(legend.position = "bottom")

print(p_trajectory_backward)
save_plot(p_trajectory_backward, paste0(model_id, "_trajectory_backward_crosslag_mi"), 11, 6.5)

############################################################
######## 24. Lateral cross-lagged developmental plot #######
############################################################

trajectory_lateral <- results_beta_plot %>%
  filter(class %in% c("ext -> int", "int -> ext"), !is.na(lag))

p_trajectory_lateral <- ggplot(
  trajectory_lateral,
  aes(x = lag, y = mean, group = class, colour = class)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.08, linewidth = 0.5) +
  facet_wrap(~ group_label) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_colour_manual(
    values = c(
      "ext -> int" = "#14D9BB",
      "int -> ext" = "#F58518"
    )
  ) +
  labs(
    title = "Developmental variation in ext–int cross-lagged effects",
    x = "Developmental interval",
    y = "Posterior estimate",
    colour = "Parameter class"
  ) +
  theme_results(12) +
  theme(legend.position = "bottom")

print(p_trajectory_lateral)
save_plot(p_trajectory_lateral, paste0(model_id, "_trajectory_ext_int_crosslag_mi"), 11, 6.5)

############################################################
######### 25. Adversity effects at age 3 baseline ##########
############################################################

trajectory_adv3 <- results_beta_plot %>%
  filter(phase == "3y baseline") %>%
  mutate(
    outcome = case_when(
      str_detect(class, "adhd$") ~ "ADHD",
      str_detect(class, "ext$") ~ "Externalizing",
      str_detect(class, "int$") ~ "Internalizing"
    )
  )

p_adv3 <- ggplot(trajectory_adv3, aes(x = class, y = mean, colour = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_point(size = 2.6) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.15, linewidth = 0.5) +
  facet_wrap(~ group_label) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_colour_manual(
    values = c(
      "ADHD" = "#14D9BB",
      "Externalizing" = "#F58518",
      "Internalizing" = "#D4B24C"
    )
  ) +
  labs(
    title = "Adversity effects on psychopathology at age 3",
    x = "Predictor -> outcome class",
    y = "Posterior estimate",
    colour = "Outcome"
  ) +
  theme_results(12) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    legend.position = "bottom"
  )

print(p_adv3)
save_plot(p_adv3, paste0(model_id, "_adversity_3y_mi"), 13, 7)

############################################################
######### 26. Adversity effects at age 5 residual ##########
############################################################

trajectory_adv5 <- results_beta_plot %>%
  filter(phase == "5y residual") %>%
  mutate(
    outcome = case_when(
      str_detect(class, "adhd$") ~ "ADHD",
      str_detect(class, "ext$") ~ "Externalizing",
      str_detect(class, "int$") ~ "Internalizing"
    )
  )

p_adv5 <- ggplot(trajectory_adv5, aes(x = class, y = mean, colour = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_point(size = 2.6) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5), width = 0.15, linewidth = 0.5) +
  facet_wrap(~ group_label) +
  coord_cartesian(ylim = c(-1, 1)) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_colour_manual(
    values = c(
      "ADHD" = "#14D9BB",
      "Externalizing" = "#F58518",
      "Internalizing" = "#D4B24C"
    )
  ) +
  labs(
    title = "Residual adversity effects on psychopathology at age 5",
    x = "Predictor -> outcome class",
    y = "Posterior estimate",
    colour = "Outcome"
  ) +
  theme_results(12) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    legend.position = "bottom"
  )

print(p_adv5)
save_plot(p_adv5, paste0(model_id, "_adversity_5y_residual_mi"), 13, 7)

############################################################
#################### 27. Final checks ######################
############################################################

cat("\n--- visualization checks ---\n")
cat("Rows in summary_table     :", nrow(summary_table), "\n")
cat("Rows in results_beta      :", nrow(results_beta), "\n")
cat("Rows in results_theta     :", nrow(results_theta), "\n")
cat("Rows in results_psi       :", nrow(results_psi), "\n")
cat("Rows in results_nu        :", nrow(results_nu), "\n")
cat("Rows in all_draws         :", nrow(all_draws), "\n")
cat("Rows in all_draws_mapped  :", nrow(all_draws_mapped), "\n")
cat("Rows in results_beta_plot :", nrow(results_beta_plot), "\n")
cat("Object all_draws_beta_plot was not created to save memory.\n")

cat("\n--- saved figures in working directory ---\n")
print(list.files(getwd(), pattern = "^model_2_.*\\.(png|pdf)$"))

expected_files <- list.files(fig_dir, pattern = "^model_2_.*\\.(png|pdf)$", full.names = FALSE)
wd_files <- list.files(getwd(), pattern = "^model_2_.*\\.(png|pdf)$", full.names = FALSE)

cat("\nNumber of model_2 figure files in fig_dir:", length(expected_files), "\n")
cat("Number of model_2 figure files in working directory:", length(wd_files), "\n")

cat("\nFiles present in fig_dir but missing in working directory:\n")
print(setdiff(expected_files, wd_files))
