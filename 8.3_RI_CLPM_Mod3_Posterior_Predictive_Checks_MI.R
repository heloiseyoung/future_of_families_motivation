library(blavaan)
library(lavaan)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)

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

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Tables")) {
  table_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Tables"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Tables")) {
  table_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Tables"
} else {
  stop("Posterior Predictive Checks_Tables directory not found.")
}

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Figures")) {
  fig_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Figures"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Figures")) {
  fig_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Posterior Predictive Checks_Figures"
} else {
  stop("Posterior Predictive Checks_Figures directory not found.")
}

options(mc.cores = parallel::detectCores())

############################################################
######################## 1. Settings #######################
############################################################

n_imp <- 20
nrep_post <- 4000
n_plot_sims_per_imp <- 5
n_line_sims_total <- 30
seed_base <- 1234

############################################################
######################## 2. Model ##########################
############################################################

riclpm_model_3 <- '

  #####################################################
  ### Create between components (random intercepts) ###
  #####################################################
  
  RI_adhd =~ 1*Adhd_3y.z + 1*Adhd_5y.z + 1*Adhd_9y.z + 1*Adhd_15y.z
  RI_ext  =~ 1*Ext_3y.z  + 1*Ext_5y.z  + 1*Ext_9y.z  + 1*Ext_15y.z
  RI_int  =~ 1*Int_3y.z  + 1*Int_5y.z  + 1*Int_9y.z  + 1*Int_15y.z

  #####################################################
  ###### Create within-person centered variables ######
  #####################################################
  
  wAdhd_3  =~ 1*Adhd_3y.z
  wAdhd_5  =~ 1*Adhd_5y.z
  wAdhd_9  =~ 1*Adhd_9y.z
  wAdhd_15 =~ 1*Adhd_15y.z

  wExt_3  =~ 1*Ext_3y.z
  wExt_5  =~ 1*Ext_5y.z
  wExt_9  =~ 1*Ext_9y.z
  wExt_15 =~ 1*Ext_15y.z

  wInt_3  =~ 1*Int_3y.z
  wInt_5  =~ 1*Int_5y.z
  wInt_9  =~ 1*Int_9y.z
  wInt_15 =~ 1*Int_15y.z

  ###############################################################################################
  ### Estimate lagged effects between within-person centered variables: AR 3y > 5y > 9y > 15y ###
  ###############################################################################################

  wAdhd_5  ~ prior("normal(0.05,0.10)")*wAdhd_3
  wAdhd_9  ~ prior("normal(0.05,0.10)")*wAdhd_5
  wAdhd_15 ~ prior("normal(0.05,0.10)")*wAdhd_9
  
  wExt_5   ~ prior("normal(0.05,0.10)")*wExt_3
  wExt_9   ~ prior("normal(0.05,0.10)")*wExt_5
  wExt_15  ~ prior("normal(0.05,0.10)")*wExt_9
  
  wInt_5   ~ prior("normal(0.05,0.10)")*wInt_3
  wInt_9   ~ prior("normal(0.05,0.10)")*wInt_5
  wInt_15  ~ prior("normal(0.05,0.10)")*wInt_9

  ###########################################################################################  
  ### Estimate lagged effects between within-person centered variables: CL adhd > ext/int ###
  ###########################################################################################  

  wExt_5   ~ prior("normal(0.15,0.15)")*wAdhd_3
  wExt_9   ~ prior("normal(0.15,0.15)")*wAdhd_5
  wExt_15  ~ prior("normal(0.15,0.15)")*wAdhd_9
  
  wInt_5   ~ prior("normal(0.15,0.15)")*wAdhd_3
  wInt_9   ~ prior("normal(0.15,0.15)")*wAdhd_5
  wInt_15  ~ prior("normal(0.15,0.15)")*wAdhd_9
  
  wAdhd_5   ~ prior("normal(0.15,0.15)")*wExt_3
  wAdhd_9   ~ prior("normal(0.15,0.15)")*wExt_5
  wAdhd_15  ~ prior("normal(0.15,0.15)")*wExt_9
  
  wAdhd_5   ~ prior("normal(0.15,0.15)")*wInt_3
  wAdhd_9   ~ prior("normal(0.15,0.15)")*wInt_5
  wAdhd_15  ~ prior("normal(0.15,0.15)")*wInt_9
  
  wExt_5   ~ prior("normal(0.15,0.15)")*wInt_3
  wExt_9   ~ prior("normal(0.15,0.15)")*wInt_5
  wExt_15  ~ prior("normal(0.15,0.15)")*wInt_9
  
  wInt_5   ~ prior("normal(0.15,0.15)")*wExt_3
  wInt_9   ~ prior("normal(0.15,0.15)")*wExt_5
  wInt_15  ~ prior("normal(0.15,0.15)")*wExt_9

  ##################################################################################  
  ### Estimate covariance between within-person centered variables at first wave ###
  ##################################################################################  

  wAdhd_3  ~~ prior("lkj_corr(3)")*wExt_3 + prior("lkj_corr(3)")*wInt_3
  wExt_3   ~~ prior("lkj_corr(3)")*wInt_3

  ##################################################################################  
  ### Estimate covariances between residuals of within-person centered variables ###
  ################################################################################## 
  
  wAdhd_5  ~~ prior("lkj_corr(3)")*wExt_5 + prior("lkj_corr(3)")*wInt_5
  wExt_5   ~~ prior("lkj_corr(3)")*wInt_5

  wAdhd_9  ~~ prior("lkj_corr(3)")*wExt_9 + prior("lkj_corr(3)")*wInt_9
  wExt_9   ~~ prior("lkj_corr(3)")*wInt_9

  wAdhd_15 ~~ prior("lkj_corr(3)")*wExt_15 + prior("lkj_corr(3)")*wInt_15
  wExt_15  ~~ prior("lkj_corr(3)")*wInt_15
  
  ############################################################# 
  ### Estimate variance and covariance of random intercepts ###
  #############################################################
  
  RI_ext  ~~ prior("gamma(2,4)[sd]")*RI_ext
  RI_int  ~~ prior("gamma(2,4)[sd]")*RI_int
  RI_adhd ~~ prior("gamma(2,4)[sd]")*RI_adhd
  
  RI_adhd ~~ RI_ext + RI_int
  RI_ext  ~~ RI_int

  ########################################################################
  ### Estimate (residual) variance of within-person centered variables ###
  ########################################################################
  
  wAdhd_3 ~~ prior("gamma(2,4)[sd]")*wAdhd_3
  wExt_3  ~~ prior("gamma(2,4)[sd]")*wExt_3
  wInt_3  ~~ prior("gamma(2,4)[sd]")*wInt_3

  wAdhd_5 ~~ prior("gamma(2,4)[sd]")*wAdhd_5
  wExt_5  ~~ prior("gamma(2,4)[sd]")*wExt_5
  wInt_5  ~~ prior("gamma(2,4)[sd]")*wInt_5
 
  wAdhd_9 ~~ prior("gamma(2,4)[sd]")*wAdhd_9
  wExt_9  ~~ prior("gamma(2,4)[sd]")*wExt_9
  wInt_9  ~~ prior("gamma(2,4)[sd]")*wInt_9

  wAdhd_15 ~~ prior("gamma(2,4)[sd]")*wAdhd_15
  wExt_15  ~~ prior("gamma(2,4)[sd]")*wExt_15
  wInt_15  ~~ prior("gamma(2,4)[sd]")*wInt_15
  
  ############################################################
  ### covariates: early adversity -> between-person traits ###
  ############################################################

  wAdhd_3 ~ prior("normal(0.10,0.10)")*early.stochasticity.3y.z +
            prior("normal(0.10,0.10)")*early.volatility.3y.z +
            prior("normal(0.10,0.10)")*early.threat.3y.z +
            prior("normal(0.10,0.10)")*early.deprivation.3y.z
  
  wExt_3  ~ prior("normal(0.10,0.10)")*early.stochasticity.3y.z +
            prior("normal(0.10,0.10)")*early.volatility.3y.z +
            prior("normal(0.10,0.10)")*early.threat.3y.z +
            prior("normal(0.10,0.10)")*early.deprivation.3y.z
  
  wInt_3  ~ prior("normal(0.10,0.10)")*early.stochasticity.3y.z + 
            prior("normal(0.10,0.10)")*early.volatility.3y.z +
            prior("normal(0.10,0.10)")*early.threat.3y.z +
            prior("normal(0.10,0.10)")*early.deprivation.3y.z
  
  wAdhd_5 ~ prior("normal(0.10,0.10)")*stoch5_resid +
            prior("normal(0.10,0.10)")*vol5_resid +
            prior("normal(0.10,0.10)")*threat5_resid +
            prior("normal(0.10,0.10)")*depr5_resid
  
  wExt_5  ~ prior("normal(0.10,0.10)")*stoch5_resid +
            prior("normal(0.10,0.10)")*vol5_resid +
            prior("normal(0.10,0.10)")*threat5_resid +
            prior("normal(0.10,0.10)")*depr5_resid
  
  wInt_5  ~ prior("normal(0.10,0.10)")*stoch5_resid +
            prior("normal(0.10,0.10)")*vol5_resid +
            prior("normal(0.10,0.10)")*threat5_resid +
            prior("normal(0.10,0.10)")*depr5_resid

  #############################################################
  ### efforts at 15: predicted by within-person state at 15 ###
  #############################################################
  
  mating.behaviour.z ~ prior("normal(0.10,0.10)")*wAdhd_15 +
                       prior("normal(0.10,0.10)")*wExt_15  + 
                       prior("normal(0.00,0.10)")*wInt_15
  
  risky.health.behaviour.z ~ prior("normal(0.10,0.10)")*wAdhd_15 + 
                             prior("normal(0.10,0.10)")*wExt_15  +
                             prior("normal(0.00,0.10)")*wInt_15

  risky.health.behaviour.z ~~ prior("lkj_corr(3)")*mating.behaviour.z

  #########################################
  ### corr RI <-> LHS (between overlap) ###
  #########################################
  
  mating.behaviour.z ~~ RI_adhd + RI_ext + RI_int
  risky.health.behaviour.z ~~ RI_adhd + RI_ext + RI_int
  
  ############################################################
  ################ observed intercepts: nu ###################
  ############################################################
  
  Adhd_3y.z  ~ prior("normal(0,0.5)")*1
  Adhd_5y.z  ~ prior("normal(0,0.5)")*1
  Adhd_9y.z  ~ prior("normal(0,0.5)")*1
  Adhd_15y.z ~ prior("normal(0,0.5)")*1

  Ext_3y.z   ~ prior("normal(0,0.5)")*1
  Ext_5y.z   ~ prior("normal(0,0.5)")*1
  Ext_9y.z   ~ prior("normal(0,0.5)")*1
  Ext_15y.z  ~ prior("normal(0,0.5)")*1

  Int_3y.z   ~ prior("normal(0,0.5)")*1
  Int_5y.z   ~ prior("normal(0,0.5)")*1
  Int_9y.z   ~ prior("normal(0,0.5)")*1
  Int_15y.z  ~ prior("normal(0,0.5)")*1
  
  early.stochasticity.3y.z ~ prior("normal(0,0.5)")*1
  early.volatility.3y.z   ~ prior("normal(0,0.5)")*1
  early.threat.3y.z       ~ prior("normal(0,0.5)")*1
  early.deprivation.3y.z  ~ prior("normal(0,0.5)")*1
  
  stoch5_resid   ~ prior("normal(0,0.5)")*1
  vol5_resid     ~ prior("normal(0,0.5)")*1
  threat5_resid  ~ prior("normal(0,0.5)")*1
  depr5_resid    ~ prior("normal(0,0.5)")*1
  
  mating.behaviour.z       ~ prior("normal(0,0.5)")*1
  risky.health.behaviour.z ~ prior("normal(0,0.5)")*1
  
  ############################################################
  ################ residual SDs: theta #######################
  ############################################################

  Adhd_3y.z  ~~ 0*Adhd_3y.z
  Adhd_5y.z  ~~ 0*Adhd_5y.z
  Adhd_9y.z  ~~ 0*Adhd_9y.z
  Adhd_15y.z ~~ 0*Adhd_15y.z
  
  Ext_3y.z   ~~ 0*Ext_3y.z
  Ext_5y.z   ~~ 0*Ext_5y.z
  Ext_9y.z   ~~ 0*Ext_9y.z
  Ext_15y.z  ~~ 0*Ext_15y.z

  Int_3y.z   ~~ 0*Int_3y.z
  Int_5y.z   ~~ 0*Int_5y.z
  Int_9y.z   ~~ 0*Int_9y.z
  Int_15y.z  ~~ 0*Int_15y.z

  early.stochasticity.3y.z ~~ prior("gamma(2,2)[sd]")*early.stochasticity.3y.z
  early.volatility.3y.z   ~~ prior("gamma(2,2)[sd]")*early.volatility.3y.z
  early.threat.3y.z       ~~ prior("gamma(2,2)[sd]")*early.threat.3y.z
  early.deprivation.3y.z  ~~ prior("gamma(2,2)[sd]")*early.deprivation.3y.z
  
  stoch5_resid   ~~ prior("gamma(2,2)[sd]")*stoch5_resid
  vol5_resid     ~~ prior("gamma(2,2)[sd]")*vol5_resid
  threat5_resid  ~~ prior("gamma(2,2)[sd]")*threat5_resid
  depr5_resid    ~~ prior("gamma(2,2)[sd]")*depr5_resid
  
  mating.behaviour.z       ~~ prior("gamma(2,4)[sd]")*mating.behaviour.z
  risky.health.behaviour.z ~~ prior("gamma(2,4)[sd]")*risky.health.behaviour.z
  
'

############################################################
################### 3. Variable groups #####################
############################################################

vars_lhs <- c("mating.behaviour.z", "risky.health.behaviour.z")

vars_adversity <- c(
  "early.stochasticity.3y.z", "early.volatility.3y.z",
  "early.threat.3y.z", "early.deprivation.3y.z",
  "stoch5_resid", "vol5_resid", "threat5_resid", "depr5_resid"
)

vars_psych <- c(
  "Adhd_3y.z", "Adhd_5y.z", "Adhd_9y.z", "Adhd_15y.z",
  "Ext_3y.z",  "Ext_5y.z",  "Ext_9y.z",  "Ext_15y.z",
  "Int_3y.z",  "Int_5y.z",  "Int_9y.z",  "Int_15y.z"
)

vars_psych3  <- c("Adhd_3y.z", "Ext_3y.z", "Int_3y.z")
vars_psych5  <- c("Adhd_5y.z", "Ext_5y.z", "Int_5y.z")
vars_psych15 <- c("Adhd_15y.z", "Ext_15y.z", "Int_15y.z")

vars_sim <- c(vars_lhs, vars_adversity, vars_psych)

############################################################
################### 4. Helper functions ####################
############################################################

bind_groups_with_sex <- function(sim_rep, vars_keep, sex_codes = c(0, 1)) {
  
  if (!is.list(sim_rep) || length(sim_rep) != 2) {
    stop("Expected one replication to be a list of length 2 (boys, girls).")
  }
  
  make_df <- function(x, sex_value) {
    x <- as.data.frame(x)
    
    if (is.null(colnames(x)) || !all(vars_keep %in% colnames(x))) {
      if (ncol(x) < length(vars_keep)) {
        stop(
          "Simulated group dataset has ", ncol(x),
          " columns, but at least ", length(vars_keep),
          " are needed for vars_keep."
        )
      }
      x <- x[, seq_along(vars_keep), drop = FALSE]
      colnames(x) <- vars_keep
    } else {
      x <- x[, vars_keep, drop = FALSE]
    }
    
    x %>%
      mutate(sex = sex_value) %>%
      select(sex, all_of(vars_keep))
  }
  
  bind_rows(
    make_df(sim_rep[[1]], sex_codes[1]),
    make_df(sim_rep[[2]], sex_codes[2])
  )
}

safe_cor <- function(x, y) {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
}

safe_quantile <- function(x, probs) {
  as.numeric(quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
}

sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  s <- sd(x)
  if (!is.finite(s) || s == 0) return(0)
  m <- mean(x)
  mean(((x - m) / s)^3)
}

sample_excess_kurtosis <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  s <- sd(x)
  if (!is.finite(s) || s == 0) return(0)
  m <- mean(x)
  mean(((x - m) / s)^4) - 3
}

safe_lm_coef <- function(formula, data, term) {
  fit <- try(lm(formula, data = data), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  cf <- coef(fit)
  if (!(term %in% names(cf))) return(NA_real_)
  unname(cf[term])
}

safe_lm_r2 <- function(formula, data) {
  fit <- try(lm(formula, data = data), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  unname(summary(fit)$r.squared)
}

get_corr_matrix <- function(dat, vars) {
  cor(dat[, vars, drop = FALSE], use = "pairwise.complete.obs")
}

save_plot <- function(plot, filename, width, height) {
  ggsave(file.path(fig_dir, paste0(filename, ".png")), plot, width = width, height = height, dpi = 400)
  ggsave(file.path(fig_dir, paste0(filename, ".pdf")), plot, width = width, height = height)
}

extract_summary_df <- function(fit_sum_i, imp) {
  
  out <- NULL
  
  if (!is.null(fit_sum_i$pe) && is.data.frame(fit_sum_i$pe)) {
    out <- as.data.frame(fit_sum_i$pe, stringsAsFactors = FALSE)
  } else {
    idx_df <- which(vapply(fit_sum_i, is.data.frame, logical(1)))
    if (length(idx_df) > 0) {
      out <- as.data.frame(fit_sum_i[[idx_df[1]]], stringsAsFactors = FALSE)
    }
  }
  
  if (is.null(out)) {
    stop("Could not extract a parameter summary table from fit_sum_i for imputation ", imp, ".")
  }
  
  out$imputation <- imp
  
  out
}

############################################################
######## 5. Targeted posterior predictive stat functions ###
############################################################

get_stats_marginal <- function(dat, vars, sex_value, dataset_id, source, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(paste0(vars, "_mean"), paste0(vars, "_sd")),
    value = c(
      sapply(dat[, vars, drop = FALSE], mean, na.rm = TRUE),
      sapply(dat[, vars, drop = FALSE], sd,   na.rm = TRUE)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_stats_ar <- function(dat, sex_value, dataset_id, source, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      "adhd_35","adhd_59","adhd_915",
      "ext_35","ext_59","ext_915",
      "int_35","int_59","int_915"
    ),
    value = c(
      safe_cor(dat$Adhd_3y.z, dat$Adhd_5y.z),
      safe_cor(dat$Adhd_5y.z, dat$Adhd_9y.z),
      safe_cor(dat$Adhd_9y.z, dat$Adhd_15y.z),
      safe_cor(dat$Ext_3y.z,  dat$Ext_5y.z),
      safe_cor(dat$Ext_5y.z,  dat$Ext_9y.z),
      safe_cor(dat$Ext_9y.z,  dat$Ext_15y.z),
      safe_cor(dat$Int_3y.z,  dat$Int_5y.z),
      safe_cor(dat$Int_5y.z,  dat$Int_9y.z),
      safe_cor(dat$Int_9y.z,  dat$Int_15y.z)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_stats_cl <- function(dat, sex_value, dataset_id, source, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      "adhd3_int5","adhd5_int9","adhd9_int15",
      "adhd3_ext5","adhd5_ext9","adhd9_ext15",
      "int3_adhd5","int5_adhd9","int9_adhd15",
      "ext3_adhd5","ext5_adhd9","ext9_adhd15",
      "ext3_int5","ext5_int9","ext9_int15",
      "int3_ext5","int5_ext9","int9_ext15"
    ),
    value = c(
      safe_cor(dat$Adhd_3y.z, dat$Int_5y.z),
      safe_cor(dat$Adhd_5y.z, dat$Int_9y.z),
      safe_cor(dat$Adhd_9y.z, dat$Int_15y.z),
      safe_cor(dat$Adhd_3y.z, dat$Ext_5y.z),
      safe_cor(dat$Adhd_5y.z, dat$Ext_9y.z),
      safe_cor(dat$Adhd_9y.z, dat$Ext_15y.z),
      safe_cor(dat$Int_3y.z,  dat$Adhd_5y.z),
      safe_cor(dat$Int_5y.z,  dat$Adhd_9y.z),
      safe_cor(dat$Int_9y.z,  dat$Adhd_15y.z),
      safe_cor(dat$Ext_3y.z,  dat$Adhd_5y.z),
      safe_cor(dat$Ext_5y.z,  dat$Adhd_9y.z),
      safe_cor(dat$Ext_9y.z,  dat$Adhd_15y.z),
      safe_cor(dat$Ext_3y.z,  dat$Int_5y.z),
      safe_cor(dat$Ext_5y.z,  dat$Int_9y.z),
      safe_cor(dat$Ext_9y.z,  dat$Int_15y.z),
      safe_cor(dat$Int_3y.z,  dat$Ext_5y.z),
      safe_cor(dat$Int_5y.z,  dat$Ext_9y.z),
      safe_cor(dat$Int_9y.z,  dat$Ext_15y.z)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_stats_withinwave <- function(dat, sex_value, dataset_id, source, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      "adhd_ext_3","adhd_int_3","ext_int_3",
      "adhd_ext_5","adhd_int_5","ext_int_5",
      "adhd_ext_9","adhd_int_9","ext_int_9",
      "adhd_ext_15","adhd_int_15","ext_int_15"
    ),
    value = c(
      safe_cor(dat$Adhd_3y.z,  dat$Ext_3y.z),
      safe_cor(dat$Adhd_3y.z,  dat$Int_3y.z),
      safe_cor(dat$Ext_3y.z,   dat$Int_3y.z),
      safe_cor(dat$Adhd_5y.z,  dat$Ext_5y.z),
      safe_cor(dat$Adhd_5y.z,  dat$Int_5y.z),
      safe_cor(dat$Ext_5y.z,   dat$Int_5y.z),
      safe_cor(dat$Adhd_9y.z,  dat$Ext_9y.z),
      safe_cor(dat$Adhd_9y.z,  dat$Int_9y.z),
      safe_cor(dat$Ext_9y.z,   dat$Int_9y.z),
      safe_cor(dat$Adhd_15y.z, dat$Ext_15y.z),
      safe_cor(dat$Adhd_15y.z, dat$Int_15y.z),
      safe_cor(dat$Ext_15y.z,  dat$Int_15y.z)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_stats_adversity <- function(dat, sex_value, dataset_id, source, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      "stoch3_adhd3","stoch3_ext3","stoch3_int3",
      "vol3_adhd3","vol3_ext3","vol3_int3",
      "threat3_adhd3","threat3_ext3","threat3_int3",
      "depr3_adhd3","depr3_ext3","depr3_int3",
      "stoch5_adhd5","stoch5_ext5","stoch5_int5",
      "vol5_adhd5","vol5_ext5","vol5_int5",
      "threat5_adhd5","threat5_ext5","threat5_int5",
      "depr5_adhd5","depr5_ext5","depr5_int5"
    ),
    value = c(
      safe_cor(dat$early.stochasticity.3y.z, dat$Adhd_3y.z),
      safe_cor(dat$early.stochasticity.3y.z, dat$Ext_3y.z),
      safe_cor(dat$early.stochasticity.3y.z, dat$Int_3y.z),
      safe_cor(dat$early.volatility.3y.z, dat$Adhd_3y.z),
      safe_cor(dat$early.volatility.3y.z, dat$Ext_3y.z),
      safe_cor(dat$early.volatility.3y.z, dat$Int_3y.z),
      safe_cor(dat$early.threat.3y.z, dat$Adhd_3y.z),
      safe_cor(dat$early.threat.3y.z, dat$Ext_3y.z),
      safe_cor(dat$early.threat.3y.z, dat$Int_3y.z),
      safe_cor(dat$early.deprivation.3y.z, dat$Adhd_3y.z),
      safe_cor(dat$early.deprivation.3y.z, dat$Ext_3y.z),
      safe_cor(dat$early.deprivation.3y.z, dat$Int_3y.z),
      safe_cor(dat$stoch5_resid, dat$Adhd_5y.z),
      safe_cor(dat$stoch5_resid, dat$Ext_5y.z),
      safe_cor(dat$stoch5_resid, dat$Int_5y.z),
      safe_cor(dat$vol5_resid, dat$Adhd_5y.z),
      safe_cor(dat$vol5_resid, dat$Ext_5y.z),
      safe_cor(dat$vol5_resid, dat$Int_5y.z),
      safe_cor(dat$threat5_resid, dat$Adhd_5y.z),
      safe_cor(dat$threat5_resid, dat$Ext_5y.z),
      safe_cor(dat$threat5_resid, dat$Int_5y.z),
      safe_cor(dat$depr5_resid, dat$Adhd_5y.z),
      safe_cor(dat$depr5_resid, dat$Ext_5y.z),
      safe_cor(dat$depr5_resid, dat$Int_5y.z)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_stats_outcomes <- function(dat, sex_value, dataset_id, source, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      "mating_risky",
      "mating_adhd15", "mating_ext15", "mating_int15",
      "risky_adhd15",  "risky_ext15",  "risky_int15"
    ),
    value = c(
      safe_cor(dat$mating.behaviour.z,       dat$risky.health.behaviour.z),
      safe_cor(dat$mating.behaviour.z,       dat$Adhd_15y.z),
      safe_cor(dat$mating.behaviour.z,       dat$Ext_15y.z),
      safe_cor(dat$mating.behaviour.z,       dat$Int_15y.z),
      safe_cor(dat$risky.health.behaviour.z, dat$Adhd_15y.z),
      safe_cor(dat$risky.health.behaviour.z, dat$Ext_15y.z),
      safe_cor(dat$risky.health.behaviour.z, dat$Int_15y.z)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_all_target_stats <- function(dat, vars_marginal, sex_value, dataset_id, source, imputation) {
  bind_rows(
    get_stats_marginal(dat, vars_marginal, sex_value, dataset_id, source, imputation),
    get_stats_ar(dat, sex_value, dataset_id, source, imputation),
    get_stats_cl(dat, sex_value, dataset_id, source, imputation),
    get_stats_withinwave(dat, sex_value, dataset_id, source, imputation),
    get_stats_adversity(dat, sex_value, dataset_id, source, imputation),
    get_stats_outcomes(dat, sex_value, dataset_id, source, imputation)
  )
}

############################################################
################ 6. Harder PPC stat functions ##############
############################################################

get_stats_shape_outcomes <- function(dat, sex_value, dataset_id, source, imputation) {
  probs <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  mating_q <- safe_quantile(dat$mating.behaviour.z, probs)
  risky_q  <- safe_quantile(dat$risky.health.behaviour.z, probs)
  
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      paste0("mating_q", c("05","25","50","75","95")),
      "mating_skew", "mating_kurtosis",
      paste0("risky_q", c("05","25","50","75","95")),
      "risky_skew", "risky_kurtosis"
    ),
    value = c(
      mating_q,
      sample_skewness(dat$mating.behaviour.z),
      sample_excess_kurtosis(dat$mating.behaviour.z),
      risky_q,
      sample_skewness(dat$risky.health.behaviour.z),
      sample_excess_kurtosis(dat$risky.health.behaviour.z)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_stats_regression_outcomes <- function(dat, sex_value, dataset_id, source, imputation) {
  form_mating <- mating.behaviour.z ~ Adhd_15y.z + Ext_15y.z + Int_15y.z
  form_risky  <- risky.health.behaviour.z ~ Adhd_15y.z + Ext_15y.z + Int_15y.z
  
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      "beta_mating_intercept",
      "beta_mating_adhd15",
      "beta_mating_ext15",
      "beta_mating_int15",
      "r2_mating",
      "beta_risky_intercept",
      "beta_risky_adhd15",
      "beta_risky_ext15",
      "beta_risky_int15",
      "r2_risky"
    ),
    value = c(
      safe_lm_coef(form_mating, dat, "(Intercept)"),
      safe_lm_coef(form_mating, dat, "Adhd_15y.z"),
      safe_lm_coef(form_mating, dat, "Ext_15y.z"),
      safe_lm_coef(form_mating, dat, "Int_15y.z"),
      safe_lm_r2(form_mating, dat),
      safe_lm_coef(form_risky, dat, "(Intercept)"),
      safe_lm_coef(form_risky, dat, "Adhd_15y.z"),
      safe_lm_coef(form_risky, dat, "Ext_15y.z"),
      safe_lm_coef(form_risky, dat, "Int_15y.z"),
      safe_lm_r2(form_risky, dat)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_stats_regression_adversity <- function(dat, sex_value, dataset_id, source, imputation) {
  
  form_adhd3 <- Adhd_3y.z ~ early.stochasticity.3y.z + early.volatility.3y.z +
    early.threat.3y.z + early.deprivation.3y.z
  
  form_ext3 <- Ext_3y.z ~ early.stochasticity.3y.z + early.volatility.3y.z +
    early.threat.3y.z + early.deprivation.3y.z
  
  form_int3 <- Int_3y.z ~ early.stochasticity.3y.z + early.volatility.3y.z +
    early.threat.3y.z + early.deprivation.3y.z
  
  form_adhd5 <- Adhd_5y.z ~ stoch5_resid + vol5_resid + threat5_resid + depr5_resid
  form_ext5  <- Ext_5y.z  ~ stoch5_resid + vol5_resid + threat5_resid + depr5_resid
  form_int5  <- Int_5y.z  ~ stoch5_resid + vol5_resid + threat5_resid + depr5_resid
  
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(
      "beta_adhd3_intercept","beta_adhd3_stoch3","beta_adhd3_vol3","beta_adhd3_threat3","beta_adhd3_depr3","r2_adhd3",
      "beta_ext3_intercept","beta_ext3_stoch3","beta_ext3_vol3","beta_ext3_threat3","beta_ext3_depr3","r2_ext3",
      "beta_int3_intercept","beta_int3_stoch3","beta_int3_vol3","beta_int3_threat3","beta_int3_depr3","r2_int3",
      "beta_adhd5_intercept","beta_adhd5_stoch5","beta_adhd5_vol5","beta_adhd5_threat5","beta_adhd5_depr5","r2_adhd5",
      "beta_ext5_intercept","beta_ext5_stoch5","beta_ext5_vol5","beta_ext5_threat5","beta_ext5_depr5","r2_ext5",
      "beta_int5_intercept","beta_int5_stoch5","beta_int5_vol5","beta_int5_threat5","beta_int5_depr5","r2_int5"
    ),
    value = c(
      safe_lm_coef(form_adhd3, dat, "(Intercept)"),
      safe_lm_coef(form_adhd3, dat, "early.stochasticity.3y.z"),
      safe_lm_coef(form_adhd3, dat, "early.volatility.3y.z"),
      safe_lm_coef(form_adhd3, dat, "early.threat.3y.z"),
      safe_lm_coef(form_adhd3, dat, "early.deprivation.3y.z"),
      safe_lm_r2(form_adhd3, dat),
      safe_lm_coef(form_ext3, dat, "(Intercept)"),
      safe_lm_coef(form_ext3, dat, "early.stochasticity.3y.z"),
      safe_lm_coef(form_ext3, dat, "early.volatility.3y.z"),
      safe_lm_coef(form_ext3, dat, "early.threat.3y.z"),
      safe_lm_coef(form_ext3, dat, "early.deprivation.3y.z"),
      safe_lm_r2(form_ext3, dat),
      safe_lm_coef(form_int3, dat, "(Intercept)"),
      safe_lm_coef(form_int3, dat, "early.stochasticity.3y.z"),
      safe_lm_coef(form_int3, dat, "early.volatility.3y.z"),
      safe_lm_coef(form_int3, dat, "early.threat.3y.z"),
      safe_lm_coef(form_int3, dat, "early.deprivation.3y.z"),
      safe_lm_r2(form_int3, dat),
      safe_lm_coef(form_adhd5, dat, "(Intercept)"),
      safe_lm_coef(form_adhd5, dat, "stoch5_resid"),
      safe_lm_coef(form_adhd5, dat, "vol5_resid"),
      safe_lm_coef(form_adhd5, dat, "threat5_resid"),
      safe_lm_coef(form_adhd5, dat, "depr5_resid"),
      safe_lm_r2(form_adhd5, dat),
      safe_lm_coef(form_ext5, dat, "(Intercept)"),
      safe_lm_coef(form_ext5, dat, "stoch5_resid"),
      safe_lm_coef(form_ext5, dat, "vol5_resid"),
      safe_lm_coef(form_ext5, dat, "threat5_resid"),
      safe_lm_coef(form_ext5, dat, "depr5_resid"),
      safe_lm_r2(form_ext5, dat),
      safe_lm_coef(form_int5, dat, "(Intercept)"),
      safe_lm_coef(form_int5, dat, "stoch5_resid"),
      safe_lm_coef(form_int5, dat, "vol5_resid"),
      safe_lm_coef(form_int5, dat, "threat5_resid"),
      safe_lm_coef(form_int5, dat, "depr5_resid"),
      safe_lm_r2(form_int5, dat)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_corr_discrepancy <- function(dat, obs_corr, vars, sex_value, dataset_id, source, imputation) {
  sim_corr <- get_corr_matrix(dat, vars)
  diff_mat <- sim_corr - obs_corr
  
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c("corr_frobenius", "corr_mean_abs_diff", "corr_max_abs_diff"),
    value = c(
      sqrt(sum(diff_mat^2, na.rm = TRUE)),
      mean(abs(diff_mat), na.rm = TRUE),
      max(abs(diff_mat), na.rm = TRUE)
    ),
    dataset_id = dataset_id,
    source = source
  )
}

get_pairwise_corrs <- function(dat, vars, sex_value, block_name, imputation) {
  cmat <- cor(dat[, vars, drop = FALSE], use = "pairwise.complete.obs")
  
  as.data.frame(as.table(cmat), stringsAsFactors = FALSE) %>%
    rename(var1 = Var1, var2 = Var2, corr = Freq) %>%
    mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
    filter(match(var1, vars) < match(var2, vars)) %>%
    mutate(
      imputation = imputation,
      sex = recode(as.character(sex_value), "0" = "boys", "1" = "girls"),
      block = block_name
    ) %>%
    select(imputation, sex, block, var1, var2, corr)
}

get_cross_block_corrs <- function(dat, vars_x, vars_y, sex_value, block_name, imputation) {
  expand_grid(var1 = vars_x, var2 = vars_y) %>%
    mutate(
      corr = map2_dbl(var1, var2, ~ safe_cor(dat[[.x]], dat[[.y]])),
      imputation = imputation,
      sex = recode(as.character(sex_value), "0" = "boys", "1" = "girls"),
      block = block_name
    ) %>%
    select(imputation, sex, block, var1, var2, corr)
}

############################################################
################# 7. Containers across MI ##################
############################################################

all_obs_target_stats <- vector("list", n_imp)
all_sim_target_stats <- vector("list", n_imp)

all_obs_hard_stats <- vector("list", n_imp)
all_sim_hard_stats <- vector("list", n_imp)

all_obs_corr_discrepancy <- vector("list", n_imp)
all_sim_corr_discrepancy <- vector("list", n_imp)

all_obs_corr_outcomes_pairs <- vector("list", n_imp)
all_obs_corr_psych15_pairs <- vector("list", n_imp)
all_obs_corr_outcomes_psych15 <- vector("list", n_imp)
all_obs_corr_adv3_pairs <- vector("list", n_imp)
all_obs_corr_adv5_pairs <- vector("list", n_imp)
all_obs_corr_adv3_psych3 <- vector("list", n_imp)
all_obs_corr_adv5_psych5 <- vector("list", n_imp)

all_obs_plot_data <- vector("list", n_imp)
all_sim_plot_data <- vector("list", n_imp)
all_fit_summaries <- vector("list", n_imp)

############################################################
################### 8. Loop over imputations ###############
############################################################

for (imp in seq_len(n_imp)) {
  
  cat("\n=====================================\n")
  cat("Running posterior predictive checks - imputation", imp, "\n")
  cat("=====================================\n")
  
  data_i <- read.csv(file.path(data_dir, paste0("data_imp_scale", imp, ".csv")))
  
  fit_i <- blavaan(
    riclpm_model_3,
    data = data_i,
    group = "sex",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    int.lv.free = FALSE,
    fixed.x = FALSE,
    target = "stan",
    inits = "prior",
    prisamp = FALSE,
    n.chains = 4,
    burnin = 2000,
    sample = 4000,
    seed = seed_base + imp
  )
  
  fit_sum_i <- summary(fit_i, standardized = TRUE, ci = TRUE)

  fit_sum_df_i <- as.data.frame(
    parameterEstimates(fit_i, standardized = TRUE, ci = TRUE),
    stringsAsFactors = FALSE
  )
  
  fit_sum_df_i$imputation <- imp
  all_fit_summaries[[imp]] <- fit_sum_df_i
 
  ############################################################
  ################ 8.1 Observed variable names ################
  ############################################################
  
  ov_names <- lavNames(fit_i, type = "ov")
  ov_names_sim <- ov_names[ov_names %in% vars_sim]
  
  if (length(ov_names_sim) != length(vars_sim)) {
    stop(
      "Not all vars_sim were found among the model observed variables in imputation ", imp, ".\n",
      "Found: ", paste(ov_names_sim, collapse = ", ")
    )
  }
  
  ############################################################
  ############# 8.2 Generate posterior simulations ###########
  ############################################################
  
  post_sims_i <- sampleData(
    fit_i,
    nrep = nrep_post,
    conditional = FALSE,
    simplify = FALSE
  )
  
  ############################################################
  ################ 8.3 Observed targeted stats ###############
  ############################################################
  
  obs_target_stats_i <- bind_rows(
    get_all_target_stats(
      dat = data_i %>% filter(sex == 0),
      vars_marginal = vars_sim,
      sex_value = 0,
      dataset_id = 0,
      source = "observed",
      imputation = imp
    ),
    get_all_target_stats(
      dat = data_i %>% filter(sex == 1),
      vars_marginal = vars_sim,
      sex_value = 1,
      dataset_id = 0,
      source = "observed",
      imputation = imp
    )
  )
  
  ############################################################
  ############### 8.4 Posterior predictive stats ############
  ############################################################
  
  sim_target_stats_i <- map_dfr(seq_along(post_sims_i), function(rep_id) {
    sim_rep <- bind_groups_with_sex(post_sims_i[[rep_id]], vars_keep = ov_names_sim)
    
    bind_rows(
      get_all_target_stats(
        dat = sim_rep %>% filter(sex == 0),
        vars_marginal = vars_sim,
        sex_value = 0,
        dataset_id = rep_id,
        source = "post",
        imputation = imp
      ),
      get_all_target_stats(
        dat = sim_rep %>% filter(sex == 1),
        vars_marginal = vars_sim,
        sex_value = 1,
        dataset_id = rep_id,
        source = "post",
        imputation = imp
      )
    )
  })
  
  ############################################################
  ################ 8.5 Harder PPC stats ######################
  ############################################################
  
  obs_hard_stats_i <- bind_rows(
    get_stats_shape_outcomes(data_i %>% filter(sex == 0), 0, 0, "observed", imp),
    get_stats_shape_outcomes(data_i %>% filter(sex == 1), 1, 0, "observed", imp),
    get_stats_regression_outcomes(data_i %>% filter(sex == 0), 0, 0, "observed", imp),
    get_stats_regression_outcomes(data_i %>% filter(sex == 1), 1, 0, "observed", imp),
    get_stats_regression_adversity(data_i %>% filter(sex == 0), 0, 0, "observed", imp),
    get_stats_regression_adversity(data_i %>% filter(sex == 1), 1, 0, "observed", imp)
  )
  
  sim_hard_stats_i <- map_dfr(seq_along(post_sims_i), function(rep_id) {
    sim_rep <- bind_groups_with_sex(post_sims_i[[rep_id]], vars_keep = ov_names_sim)
    
    bind_rows(
      get_stats_shape_outcomes(sim_rep %>% filter(sex == 0), 0, rep_id, "post", imp),
      get_stats_shape_outcomes(sim_rep %>% filter(sex == 1), 1, rep_id, "post", imp),
      get_stats_regression_outcomes(sim_rep %>% filter(sex == 0), 0, rep_id, "post", imp),
      get_stats_regression_outcomes(sim_rep %>% filter(sex == 1), 1, rep_id, "post", imp),
      get_stats_regression_adversity(sim_rep %>% filter(sex == 0), 0, rep_id, "post", imp),
      get_stats_regression_adversity(sim_rep %>% filter(sex == 1), 1, rep_id, "post", imp)
    )
  })
  
  ############################################################
  ################ 8.6 Correlation discrepancy ###############
  ############################################################
  
  obs_corr_boys_i  <- get_corr_matrix(data_i %>% filter(sex == 0), vars_sim)
  obs_corr_girls_i <- get_corr_matrix(data_i %>% filter(sex == 1), vars_sim)
  
  obs_corr_discrepancy_i <- bind_rows(
    tibble(imputation = imp, sex = 0, stat = c("corr_frobenius","corr_mean_abs_diff","corr_max_abs_diff"),
           value = c(0,0,0), dataset_id = 0, source = "observed"),
    tibble(imputation = imp, sex = 1, stat = c("corr_frobenius","corr_mean_abs_diff","corr_max_abs_diff"),
           value = c(0,0,0), dataset_id = 0, source = "observed")
  )
  
  sim_corr_discrepancy_i <- map_dfr(seq_along(post_sims_i), function(rep_id) {
    sim_rep <- bind_groups_with_sex(post_sims_i[[rep_id]], vars_keep = ov_names_sim)
    
    bind_rows(
      get_corr_discrepancy(sim_rep %>% filter(sex == 0), obs_corr_boys_i, vars_sim, 0, rep_id, "post", imp),
      get_corr_discrepancy(sim_rep %>% filter(sex == 1), obs_corr_girls_i, vars_sim, 1, rep_id, "post", imp)
    )
  })
  
  ############################################################
  ################ 8.7 Observed-only correlation tables #####
  ############################################################
  
  vars_outcomes <- c("mating.behaviour.z", "risky.health.behaviour.z")
  vars_adv3 <- c("early.stochasticity.3y.z", "early.volatility.3y.z", "early.threat.3y.z", "early.deprivation.3y.z")
  vars_adv5 <- c("stoch5_resid", "vol5_resid", "threat5_resid", "depr5_resid")
  
  obs_corr_outcomes_pairs_i <- bind_rows(
    get_pairwise_corrs(data_i %>% filter(sex == 0), vars_outcomes, 0, "outcomes", imp),
    get_pairwise_corrs(data_i %>% filter(sex == 1), vars_outcomes, 1, "outcomes", imp)
  )
  
  obs_corr_psych15_pairs_i <- bind_rows(
    get_pairwise_corrs(data_i %>% filter(sex == 0), vars_psych15, 0, "psych15", imp),
    get_pairwise_corrs(data_i %>% filter(sex == 1), vars_psych15, 1, "psych15", imp)
  )
  
  obs_corr_outcomes_psych15_i <- bind_rows(
    get_cross_block_corrs(data_i %>% filter(sex == 0), vars_outcomes, vars_psych15, 0, "outcomes_psych15", imp),
    get_cross_block_corrs(data_i %>% filter(sex == 1), vars_outcomes, vars_psych15, 1, "outcomes_psych15", imp)
  )
  
  obs_corr_adv3_pairs_i <- bind_rows(
    get_pairwise_corrs(data_i %>% filter(sex == 0), vars_adv3, 0, "adv3", imp),
    get_pairwise_corrs(data_i %>% filter(sex == 1), vars_adv3, 1, "adv3", imp)
  )
  
  obs_corr_adv5_pairs_i <- bind_rows(
    get_pairwise_corrs(data_i %>% filter(sex == 0), vars_adv5, 0, "adv5", imp),
    get_pairwise_corrs(data_i %>% filter(sex == 1), vars_adv5, 1, "adv5", imp)
  )
  
  obs_corr_adv3_psych3_i <- bind_rows(
    get_cross_block_corrs(data_i %>% filter(sex == 0), vars_adv3, vars_psych3, 0, "adv3_psych3", imp),
    get_cross_block_corrs(data_i %>% filter(sex == 1), vars_adv3, vars_psych3, 1, "adv3_psych3", imp)
  )
  
  obs_corr_adv5_psych5_i <- bind_rows(
    get_cross_block_corrs(data_i %>% filter(sex == 0), vars_adv5, vars_psych5, 0, "adv5_psych5", imp),
    get_cross_block_corrs(data_i %>% filter(sex == 1), vars_adv5, vars_psych5, 1, "adv5_psych5", imp)
  )
  
  ############################################################
  ################ 8.8 Plotting data #########################
  ############################################################
  
  obs_plot_data_i <- data_i %>%
    select(all_of(vars_sim), sex) %>%
    mutate(
      imputation = imp,
      dataset_id = 0,
      source = "obs"
    )
  
  plot_rep_ids <- seq_len(min(n_plot_sims_per_imp, length(post_sims_i)))
  
  sim_plot_data_i <- map_dfr(plot_rep_ids, function(rep_id) {
    bind_groups_with_sex(post_sims_i[[rep_id]], vars_keep = ov_names_sim) %>%
      mutate(
        imputation = imp,
        dataset_id = rep_id,
        source = "sim"
      )
  })
  
  ############################################################
  ################ 8.9 Save per-imputation tables ###########
  ############################################################
  
  write_csv(obs_target_stats_i, file.path(table_dir, paste0("imp_", imp, "_obs_target_stats.csv")))
  write_csv(sim_target_stats_i, file.path(table_dir, paste0("imp_", imp, "_sim_target_stats.csv")))
  write_csv(obs_hard_stats_i, file.path(table_dir, paste0("imp_", imp, "_obs_hard_stats.csv")))
  write_csv(sim_hard_stats_i, file.path(table_dir, paste0("imp_", imp, "_sim_hard_stats.csv")))
  write_csv(obs_corr_discrepancy_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_discrepancy.csv")))
  write_csv(sim_corr_discrepancy_i, file.path(table_dir, paste0("imp_", imp, "_sim_corr_discrepancy.csv")))
  write_csv(obs_corr_outcomes_pairs_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_outcomes_pairs.csv")))
  write_csv(obs_corr_psych15_pairs_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_psych15_pairs.csv")))
  write_csv(obs_corr_outcomes_psych15_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_outcomes_psych15.csv")))
  write_csv(obs_corr_adv3_pairs_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_adv3_pairs.csv")))
  write_csv(obs_corr_adv5_pairs_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_adv5_pairs.csv")))
  write_csv(obs_corr_adv3_psych3_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_adv3_psych3.csv")))
  write_csv(obs_corr_adv5_psych5_i, file.path(table_dir, paste0("imp_", imp, "_obs_corr_adv5_psych5.csv")))
  write.csv(all_fit_summaries[[imp]], file.path(table_dir, paste0("imp_", imp, "_posterior_fit_summary_df.csv")), row.names = FALSE)  

  ############################################################
  ################ 8.10 Store containers ######################
  ############################################################
  
  all_obs_target_stats[[imp]] <- obs_target_stats_i
  all_sim_target_stats[[imp]] <- sim_target_stats_i
  all_obs_hard_stats[[imp]] <- obs_hard_stats_i
  all_sim_hard_stats[[imp]] <- sim_hard_stats_i
  all_obs_corr_discrepancy[[imp]] <- obs_corr_discrepancy_i
  all_sim_corr_discrepancy[[imp]] <- sim_corr_discrepancy_i
  all_obs_corr_outcomes_pairs[[imp]] <- obs_corr_outcomes_pairs_i
  all_obs_corr_psych15_pairs[[imp]] <- obs_corr_psych15_pairs_i
  all_obs_corr_outcomes_psych15[[imp]] <- obs_corr_outcomes_psych15_i
  all_obs_corr_adv3_pairs[[imp]] <- obs_corr_adv3_pairs_i
  all_obs_corr_adv5_pairs[[imp]] <- obs_corr_adv5_pairs_i
  all_obs_corr_adv3_psych3[[imp]] <- obs_corr_adv3_psych3_i
  all_obs_corr_adv5_psych5[[imp]] <- obs_corr_adv5_psych5_i
  all_obs_plot_data[[imp]] <- obs_plot_data_i
  all_sim_plot_data[[imp]] <- sim_plot_data_i
  
  rm(fit_i, fit_sum_i, data_i, post_sims_i, obs_plot_data_i, sim_plot_data_i)
  gc()
}

############################################################
################## 9. Bind all imputations #################
############################################################

obs_target_stats <- bind_rows(all_obs_target_stats)
sim_target_stats <- bind_rows(all_sim_target_stats)

obs_hard_stats <- bind_rows(all_obs_hard_stats)
sim_hard_stats <- bind_rows(all_sim_hard_stats)

obs_corr_discrepancy <- bind_rows(all_obs_corr_discrepancy)
sim_corr_discrepancy <- bind_rows(all_sim_corr_discrepancy)

obs_corr_outcomes_pairs <- bind_rows(all_obs_corr_outcomes_pairs)
obs_corr_psych15_pairs <- bind_rows(all_obs_corr_psych15_pairs)
obs_corr_outcomes_psych15 <- bind_rows(all_obs_corr_outcomes_psych15)
obs_corr_adv3_pairs <- bind_rows(all_obs_corr_adv3_pairs)
obs_corr_adv5_pairs <- bind_rows(all_obs_corr_adv5_pairs)
obs_corr_adv3_psych3 <- bind_rows(all_obs_corr_adv3_psych3)
obs_corr_adv5_psych5 <- bind_rows(all_obs_corr_adv5_psych5)

obs_plot_data <- bind_rows(all_obs_plot_data)
sim_plot_data <- bind_rows(all_sim_plot_data)
fit_summaries_df <- do.call(rbind, all_fit_summaries)

############################################################
############### 10. Aggregate observed over MI #############
############################################################

obs_target_stats_mi <- obs_target_stats %>%
  group_by(sex, stat) %>%
  summarise(
    obs_value = mean(value, na.rm = TRUE),
    obs_sd_between_imp = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

obs_hard_stats_mi <- obs_hard_stats %>%
  group_by(sex, stat) %>%
  summarise(
    obs_value = mean(value, na.rm = TRUE),
    obs_sd_between_imp = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

obs_corr_discrepancy_mi <- obs_corr_discrepancy %>%
  group_by(sex, stat) %>%
  summarise(
    obs_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
############### 11. Posterior predictive summaries #########
############################################################

ppc_target_summary_mi <- sim_target_stats %>%
  group_by(sex, stat) %>%
  summarise(
    post_mean = mean(value, na.rm = TRUE),
    post_sd   = sd(value, na.rm = TRUE),
    post_q025 = quantile(value, 0.025, na.rm = TRUE),
    post_q50  = quantile(value, 0.50,  na.rm = TRUE),
    post_q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(obs_target_stats_mi %>% select(sex, stat, obs_value), by = c("sex", "stat")) %>%
  mutate(
    obs_in_95 = obs_value >= post_q025 & obs_value <= post_q975,
    abs_gap_from_median = abs(obs_value - post_q50),
    scaled_gap = ifelse(post_sd > 0, abs(obs_value - post_q50) / post_sd, NA_real_),
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  ) %>%
  arrange(sex, stat)

ppp_target_mi <- sim_target_stats %>%
  left_join(obs_target_stats_mi %>% select(sex, stat, obs_value), by = c("sex", "stat")) %>%
  group_by(sex, stat, obs_value) %>%
  summarise(
    sim_median = median(value, na.rm = TRUE),
    ppp = mean(
      abs(value - median(value, na.rm = TRUE)) >=
        abs(obs_value[1] - median(value, na.rm = TRUE)),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  ) %>%
  arrange(sex, stat)

############################################################
################ 12. Focused output tables #################
############################################################

ppc_target_marginal_mi <- ppc_target_summary_mi %>%
  filter(grepl("_mean$|_sd$", stat))

ppc_target_ar_mi <- ppc_target_summary_mi %>%
  filter(stat %in% c(
    "adhd_35","adhd_59","adhd_915",
    "ext_35","ext_59","ext_915",
    "int_35","int_59","int_915"
  ))

ppc_target_cl_mi <- ppc_target_summary_mi %>%
  filter(stat %in% c(
    "adhd3_int5","adhd5_int9","adhd9_int15",
    "adhd3_ext5","adhd5_ext9","adhd9_ext15",
    "int3_adhd5","int5_adhd9","int9_adhd15",
    "ext3_adhd5","ext5_adhd9","ext9_adhd15",
    "ext3_int5","ext5_int9","ext9_int15",
    "int3_ext5","int5_ext9","int9_ext15"
  ))

ppc_target_withinwave_mi <- ppc_target_summary_mi %>%
  filter(stat %in% c(
    "adhd_ext_3","adhd_int_3","ext_int_3",
    "adhd_ext_5","adhd_int_5","ext_int_5",
    "adhd_ext_9","adhd_int_9","ext_int_9",
    "adhd_ext_15","adhd_int_15","ext_int_15"
  ))

ppc_target_adversity_mi <- ppc_target_summary_mi %>%
  filter(stat %in% c(
    "stoch3_adhd3","stoch3_ext3","stoch3_int3",
    "vol3_adhd3","vol3_ext3","vol3_int3",
    "threat3_adhd3","threat3_ext3","threat3_int3",
    "depr3_adhd3","depr3_ext3","depr3_int3",
    "stoch5_adhd5","stoch5_ext5","stoch5_int5",
    "vol5_adhd5","vol5_ext5","vol5_int5",
    "threat5_adhd5","threat5_ext5","threat5_int5",
    "depr5_adhd5","depr5_ext5","depr5_int5"
  ))

ppc_target_outcomes_mi <- ppc_target_summary_mi %>%
  filter(stat %in% c(
    "mating_risky",
    "mating_adhd15", "mating_ext15", "mating_int15",
    "risky_adhd15",  "risky_ext15",  "risky_int15"
  ))

############################################################
################ 13. Quick counts ##########################
############################################################

ppc_target_counts_global_mi <- ppc_target_summary_mi %>%
  summarise(
    n_in_95 = sum(obs_in_95, na.rm = TRUE),
    total = n(),
    prop_in_95 = n_in_95 / total
  )

ppc_target_counts_by_sex_mi <- ppc_target_summary_mi %>%
  group_by(sex) %>%
  summarise(
    n_in_95 = sum(obs_in_95, na.rm = TRUE),
    total = n(),
    prop_in_95 = n_in_95 / total,
    .groups = "drop"
  )

ppc_target_counts_by_family_mi <- bind_rows(
  ppc_target_marginal_mi   %>% mutate(family = "marginal"),
  ppc_target_ar_mi         %>% mutate(family = "ar"),
  ppc_target_cl_mi         %>% mutate(family = "cl"),
  ppc_target_withinwave_mi %>% mutate(family = "withinwave"),
  ppc_target_adversity_mi  %>% mutate(family = "adversity"),
  ppc_target_outcomes_mi   %>% mutate(family = "outcomes")
) %>%
  group_by(sex, family) %>%
  summarise(
    n_in_95 = sum(obs_in_95, na.rm = TRUE),
    total = n(),
    prop_in_95 = n_in_95 / total,
    .groups = "drop"
  )

############################################################
############### 14. Harder PPC summaries ###################
############################################################

ppc_hard_summary_mi <- sim_hard_stats %>%
  group_by(sex, stat) %>%
  summarise(
    post_mean = mean(value, na.rm = TRUE),
    post_sd   = sd(value, na.rm = TRUE),
    post_q025 = quantile(value, 0.025, na.rm = TRUE),
    post_q50  = quantile(value, 0.50,  na.rm = TRUE),
    post_q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(obs_hard_stats_mi %>% select(sex, stat, obs_value), by = c("sex", "stat")) %>%
  mutate(
    obs_in_95 = obs_value >= post_q025 & obs_value <= post_q975,
    abs_gap_from_median = abs(obs_value - post_q50),
    scaled_gap = ifelse(post_sd > 0, abs(obs_value - post_q50) / post_sd, NA_real_),
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  ) %>%
  arrange(sex, stat)

ppp_hard_mi <- sim_hard_stats %>%
  left_join(obs_hard_stats_mi %>% select(sex, stat, obs_value), by = c("sex", "stat")) %>%
  group_by(sex, stat, obs_value) %>%
  summarise(
    sim_median = median(value, na.rm = TRUE),
    ppp = mean(
      abs(value - median(value, na.rm = TRUE)) >=
        abs(obs_value[1] - median(value, na.rm = TRUE)),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  ) %>%
  arrange(sex, stat)

ppc_hard_shape_mi <- ppc_hard_summary_mi %>%
  filter(grepl("^mating_q|^risky_q|_skew$|_kurtosis$", stat))

ppc_hard_regression_outcomes_mi <- ppc_hard_summary_mi %>%
  filter(grepl("^beta_mating_|^beta_risky_|^r2_mating$|^r2_risky$", stat))

ppc_hard_regression_adversity_mi <- ppc_hard_summary_mi %>%
  filter(grepl(
    "^beta_adhd3_|^beta_ext3_|^beta_int3_|^r2_adhd3$|^r2_ext3$|^r2_int3$|^beta_adhd5_|^beta_ext5_|^beta_int5_|^r2_adhd5$|^r2_ext5$|^r2_int5$",
    stat
  ))

ppc_hard_counts_global_mi <- ppc_hard_summary_mi %>%
  summarise(
    n_in_95 = sum(obs_in_95, na.rm = TRUE),
    total = n(),
    prop_in_95 = n_in_95 / total
  )

ppc_hard_counts_by_sex_mi <- ppc_hard_summary_mi %>%
  group_by(sex) %>%
  summarise(
    n_in_95 = sum(obs_in_95, na.rm = TRUE),
    total = n(),
    prop_in_95 = n_in_95 / total,
    .groups = "drop"
  )

############################################################
############### 15. Matrix discrepancy summaries ###########
############################################################

ppc_corr_discrepancy_summary_mi <- sim_corr_discrepancy %>%
  group_by(sex, stat) %>%
  summarise(
    post_mean = mean(value, na.rm = TRUE),
    post_sd   = sd(value, na.rm = TRUE),
    post_q025 = quantile(value, 0.025, na.rm = TRUE),
    post_q50  = quantile(value, 0.50,  na.rm = TRUE),
    post_q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(obs_corr_discrepancy_mi %>% select(sex, stat, obs_value), by = c("sex", "stat")) %>%
  mutate(
    scaled_gap = ifelse(post_sd > 0, abs(obs_value - post_q50) / post_sd, NA_real_),
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  ) %>%
  arrange(sex, stat)

ppp_corr_discrepancy_mi <- sim_corr_discrepancy %>%
  group_by(sex, stat) %>%
  summarise(
    ppp_left = mean(value <= 0, na.rm = TRUE),
    ppp_right = mean(value >= 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  ) %>%
  arrange(sex, stat)

############################################################
########## 16. Optional focused posterior-vs-observed ######
############################################################

ppc_target_outcomes_pairs_mi <- ppc_target_summary_mi %>%
  filter(stat %in% c(
    "mating_risky",
    "mating_adhd15", "mating_ext15", "mating_int15",
    "risky_adhd15",  "risky_ext15",  "risky_int15"
  )) %>%
  arrange(sex, stat)

ppc_target_adversity_pairs_mi <- ppc_target_summary_mi %>%
  filter(stat %in% c(
    "stoch3_adhd3","stoch3_ext3","stoch3_int3",
    "vol3_adhd3","vol3_ext3","vol3_int3",
    "threat3_adhd3","threat3_ext3","threat3_int3",
    "depr3_adhd3","depr3_ext3","depr3_int3",
    "stoch5_adhd5","stoch5_ext5","stoch5_int5",
    "vol5_adhd5","vol5_ext5","vol5_int5",
    "threat5_adhd5","threat5_ext5","threat5_int5",
    "depr5_adhd5","depr5_ext5","depr5_int5"
  )) %>%
  arrange(sex, stat)

############################################################
################### 17. Save result tables #################
############################################################

write_csv(obs_target_stats, file.path(table_dir, "obs_target_stats_all_imputations.csv"))
write_csv(sim_target_stats, file.path(table_dir, "sim_target_stats_all_imputations.csv"))
write_csv(ppc_target_summary_mi, file.path(table_dir, "ppc_target_summary_mi.csv"))
write_csv(ppp_target_mi, file.path(table_dir, "ppp_target_mi.csv"))

write_csv(ppc_target_marginal_mi, file.path(table_dir, "ppc_target_marginal_mi.csv"))
write_csv(ppc_target_ar_mi, file.path(table_dir, "ppc_target_ar_mi.csv"))
write_csv(ppc_target_cl_mi, file.path(table_dir, "ppc_target_cl_mi.csv"))
write_csv(ppc_target_withinwave_mi, file.path(table_dir, "ppc_target_withinwave_mi.csv"))
write_csv(ppc_target_adversity_mi, file.path(table_dir, "ppc_target_adversity_mi.csv"))
write_csv(ppc_target_outcomes_mi, file.path(table_dir, "ppc_target_outcomes_mi.csv"))

write_csv(ppc_target_counts_global_mi, file.path(table_dir, "ppc_target_counts_global_mi.csv"))
write_csv(ppc_target_counts_by_sex_mi, file.path(table_dir, "ppc_target_counts_by_sex_mi.csv"))
write_csv(ppc_target_counts_by_family_mi, file.path(table_dir, "ppc_target_counts_by_family_mi.csv"))

write_csv(obs_corr_outcomes_pairs, file.path(table_dir, "obs_corr_outcomes_pairs_all_imputations.csv"))
write_csv(obs_corr_psych15_pairs, file.path(table_dir, "obs_corr_psych15_pairs_all_imputations.csv"))
write_csv(obs_corr_outcomes_psych15, file.path(table_dir, "obs_corr_outcomes_psych15_all_imputations.csv"))
write_csv(obs_corr_adv3_pairs, file.path(table_dir, "obs_corr_adv3_pairs_all_imputations.csv"))
write_csv(obs_corr_adv5_pairs, file.path(table_dir, "obs_corr_adv5_pairs_all_imputations.csv"))
write_csv(obs_corr_adv3_psych3, file.path(table_dir, "obs_corr_adv3_psych3_all_imputations.csv"))
write_csv(obs_corr_adv5_psych5, file.path(table_dir, "obs_corr_adv5_psych5_all_imputations.csv"))

write_csv(ppc_target_outcomes_pairs_mi, file.path(table_dir, "ppc_target_outcomes_pairs_mi.csv"))
write_csv(ppc_target_adversity_pairs_mi, file.path(table_dir, "ppc_target_adversity_pairs_mi.csv"))

write_csv(obs_hard_stats, file.path(table_dir, "obs_hard_stats_all_imputations.csv"))
write_csv(sim_hard_stats, file.path(table_dir, "sim_hard_stats_all_imputations.csv"))
write_csv(ppc_hard_summary_mi, file.path(table_dir, "ppc_hard_summary_mi.csv"))
write_csv(ppp_hard_mi, file.path(table_dir, "ppp_hard_mi.csv"))

write_csv(ppc_hard_shape_mi, file.path(table_dir, "ppc_hard_shape_mi.csv"))
write_csv(ppc_hard_regression_outcomes_mi, file.path(table_dir, "ppc_hard_regression_outcomes_mi.csv"))
write_csv(ppc_hard_regression_adversity_mi, file.path(table_dir, "ppc_hard_regression_adversity_mi.csv"))
write_csv(ppc_hard_counts_global_mi, file.path(table_dir, "ppc_hard_counts_global_mi.csv"))
write_csv(ppc_hard_counts_by_sex_mi, file.path(table_dir, "ppc_hard_counts_by_sex_mi.csv"))

write_csv(obs_corr_discrepancy, file.path(table_dir, "obs_corr_discrepancy_all_imputations.csv"))
write_csv(sim_corr_discrepancy, file.path(table_dir, "sim_corr_discrepancy_all_imputations.csv"))
write_csv(ppc_corr_discrepancy_summary_mi, file.path(table_dir, "ppc_corr_discrepancy_summary_mi.csv"))
write_csv(ppp_corr_discrepancy_mi, file.path(table_dir, "ppp_corr_discrepancy_mi.csv"))

write.csv(fit_summaries_df, file.path(table_dir, "posterior_fit_summaries_df_all_imputations.csv"), row.names = FALSE)

############################################################
############################################################
##################### 18. Visualization ####################
############################################################
############################################################

############################################################
############ 18.1 Prepare plotting datasets ################
############################################################

plot_data <- bind_rows(obs_plot_data, sim_plot_data) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  )

############################################################
######## 18.2 Observed vs simulated densities ##############
############################################################

density_data <- plot_data %>%
  pivot_longer(
    cols = all_of(vars_sim),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable_label = case_when(
      variable == "Adhd_3y.z" ~ "ADHD 3y",
      variable == "Adhd_5y.z" ~ "ADHD 5y",
      variable == "Adhd_9y.z" ~ "ADHD 9y",
      variable == "Adhd_15y.z" ~ "ADHD 15y",
      variable == "Ext_3y.z" ~ "EXT 3y",
      variable == "Ext_5y.z" ~ "EXT 5y",
      variable == "Ext_9y.z" ~ "EXT 9y",
      variable == "Ext_15y.z" ~ "EXT 15y",
      variable == "Int_3y.z" ~ "INT 3y",
      variable == "Int_5y.z" ~ "INT 5y",
      variable == "Int_9y.z" ~ "INT 9y",
      variable == "Int_15y.z" ~ "INT 15y",
      variable == "early.stochasticity.3y.z" ~ "stoch 3y",
      variable == "early.volatility.3y.z" ~ "vol 3y",
      variable == "early.threat.3y.z" ~ "threat 3y",
      variable == "early.deprivation.3y.z" ~ "depr 3y",
      variable == "stoch5_resid" ~ "stoch 5y resid",
      variable == "vol5_resid" ~ "vol 5y resid",
      variable == "threat5_resid" ~ "threat 5y resid",
      variable == "depr5_resid" ~ "depr 5y resid",
      variable == "mating.behaviour.z" ~ "Mating",
      variable == "risky.health.behaviour.z" ~ "Risky health",
      TRUE ~ variable
    )
  ) %>%
  mutate(
    variable_label = factor(
      variable_label,
      levels = c(
        "ADHD 3y", "ADHD 5y", "ADHD 9y", "ADHD 15y",
        "EXT 3y", "EXT 5y", "EXT 9y", "EXT 15y",
        "INT 3y", "INT 5y", "INT 9y", "INT 15y",
        "stoch 3y", "vol 3y", "threat 3y", "depr 3y",
        "stoch 5y resid", "vol 5y resid", "threat 5y resid", "depr 5y resid",
        "Mating", "Risky health"
      )
    )
  )

make_density_plot <- function(dat, vars_keep, colour_sim, colour_obs, title_text) {
  
  dat_sub <- dat %>%
    filter(variable %in% vars_keep)
  
  ggplot(dat_sub, aes(x = value, group = interaction(imputation, dataset_id, source))) +
    geom_density(
      data = ~ subset(.x, source == "sim"),
      colour = colour_sim,
      alpha = 0.10,
      linewidth = 0.18
    ) +
    geom_density(
      data = ~ subset(.x, source == "obs"),
      colour = colour_obs,
      linewidth = 0.55
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    facet_grid(sex ~ variable_label, scales = "free") +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.2, colour = "grey88"),
      panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey90"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 13)
    ) +
    labs(
      title = title_text,
      subtitle = "Observed vs simulated distributions across imputations",
      x = "Standardized value (z-score)",
      y = "Density"
    )
}

plot_density_adhd <- make_density_plot(
  density_data,
  c("Adhd_3y.z", "Adhd_5y.z", "Adhd_9y.z", "Adhd_15y.z"),
  "#14D9BB", "#F58518",
  "Posterior predictive densities – ADHD"
)

plot_density_ext <- make_density_plot(
  density_data,
  c("Ext_3y.z", "Ext_5y.z", "Ext_9y.z", "Ext_15y.z"),
  "#14D9BB", "#F58518",
  "Posterior predictive densities – externalizing"
)

plot_density_int <- make_density_plot(
  density_data,
  c("Int_3y.z", "Int_5y.z", "Int_9y.z", "Int_15y.z"),
  "#14D9BB", "#F58518",
  "Posterior predictive densities – internalizing"
)

plot_density_adv3 <- make_density_plot(
  density_data,
  c("early.stochasticity.3y.z", "early.volatility.3y.z", "early.threat.3y.z", "early.deprivation.3y.z"),
  "#14D9BB", "#F58518",
  "Posterior predictive densities – adversity at 3 years"
)

plot_density_adv5 <- make_density_plot(
  density_data,
  c("stoch5_resid", "vol5_resid", "threat5_resid", "depr5_resid"),
  "#14D9BB", "#F58518",
  "Posterior predictive densities – adversity residuals at 5 years"
)

plot_density_lhs <- make_density_plot(
  density_data,
  c("mating.behaviour.z", "risky.health.behaviour.z"),
  "#14D9BB", "#F58518",
  "Posterior predictive densities – mating and risky health"
)

print(plot_density_adhd)
print(plot_density_ext)
print(plot_density_int)
print(plot_density_adv3)
print(plot_density_adv5)
print(plot_density_lhs)

############################################################
######## 18.3 Prepare pairs to plot ########################
############################################################

pairs_vars <- tribble(
  ~block,              ~family,           ~xvar,                      ~yvar,                         ~panel,
  "psych_cascade",     "ar",              "Adhd_3y.z",                "Adhd_5y.z",                  "ADHD 3y → ADHD 5y",
  "psych_cascade",     "ar",              "Adhd_5y.z",                "Adhd_9y.z",                  "ADHD 5y → ADHD 9y",
  "psych_cascade",     "ar",              "Adhd_9y.z",                "Adhd_15y.z",                 "ADHD 9y → ADHD 15y",
  "psych_cascade",     "ar",              "Ext_3y.z",                 "Ext_5y.z",                   "EXT 3y → EXT 5y",
  "psych_cascade",     "ar",              "Ext_5y.z",                 "Ext_9y.z",                   "EXT 5y → EXT 9y",
  "psych_cascade",     "ar",              "Ext_9y.z",                 "Ext_15y.z",                  "EXT 9y → EXT 15y",
  "psych_cascade",     "ar",              "Int_3y.z",                 "Int_5y.z",                   "INT 3y → INT 5y",
  "psych_cascade",     "ar",              "Int_5y.z",                 "Int_9y.z",                   "INT 5y → INT 9y",
  "psych_cascade",     "ar",              "Int_9y.z",                 "Int_15y.z",                  "INT 9y → INT 15y",
  "psych_cascade",     "adhd_to_other",   "Adhd_3y.z",                "Ext_5y.z",                   "ADHD 3y → EXT 5y",
  "psych_cascade",     "adhd_to_other",   "Adhd_5y.z",                "Ext_9y.z",                   "ADHD 5y → EXT 9y",
  "psych_cascade",     "adhd_to_other",   "Adhd_9y.z",                "Ext_15y.z",                  "ADHD 9y → EXT 15y",
  "psych_cascade",     "adhd_to_other",   "Adhd_3y.z",                "Int_5y.z",                   "ADHD 3y → INT 5y",
  "psych_cascade",     "adhd_to_other",   "Adhd_5y.z",                "Int_9y.z",                   "ADHD 5y → INT 9y",
  "psych_cascade",     "adhd_to_other",   "Adhd_9y.z",                "Int_15y.z",                  "ADHD 9y → INT 15y",
  "psych_cascade",     "other_to_adhd",   "Ext_3y.z",                 "Adhd_5y.z",                  "EXT 3y → ADHD 5y",
  "psych_cascade",     "other_to_adhd",   "Ext_5y.z",                 "Adhd_9y.z",                  "EXT 5y → ADHD 9y",
  "psych_cascade",     "other_to_adhd",   "Ext_9y.z",                 "Adhd_15y.z",                 "EXT 9y → ADHD 15y",
  "psych_cascade",     "other_to_adhd",   "Int_3y.z",                 "Adhd_5y.z",                  "INT 3y → ADHD 5y",
  "psych_cascade",     "other_to_adhd",   "Int_5y.z",                 "Adhd_9y.z",                  "INT 5y → ADHD 9y",
  "psych_cascade",     "other_to_adhd",   "Int_9y.z",                 "Adhd_15y.z",                 "INT 9y → ADHD 15y",
  "psych_cascade",     "ext_int",         "Ext_3y.z",                 "Int_5y.z",                   "EXT 3y → INT 5y",
  "psych_cascade",     "ext_int",         "Ext_5y.z",                 "Int_9y.z",                   "EXT 5y → INT 9y",
  "psych_cascade",     "ext_int",         "Ext_9y.z",                 "Int_15y.z",                  "EXT 9y → INT 15y",
  "psych_cascade",     "int_ext",         "Int_3y.z",                 "Ext_5y.z",                   "INT 3y → EXT 5y",
  "psych_cascade",     "int_ext",         "Int_5y.z",                 "Ext_9y.z",                   "INT 5y → EXT 9y",
  "psych_cascade",     "int_ext",         "Int_9y.z",                 "Ext_15y.z",                  "INT 9y → EXT 15y",
  "adversity_psych",   "stochasticity",   "early.stochasticity.3y.z", "Adhd_3y.z",                  "stoch 3y → ADHD 3y",
  "adversity_psych",   "stochasticity",   "early.stochasticity.3y.z", "Ext_3y.z",                   "stoch 3y → EXT 3y",
  "adversity_psych",   "stochasticity",   "early.stochasticity.3y.z", "Int_3y.z",                   "stoch 3y → INT 3y",
  "adversity_psych",   "stochasticity",   "stoch5_resid",             "Adhd_5y.z",                  "stoch 5y resid → ADHD 5y",
  "adversity_psych",   "stochasticity",   "stoch5_resid",             "Ext_5y.z",                   "stoch 5y resid → EXT 5y",
  "adversity_psych",   "stochasticity",   "stoch5_resid",             "Int_5y.z",                   "stoch 5y resid → INT 5y",
  "adversity_psych",   "volatility",      "early.volatility.3y.z",    "Adhd_3y.z",                  "vol 3y → ADHD 3y",
  "adversity_psych",   "volatility",      "early.volatility.3y.z",    "Ext_3y.z",                   "vol 3y → EXT 3y",
  "adversity_psych",   "volatility",      "early.volatility.3y.z",    "Int_3y.z",                   "vol 3y → INT 3y",
  "adversity_psych",   "volatility",      "vol5_resid",               "Adhd_5y.z",                  "vol 5y resid → ADHD 5y",
  "adversity_psych",   "volatility",      "vol5_resid",               "Ext_5y.z",                   "vol 5y resid → EXT 5y",
  "adversity_psych",   "volatility",      "vol5_resid",               "Int_5y.z",                   "vol 5y resid → INT 5y",
  "adversity_psych",   "threat",          "early.threat.3y.z",        "Adhd_3y.z",                  "threat 3y → ADHD 3y",
  "adversity_psych",   "threat",          "early.threat.3y.z",        "Ext_3y.z",                   "threat 3y → EXT 3y",
  "adversity_psych",   "threat",          "early.threat.3y.z",        "Int_3y.z",                   "threat 3y → INT 3y",
  "adversity_psych",   "threat",          "threat5_resid",            "Adhd_5y.z",                  "threat 5y resid → ADHD 5y",
  "adversity_psych",   "threat",          "threat5_resid",            "Ext_5y.z",                   "threat 5y resid → EXT 5y",
  "adversity_psych",   "threat",          "threat5_resid",            "Int_5y.z",                   "threat 5y resid → INT 5y",
  "adversity_psych",   "deprivation",     "early.deprivation.3y.z",   "Adhd_3y.z",                  "depr 3y → ADHD 3y",
  "adversity_psych",   "deprivation",     "early.deprivation.3y.z",   "Ext_3y.z",                   "depr 3y → EXT 3y",
  "adversity_psych",   "deprivation",     "early.deprivation.3y.z",   "Int_3y.z",                   "depr 3y → INT 3y",
  "adversity_psych",   "deprivation",     "depr5_resid",              "Adhd_5y.z",                  "depr 5y resid → ADHD 5y",
  "adversity_psych",   "deprivation",     "depr5_resid",              "Ext_5y.z",                   "depr 5y resid → EXT 5y",
  "adversity_psych",   "deprivation",     "depr5_resid",              "Int_5y.z",                   "depr 5y resid → INT 5y",
  "psych15_outcomes",  "late_outcomes",   "Adhd_15y.z",               "mating.behaviour.z",         "ADHD15 → mating",
  "psych15_outcomes",  "late_outcomes",   "Ext_15y.z",                "mating.behaviour.z",         "EXT15 → mating",
  "psych15_outcomes",  "late_outcomes",   "Int_15y.z",                "mating.behaviour.z",         "INT15 → mating",
  "psych15_outcomes",  "late_outcomes",   "Adhd_15y.z",               "risky.health.behaviour.z",   "ADHD15 → risky health",
  "psych15_outcomes",  "late_outcomes",   "Ext_15y.z",                "risky.health.behaviour.z",   "EXT15 → risky health",
  "psych15_outcomes",  "late_outcomes",   "Int_15y.z",                "risky.health.behaviour.z",   "INT15 → risky health"
)

obs_scatter_data <- map_dfr(seq_len(nrow(pairs_vars)), function(i) {
  xvar   <- pairs_vars$xvar[i]
  yvar   <- pairs_vars$yvar[i]
  panel  <- pairs_vars$panel[i]
  family <- pairs_vars$family[i]
  block  <- pairs_vars$block[i]
  
  obs_plot_data %>%
    transmute(
      sex,
      imputation,
      block,
      family,
      panel,
      x = .data[[xvar]],
      y = .data[[yvar]]
    )
}) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  )

line_imps <- unique(sim_plot_data$imputation)
line_imps <- line_imps[seq_len(min(length(line_imps), n_line_sims_total))]

posterior_line_data <- map_dfr(seq_len(nrow(pairs_vars)), function(j) {
  xvar   <- pairs_vars$xvar[j]
  yvar   <- pairs_vars$yvar[j]
  panel  <- pairs_vars$panel[j]
  family <- pairs_vars$family[j]
  block  <- pairs_vars$block[j]
  
  sim_plot_data %>%
    filter(imputation %in% line_imps) %>%
    transmute(
      sex,
      imputation,
      dataset_id,
      block,
      family,
      panel,
      x = .data[[xvar]],
      y = .data[[yvar]]
    )
}) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  )

make_regression_plot <- function(data_block = NULL, family_name = NULL, title_text) {
  
  posterior_sub <- posterior_line_data
  obs_sub   <- obs_scatter_data
  
  if (!is.null(data_block)) {
    posterior_sub <- posterior_sub %>% filter(block == data_block)
    obs_sub   <- obs_sub %>% filter(block == data_block)
  }
  
  if (!is.null(family_name)) {
    posterior_sub <- posterior_sub %>% filter(family == family_name)
    obs_sub   <- obs_sub %>% filter(family == family_name)
  }
  
  ggplot() +
    geom_smooth(
      data = posterior_sub,
      aes(x = x, y = y, group = interaction(imputation, dataset_id, sex, panel)),
      method = "lm",
      se = FALSE,
      linewidth = 0.4,
      alpha = 0.2,
      colour = "#14D9BB"
    ) +
    geom_point(
      data = obs_sub,
      aes(x = x, y = y),
      size = 0.8,
      alpha = 0.5
    ) +
    geom_smooth(
      data = obs_sub,
      aes(x = x, y = y),
      method = "lm",
      se = FALSE,
      linewidth = 1,
      colour = "#F58518"
    ) +
    facet_grid(sex ~ panel, scales = "free") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      strip.text = element_text(size = 8, face = "bold")
    ) +
    labs(
      title = title_text,
      subtitle = "Orange = observed regression line ; Turquoise = posterior simulated draws across imputations",
      x = NULL,
      y = NULL
    )
}

plot_posterior_scatter_ar <- make_regression_plot(
  family_name = "ar",
  title_text = "Posterior predictive check – psychopathology cascade (autoregressive paths)"
)

plot_posterior_scatter_adhd_to_other <- make_regression_plot(
  family_name = "adhd_to_other",
  title_text = "Posterior predictive check – psychopathology cascade (ADHD → EXT / INT)"
)

plot_posterior_scatter_other_to_adhd <- make_regression_plot(
  family_name = "other_to_adhd",
  title_text = "Posterior predictive check – psychopathology cascade (EXT / INT → ADHD)"
)

plot_posterior_scatter_ext_int <- make_regression_plot(
  family_name = "ext_int",
  title_text = "Posterior predictive check – psychopathology cascade (EXT → INT)"
)

plot_posterior_scatter_int_ext <- make_regression_plot(
  family_name = "int_ext",
  title_text = "Posterior predictive check – psychopathology cascade (INT → EXT)"
)

plot_posterior_scatter_stoch <- make_regression_plot(
  family_name = "stochasticity",
  title_text = "Posterior predictive check – regression structure (stochasticity)"
)

plot_posterior_scatter_vol <- make_regression_plot(
  family_name = "volatility",
  title_text = "Posterior predictive check – regression structure (volatility)"
)

plot_posterior_scatter_threat <- make_regression_plot(
  family_name = "threat",
  title_text = "Posterior predictive check – regression structure (threat)"
)

plot_posterior_scatter_depr <- make_regression_plot(
  family_name = "deprivation",
  title_text = "Posterior predictive check – regression structure (deprivation)"
)

plot_posterior_scatter_lhs <- make_regression_plot(
  data_block = "psych15_outcomes",
  title_text = "Posterior predictive check – regression structure (psychopathology at age 15 → mating / risky health)"
)

print(plot_posterior_scatter_ar)
print(plot_posterior_scatter_adhd_to_other)
print(plot_posterior_scatter_other_to_adhd)
print(plot_posterior_scatter_ext_int)
print(plot_posterior_scatter_int_ext)
print(plot_posterior_scatter_stoch)
print(plot_posterior_scatter_vol)
print(plot_posterior_scatter_threat)
print(plot_posterior_scatter_depr)
print(plot_posterior_scatter_lhs)

############################################################
###### 18.4 Observed vs simulated corr heatmaps ###########
############################################################

var_labels <- c(
  "Adhd_3y.z" = "ADHD 3y",
  "Adhd_5y.z" = "ADHD 5y",
  "Adhd_9y.z" = "ADHD 9y",
  "Adhd_15y.z" = "ADHD 15y",
  "Ext_3y.z" = "EXT 3y",
  "Ext_5y.z" = "EXT 5y",
  "Ext_9y.z" = "EXT 9y",
  "Ext_15y.z" = "EXT 15y",
  "Int_3y.z" = "INT 3y",
  "Int_5y.z" = "INT 5y",
  "Int_9y.z" = "INT 9y",
  "Int_15y.z" = "INT 15y",
  "early.stochasticity.3y.z" = "stoch 3y",
  "early.volatility.3y.z" = "vol 3y",
  "early.threat.3y.z" = "threat 3y",
  "early.deprivation.3y.z" = "depr 3y",
  "stoch5_resid" = "stoch 5y resid",
  "vol5_resid" = "vol 5y resid",
  "threat5_resid" = "threat 5y resid",
  "depr5_resid" = "depr 5y resid",
  "mating.behaviour.z" = "Mating",
  "risky.health.behaviour.z" = "Risky health"
)

make_corr_df <- function(dat, vars, sex_label, source_label) {
  cmat <- cor(dat[, vars, drop = FALSE], use = "pairwise.complete.obs")
  as.data.frame(as.table(cmat), stringsAsFactors = FALSE) %>%
    rename(var1 = Var1, var2 = Var2, corr = Freq) %>%
    mutate(
      sex = sex_label,
      source = source_label,
      var1_label = recode(var1, !!!var_labels),
      var2_label = recode(var2, !!!var_labels)
    )
}

heatmap_example <- sim_plot_data %>%
  filter(imputation == 1, dataset_id == 1)

corr_heatmap_data <- bind_rows(
  make_corr_df(
    obs_plot_data %>% filter(imputation == 1, sex == 0),
    vars_sim,
    sex_label = "boys",
    source_label = "observed"
  ),
  make_corr_df(
    obs_plot_data %>% filter(imputation == 1, sex == 1),
    vars_sim,
    sex_label = "girls",
    source_label = "observed"
  ),
  make_corr_df(
    heatmap_example %>% filter(sex == 0) %>% select(-source, -imputation, -dataset_id),
    vars_sim,
    sex_label = "boys",
    source_label = "simulated"
  ),
  make_corr_df(
    heatmap_example %>% filter(sex == 1) %>% select(-source, -imputation, -dataset_id),
    vars_sim,
    sex_label = "girls",
    source_label = "simulated"
  )
) %>%
  mutate(
    var1_label = factor(var1_label, levels = unname(var_labels[vars_sim])),
    var2_label = factor(var2_label, levels = rev(unname(var_labels[vars_sim])))
  )

plot_corr_heatmaps <- ggplot(corr_heatmap_data, aes(x = var1_label, y = var2_label, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#F58518",
    mid = "#F7F7F7",
    high = "#14D9BB",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  facet_grid(sex ~ source) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    title = "Observed vs simulated correlation matrices",
    subtitle = "Example shown: imputation 1, posterior replicate 1",
    x = NULL,
    y = NULL,
    fill = "r"
  )

print(plot_corr_heatmaps)

############################################################
###### 18.5 Global corr discrepancy distributions ##########
############################################################

corr_discrepancy_data <- sim_corr_discrepancy %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  )

plot_corr_discrepancy <- ggplot(corr_discrepancy_data, aes(x = value)) +
  geom_histogram(
    bins = 30,
    fill = "#14D9BB",
    color = "white",
    linewidth = 0.2
  ) +
  facet_grid(sex ~ stat, scales = "free") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Posterior distribution of global correlation discrepancies",
    subtitle = "Distance between observed and simulated correlation matrices across imputations",
    x = "Discrepancy",
    y = "Count"
  )

print(plot_corr_discrepancy)

############################################################
#################### 19. Save figures ######################
############################################################

save_plot(plot_density_adhd,     "ppc_density_adhd_mi",     8, 7)
save_plot(plot_density_ext,      "ppc_density_ext_mi",      8, 7)
save_plot(plot_density_int,      "ppc_density_int_mi",      8, 7)
save_plot(plot_density_adv3,     "ppc_density_adv3_mi",     8, 7)
save_plot(plot_density_adv5,     "ppc_density_adv5_mi",     8, 7)
save_plot(plot_density_lhs,      "ppc_density_lhs_mi",      8, 7)

save_plot(plot_posterior_scatter_ar,            "posterior_scatter_ar_mi",            16, 7)
save_plot(plot_posterior_scatter_adhd_to_other, "posterior_scatter_adhd_to_other_mi", 16, 7)
save_plot(plot_posterior_scatter_other_to_adhd, "posterior_scatter_other_to_adhd_mi", 16, 7)
save_plot(plot_posterior_scatter_ext_int,       "posterior_scatter_ext_int_mi",       16, 7)
save_plot(plot_posterior_scatter_int_ext,       "posterior_scatter_int_ext_mi",       16, 7)
save_plot(plot_posterior_scatter_stoch,         "posterior_scatter_stoch_mi",         16, 7)
save_plot(plot_posterior_scatter_vol,           "posterior_scatter_vol_mi",           16, 7)
save_plot(plot_posterior_scatter_threat,        "posterior_scatter_threat_mi",        16, 7)
save_plot(plot_posterior_scatter_depr,          "posterior_scatter_depr_mi",          16, 7)
save_plot(plot_posterior_scatter_lhs,           "posterior_scatter_lhs_mi",           16, 7)

save_plot(plot_corr_heatmaps,    "ppc_corr_heatmaps_mi",    14, 10)
save_plot(plot_corr_discrepancy, "ppc_corr_discrepancy_mi", 12, 6)

############################################################
#################### 20. Sanity checks #####################
############################################################

cat("\n--- posterior predictive checks: sanity checks ---\n")
cat("n imputations:", n_imp, "\n")
cat("nrep posterior per imputation:", nrep_post, "\n")
cat("Rows obs_target_stats:", nrow(obs_target_stats), "\n")
cat("Rows sim_target_stats:", nrow(sim_target_stats), "\n")
cat("Rows obs_hard_stats:", nrow(obs_hard_stats), "\n")
cat("Rows sim_hard_stats:", nrow(sim_hard_stats), "\n")
cat("Rows sim_corr_discrepancy:", nrow(sim_corr_discrepancy), "\n")
cat("Rows fit_summaries_df:", nrow(fit_summaries_df), "\n")

print(ppc_target_counts_global_mi)
print(ppc_target_counts_by_sex_mi)
print(ppc_target_counts_by_family_mi)
print(ppc_hard_counts_global_mi)
print(ppc_hard_counts_by_sex_mi)
print(ppc_corr_discrepancy_summary_mi)
