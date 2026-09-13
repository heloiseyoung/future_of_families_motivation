library(blavaan)
library(lavaan)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)

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

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Tables")) {
  table_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Tables"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Tables")) {
  table_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Tables"
} else {
  stop("Prior Predictive Checks_Tables directory not found.")
}

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Figures")) {
  fig_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Figures"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Figures")) {
  fig_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_2/Prior Predictive Checks_Figures"
} else {
  stop("Prior Predictive Checks_Figures directory not found.")
}

options(mc.cores = parallel::detectCores())

############################################################
######################## 1. Settings #######################
############################################################

n_imp <- 20
nrep_prior <- 4000
n_plot_sims_per_imp <- 5
n_line_sims_total <- 30
seed_base <- 1234

############################################################
######################## 2. Model ##########################
############################################################

riclpm_model_2 <- '

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

'

############################################################
################### 3. Variable groups #####################
############################################################

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

vars_sim <- c(vars_adversity, vars_psych)

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

save_plot <- function(plot, filename, width, height) {
  ggsave(file.path(fig_dir, paste0(filename, ".png")), plot, width = width, height = height, dpi = 400)
  ggsave(file.path(fig_dir, paste0(filename, ".pdf")), plot, width = width, height = height)
}

############################################################
################ 5. Prior target statistics ################
############################################################

temporal_stat_map <- tribble(
  ~stat,             ~xvar,                         ~yvar,
  
  # autoregressive paths
  "adhd_35",         "Adhd_3y.z",                  "Adhd_5y.z",
  "adhd_59",         "Adhd_5y.z",                  "Adhd_9y.z",
  "adhd_915",        "Adhd_9y.z",                  "Adhd_15y.z",
  "ext_35",          "Ext_3y.z",                   "Ext_5y.z",
  "ext_59",          "Ext_5y.z",                   "Ext_9y.z",
  "ext_915",         "Ext_9y.z",                   "Ext_15y.z",
  "int_35",          "Int_3y.z",                   "Int_5y.z",
  "int_59",          "Int_5y.z",                   "Int_9y.z",
  "int_915",         "Int_9y.z",                   "Int_15y.z",
  
  # cross-lagged cascade paths
  "adhd_ext_35",     "Adhd_3y.z",                  "Ext_5y.z",
  "adhd_ext_59",     "Adhd_5y.z",                  "Ext_9y.z",
  "adhd_ext_915",    "Adhd_9y.z",                  "Ext_15y.z",
  "adhd_int_35",     "Adhd_3y.z",                  "Int_5y.z",
  "adhd_int_59",     "Adhd_5y.z",                  "Int_9y.z",
  "adhd_int_915",    "Adhd_9y.z",                  "Int_15y.z",
  
  "ext_adhd_35",     "Ext_3y.z",                   "Adhd_5y.z",
  "ext_adhd_59",     "Ext_5y.z",                   "Adhd_9y.z",
  "ext_adhd_915",    "Ext_9y.z",                   "Adhd_15y.z",
  "ext_int_35",      "Ext_3y.z",                   "Int_5y.z",
  "ext_int_59",      "Ext_5y.z",                   "Int_9y.z",
  "ext_int_915",     "Ext_9y.z",                   "Int_15y.z",
  
  "int_adhd_35",     "Int_3y.z",                   "Adhd_5y.z",
  "int_adhd_59",     "Int_5y.z",                   "Adhd_9y.z",
  "int_adhd_915",    "Int_9y.z",                   "Adhd_15y.z",
  "int_ext_35",      "Int_3y.z",                   "Ext_5y.z",
  "int_ext_59",      "Int_5y.z",                   "Ext_9y.z",
  "int_ext_915",     "Int_9y.z",                   "Ext_15y.z",
  
  # within-wave contemporaneous psychopathology associations
  "adhd_ext_3",      "Adhd_3y.z",                  "Ext_3y.z",
  "adhd_int_3",      "Adhd_3y.z",                  "Int_3y.z",
  "ext_int_3",       "Ext_3y.z",                   "Int_3y.z",
  "adhd_ext_5",      "Adhd_5y.z",                  "Ext_5y.z",
  "adhd_int_5",      "Adhd_5y.z",                  "Int_5y.z",
  "ext_int_5",       "Ext_5y.z",                   "Int_5y.z",
  "adhd_ext_9",      "Adhd_9y.z",                  "Ext_9y.z",
  "adhd_int_9",      "Adhd_9y.z",                  "Int_9y.z",
  "ext_int_9",       "Ext_9y.z",                   "Int_9y.z",
  "adhd_ext_15",     "Adhd_15y.z",                 "Ext_15y.z",
  "adhd_int_15",     "Adhd_15y.z",                 "Int_15y.z",
  "ext_int_15",      "Ext_15y.z",                  "Int_15y.z",
  
  # adversity -> psychopathology at age 3
  "stoch3_adhd3",    "early.stochasticity.3y.z",   "Adhd_3y.z",
  "stoch3_ext3",     "early.stochasticity.3y.z",   "Ext_3y.z",
  "stoch3_int3",     "early.stochasticity.3y.z",   "Int_3y.z",
  "vol3_adhd3",      "early.volatility.3y.z",      "Adhd_3y.z",
  "vol3_ext3",       "early.volatility.3y.z",      "Ext_3y.z",
  "vol3_int3",       "early.volatility.3y.z",      "Int_3y.z",
  "threat3_adhd3",   "early.threat.3y.z",          "Adhd_3y.z",
  "threat3_ext3",    "early.threat.3y.z",          "Ext_3y.z",
  "threat3_int3",    "early.threat.3y.z",          "Int_3y.z",
  "depr3_adhd3",     "early.deprivation.3y.z",     "Adhd_3y.z",
  "depr3_ext3",      "early.deprivation.3y.z",     "Ext_3y.z",
  "depr3_int3",      "early.deprivation.3y.z",     "Int_3y.z",
  
  # adversity -> psychopathology at age 5
  "stoch5_adhd5",    "stoch5_resid",               "Adhd_5y.z",
  "stoch5_ext5",     "stoch5_resid",               "Ext_5y.z",
  "stoch5_int5",     "stoch5_resid",               "Int_5y.z",
  "vol5_adhd5",      "vol5_resid",                 "Adhd_5y.z",
  "vol5_ext5",       "vol5_resid",                 "Ext_5y.z",
  "vol5_int5",       "vol5_resid",                 "Int_5y.z",
  "threat5_adhd5",   "threat5_resid",              "Adhd_5y.z",
  "threat5_ext5",    "threat5_resid",              "Ext_5y.z",
  "threat5_int5",    "threat5_resid",              "Int_5y.z",
  "depr5_adhd5",     "depr5_resid",                "Adhd_5y.z",
  "depr5_ext5",      "depr5_resid",                "Ext_5y.z",
  "depr5_int5",      "depr5_resid",                "Int_5y.z",
)

get_stats_marginal <- function(dat, vars, sex_value, dataset_id, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = c(paste0(vars, "_mean"), paste0(vars, "_sd")),
    value = c(
      sapply(dat[, vars, drop = FALSE], mean, na.rm = TRUE),
      sapply(dat[, vars, drop = FALSE], sd,   na.rm = TRUE)
    ),
    dataset_id = dataset_id
  )
}

get_stats_temporal <- function(dat, sex_value, dataset_id, imputation) {
  tibble(
    imputation = imputation,
    sex = sex_value,
    stat = temporal_stat_map$stat,
    value = map2_dbl(
      temporal_stat_map$xvar,
      temporal_stat_map$yvar,
      ~ safe_cor(dat[[.x]], dat[[.y]])
    ),
    dataset_id = dataset_id
  )
}

get_all_prior_stats <- function(dat, vars_marginal, sex_value, dataset_id, imputation) {
  bind_rows(
    get_stats_marginal(dat, vars_marginal, sex_value, dataset_id, imputation),
    get_stats_temporal(dat, sex_value, dataset_id, imputation)
  )
}

############################################################
########## 6. Domain-based plausibility diagnostics ########
############################################################

check_prior_implausibility <- function(dat, sex_value, dataset_id, imputation) {
  
  cors <- cor(dat[, vars_sim, drop = FALSE], use = "pairwise.complete.obs")
  
  tibble(
    imputation = imputation,
    sex = sex_value,
    dataset_id = dataset_id,
    
    prop_vars_sd_gt_2 = mean(
      sapply(dat[, vars_sim, drop = FALSE], sd, na.rm = TRUE) > 2,
      na.rm = TRUE
    ),
    prop_vars_sd_lt_0.3 = mean(
      sapply(dat[, vars_sim, drop = FALSE], sd, na.rm = TRUE) < 0.3,
      na.rm = TRUE
    ),
    
    max_abs_corr = max(abs(cors[upper.tri(cors)]), na.rm = TRUE),
    mean_abs_corr = mean(abs(cors[upper.tri(cors)]), na.rm = TRUE),
    
    any_adv_abs_gt_6 = any(
      abs(as.matrix(dat[, vars_adversity, drop = FALSE])) > 6,
      na.rm = TRUE
    ),
    prop_adv_abs_gt_4 = mean(
      abs(as.matrix(dat[, vars_adversity, drop = FALSE])) > 4,
      na.rm = TRUE
    ),

    any_psych_abs_gt_6 = any(
      abs(as.matrix(dat[, vars_psych, drop = FALSE])) > 6,
      na.rm = TRUE
    ),
    prop_psych_abs_gt_4 = mean(
      abs(as.matrix(dat[, vars_psych, drop = FALSE])) > 4,
      na.rm = TRUE
    ),
    
    any_near_deterministic_corr = max(abs(cors[upper.tri(cors)]), na.rm = TRUE) > 0.95,
    
    ar_mean = mean(c(
      safe_cor(dat$Adhd_3y.z, dat$Adhd_5y.z),
      safe_cor(dat$Adhd_5y.z, dat$Adhd_9y.z),
      safe_cor(dat$Adhd_9y.z, dat$Adhd_15y.z),
      safe_cor(dat$Ext_3y.z, dat$Ext_5y.z),
      safe_cor(dat$Ext_5y.z, dat$Ext_9y.z),
      safe_cor(dat$Ext_9y.z, dat$Ext_15y.z),
      safe_cor(dat$Int_3y.z, dat$Int_5y.z),
      safe_cor(dat$Int_5y.z, dat$Int_9y.z),
      safe_cor(dat$Int_9y.z, dat$Int_15y.z)
    ), na.rm = TRUE),
    
    cl_adhd_to_other_mean_abs = mean(c(
      abs(safe_cor(dat$Adhd_3y.z, dat$Ext_5y.z)),
      abs(safe_cor(dat$Adhd_5y.z, dat$Ext_9y.z)),
      abs(safe_cor(dat$Adhd_9y.z, dat$Ext_15y.z)),
      abs(safe_cor(dat$Adhd_3y.z, dat$Int_5y.z)),
      abs(safe_cor(dat$Adhd_5y.z, dat$Int_9y.z)),
      abs(safe_cor(dat$Adhd_9y.z, dat$Int_15y.z))
    ), na.rm = TRUE),
    
    cl_other_to_adhd_mean_abs = mean(c(
      abs(safe_cor(dat$Ext_3y.z, dat$Adhd_5y.z)),
      abs(safe_cor(dat$Ext_5y.z, dat$Adhd_9y.z)),
      abs(safe_cor(dat$Ext_9y.z, dat$Adhd_15y.z)),
      abs(safe_cor(dat$Int_3y.z, dat$Adhd_5y.z)),
      abs(safe_cor(dat$Int_5y.z, dat$Adhd_9y.z)),
      abs(safe_cor(dat$Int_9y.z, dat$Adhd_15y.z))
    ), na.rm = TRUE),
    
    cl_ext_int_mean_abs = mean(c(
      abs(safe_cor(dat$Ext_3y.z, dat$Int_5y.z)),
      abs(safe_cor(dat$Ext_5y.z, dat$Int_9y.z)),
      abs(safe_cor(dat$Ext_9y.z, dat$Int_15y.z)),
      abs(safe_cor(dat$Int_3y.z, dat$Ext_5y.z)),
      abs(safe_cor(dat$Int_5y.z, dat$Ext_9y.z)),
      abs(safe_cor(dat$Int_9y.z, dat$Ext_15y.z))
    ), na.rm = TRUE),
  )
}

############################################################
################# 7. Containers across MI ##################
############################################################

all_prior_target_stats <- vector("list", n_imp)
all_prior_implausibility <- vector("list", n_imp)
all_obs_prior_context <- vector("list", n_imp)
all_prior_plot_data <- vector("list", n_imp)
all_obs_plot_data <- vector("list", n_imp)
all_fit_summaries <- vector("list", n_imp)

############################################################
############### 8. Loop over imputations ###################
############################################################

for (imp in seq_len(n_imp)) {
  
  cat("\n=====================================\n")
  cat("Running prior predictive checks - imputation", imp, "\n")
  cat("=====================================\n")
  
  data_i <- read.csv(file.path(data_dir, paste0("data_imp_scale", imp, ".csv")))
  
  ############################################################
  ################ 8.1 Fit model from priors #################
  ############################################################
  
  fit_i <- blavaan(
    riclpm_model_2,
    data = data_i,
    group = "sex",
    meanstructure = TRUE,
    int.ov.free = TRUE,
    int.lv.free = FALSE,
    fixed.x = FALSE,
    target = "stan",
    inits = "prior",
    prisamp = TRUE,
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
  ################ 8.2 Observed variable names ################
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
  ############# 8.3 Generate prior replicated data ###########
  ############################################################
  
  prior_sims_i <- sampleData(
    fit_i,
    nrep = nrep_prior,
    conditional = FALSE,
    simplify = FALSE
  )
  
  ############################################################
  ################ 8.4 Prior target statistics ###############
  ############################################################
  
  prior_target_stats_i <- map_dfr(seq_along(prior_sims_i), function(rep_id) {
    
    sim_rep <- bind_groups_with_sex(prior_sims_i[[rep_id]], vars_keep = ov_names_sim)
    
    bind_rows(
      get_all_prior_stats(
        dat = sim_rep %>% filter(sex == 0),
        vars_marginal = vars_sim,
        sex_value = 0,
        dataset_id = rep_id,
        imputation = imp
      ),
      get_all_prior_stats(
        dat = sim_rep %>% filter(sex == 1),
        vars_marginal = vars_sim,
        sex_value = 1,
        dataset_id = rep_id,
        imputation = imp
      )
    )
  })
  
  ############################################################
  ############ 8.5 Prior implausibility diagnostics ##########
  ############################################################
  
  prior_implausibility_i <- map_dfr(seq_along(prior_sims_i), function(rep_id) {
    
    sim_rep <- bind_groups_with_sex(prior_sims_i[[rep_id]], vars_keep = ov_names_sim)
    
    bind_rows(
      check_prior_implausibility(sim_rep %>% filter(sex == 0), 0, rep_id, imp),
      check_prior_implausibility(sim_rep %>% filter(sex == 1), 1, rep_id, imp)
    )
  })
  
  ############################################################
  ######### 8.6 Observed context for light comparison ########
  ############################################################
  
  obs_prior_context_i <- bind_rows(
    get_all_prior_stats(
      dat = data_i %>% filter(sex == 0),
      vars_marginal = vars_sim,
      sex_value = 0,
      dataset_id = 0,
      imputation = imp
    ),
    get_all_prior_stats(
      dat = data_i %>% filter(sex == 1),
      vars_marginal = vars_sim,
      sex_value = 1,
      dataset_id = 0,
      imputation = imp
    )
  ) %>%
    rename(obs_value = value)
  
  ############################################################
  ################ 8.7 Data retained for plots ###############
  ############################################################
  
  plot_rep_ids <- seq_len(min(n_plot_sims_per_imp, length(prior_sims_i)))
  
  prior_plot_data_i <- map_dfr(plot_rep_ids, function(rep_id) {
    bind_groups_with_sex(prior_sims_i[[rep_id]], vars_keep = ov_names_sim) %>%
      mutate(
        imputation = imp,
        dataset_id = rep_id,
        source = "sim"
      )
  })
  
  obs_plot_data_i <- data_i %>%
    select(all_of(vars_sim), sex) %>%
    mutate(
      imputation = imp,
      dataset_id = 0,
      source = "obs"
    )
  
  ############################################################
  ################ 8.8 Save per-imputation tables ###########
  ############################################################
  
  write_csv(prior_target_stats_i, file.path(table_dir, paste0("imp_", imp, "_prior_target_stats.csv")))
  write_csv(prior_implausibility_i, file.path(table_dir, paste0("imp_", imp, "_prior_implausibility.csv")))
  write_csv(obs_prior_context_i, file.path(table_dir, paste0("imp_", imp, "_obs_prior_context.csv")))
  write.csv(all_fit_summaries[[imp]], file.path(table_dir, paste0("imp_", imp, "_prior_fit_summary_df.csv")), row.names = FALSE)  
  
  ############################################################
  ################ 8.9 Store in global containers ###########
  ############################################################
  
  all_prior_target_stats[[imp]] <- prior_target_stats_i
  all_prior_implausibility[[imp]] <- prior_implausibility_i
  all_obs_prior_context[[imp]] <- obs_prior_context_i
  all_prior_plot_data[[imp]] <- prior_plot_data_i
  all_obs_plot_data[[imp]] <- obs_plot_data_i
  
  rm(fit_i, fit_sum_i, prior_sims_i, data_i, prior_plot_data_i, obs_plot_data_i)
  gc()
}

############################################################
################## 9. Bind all imputations #################
############################################################

prior_target_stats <- bind_rows(all_prior_target_stats)
prior_implausibility <- bind_rows(all_prior_implausibility)
obs_prior_context <- bind_rows(all_obs_prior_context)
prior_plot_data <- bind_rows(all_prior_plot_data)
obs_plot_data <- bind_rows(all_obs_plot_data)
fit_summaries_df <- do.call(rbind, all_fit_summaries)

############################################################
############### 10. Prior predictive summaries #############
############################################################

prior_summary_mi <- prior_target_stats %>%
  group_by(sex, stat) %>%
  summarise(
    prior_mean = mean(value, na.rm = TRUE),
    prior_sd   = sd(value, na.rm = TRUE),
    prior_q025 = quantile(value, 0.025, na.rm = TRUE),
    prior_q50  = quantile(value, 0.50,  na.rm = TRUE),
    prior_q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  ) %>%
  arrange(sex, stat)

print(prior_summary_mi)

############################################################
########### 11. Prior implausibility summaries #############
############################################################

prior_implausibility <- prior_implausibility %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  )

prior_implausibility_summary_mi <- prior_implausibility %>%
  pivot_longer(
    cols = -c(imputation, sex, dataset_id),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(sex, metric) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    q025 = quantile(value, 0.025, na.rm = TRUE),
    q50  = quantile(value, 0.50, na.rm = TRUE),
    q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

print(prior_implausibility_summary_mi)

############################################################
######### 12. Compare to observed values lightly ###########
############################################################

obs_prior_context <- obs_prior_context %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  )

obs_prior_context_mi <- obs_prior_context %>%
  group_by(sex, stat) %>%
  summarise(
    obs_value = mean(obs_value, na.rm = TRUE),
    obs_sd_between_imp = sd(obs_value, na.rm = TRUE),
    .groups = "drop"
  )

prior_vs_obs_mi <- prior_summary_mi %>%
  left_join(
    obs_prior_context_mi %>% select(sex, stat, obs_value),
    by = c("sex", "stat")
  ) %>%
  mutate(
    obs_in_prior_95 = obs_value >= prior_q025 & obs_value <= prior_q975
  )

print(prior_vs_obs_mi)

############################################################
############# 13. Focused output tables ####################
############################################################

prior_summary_marginal_mi <- prior_summary_mi %>%
  filter(grepl("_mean$|_sd$", stat))

prior_summary_temporal_mi <- prior_summary_mi %>%
  filter(stat %in% temporal_stat_map$stat)

print(prior_summary_marginal_mi)
print(prior_summary_temporal_mi)

############################################################
################### 14. Save result tables #################
############################################################

write_csv(prior_target_stats, file.path(table_dir, "prior_target_stats_all_imputations.csv"))
write_csv(prior_summary_mi, file.path(table_dir, "prior_summary_mi.csv"))
write_csv(prior_implausibility, file.path(table_dir, "prior_implausibility_all_imputations.csv"))
write_csv(prior_implausibility_summary_mi, file.path(table_dir, "prior_implausibility_summary_mi.csv"))
write_csv(obs_prior_context, file.path(table_dir, "obs_prior_context_all_imputations.csv"))
write_csv(obs_prior_context_mi, file.path(table_dir, "obs_prior_context_mi.csv"))
write_csv(prior_vs_obs_mi, file.path(table_dir, "prior_vs_obs_mi.csv"))
write_csv(prior_summary_marginal_mi, file.path(table_dir, "prior_summary_marginal_mi.csv"))
write_csv(prior_summary_temporal_mi, file.path(table_dir, "prior_summary_temporal_mi.csv"))
write.csv(fit_summaries_df, file.path(table_dir, "prior_fit_summaries_df_all_imputations.csv"), row.names = FALSE)

############################################################
############################################################
##################### 15. Visualization ####################
############################################################
############################################################

############################################################
################ 15.1 Data for prior plots #################
############################################################

plot_data_prior <- bind_rows(obs_plot_data, prior_plot_data) %>%
  mutate(
    sex = recode(as.character(sex), "0" = "boys", "1" = "girls")
  )

############################################################
############### 15.2 Separate density figures ##############
############################################################

density_data <- plot_data_prior %>%
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
      variable == "early.stochasticity.3y.z" ~ "stochasticity 3y",
      variable == "early.volatility.3y.z" ~ "volatility 3y",
      variable == "early.threat.3y.z" ~ "threat 3y",
      variable == "early.deprivation.3y.z" ~ "deprivation 3y",
      variable == "stoch5_resid" ~ "stochasticity 5y residual",
      variable == "vol5_resid" ~ "volatility 5y residual",
      variable == "threat5_resid" ~ "threat 5y residual",
      variable == "depr5_resid" ~ "deprivation 5y residual",
      TRUE ~ variable
    )
  )

make_density_plot <- function(dat, vars_keep, title_text, facet_levels = NULL) {
  
  dat_sub <- dat %>%
    filter(variable %in% vars_keep)
  
  if (!is.null(facet_levels)) {
    dat_sub <- dat_sub %>%
      mutate(variable_label = factor(variable_label, levels = facet_levels))
  }
  
  ggplot(dat_sub, aes(x = value, group = interaction(imputation, dataset_id, source))) +
    geom_density(
      data = ~ subset(.x, source == "sim"),
      colour = "#14D9BB",
      alpha = 0.10,
      linewidth = 0.18
    ) +
    geom_density(
      data = ~ subset(.x, source == "obs"),
      colour = "#F58518",
      linewidth = 0.55
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    facet_grid(sex ~ variable_label, scales = "free") +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.2, colour = "grey88"),
      panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey90"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(size = 12)
    ) +
    labs(
      title = title_text,
      subtitle = "Orange line = observed data; Blue lines = prior simulated datasets across imputations",
      x = "Standardized value (z-score)",
      y = "Density"
    )
}

plot_prior_density_adhd <- make_density_plot(
  dat = density_data,
  vars_keep = c("Adhd_3y.z", "Adhd_5y.z", "Adhd_9y.z", "Adhd_15y.z"),
  title_text = "Prior predictive densities – ADHD",
  facet_levels = c("ADHD 3y", "ADHD 5y", "ADHD 9y", "ADHD 15y")
)

plot_prior_density_ext <- make_density_plot(
  dat = density_data,
  vars_keep = c("Ext_3y.z", "Ext_5y.z", "Ext_9y.z", "Ext_15y.z"),
  title_text = "Prior predictive densities – externalizing",
  facet_levels = c("EXT 3y", "EXT 5y", "EXT 9y", "EXT 15y")
)

plot_prior_density_int <- make_density_plot(
  dat = density_data,
  vars_keep = c("Int_3y.z", "Int_5y.z", "Int_9y.z", "Int_15y.z"),
  title_text = "Prior predictive densities – internalizing",
  facet_levels = c("INT 3y", "INT 5y", "INT 9y", "INT 15y")
)

plot_prior_density_adv3 <- make_density_plot(
  dat = density_data,
  vars_keep = c(
    "early.stochasticity.3y.z",
    "early.volatility.3y.z",
    "early.threat.3y.z",
    "early.deprivation.3y.z"
  ),
  title_text = "Prior predictive densities – adversity at age 3",
  facet_levels = c("stochasticity 3y", "volatility 3y", "threat 3y", "deprivation 3y")
)

plot_prior_density_adv5 <- make_density_plot(
  dat = density_data,
  vars_keep = c(
    "stoch5_resid",
    "vol5_resid",
    "threat5_resid",
    "depr5_resid"
  ),
  title_text = "Prior predictive densities – residualized adversity at age 5",
  facet_levels = c(
    "stochasticity 5y residual",
    "volatility 5y residual",
    "threat 5y residual",
    "deprivation 5y residual"
  )
)

print(plot_prior_density_adhd)
print(plot_prior_density_ext)
print(plot_prior_density_int)
print(plot_prior_density_adv3)
print(plot_prior_density_adv5)

############################################################
############### 15.3 Prior regression lines ################
############################################################

pairs_vars <- tribble(
  ~block,              ~family,           ~xvar,                      ~yvar,                         ~panel,
  
  # psychopathology cascade: autoregressive
  "psych_cascade",     "ar",              "Adhd_3y.z",                "Adhd_5y.z",                  "ADHD 3y → ADHD 5y",
  "psych_cascade",     "ar",              "Adhd_5y.z",                "Adhd_9y.z",                  "ADHD 5y → ADHD 9y",
  "psych_cascade",     "ar",              "Adhd_9y.z",                "Adhd_15y.z",                 "ADHD 9y → ADHD 15y",
  "psych_cascade",     "ar",              "Ext_3y.z",                 "Ext_5y.z",                   "EXT 3y → EXT 5y",
  "psych_cascade",     "ar",              "Ext_5y.z",                 "Ext_9y.z",                   "EXT 5y → EXT 9y",
  "psych_cascade",     "ar",              "Ext_9y.z",                 "Ext_15y.z",                  "EXT 9y → EXT 15y",
  "psych_cascade",     "ar",              "Int_3y.z",                 "Int_5y.z",                   "INT 3y → INT 5y",
  "psych_cascade",     "ar",              "Int_5y.z",                 "Int_9y.z",                   "INT 5y → INT 9y",
  "psych_cascade",     "ar",              "Int_9y.z",                 "Int_15y.z",                  "INT 9y → INT 15y",
  
  # psychopathology cascade: cross-lagged
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
  
  # adversity -> psych
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
  "adversity_psych",   "deprivation",     "depr5_resid",              "Int_5y.z",                   "depr 5y resid → INT 5y"
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

line_imps <- unique(prior_plot_data$imputation)
line_imps <- line_imps[seq_len(min(length(line_imps), n_line_sims_total))]

prior_line_data <- map_dfr(seq_len(nrow(pairs_vars)), function(j) {
  xvar   <- pairs_vars$xvar[j]
  yvar   <- pairs_vars$yvar[j]
  panel  <- pairs_vars$panel[j]
  family <- pairs_vars$family[j]
  block  <- pairs_vars$block[j]
  
  prior_plot_data %>%
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
  
  prior_sub <- prior_line_data
  obs_sub   <- obs_scatter_data
  
  if (!is.null(data_block)) {
    prior_sub <- prior_sub %>% filter(block == data_block)
    obs_sub   <- obs_sub %>% filter(block == data_block)
  }
  
  if (!is.null(family_name)) {
    prior_sub <- prior_sub %>% filter(family == family_name)
    obs_sub   <- obs_sub %>% filter(family == family_name)
  }
  
  ggplot() +
    geom_smooth(
      data = prior_sub,
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
      subtitle = "Orange = observed regression line ; Turquoise = prior simulated draws across imputations",
      x = NULL,
      y = NULL
    )
}

plot_prior_scatter_ar <- make_regression_plot(
  family_name = "ar",
  title_text = "Prior predictive check – psychopathology cascade (autoregressive paths)"
)

plot_prior_scatter_adhd_to_other <- make_regression_plot(
  family_name = "adhd_to_other",
  title_text = "Prior predictive check – psychopathology cascade (ADHD → EXT / INT)"
)

plot_prior_scatter_other_to_adhd <- make_regression_plot(
  family_name = "other_to_adhd",
  title_text = "Prior predictive check – psychopathology cascade (EXT / INT → ADHD)"
)

plot_prior_scatter_ext_int <- make_regression_plot(
  family_name = "ext_int",
  title_text = "Prior predictive check – psychopathology cascade (EXT → INT)"
)

plot_prior_scatter_int_ext <- make_regression_plot(
  family_name = "int_ext",
  title_text = "Prior predictive check – psychopathology cascade (INT → EXT)"
)

plot_prior_scatter_stoch <- make_regression_plot(
  family_name = "stochasticity",
  title_text = "Prior predictive check – regression structure (stochasticity)"
)

plot_prior_scatter_vol <- make_regression_plot(
  family_name = "volatility",
  title_text = "Prior predictive check – regression structure (volatility)"
)

plot_prior_scatter_threat <- make_regression_plot(
  family_name = "threat",
  title_text = "Prior predictive check – regression structure (threat)"
)

plot_prior_scatter_depr <- make_regression_plot(
  family_name = "deprivation",
  title_text = "Prior predictive check – regression structure (deprivation)"
)

print(plot_prior_scatter_ar)
print(plot_prior_scatter_adhd_to_other)
print(plot_prior_scatter_other_to_adhd)
print(plot_prior_scatter_ext_int)
print(plot_prior_scatter_int_ext)
print(plot_prior_scatter_stoch)
print(plot_prior_scatter_vol)
print(plot_prior_scatter_threat)
print(plot_prior_scatter_depr)

############################################################
################### 15.4 Correlation heatmaps ##############
############################################################

make_corr_df <- function(dat, vars, sex_label, source_label) {
  cmat <- cor(dat[, vars, drop = FALSE], use = "pairwise.complete.obs")
  as.data.frame(as.table(cmat), stringsAsFactors = FALSE) %>%
    rename(var1 = Var1, var2 = Var2, corr = Freq) %>%
    mutate(
      sex = sex_label,
      source = source_label
    )
}

prior_heatmap_example <- prior_plot_data %>%
  filter(imputation == 1, dataset_id == 1)

corr_heatmap_data_prior <- bind_rows(
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
    prior_heatmap_example %>% filter(sex == 0) %>% select(-source, -imputation, -dataset_id),
    vars_sim,
    sex_label = "boys",
    source_label = "prior draw"
  ),
  make_corr_df(
    prior_heatmap_example %>% filter(sex == 1) %>% select(-source, -imputation, -dataset_id),
    vars_sim,
    sex_label = "girls",
    source_label = "prior draw"
  )
)

plot_prior_corr_heatmaps <- ggplot(corr_heatmap_data_prior, aes(x = var1, y = var2, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#F58518",
    mid = "#F7F7F7",
    high = "#14D9BB",
    midpoint = 0,
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1)
  ) +
  facet_grid(sex ~ source) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    title = "Observed vs prior predictive correlation matrices",
    subtitle = "Example shown: imputation 1, prior replicate 1",
    x = NULL,
    y = NULL,
    fill = "r"
  )

print(plot_prior_corr_heatmaps)

############################################################
#################### 16. Save figures ######################
############################################################

save_plot(plot_prior_scatter_ar,              "plot_prior_scatter_ar_mi",              20, 8)
save_plot(plot_prior_scatter_adhd_to_other,   "plot_prior_scatter_adhd_to_other_mi",   20, 8)
save_plot(plot_prior_scatter_other_to_adhd,   "plot_prior_scatter_other_to_adhd_mi",   20, 8)
save_plot(plot_prior_scatter_ext_int,         "plot_prior_scatter_ext_int_mi",         20, 8)
save_plot(plot_prior_scatter_int_ext,         "plot_prior_scatter_int_ext_mi",         20, 8)

save_plot(plot_prior_density_adhd,   "prior_density_adhd_mi",         8, 7)
save_plot(plot_prior_density_ext,    "prior_density_ext_mi",          8, 7)
save_plot(plot_prior_density_int,    "prior_density_int_mi",          8, 7)
save_plot(plot_prior_density_adv3,   "prior_density_adversity_3y_mi", 10, 7)
save_plot(plot_prior_density_adv5,   "prior_density_adversity_5y_mi", 10, 7)

save_plot(plot_prior_scatter_stoch,  "plot_prior_scatter_stoch_mi",   20, 8)
save_plot(plot_prior_scatter_vol,    "plot_prior_scatter_vol_mi",     20, 8)
save_plot(plot_prior_scatter_threat, "plot_prior_scatter_threat_mi",  20, 8)
save_plot(plot_prior_scatter_depr,   "plot_prior_scatter_depr_mi",    20, 8)

save_plot(plot_prior_corr_heatmaps,  "prior_corr_heatmaps_mi",        12, 8)

############################################################
#################### 17. Sanity checks #####################
############################################################

cat("\n--- prior predictive checks: sanity checks ---\n")
cat("n imputations:", n_imp, "\n")
cat("nrep prior per imputation:", nrep_prior, "\n")
cat("Rows prior_target_stats:", nrow(prior_target_stats), "\n")
cat("Rows prior_implausibility:", nrow(prior_implausibility), "\n")
cat("Rows obs_prior_context:", nrow(obs_prior_context), "\n")
