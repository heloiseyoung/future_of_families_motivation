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

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables")) {
  table_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables")) {
  table_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables"
} else {
  stop("Results_Tables directory not found.")
}

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects")) {
  fit_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects")) {
  fit_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects"
} else {
  fit_dir <- file.path(dirname(table_dir), "Fit_Objects")
  dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)
}

options(mc.cores = parallel::detectCores())

############################################################
######################## 1. Settings #######################
############################################################

n_imp <- 20
seed_base <- 1234
model_id <- "model_3"

# FALSE = ne refitte pas les imputations déjà sauvegardées
# TRUE  = refitte même si le .rds existe déjà
refit_existing <- FALSE

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
############### 3. Fit and save .rds objects ###############
############################################################

for (i in seq_len(n_imp)) {
  
  cat("\n============================================================\n")
  cat("Processing imputation ", i, " / ", n_imp, "\n", sep = "")
  cat("============================================================\n")
  
  data_file_i <- file.path(
    data_dir,
    paste0("data_imp_scale", i, ".csv")
  )
  
  fit_file_i <- file.path(
    fit_dir,
    paste0(model_id, "_fit_imp_", i, ".rds")
  )
  
  if (file.exists(fit_file_i) && !isTRUE(refit_existing)) {
    cat("Fit already exists. Skipping:\n", fit_file_i, "\n", sep = "")
    next
  }
  
  if (!file.exists(data_file_i)) {
    stop("Imputed data file not found: ", data_file_i)
  }
  
  cat("Reading data:\n", data_file_i, "\n", sep = "")
  data_i <- read.csv(data_file_i)
  
  cat("Fitting model for imputation ", i, "...\n", sep = "")
  
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
    seed = seed_base + i
  )
  
  cat("Saving fit object to:\n", fit_file_i, "\n", sep = "")
  
  saveRDS(
    fit_i,
    fit_file_i,
    compress = "xz"
  )
  
  rm(data_i, fit_i)
  gc()
  
  cat("Finished imputation ", i, ".\n", sep = "")
}

############################################################
###################### 4. Final check ######################
############################################################

fit_files_index <- data.frame(
  imputation = seq_len(n_imp),
  fit_file = file.path(
    fit_dir,
    paste0(model_id, "_fit_imp_", seq_len(n_imp), ".rds")
  ),
  exists = file.exists(
    file.path(
      fit_dir,
      paste0(model_id, "_fit_imp_", seq_len(n_imp), ".rds")
    )
  ),
  stringsAsFactors = FALSE
)

cat("\n--- fit files index ---\n")
print(fit_files_index)

cat("\nNumber of saved .rds fit files: ", sum(fit_files_index$exists), " / ", n_imp, "\n", sep = "")