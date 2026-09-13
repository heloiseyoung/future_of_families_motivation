### Fragile Families cohort indicator creation script
# Pierre Jacquet 2026
# This loops through the imputed datasets and creates the composite variables for use in the
# regressions and control analyses

library(tidyverse)
library(readr)
library(psych)
library(psy)
library(GPArotation)
library(PerformanceAnalytics)
library(FactoMineR)
library(factoextra)
library(VIM)
library(corrplot)
library(dplyr)

setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/data")
setwd("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise/Child Development/Short-term mindset/data")

# mydf.full <- read_csv("mydf.full.csv")
data_imp <- read.csv("data_imp1.csv")

# Identify complete cases
idnas <- data_imp
idnas <- idnas[complete.cases(idnas),]
length(idnas$idnum) # Final sample size

#### Some checks

# Raw percentage of missing cases
missing <- ifelse(data_imp < 0,1,0) %>%
  colMeans() %>%
  as.data.frame()

# missingness #

data_aggr = aggr(data_imp,
                 col=c("navyblue", "red"), 
                 numbers=TRUE,
                 sortVars=TRUE,
                 labels=names(data_imp),
                 cex.axis=0.7,
                 gap=1,
                 ylab=c("Proportion of missingness","Missingness Pattern"))


# z-score within sex (gère NA + cas sd=0)
z_by_sex <- function(x, sex) {
  out <- rep(NA_real_, length(x))
  for (g in unique(sex[!is.na(sex)])) {
    idx <- which(sex == g & !is.na(x))
    if (length(idx) >= 2) {
      s <- sd(x[idx])
      if (!is.na(s) && s > 0) {
        out[idx] <- (x[idx] - mean(x[idx])) / s
      } else {
        out[idx] <- 0
      }
    } else if (length(idx) == 1) {
      out[idx] <- 0
    }
  }
  out
}

for (i in 1:20) {
  data <- read.csv(paste("data_imp", i, ".csv", sep=""), sep=",", header=TRUE)
  
  # ---- IMPORTANT ----
  # remplace "sex" par le nom exact de ta variable de sexe
  grp <- data$sex
  # -------------------
  
  #################################
  ### Mating and health efforts ###
  #################################
  data <- data %>%
    mutate(
      age.1st.date.z       = z_by_sex(age.1st.date, grp),
      num.date.z           = z_by_sex(num.date, grp),
      age.1st.sex.z        = z_by_sex(age.1st.sex, grp),
      num.sex.z            = z_by_sex(num.sex, grp),
      age.1st.cig.z        = z_by_sex(age.1st.cig, grp),
      freq.smoke.month.z   = z_by_sex(freq.smoke.month, grp),
      quant.smoke.day.z    = z_by_sex(quant.smoke.day, grp),
      age.1st.drank.z      = z_by_sex(age.1st.drank, grp),
      freq.alc.month.z     = z_by_sex(freq.alc.month, grp)
    )

  mating.behaviour_cols <- c("age.1st.date.z","num.date.z",'age.1st.sex.z','num.sex.z')
  data <- data %>%
    mutate(
      mating.behaviour   = rowSums(across(all_of(mating.behaviour_cols)), na.rm = FALSE),
      mating.behaviour.z = z_by_sex(mating.behaviour, grp)
    )

  risky.health.behaviour_cols <- c('age.1st.cig.z', 'freq.smoke.month.z','age.1st.drank.z','freq.alc.month.z')
  data <- data %>%
    mutate(
      risky.health.behaviour   = rowSums(across(all_of(risky.health.behaviour_cols)), na.rm = FALSE),
      risky.health.behaviour.z = z_by_sex(risky.health.behaviour, grp)
    )
  
  
  #########################
  ### Early deprivation ###
  #########################
  data$act <- rowMeans(data[,c("act_3y","act_5y")])
  data$toys <- rowMeans(data[,c("toys_3y","toys_5y")])
  data$interact <- rowMeans(data[,c("interact_3y","interact_5y")])
  
  data <- data %>%
    mutate(
      act.z         = z_by_sex(act, grp),
      toys.z        = z_by_sex(toys, grp),
      interact.z    = z_by_sex(interact, grp),
      act_3y.z      = z_by_sex(act_3y, grp),
      toys_3y.z     = z_by_sex(toys_3y, grp),
      interact_3y.z = z_by_sex(interact_3y, grp),
      act_5y.z      = z_by_sex(act_5y, grp),
      toys_5y.z     = z_by_sex(toys_5y, grp),
      interact_5y.z = z_by_sex(interact_5y, grp)
    )
  
  early.deprivation_cols <- c('act.z','toys.z','interact.z')
  data <- data %>%
    mutate(
      early.deprivation   = rowSums(across(all_of(early.deprivation_cols)), na.rm = FALSE),
      early.deprivation.z = z_by_sex(early.deprivation, grp)
    )
  
  early.deprivation.3y_cols <- c('act_3y.z','toys_3y.z','interact_3y.z')
  data <- data %>%
    mutate(
      early.deprivation.3y   = rowSums(across(all_of(early.deprivation.3y_cols)), na.rm = FALSE),
      early.deprivation.3y.z = z_by_sex(early.deprivation.3y, grp)
    )
  
  early.deprivation.5y_cols <- c('act_5y.z','toys_5y.z','interact_5y.z')
  data <- data %>%
    mutate(
      early.deprivation.5y   = rowSums(across(all_of(early.deprivation.5y_cols)), na.rm = FALSE),
      early.deprivation.5y.z = z_by_sex(early.deprivation.5y, grp)
    )
  
  ####################
  ### Early threat ###
  ####################
  data$phys_agg <- rowMeans(data[,c("phys_agg_3y","phys_agg_5y")])
  data$psych_agg <- rowMeans(data[,c("psych_agg_3y","psych_agg_5y")])
  data$violence <- rowMeans(data[,c("violence_3y","violence_5y")])
  
  data <- data %>%
    mutate(
      phys_agg.z     = z_by_sex(phys_agg, grp),
      psych_agg.z    = z_by_sex(psych_agg, grp),
      violence.z     = z_by_sex(violence, grp),
      phys_agg_3y.z  = z_by_sex(phys_agg_3y, grp),
      psych_agg_3y.z = z_by_sex(psych_agg_3y, grp),
      violence_3y.z  = z_by_sex(violence_3y, grp),
      phys_agg_5y.z  = z_by_sex(phys_agg_5y, grp),
      psych_agg_5y.z = z_by_sex(psych_agg_5y, grp),
      violence_5y.z  = z_by_sex(violence_5y, grp)
    )
  
  early.threat_cols <- c('phys_agg.z','psych_agg.z','violence.z')
  data <- data %>%
    mutate(
      early.threat   = rowSums(across(all_of(early.threat_cols)), na.rm = FALSE),
      early.threat.z = z_by_sex(early.threat, grp)
    )
  
  early.threat.3y_cols <- c('phys_agg_3y.z','psych_agg_3y.z','violence_3y.z')
  data <- data %>%
    mutate(
      early.threat.3y   = rowSums(across(all_of(early.threat.3y_cols)), na.rm = FALSE),
      early.threat.3y.z = z_by_sex(early.threat.3y, grp)
    )
  
  early.threat.5y_cols <- c('phys_agg_5y.z','psych_agg_5y.z','violence_5y.z')
  data <- data %>%
    mutate(
      early.threat.5y   = rowSums(across(all_of(early.threat.5y_cols)), na.rm = FALSE),
      early.threat.5y.z = z_by_sex(early.threat.5y, grp)
    )
  
  ###########
  ### SES ###
  ###########
  data <- data %>%
    mutate(
      ses_15y.z = z_by_sex(ses_15y, grp)
    )
  
  ###################################
  ### Early and Current harshness ###
  ###################################
  early.harshness_cols <- c('act.z','toys.z','interact.z','phys_agg.z','psych_agg.z','violence.z')
  data <- data %>%
    mutate(
      early.harshness   = rowSums(across(all_of(early.harshness_cols)), na.rm = FALSE),
      early.harshness.z = z_by_sex(early.harshness, grp)
    )
  
  early.harshness.3y_cols <- c('act_3y.z','toys_3y.z','interact_3y.z','phys_agg_3y.z','psych_agg_3y.z','violence_3y.z')
  data <- data %>%
    mutate(
      early.harshness.3y   = rowSums(across(all_of(early.harshness.3y_cols)), na.rm = FALSE),
      early.harshness.3y.z = z_by_sex(early.harshness.3y, grp)
    )
  
  early.harshness.5y_cols <- c('act_5y.z','toys_5y.z','interact_5y.z','phys_agg_5y.z','psych_agg_5y.z','violence_5y.z')
  data <- data %>%
    mutate(
      early.harshness.5y   = rowSums(across(all_of(early.harshness.5y_cols)), na.rm = FALSE),
      early.harshness.5y.z = z_by_sex(early.harshness.5y, grp)
    )
  
  ###########################
  ### Early stochasticity ###
  ###########################
  data <- data %>%
    mutate(
      arrang = rowMeans(across(c(arrang_3y, arrang_5y), ~ as.numeric(.x)), na.rm = TRUE),
      bedtime = rowMeans(across(c(bedtime_3y, bedtime_5y), ~ as.numeric(.x)), na.rm = TRUE),
      bed_routine = rowMeans(across(c(bed_routine_3y, bed_routine_5y), ~ as.numeric(.x)), na.rm = TRUE),
      mom_depress = rowMeans(across(c(mom_depress_3y, mom_depress_5y), ~ as.numeric(.x)), na.rm = TRUE)
    ) %>%
    mutate(
      arrang.z        = z_by_sex(arrang, grp),
      bedtime.z       = z_by_sex(bedtime, grp),
      bed_routine.z   = z_by_sex(bed_routine, grp),
      mom_depress.z   = z_by_sex(mom_depress, grp),
      
      arrang_3y.z     = z_by_sex(arrang_3y, grp),
      bedtime_3y.z    = z_by_sex(bedtime_3y, grp),
      bed_routine_3y.z= z_by_sex(bed_routine_3y, grp),
      mom_depress_3y.z= z_by_sex(mom_depress_3y, grp),
      
      arrang_5y.z     = z_by_sex(arrang_5y, grp),
      bedtime_5y.z    = z_by_sex(bedtime_5y, grp),
      bed_routine_5y.z= z_by_sex(bed_routine_5y, grp),
      mom_depress_5y.z= z_by_sex(mom_depress_5y, grp),
      
      chaos_9y.z      = z_by_sex(chaos_9y, grp)
    )
  
  early.stochasticity_cols <- c('arrang.z','bedtime.z','bed_routine.z')
  data <- data %>%
    mutate(
      early.stochasticity   = rowSums(across(all_of(early.stochasticity_cols)), na.rm = FALSE),
      early.stochasticity.z = z_by_sex(early.stochasticity, grp)
    )
  
  early.stochasticity.3y_cols <- c('arrang_3y.z','bedtime_3y.z','bed_routine_3y.z')
  data <- data %>%
    mutate(
      early.stochasticity.3y   = rowSums(across(all_of(early.stochasticity.3y_cols)), na.rm = FALSE),
      early.stochasticity.3y.z = z_by_sex(early.stochasticity.3y, grp)
    )
  
  early.stochasticity.5y_cols <- c('arrang_5y.z','bedtime_5y.z','bed_routine_5y.z')
  data <- data %>%
    mutate(
      early.stochasticity.5y   = rowSums(across(all_of(early.stochasticity.5y_cols)), na.rm = FALSE),
      early.stochasticity.5y.z = z_by_sex(early.stochasticity.5y, grp)
    )
  
  ########################
  ### Early volatility ###
  ########################
  data <- data %>%
    mutate(
      sep = rowMeans(across(c(sep_3y, sep_5y), ~ as.numeric(.x)), na.rm = TRUE),
      move = rowMeans(across(c(move_3y, move_5y), ~ as.numeric(.x)), na.rm = TRUE),
      jobs = rowMeans(across(c(jobs_3y, jobs_5y), ~ as.numeric(.x)), na.rm = TRUE),
      mom_depress_ch = rowMeans(across(c(mom_depress_ch_3y, mom_depress_ch_5y), ~ as.numeric(.x)), na.rm = TRUE)
    ) %>%
    mutate(
      sep.z            = z_by_sex(sep, grp),
      move.z           = z_by_sex(move, grp),
      jobs.z           = z_by_sex(jobs, grp),
      mom_depress_ch.z = z_by_sex(mom_depress_ch, grp),
      
      sep_3y.z         = z_by_sex(sep_3y, grp),
      move_3y.z        = z_by_sex(move_3y, grp),
      jobs_3y.z        = z_by_sex(jobs_3y, grp),
      mom_depress_ch_3y.z = z_by_sex(mom_depress_ch_3y, grp),
      
      sep_5y.z         = z_by_sex(sep_5y, grp),
      move_5y.z        = z_by_sex(move_5y, grp),
      jobs_5y.z        = z_by_sex(jobs_5y, grp),
      mom_depress_ch_5y.z = z_by_sex(mom_depress_ch_5y, grp)
    )
  
  early.volatility_cols <- c('sep.z','move.z','jobs.z','mom_depress_ch.z')
  data <- data %>%
    mutate(
      early.volatility   = rowSums(across(all_of(early.volatility_cols)), na.rm = FALSE),
      early.volatility.z = z_by_sex(early.volatility, grp)
    )
  
  early.volatility.3y_cols <- c('sep_3y.z','move_3y.z','jobs_3y.z','mom_depress_ch_3y.z')
  data <- data %>%
    mutate(
      early.volatility.3y   = rowSums(across(all_of(early.volatility.3y_cols)), na.rm = FALSE),
      early.volatility.3y.z = z_by_sex(early.volatility.3y, grp)
    )
  
  early.volatility.5y_cols <- c('sep_5y.z','move_5y.z','jobs_5y.z','mom_depress_ch_5y.z')
  data <- data %>%
    mutate(
      early.volatility.5y   = rowSums(across(all_of(early.volatility.5y_cols)), na.rm = FALSE),
      early.volatility.5y.z = z_by_sex(early.volatility.5y, grp)
    )
  
  #############################
  ### Early unpredictability ###
  #############################
  early.unpredictability_cols <- c(
    'sep.z','move.z','jobs.z','mom_depress_ch.z',
    'arrang.z','bedtime.z','bed_routine.z'
  )
  data <- data %>%
    mutate(
      early.unpredictability   = rowSums(across(all_of(early.unpredictability_cols)), na.rm = FALSE),
      early.unpredictability.z = z_by_sex(early.unpredictability, grp)
    )
  
  early.unpredictability.3y_cols <- c(
    'sep_3y.z','move_3y.z','jobs_3y.z','mom_depress_ch_3y.z',
    'arrang_3y.z','bedtime_3y.z','bed_routine_3y.z'
  )
  data <- data %>%
    mutate(
      early.unpredictability.3y   = rowSums(across(all_of(early.unpredictability.3y_cols)), na.rm = FALSE),
      early.unpredictability.3y.z = z_by_sex(early.unpredictability.3y, grp)
    )
  
  early.unpredictability.5y_cols <- c(
    'sep_5y.z','move_5y.z','jobs_5y.z','mom_depress_ch_5y.z',
    'arrang_5y.z','bedtime_5y.z','bed_routine_5y.z'
  )
  data <- data %>%
    mutate(
      early.unpredictability.5y   = rowSums(across(all_of(early.unpredictability.5y_cols)), na.rm = FALSE),
      early.unpredictability.5y.z = z_by_sex(early.unpredictability.5y, grp)
    )
  
  ############
  ### ADHD ###
  ############
  data <- data %>%
    mutate(
      Adhd_3y.z     = z_by_sex(Adhd_3y, grp),
      Adhd_5y.z     = z_by_sex(Adhd_5y, grp),
      Adhd_9y.z     = z_by_sex(Adhd_9y, grp),
      Adhd_15y.z    = z_by_sex(Adhd_15y, grp)
    )
  
  ############
  ### CBCL ###
  ############
  data <- data %>%
    mutate(
      Int_3y.z     = z_by_sex(Int_3y, grp),
      Int_5y.z     = z_by_sex(Int_5y, grp),
      Int_9y.z     = z_by_sex(Int_9y, grp),
      Int_15y.z    = z_by_sex(Int_15y, grp),
      Ext_3y.z     = z_by_sex(Ext_3y, grp),
      Ext_5y.z     = z_by_sex(Ext_5y, grp),
      Ext_9y.z     = z_by_sex(Ext_9y, grp),
      Ext_15y.z    = z_by_sex(Ext_15y, grp)
    )
  
  ###################################################################
  ### 5y deprivation, threat, volatility, stochasticity residuals ###
  ###################################################################
  data$stoch5_resid <- resid(
    lm(early.stochasticity.5y.z ~ early.stochasticity.3y.z + early.volatility.3y.z + early.threat.3y.z + early.deprivation.3y.z,
       data = data)
  )
  
  data$vol5_resid <- resid(
    lm(early.volatility.5y.z ~ early.stochasticity.3y.z + early.volatility.3y.z + early.threat.3y.z + early.deprivation.3y.z,
       data = data)
  )
  
  data$threat5_resid <- resid(
    lm(early.threat.5y.z ~ early.stochasticity.3y.z + early.volatility.3y.z + early.threat.3y.z + early.deprivation.3y.z,
       data = data)
  )
  
  data$depr5_resid <- resid(
    lm(early.deprivation.5y.z ~ early.stochasticity.3y.z + early.volatility.3y.z + early.threat.3y.z + early.deprivation.3y.z,
       data = data)
  )
  
  assign(paste("data_imp", i, sep= ""), data)
}


write.csv(data_imp1, file = "data_imp_scale1.csv",row.names = F)
write.csv(data_imp2, file = "data_imp_scale2.csv",row.names = F)
write.csv(data_imp3, file = "data_imp_scale3.csv",row.names = F)
write.csv(data_imp4, file = "data_imp_scale4.csv",row.names = F)
write.csv(data_imp5, file = "data_imp_scale5.csv",row.names = F)
write.csv(data_imp6, file = "data_imp_scale6.csv",row.names = F)
write.csv(data_imp7, file = "data_imp_scale7.csv",row.names = F)
write.csv(data_imp8, file = "data_imp_scale8.csv",row.names = F)
write.csv(data_imp9, file = "data_imp_scale9.csv",row.names = F)
write.csv(data_imp10, file = "data_imp_scale10.csv",row.names = F)
write.csv(data_imp11, file = "data_imp_scale11.csv",row.names = F)
write.csv(data_imp12, file = "data_imp_scale12.csv",row.names = F)
write.csv(data_imp13, file = "data_imp_scale13.csv",row.names = F)
write.csv(data_imp14, file = "data_imp_scale14.csv",row.names = F)
write.csv(data_imp15, file = "data_imp_scale15.csv",row.names = F)
write.csv(data_imp16, file = "data_imp_scale16.csv",row.names = F)
write.csv(data_imp17, file = "data_imp_scale17.csv",row.names = F)
write.csv(data_imp18, file = "data_imp_scale18.csv",row.names = F)
write.csv(data_imp19, file = "data_imp_scale19.csv",row.names = F)
write.csv(data_imp20, file = "data_imp_scale20.csv",row.names = F)

