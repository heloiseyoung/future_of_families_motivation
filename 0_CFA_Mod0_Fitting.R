library(lavaan)
library(lavaan.mi)
library(dplyr)
library(tibble)
library(readr)
library(purrr)
library(psych)

############################################################
################### 0. Working directory ###################
############################################################

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/data")) {
  data_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/data"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/data")) {
  data_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/data"
} else {
  stop("Data directory not found.")
}

j=1
for(i in 1:20){
  assign(paste("data_imp_scale",j,sep= "") ,read.csv(paste("data_imp_scale", i,".csv",sep=""), sep=",",header=T))
  j=j+1
}

data_SEM<-list(data_imp_scale1,data_imp_scale2,data_imp_scale3,data_imp_scale4,data_imp_scale5,
               data_imp_scale6,data_imp_scale7,data_imp_scale8,data_imp_scale9,data_imp_scale10,
               data_imp_scale11,data_imp_scale12,data_imp_scale13,data_imp_scale14,data_imp_scale15,
               data_imp_scale16,data_imp_scale17,data_imp_scale18,data_imp_scale19,data_imp_scale20)

############################################################
############ 1. CFA model for adolescent outcomes ##########
############################################################

cfa.outcomes <- '

  risky.health.behaviour.lt =~ age.1st.cig.z + freq.smoke.month.z + age.1st.drank.z + freq.alc.month.z

  mating.effort.lt =~ age.1st.date.z + num.date.z + age.1st.sex.z + num.sex.z

  age.1st.cig.z ~~ freq.smoke.month.z
  age.1st.drank.z ~~ freq.alc.month.z
  
  age.1st.date.z ~~ num.date.z
  age.1st.sex.z ~~ num.sex.z
  
'

############################################################
############ 2. Fit CFA on imputed datasets ################
############################################################

fit.cfa.outcomes <- lavaan.mi(
  model     = cfa.outcomes,
  data      = data_SEM,
  cmd       = 'cfa',
  group     = 'sex',
  std.lv    = TRUE,
  estimator = "MLR"
)

summary.fit.cfa.outcomes <- summary(
  fit.cfa.outcomes,
  ci = TRUE,
  standardized = TRUE,
  rsquare = TRUE,
  fmi = FALSE
)

summary.pe.cfa.outcomes <- data.frame(summary.fit.cfa.outcomes$pe)
fit.indices.cfa.outcomes <- data.frame(fitMeasures(fit.cfa.outcomes))


if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_0/Model Fit_Tables")) {
  table_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_0/Model Fit_Tables"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_0/Model Fit_Tables")) {
  table_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_0/Model Fit_Tables"
} else {
  stop("Results_Tables directory not found.")
}

write_csv(
  summary.pe.cfa.outcomes,
  file.path(table_dir, "summary_pe_cfa_outcomes.csv")
)

write_csv(
  fit.indices.cfa.outcomes,
  file.path(table_dir, "fit_indices_cfa_outcomes.csv")
)

message("Tables saved in: ", table_dir)