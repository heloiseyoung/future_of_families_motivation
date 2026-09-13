### Fragile Families cohort imputation script
# Pierre O Jacquet 2026
# This performs multiple imputation for missing data using the mice package and stores
# the created 20 imputation sets in csv files

#### Call packages and read data

library(tidyverse)
library(haven)
library(readr)
library(mice)
library(future)
library(future.apply)
library(progressr)

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")) {
  setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")) {
  setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
} else {
  stop("Working directory not found.")
}

mydf.imp <- read_csv("mydf.select.csv")
mydf.imp <- readr::read_csv("mydf.select.csv", show_col_types = FALSE)
mydf.imp <- as.data.frame(mydf.imp)

# Quick diagnostics of missing cases and response modalities
sum(mydf.imp$attr == 0)        # Nr of subjects completing all waves
mean(mydf.imp$attr)            # Attrition rate
sum(complete.cases(mydf.imp))  # Nr of subjects without any missing data
var_unique <- data.frame(sapply(mydf.imp, function(x) length(unique(x[!is.na(x)]))))

missing_summary <- mydf.imp %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent.NA"
  ) %>%
  arrange(desc(percent.NA))

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")) {
  setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")) {
  setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/RI-CLPM")
} else {
  stop("Working directory not found.")
}

missing_summary_table <- "percentages_missing_cases_by_indicator.csv"
write.csv2(missing_summary, missing_summary_table, row.names = FALSE)


# Multiple imputation

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")) {
  setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")) {
  setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
} else {
  stop("Working directory not found.")
}

bin_cols <- c('police.off.school','secure.off.school','arms.home',
              'mom_depress_1y','mom_depress_3y','mom_depress_5y','mom_depress_ch_3y','mom_depress_ch_5y')

ord_cols <- c('insecure.school','trouble.students',
              'school.bullying.1','school.bullying.2','school.bullying.3','school.bullying.4','school.bullying.5','school.bullying.6',
              'neighborhood.safety.1','neighborhood.safety.2',
              'family.safety.1','family.safety.2','family.safety.3','family.safety.4',
              'arrang_3y','arrang_5y','bedtime_3y','bedtime_5y','bed_routine_3y','bed_routine_5y',
              'sep_3y','sep_5y','move_3y','move_5y','jobs_3y','jobs_5y')

cont_cols <- c('act_3y','act_5y','toys_3y','toys_5y','interact_3y','interact_5y',
               'phys_agg_3y','phys_agg_5y','psych_agg_3y','psych_agg_5y','violence_3y','violence_5y',
               'chaos_9y',
               'ses_15y',
               'Adhd_3y','Adhd_5y','Adhd_9y','Adhd_15y',
               'Int_3y','Int_5y','Int_9y','Int_15y',
               'Ext_3y','Ext_5y','Ext_9y','Ext_15y')

imp_cols <- unique(c(bin_cols, ord_cols, cont_cols))
imp_cols <- intersect(imp_cols, colnames(mydf.imp))  # sécurité

mydf.imp[bin_cols] <- lapply(mydf.imp[bin_cols], as.factor) 
mydf.imp[ord_cols] <- lapply(mydf.imp[ord_cols], as.ordered) 
mydf.imp[cont_cols] <- lapply(mydf.imp[cont_cols], as.numeric)

# methods par défaut de mice (adaptées aux types), puis on interdit le reste
meth <- make.method(mydf.imp)
meth[bin_cols]  <- "logreg"
meth[ord_cols]  <- "cart"
meth[cont_cols] <- "pmm"

predmat <- matrix(1,nrow = length(mydf.imp),ncol = length(mydf.imp))
wheremat <- is.na(mydf.imp)
wheremat[,c(1,2)] <- 0 # Don't impute id number, sex, depression criteria, LHS variables and the CBCL
predmat[,c(1,2)] <- 0 # Don't use id number for imputation

imp <- mice(
  mydf.imp,
  m = 20,
  predictorMatrix = predmat, 
  where = wheremat,
  method = meth,          
  seed = 123,
  printFlag = TRUE
)

imp$method             # default methods: pmm for numeric, logreg for binary, polr for ordered

before  <- colSums(is.na(mydf.imp))
after  <- colSums(is.na(complete(imp, 1)))
diff <- before - after

cbind(before, after, diff)

# imp.df <- complete(imp, 1)      # data.frame

# data.imp <- cbind(mydf.no.imp,imp.df[,c(37:86)])

# Select and write imputed data

for (i in 1:20) {
  assign(paste("data_imp",i,sep=""), complete(imp,i))
}

write.csv(data_imp1, file = "data_imp1.csv",row.names = F)
write.csv(data_imp2, file = "data_imp2.csv",row.names = F)
write.csv(data_imp3, file = "data_imp3.csv",row.names = F)
write.csv(data_imp4, file = "data_imp4.csv",row.names = F)
write.csv(data_imp5, file = "data_imp5.csv",row.names = F)
write.csv(data_imp6, file = "data_imp6.csv",row.names = F)
write.csv(data_imp7, file = "data_imp7.csv",row.names = F)
write.csv(data_imp8, file = "data_imp8.csv",row.names = F)
write.csv(data_imp9, file = "data_imp9.csv",row.names = F)
write.csv(data_imp10, file = "data_imp10.csv",row.names = F)
write.csv(data_imp11, file = "data_imp11.csv",row.names = F)
write.csv(data_imp12, file = "data_imp12.csv",row.names = F)
write.csv(data_imp13, file = "data_imp13.csv",row.names = F)
write.csv(data_imp14, file = "data_imp14.csv",row.names = F)
write.csv(data_imp15, file = "data_imp15.csv",row.names = F)
write.csv(data_imp16, file = "data_imp16.csv",row.names = F)
write.csv(data_imp17, file = "data_imp17.csv",row.names = F)
write.csv(data_imp18, file = "data_imp18.csv",row.names = F)
write.csv(data_imp19, file = "data_imp19.csv",row.names = F)
write.csv(data_imp20, file = "data_imp20.csv",row.names = F)
