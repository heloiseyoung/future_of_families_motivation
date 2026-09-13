### Fragile Families cohort imputation script
# Pierre O Jacquet 2026
# This performs diagnostic of missingness patterns

#### Call packages and read data





setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")

mydf.missing.diag <- read_csv("mydf.select.csv")
mydf.missing.diag <- readr::read_csv("mydf.select.csv", show_col_types = FALSE)
mydf.missing.diag <- as.data.frame(mydf.missing.diag)

mydf.missing.diag <- mydf.imp[,-c(30:46,77)]

library(naniar)

var.bloc.1 <- mydf.missing.diag |>
  dplyr::select(
    sex,
    age.1st.date,
    num.date,
    age.1st.sex,
    num.sex,
    age.1st.cig,
    freq.smoke.month,
    quant.smoke.day,
    age.1st.drank,
    freq.alc.month,
    quant.alc.month,
    freq.alc.year,
    quant.alc.year,
    quant2.alc.year,
    freq.drunk
    )

var.bloc.2 <- mydf.missing.diag |>
  dplyr::select(
    Adhd_3y,
    Adhd_5y,
    Adhd_9y,
    Adhd_15y,
    Int_3y,
    Int_5y,     
    Int_9y,
    Int_15y,
    Ext_3y,
    Ext_5y,
    Ext_9y,
    Ext_15y
  )

var.bloc.3 <- mydf.missing.diag |>
  dplyr::select(
    act_3y,
    act_5y,
    toys_3y,
    toys_5y,
    interact_3y,
    interact_5y,
    psych_agg_3y,
    psych_agg_5y,
    phys_agg_3y,
    phys_agg_5y,
    violence_3y,
    violence_5y,
    arrang_3y,
    arrang_5y,
    bedtime_3y,
    bedtime_5y,
    bed_routine_3y,
    bed_routine_5y,
    chaos_9y,
    sep_3y,
    sep_5y,
    move_3y,
    move_5y,
    jobs_3y,
    jobs_5y,
    mom_depress_1y,
    mom_depress_3y,
    mom_depress_5y,
    mom_depress_ch_3y,
    mom_depress_ch_5y
  )

mcar_test(var.bloc.1)
mcar_test(var.bloc.2)
mcar_test(var.bloc.3)

mydf.missing.diag$miss_Adhd15 <- as.integer(is.na(mydf.missing.diag$Adhd_15y))
mydf.missing.diag$miss_Int15  <- as.integer(is.na(mydf.missing.diag$Int_15y))
mydf.missing.diag$miss_Ext15  <- as.integer(is.na(mydf.missing.diag$Ext_15y))

mydf.missing.diag$miss_age.1st.date  <- as.integer(is.na(mydf.missing.diag$age.1st.date))
mydf.missing.diag$miss_num.date  <- as.integer(is.na(mydf.missing.diag$num.date))
mydf.missing.diag$miss_age.1st.sex  <- as.integer(is.na(mydf.missing.diag$age.1st.sex))
mydf.missing.diag$miss_num.sex  <- as.integer(is.na(mydf.missing.diag$num.sex))

mydf.missing.diag$miss_age.1st.cig  <- as.integer(is.na(mydf.missing.diag$age.1st.cig))
mydf.missing.diag$miss_freq.smoke.month  <- as.integer(is.na(mydf.missing.diag$freq.smoke.month))
mydf.missing.diag$miss_age.1st.drank  <- as.integer(is.na(mydf.missing.diag$age.1st.drank))
mydf.missing.diag$miss_freq.alc.month  <- as.integer(is.na(mydf.missing.diag$freq.alc.month))


glm(miss_Adhd15 ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Int15  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Ext15  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.date ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.date  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.sex  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.sex  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.cig ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.smoke.month  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.drank  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.alc.month  ~ Adhd_3y + Int_3y + Ext_3y + sex, family = binomial, data = mydf.missing.diag)


glm(miss_Adhd15 ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Int15  ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Ext15  ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.date ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.date     ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.sex  ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.sex      ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.cig      ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.smoke.month ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.drank    ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.alc.month   ~ Adhd_5y + Int_5y + Ext_5y + sex, family = binomial, data = mydf.missing.diag)


glm(miss_Adhd15 ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Int15  ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Ext15  ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.date ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.date     ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.sex  ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.sex      ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.cig      ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.smoke.month ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.drank    ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.alc.month   ~ Adhd_9y + Int_9y + Ext_9y + sex, family = binomial, data = mydf.missing.diag)


adv_3y <- with(mydf.missing.diag,
               act_3y + toys_3y + interact_3y + psych_agg_3y +
                 phys_agg_3y + violence_3y + arrang_3y + bedtime_3y +
                 bed_routine_3y + sep_3y + move_3y + jobs_3y +
                 mom_depress_3y + mom_depress_ch_3y)

adv_5y <- with(mydf.missing.diag,
               act_5y + toys_5y + interact_5y + psych_agg_5y +
                 phys_agg_5y + violence_5y + arrang_5y + bedtime_5y +
                 bed_routine_5y + sep_5y + move_5y + jobs_5y +
                 mom_depress_5y + mom_depress_ch_5y)

glm(miss_Adhd15 ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Adhd15 ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Int15  ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Int15  ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Ext15  ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_Ext15  ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.date ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.date ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.date ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.date ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.sex ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.sex ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.sex ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_num.sex ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)

glm(miss_age.1st.cig ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.cig ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.smoke.month ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.smoke.month ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.drank ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_age.1st.drank ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.alc.month ~ adv_3y + sex, family = binomial, data = mydf.missing.diag)
glm(miss_freq.alc.month ~ adv_5y + sex, family = binomial, data = mydf.missing.diag)
