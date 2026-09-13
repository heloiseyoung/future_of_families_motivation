#### Call packages and read data

library(tidyverse)
library(car)
library(haven)
library(readr)

setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")

# mydf.raw = read.csv("mydf.raw.csv", header = T, sep = ",")
mydf.rec = read.csv("mydf.raw.csv", header = T, sep = ",")


########################
### Mate acquisition ###
########################

# Dates
mydf.rec$k6f4[mydf.rec$k6f4 == 2] <- 0                             
mydf.rec <- mydf.rec %>%
  mutate(across(k6f4, ~ replace(., . < 0, NA)))      
mydf.rec$ever.dated <- mydf.rec$k6f4

mydf.rec <- mydf.rec %>%
  mutate(across(k6f5, ~ replace(., . < 0, NA)))     
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6f5 = dplyr::case_when(
      !is.na(ever.dated) & k6f5 < 13 ~ 3L,
      !is.na(ever.dated) & k6f5 >= 13 & k6f5 <= 14 ~ 2L,
      !is.na(ever.dated) & k6f5 > 14 ~ 1L,
      !is.na(ever.dated) & is.na(k6f5) ~ 0L,
      is.na(ever.dated) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  )
mydf.rec$age.1st.date <- mydf.rec$k6f5

mydf.rec <- mydf.rec %>%
  mutate(across(k6f6, ~ replace(., . < 0, NA)))     
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6f6 = dplyr::case_when(
      !is.na(age.1st.date) & age.1st.date == 0 ~ 0L,
      TRUE ~ k6f6  
    ) |> as.integer()
  )
mydf.rec$num.date <- mydf.rec$k6f6

# Sexual intercourse
mydf.rec$k6f26[mydf.rec$k6f26 == 2] <- 0                             
mydf.rec <- mydf.rec %>%
  mutate(across(k6f26, ~ replace(., . < 0, NA)))      
mydf.rec$ever.sex <- mydf.rec$k6f26

mydf.rec <- mydf.rec %>%
  mutate(across(k6f31, ~ replace(., . < 0, NA)))     
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6f31 = dplyr::case_when(
      !is.na(ever.sex) & k6f31 < 13 ~ 3L,
      !is.na(ever.sex) & k6f31 >= 13 & k6f31 <= 14 ~ 2L,
      !is.na(ever.sex) & k6f31 > 14 ~ 1L,
      !is.na(ever.sex) & is.na(k6f31) ~ 0L,
      is.na(ever.sex) ~ NA_integer_,
      TRUE ~ NA_integer_
      )
  )
mydf.rec$age.1st.sex <- mydf.rec$k6f31

mydf.rec <- mydf.rec %>%
  mutate(across(k6f35, ~ replace(., . < 0, NA)))     
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6f35 = dplyr::case_when(
      !is.na(age.1st.sex) & age.1st.sex == 0 ~ 0L,
      TRUE ~ k6f35 
    ) |> as.integer()
  )
mydf.rec$num.sex <- mydf.rec$k6f35

### Health efforts
# Tobacco
mydf.rec$k6d40[mydf.rec$k6d40 == 2] <- 0                             
mydf.rec <- mydf.rec %>%
  mutate(across(k6d40, ~ replace(., . < 0, NA)))      
mydf.rec$ever.smoked <- mydf.rec$k6d40

mydf.rec <- mydf.rec %>%
  mutate(across(k6d41, ~ replace(., . < 0, NA)))     
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d41 = dplyr::case_when(
      !is.na(ever.smoked) & k6d41 < 13 ~ 3L,
      !is.na(ever.smoked) & k6d41 >= 13 & k6d41 <= 14 ~ 2L,
      !is.na(ever.smoked) & k6d41 > 14 ~ 1L,
      !is.na(ever.smoked) & is.na(k6d41) ~ 0L,
      TRUE ~ k6d41  
    ) |> as.integer()
  )
mydf.rec$age.1st.cig <- mydf.rec$k6d41

mydf.rec <- mydf.rec %>%
  mutate(across(k6d42, ~ replace(., . < 0, NA)))     
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d42 = dplyr::case_when(
      !is.na(age.1st.cig) & age.1st.cig == 0 ~ 0L,
      TRUE ~ k6d42  
    ) |> as.integer()
  )
mydf.rec$freq.smoke.month <- mydf.rec$k6d42

mydf.rec <- mydf.rec %>%
  mutate(across(k6d43, ~ replace(., . < 0, NA)))     
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d43 = dplyr::case_when(
      freq.smoke.month <= 1 ~ 0L,
      TRUE ~ k6d43  
    ) |> as.integer()
  )
mydf.rec$quant.smoke.day <- mydf.rec$k6d43

# Alcohol
mydf.rec$k6d48[mydf.rec$k6d48 == 2] <- 0                             
mydf.rec <- mydf.rec %>%
  mutate(across(k6d48, ~ replace(., . < 0, NA)))      
mydf.rec$ever.drank <- mydf.rec$k6d48

mydf.rec <- mydf.rec %>%
  mutate(across(k6d49, ~ replace(., . < 0, NA)))      
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d49 = dplyr::case_when(
      !is.na(ever.drank) & k6d49 < 13 ~ 3L,
      !is.na(ever.drank) & k6d49 >= 13 & k6d49 <= 14 ~ 2L,
      !is.na(ever.drank) & k6d49 > 14 ~ 1L,
      !is.na(ever.drank) & is.na(k6d49) ~ 0L,
      TRUE ~ k6d49  
    ) |> as.integer()
  )
mydf.rec$age.1st.drank <- mydf.rec$k6d49

mydf.rec <- mydf.rec %>%
  mutate(across(k6d50, ~ replace(., . < 0, NA))) 
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d50 = dplyr::case_when(
      !is.na(age.1st.drank) & age.1st.drank == 0 ~ 0L,
      TRUE ~ k6d50  
    ) |> as.integer()
  )
mydf.rec$freq.alc.month <- mydf.rec$k6d50

mydf.rec <- mydf.rec %>%
  mutate(across(k6d51, ~ replace(., . < 0, NA))) 
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d51 = dplyr::case_when(
      freq.alc.month <= 1 ~ 0L,
      TRUE ~ k6d51 
    ) |> as.integer()
  )
mydf.rec$quant.alc.month <- mydf.rec$k6d51

mydf.rec <- mydf.rec %>%
  mutate(across(k6d52, ~ replace(., . < 0, NA))) 
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d52 = dplyr::case_when(
      !is.na(age.1st.drank) & age.1st.drank == 0 ~ 0L,
      TRUE ~ k6d52  
    ) |> as.integer()
  )
mydf.rec$freq.alc.year <- mydf.rec$k6d52

mydf.rec <- mydf.rec %>%
  mutate(across(k6d53, ~ replace(., . < 0, NA))) 
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d53 = dplyr::case_when(
      freq.alc.year <= 1 ~ 0L,
      TRUE ~ freq.alc.year  
    ) |> as.integer()
  )
mydf.rec$quant.alc.year <- mydf.rec$k6d53

mydf.rec <- mydf.rec %>%
  mutate(across(k6d54, ~ replace(., . < 0, NA))) 
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d54 = dplyr::case_when(
      quant.alc.year == 0 ~ 1L,
      TRUE ~ quant.alc.year  
    ) |> as.integer()
  )
mydf.rec$quant2.alc.year <- mydf.rec$k6d54

mydf.rec <- mydf.rec %>%
  mutate(across(k6d55, ~ replace(., . < 0, NA))) 
mydf.rec <- mydf.rec |>
  dplyr::mutate(
    k6d55 = dplyr::case_when(
      freq.alc.year == 0 ~ 1L,
      TRUE ~ k6d55  
    ) |> as.integer()
  )
mydf.rec$freq.drunk <- mydf.rec$k6d55


######################
### Current threat ###
######################

        # School
mydf.rec <- mydf.rec %>%
  mutate(across(k6b1d, ~ replace(., . < 0, NA)))
mydf.rec <- mydf.rec %>%
  mutate(across(k6b1d, ~ replace(., . == 7, NA)))
mydf.rec <- mydf.rec %>%
  mutate(across(k6b1d, ~ 5 - .))
mydf.rec$insecure.school <- mydf.rec$k6b1d # scores 1:4

mydf.rec$k6b2[mydf.rec$k6b2 == 2] <- 0                             
mydf.rec <- mydf.rec %>%
  mutate(across(k6b2, ~ replace(., . < 0, NA)))    
mydf.rec <- mydf.rec %>%
  mutate(across(k6b2, ~ replace(., . == 7, NA)))
mydf.rec$police.off.school <- mydf.rec$k6b2 # scores 0:1

mydf.rec$k6b3[mydf.rec$k6b3 == 2] <- 0                             
mydf.rec <- mydf.rec %>%
  mutate(across(k6b3, ~ replace(., . < 0, NA)))    
mydf.rec <- mydf.rec %>%
  mutate(across(k6b3, ~ replace(., . == 7, NA)))
mydf.rec$secure.off.school <- mydf.rec$k6b3 # scores 0:1

mydf.rec <- mydf.rec %>%
  mutate(across(k6b21d, ~ replace(., . < 0, NA)))
mydf.rec <- mydf.rec %>%
  mutate(across(k6b21d, ~ replace(., . == 7, NA)))
mydf.rec$trouble.students <- mydf.rec$k6b21d # scores 1:3

mydf.rec <- mydf.rec %>%
  mutate(across(k6b32a:k6b32f, ~ replace(., . < 0, NA))) %>%
  mutate(across(k6b32a:k6b32f, ~ replace(., . == 7, NA))) # scores 0:4
mydf.rec$school.bullying.1 <- mydf.rec$k6b32a
mydf.rec$school.bullying.2 <- mydf.rec$k6b32b
mydf.rec$school.bullying.3 <- mydf.rec$k6b32c
mydf.rec$school.bullying.4 <- mydf.rec$k6b32d
mydf.rec$school.bullying.5 <- mydf.rec$k6b32e
mydf.rec$school.bullying.6 <- mydf.rec$k6b32f

        # Neighborhood
mydf.rec <- mydf.rec %>%
  mutate(across(k6e4b:k6e4c, ~ replace(., . < 0, NA))) %>%
  mutate(across(k6e4b:k6e4c, ~ 5 - .)) # scores 1:4
mydf.rec$neighborhood.safety.1 <- mydf.rec$k6e4b
mydf.rec$neighborhood.safety.2 <- mydf.rec$k6e4c

        # Family
mydf.rec$k6c5[mydf.rec$k6c5 == 2] <- 0                             
mydf.rec <- mydf.rec %>%
  mutate(across(k6c5, ~ replace(., . < 0, NA)))
mydf.rec$arms.home <- mydf.rec$k6c5  # scores 0:1

mydf.rec <- mydf.rec %>%
  mutate(across(k6c8, ~ replace(., . < 0, NA)))
mydf.rec <- mydf.rec %>%
  mutate(across(k6c8, ~ 4 - .))
mydf.rec$family.safety.1 <- mydf.rec$k6c8 # scores 1:3

mydf.rec <- mydf.rec %>%
  mutate(across(k6c9b, ~ replace(., . < 0, NA)))
mydf.rec$family.safety.2 <- mydf.rec$k6c9b # scores 1:3

mydf.rec <- mydf.rec %>%
  mutate(across(k6c9c, ~ replace(., . < 0, NA)))
mydf.rec$family.safety.3 <- mydf.rec$k6c9c # scores 1:3

mydf.rec <- mydf.rec %>%
  mutate(across(k6c9d, ~ replace(., . < 0, NA)))
mydf.rec$family.safety.4 <- mydf.rec$k6c9d # scores 1:3


#########################  
### Early deprivation ###
#########################

  # Activities 3 y
  # min = 0   max = 13
mydf.rec <- mydf.rec %>%
  mutate(across(m3b4a:m3b4m, ~ replace(., . < 0, NA)))
mydf.rec <- mydf.rec %>%
  mutate(across(m3b4a:m3b4m, ~ ifelse(. > mean(., na.rm = TRUE), 0, 1)))
mydf.rec$act.3y.1 <- mydf.rec$m3b4a
mydf.rec$act.3y.2 <- mydf.rec$m3b4b
mydf.rec$act.3y.3 <- mydf.rec$m3b4c
mydf.rec$act.3y.4 <- mydf.rec$m3b4d
mydf.rec$act.3y.5 <- mydf.rec$m3b4e
mydf.rec$act.3y.6 <- mydf.rec$m3b4f
mydf.rec$act.3y.7 <- mydf.rec$m3b4g
mydf.rec$act.3y.8 <- mydf.rec$m3b4h
mydf.rec$act.3y.9 <- mydf.rec$m3b4i
mydf.rec$act.3y.10 <- mydf.rec$m3b4j
mydf.rec$act.3y.11 <- mydf.rec$m3b4k
mydf.rec$act.3y.12 <- mydf.rec$m3b4l
mydf.rec$act.3y.13 <- mydf.rec$m3b4m
mydf.rec <- mydf.rec %>%
  mutate(act_3y = rowSums(across(act.3y.1:act.3y.13), na.rm = FALSE)) 
mydf.rec <- mydf.rec %>%
  mutate(act_3y = as.numeric(scale(act_3y)))

  # Activities 5 y
  # min = 0   max = 8
mydf.rec <- mydf.rec %>%
  mutate(across(m4b4a1:m4b4a8, ~ replace(., . < 0, NA))) 
mydf.rec <- mydf.rec %>%
  mutate(across(m4b4a1:m4b4a8, ~ ifelse(. > mean(., na.rm = TRUE), 0, 1)))
mydf.rec$act.5y.1 <- mydf.rec$m4b4a1
mydf.rec$act.5y.2 <- mydf.rec$m4b4a2
mydf.rec$act.5y.3 <- mydf.rec$m4b4a3
mydf.rec$act.5y.4 <- mydf.rec$m4b4a4
mydf.rec$act.5y.5 <- mydf.rec$m4b4a5
mydf.rec$act.5y.6 <- mydf.rec$m4b4a6
mydf.rec$act.5y.7 <- mydf.rec$m4b4a7
mydf.rec$act.5y.8 <- mydf.rec$m4b4a8
mydf.rec <- mydf.rec %>%
  mutate(act_5y = rowSums(across(act.5y.1:act.5y.8), na.rm = FALSE)) 
mydf.rec <- mydf.rec %>%
  mutate(act_5y = as.numeric(scale(act_5y)))

  # Toys and books 3 y
  # min = 8   max = 32
mydf.rec <- mydf.rec %>%
  mutate(across(p3c1a:p3c1h, ~ replace(., . < 0, NA))) %>%
  mutate(across(p3c1a:p3c1h, ~ 5 - .))
mydf.rec$toys.3y.1 <- mydf.rec$p3c1a
mydf.rec$toys.3y.2 <- mydf.rec$p3c1b
mydf.rec$toys.3y.3 <- mydf.rec$p3c1c
mydf.rec$toys.3y.4 <- mydf.rec$p3c1d
mydf.rec$toys.3y.5 <- mydf.rec$p3c1e
mydf.rec$toys.3y.6 <- mydf.rec$p3c1f
mydf.rec$toys.3y.7 <- mydf.rec$p3c1g
mydf.rec$toys.3y.8 <- mydf.rec$p3c1h
mydf.rec <- mydf.rec %>%
  mutate(toys_3y = rowSums(across(toys.3y.1:toys.3y.8), na.rm = FALSE))
mydf.rec <- mydf.rec %>%
  mutate(toys_3y = as.numeric(scale(toys_3y)))

  # Toys and books 5 y
  # min = 19    max = 76
mydf.rec <- mydf.rec %>%
  mutate(across(p4c1a:p4c1h, ~ replace(., . < 0, NA))) %>%
  mutate(across(p4c1a:p4c1h, ~ 5 - .))
mydf.rec$toys.5y.1 <- mydf.rec$p4c1a
mydf.rec$toys.5y.2 <- mydf.rec$p4c1b
mydf.rec$toys.5y.3 <- mydf.rec$p4c1c
mydf.rec$toys.5y.4 <- mydf.rec$p4c1d
mydf.rec$toys.5y.5 <- mydf.rec$p4c1e
mydf.rec$toys.5y.6 <- mydf.rec$p4c1f
mydf.rec$toys.5y.7 <- mydf.rec$p4c1g
mydf.rec$toys.5y.8 <- mydf.rec$p4c1h
mydf.rec <- mydf.rec %>%
  mutate(toys_5y = rowSums(across(toys.5y.1:toys.5y.8), na.rm = FALSE))
mydf.rec <- mydf.rec %>%
  mutate(toys_5y = as.numeric(scale(toys_5y)))

  # Parent-child interactions 3 y
  # min = 0   max = 16
mydf.rec <- mydf.rec %>%
  mutate(across(o3t1:o3t16, ~ replace(., . < 0, NA)))
mydf.rec <- mydf.rec %>%
  mutate(across(c(o3t1:o3t16), ~ 1 - .))
mydf.rec$interact.3y.1 <- mydf.rec$o3t1
mydf.rec$interact.3y.2 <- mydf.rec$o3t2
mydf.rec$interact.3y.3 <- mydf.rec$o3t3
mydf.rec$interact.3y.4 <- mydf.rec$o3t7
mydf.rec$interact.3y.5 <- mydf.rec$o3t8
mydf.rec$interact.3y.6 <- mydf.rec$o3t9
mydf.rec$interact.3y.7 <- mydf.rec$o3t10
mydf.rec$interact.3y.8 <- mydf.rec$o3t11
mydf.rec$interact.3y.9 <- mydf.rec$o3t12
mydf.rec$interact.3y.10 <- mydf.rec$o3t13
mydf.rec$interact.3y.11 <- mydf.rec$o3t14
mydf.rec$interact.3y.12 <- mydf.rec$o3t15
mydf.rec$interact.3y.13 <- mydf.rec$o3t16
mydf.rec <- mydf.rec %>%
  mutate(interact_3y = rowSums(across(interact.3y.1:interact.3y.13), na.rm = FALSE))
mydf.rec <- mydf.rec %>%
  mutate(interact_3y = as.numeric(scale(interact_3y)))

  # Parent-child interactions 5 y
  # min = 0   max = 16
mydf.rec <- mydf.rec %>%
  mutate(across(o4t1:o4t13, ~ replace(., . < 0, NA)))
mydf.rec <- mydf.rec %>%
  mutate(across(o4t1:o4t8, ~ 1 - .))
mydf.rec$interact.5y.1 <- mydf.rec$o4t1
mydf.rec$interact.5y.2 <- mydf.rec$o4t2
mydf.rec$interact.5y.3 <- mydf.rec$o4t3
mydf.rec$interact.5y.4 <- mydf.rec$o4t4
mydf.rec$interact.5y.5 <- mydf.rec$o4t5
mydf.rec$interact.5y.6 <- mydf.rec$o4t6
mydf.rec$interact.5y.7 <- mydf.rec$o4t7
mydf.rec$interact.5y.8 <- mydf.rec$o4t8
mydf.rec$interact.5y.9 <- mydf.rec$o4t9
mydf.rec$interact.5y.10 <- mydf.rec$o4t11
mydf.rec$interact.5y.11 <- mydf.rec$o4t12
mydf.rec$interact.5y.12 <- mydf.rec$o4t13
mydf.rec <- mydf.rec %>%
  mutate(interact_5y = rowSums(across(interact.5y.1:interact.5y.12), na.rm = FALSE))
mydf.rec <- mydf.rec %>%
  mutate(interact_5y = as.numeric(scale(interact_5y)))

##############
### Threat ###
##############
  
  # Psych aggr 3 y
  # min = 0   max = 60
mydf.rec <- mydf.rec %>%
  mutate(across(p3j1:p3j19, ~ ifelse(. == 7, 0, .))) %>%   
  mutate(across(p3j1:p3j19, ~ replace(., . < 0, NA)))
mydf.rec$psych.agg.3y.1 <- mydf.rec$p3j6
mydf.rec$psych.agg.3y.2 <- mydf.rec$p3j8
mydf.rec$psych.agg.3y.3 <- mydf.rec$p3j9
mydf.rec$psych.agg.3y.4 <- mydf.rec$p3j10
mydf.rec$psych.agg.3y.5 <- mydf.rec$p3j14
mydf.rec <- mydf.rec %>%
  mutate(psych_agg_3y = rowSums(across(psych.agg.3y.1:psych.agg.3y.5), na.rm = FALSE))
 
  # Psych aggr 5 y
  # min = 0   max = 60
mydf.rec <- mydf.rec %>%
  mutate(across(p4g1:p4g19, ~ ifelse(. == 7, 0, .))) %>%       
  mutate(across(p4g1:p4g19, ~ replace(., . < 0, NA)))      
mydf.rec$psych.agg.5y.1 <- mydf.rec$p4g6
mydf.rec$psych.agg.5y.2 <- mydf.rec$p4g8
mydf.rec$psych.agg.5y.3 <- mydf.rec$p4g9
mydf.rec$psych.agg.5y.4 <- mydf.rec$p4g10
mydf.rec$psych.agg.5y.5 <- mydf.rec$p4g14
mydf.rec <- mydf.rec %>%
  mutate(psych_agg_5y = rowSums(across(psych.agg.5y.1:psych.agg.5y.5), na.rm = FALSE))  

  # Phys aggr 3 y
  # min = 0   max = 60
mydf.rec <- mydf.rec %>%
  mutate(across(c(p3j3,p3j4,p3j7,p3j11,p3j13), ~ replace(., . < 0, NA)))
mydf.rec$phys.agg.3y.1 <- mydf.rec$p3j3
mydf.rec$phys.agg.3y.2 <- mydf.rec$p3j4
mydf.rec$phys.agg.3y.3 <- mydf.rec$p3j7
mydf.rec$phys.agg.3y.4 <- mydf.rec$p3j11
mydf.rec$phys.agg.3y.5 <- mydf.rec$p3j13
mydf.rec <- mydf.rec %>%
  mutate(phys_agg_3y = rowSums(across(phys.agg.3y.1:phys.agg.3y.5), na.rm = FALSE)) 

  # Phys aggr 5 y
  # min = 0   max = 60
mydf.rec <- mydf.rec %>%
  mutate(across(c(p4g3,p4g4,p4g7,p4g11,p4g13), ~ replace(., . < 0, NA)))
mydf.rec$phys.agg.5y.1 <- mydf.rec$p4g3
mydf.rec$phys.agg.5y.2 <- mydf.rec$p4g4
mydf.rec$phys.agg.5y.3 <- mydf.rec$p4g7
mydf.rec$phys.agg.5y.4 <- mydf.rec$p4g11
mydf.rec$phys.agg.5y.5 <- mydf.rec$p4g13
mydf.rec <- mydf.rec %>%
  mutate(phys_agg_5y = rowSums(across(phys.agg.5y.1:phys.agg.5y.5), na.rm = FALSE))
 
  # Community violence 3 y
  # min = 0   max = 28
mydf.rec <- mydf.rec %>%
  mutate(across(p3l1:p3l7, ~ replace(., . < 0, NA)))
mydf.rec$violence.3y.1 <- mydf.rec$p3l1
mydf.rec$violence.3y.2 <- mydf.rec$p3l2
mydf.rec$violence.3y.3 <- mydf.rec$p3l3
mydf.rec$violence.3y.4 <- mydf.rec$p3l4
mydf.rec$violence.3y.5 <- mydf.rec$p3l5
mydf.rec$violence.3y.6 <- mydf.rec$p3l6
mydf.rec$violence.3y.7 <- mydf.rec$p3l7
mydf.rec <- mydf.rec %>%
  mutate(violence_3y = rowSums(across(violence.3y.1:violence.3y.7), na.rm = FALSE))

  # Community violence 5 y
  # min = 0   max = 28
mydf.rec <- mydf.rec %>%
  mutate(across(p4h1:p4h7, ~ replace(., . < 0, NA)))
mydf.rec$violence.5y.1 <- mydf.rec$p4h1
mydf.rec$violence.5y.2 <- mydf.rec$p4h2
mydf.rec$violence.5y.3 <- mydf.rec$p4h3
mydf.rec$violence.5y.4 <- mydf.rec$p4h4
mydf.rec$violence.5y.5 <- mydf.rec$p4h5
mydf.rec$violence.5y.6 <- mydf.rec$p4h6
mydf.rec$violence.5y.7 <- mydf.rec$p4h7
mydf.rec <- mydf.rec %>%
  mutate(violence_5y = rowSums(across(violence.5y.1:violence.5y.7), na.rm = FALSE))

###########################  
### Early stochasticity ###
###########################

  # Arrangements 3 y
  # min = 0   max = 3
mydf.rec$m3b7b[mydf.rec$m3b7 == 2] <- 0                             # replace 2 = yes by 0 = yes
mydf.rec <- mydf.rec %>%
  mutate(across(m3b7b, ~ replace(., . < 0, NA))) %>%       
  mutate(across(m3b7b, ~ replace(., . > 3, 3)))      
mydf.rec$arrang_3y <- mydf.rec$m3b7b

  # Arrangements 5 y
  # min = 0   max = 3
mydf.rec$m4b9b[mydf.rec$m4b8h == 1] <- 0
mydf.rec <- mydf.rec %>%
  mutate(across(m4b9b, ~ replace(., . < 0, NA))) %>%       
  mutate(across(m4b9b, ~ replace(., . > 3, 3)))      
mydf.rec$arrang_5y <- mydf.rec$m4b9b

  # Regular bedtime 3 y
  # min = 1   max = 6
mydf.rec$p3b5[mydf.rec$p3b3 == 0] <- 0
mydf.rec <- mydf.rec %>%
  mutate(across(p3b5, ~ replace(., . < 0, NA))) %>%       
  mutate(across(p3b5, ~ 6 - .))       
mydf.rec$bedtime_3y <- mydf.rec$p3b5

  # Regular bedtime 5 y
  # min = 1   max = 6
mydf.rec$p4b13[mydf.rec$p4b11 == 0] <- 0
mydf.rec <- mydf.rec %>%
  mutate(across(p4b13, ~ replace(., . < 0, NA))) %>%       
  mutate(across(p4b13, ~ 6 - .))       
mydf.rec$bedtime_5y <- mydf.rec$p4b13

  # Regular bedtime routine 3 y
  # min = 1   max = 6
mydf.rec$p3b8[mydf.rec$p3b6a == 0] <- 0
mydf.rec <- mydf.rec %>%
  mutate(across(p3b8, ~ replace(., . < 0, NA))) %>%       
  mutate(across(p3b8, ~ 6 - .))       
mydf.rec$bed_routine_3y <- mydf.rec$p3b8

  # Regular bedtime routine 5 y
  # min = 1   max = 6
mydf.rec$p4b16[mydf.rec$p4b15 == 0] <- 0
mydf.rec <- mydf.rec %>%
  mutate(across(p4b16, ~ replace(., . < 0, NA))) %>%       
  mutate(across(p4b16, ~ 6 - .))       
mydf.rec$bed_routine_5y <- mydf.rec$p4b16

  # CHAOS 9 y
  # min = 5   max = 25
mydf.rec <- mydf.rec %>%
  mutate(across(p5i22a:p5i22e, ~ replace(., . < 0, NA))) %>%
  mutate(across(c(p5i22c:p5i22e), ~ 6 - .))
mydf.rec$chaos.9y.1 <- mydf.rec$p5i22a
mydf.rec$chaos.9y.2 <- mydf.rec$p5i22b
mydf.rec$chaos.9y.3 <- mydf.rec$p5i22c
mydf.rec$chaos.9y.4 <- mydf.rec$p5i22d
mydf.rec$chaos.9y.5 <- mydf.rec$p5i22e
mydf.rec <- mydf.rec %>%
  mutate(chaos_9y = rowSums(across(chaos.9y.1:chaos.9y.5), na.rm = FALSE))

  # Mother depression 1 y (this is just for constructing the change)
  # min = 0   max = 1
mydf.rec <- mydf.rec %>%
  mutate(across(cm2md_case_lib, ~ replace(., . < 0, NA)))
mydf.rec$mom_depress_1y <- mydf.rec$cm2md_case_lib

  # Mother depression 3 y
  # min = 0   max = 1
mydf.rec <- mydf.rec %>%
  mutate(across(cm3md_case_lib, ~ replace(., . < 0, NA)))
mydf.rec$mom_depress_3y <- mydf.rec$cm3md_case_lib

  # Mother depression 5 y
  # min = 0   max = 1
mydf.rec <- mydf.rec %>%
  mutate(across(cm4md_case_lib, ~ replace(., . < 0, NA)))
mydf.rec$mom_depress_5y <- mydf.rec$cm4md_case_lib

  # Mother depression changed 3 y
  # min = 0   max = 1
mydf.rec$mom_depress_ch_3y <- ifelse(mydf.rec$mom_depress_3y == mydf.rec$mom_depress_1y,0,1)

  # Mother depression changed 5 y
  # min = 0   max = 1
mydf.rec$mom_depress_ch_5y <- ifelse(mydf.rec$mom_depress_5y == mydf.rec$mom_depress_3y,0,1)

########################
### Early volatility ###
########################

  # Separations 3 y
  # min = 0   max = 3
mydf.rec <- mydf.rec %>%
  mutate(across(m3b3, ~ replace(., . < 0, NA))) %>%
  mutate(across(m3b3, ~ replace(., . > 3, 3)))
mydf.rec$sep_3y <- mydf.rec$m3b3

  # Separations 5 y
  # min = 0   max = 3
mydf.rec <- mydf.rec %>%
  mutate(across(m4b3, ~ replace(., . < 0, NA))) %>%
  mutate(across(m4b3, ~ replace(., . > 3, 3)))
mydf.rec$sep_5y <- mydf.rec$m4b3

  # Moves 3 y
  # min = 0   max = 3
mydf.rec$m3i1a[mydf.rec$m3i1 == 2] <- 0
mydf.rec <- mydf.rec %>%
  mutate(across(m3i1a, ~ replace(., . < 0, NA))) %>%
  mutate(across(m3i1a, ~ replace(., . > 3, 3)))
mydf.rec$move_3y <- mydf.rec$m3i1a

  # Moves 5 y
  # min = 0   max = 3
mydf.rec$m4i1a[mydf.rec$m4i1 == 2] <- 0
mydf.rec <- mydf.rec %>%
  mutate(across(m4i1a, ~ replace(., . < 0, NA))) %>%
  mutate(across(m4i1a, ~ replace(., . > 3, 3)))
mydf.rec$move_5y <- mydf.rec$m4i1a

  # Jobs 3 y
  # min = 0   max = 3
mydf.rec <- mydf.rec %>%
  mutate(across(ch3emp_totjob, ~ replace(., . < 0, NA))) %>%
  mutate(across(ch3emp_totjob, ~ replace(., . > 3, 3)))
mydf.rec$jobs_3y <- mydf.rec$ch3emp_totjob

  # Jobs 5 y
  # min = 0   max = 3
mydf.rec <- mydf.rec %>%
  mutate(across(ch4emp_totjob, ~ replace(., . < 0, NA))) %>%
  mutate(across(ch4emp_totjob, ~ replace(., . > 3, 3)))
mydf.rec$jobs_5y <- mydf.rec$ch4emp_totjob


############
### CBCL ###
############

  # Recode all CBCL and social skills items

mydf.rec <- mydf.rec %>%
  mutate(across(c(p5q3a:p5q3do,p6b35:p6b68,k6d1a:k6d1l), ~ . - 1))
mydf.rec <- mydf.rec %>%
  mutate(across(c(p3m1:p3m50,m4b4b1:m4b4b19,p4l1:p4l66,p5q3a:p5q3do,p6b35:p6b68,k6d1a:k6d1l), ~ replace(., . < 0, NA)))

## CBCL ADHD

# ADHD 3y 
adhd3_cols <- c('p3m18a','p3m28a','p3m2a','p3m2b','p3m2c','p3m48')

# ADHD 5y 
adhd5_cols <- c('m4b4b1','m4b4b2','m4b4b9','m4b4b19','p4l6','p4l8','p4l24','p4l27','p4l28','p4l34','p4l35','p4l47')

# ADHD 9y
adhd9_cols <- c('p5q3a','p5q3d','p5q3g','p5q3i','p5q3l','p5q3p','p5q3an','p5q3ar','p5q3bg','p5q3bh','p5q3bw')

# ADHD 15y 
adhd15_cols <- c('p6b46','p6b47','p6b48')


## CBCL Aggressive behaviours

# Aggress 3y
aggress3_cols <- c('p3m14','p3m18','p3m21','p3m23','p3m28','p3m30','p3m33','p3m41','p3m5','p3m6','p3m7','p3m17','p3m40','p3m47','p3m49')

# Aggress 5y 
aggress5_cols <- c('m4b4b11','m4b4b12','m4b4b13','m4b4b16','p4l1','p4l10','p4l12','p4l13','p4l21','p4l33','p4l40','p4l57',
                   'p4l59','p4l62','p4l7','p4l9','p4l16','p4l2','p4l45','p4l56')

# Aggress 9y
aggress9_cols <- c('p5q3aj','p5q3bc','p5q3bn','p5q3c','p5q3cf','p5q3cg','p5q3cn','p5q3co','p5q3cq','p5q3cw','p5q3o','p5q3r',
                   'p5q3s','p5q3t','p5q3u','p5q3v','p5q3bt','p5q3cm','p5q3f','p5q3y')

# Aggress 15y 
aggress15_cols <- c('p6b35','p6b37','p6b38','p6b39','p6b41','p6b42','p6b43','p6b44','p6b45','p6b57','p6b58','p6b59')


## CBCL Rule Breaking behaviours

# Rule Break 3y
# No rule breaking subscale at that age

# Rule Break 5y 
rulebreak5_cols <- c('m4b4b7','p4l23','p4l26','p4l36','p4l39','p4l44','p4l49','p4l50','p4l54','p4l64','m4b4b5')

# Rule Break 9y
rulebreak9_cols <- c('p5q3al','p5q3ap','p5q3bi','p5q3bm','p5q3br','p5q3bz','p5q3ca','p5q3cj','p5q3cp','p5q3ct','p5q3cx',
                     'p5q3x','p5q3cy','p5q3aa','p5q3b','p5q3cr','p5q3n')

# Rule Break 15y 
rulebreak15_cols <- c('p6b49','p6b50','p6b51','p6b60','p6b61','p6b62','p6b63','p6b64','p6b67')


## CBCL Anxiety/Depression

# Anxiety/Depression 3y
anxdep3_cols <- c('p3m16','p3m19','p3m22','p3m25','p3m3','p3m32','p3m42','p3m46','p3m26','p3m37')

# Anxiety/Depression 5y 
anxdep5_cols <- c('m4b418','m4b4b14','m4b4b4','p4l17','p4l18','p4l19','p4l29','p4l43','p4l65','p4l53','p4l20','p4l5','m4b4b15')

# Anxiety/Depression 9y
anxdep9_cols <- c('p5q3ad','p5q3ae','p5q3af','p5q3ah','p5q3aq','p5q3av','p5q3ax','p5q3bq','p5q3db','p5q3m','p5q3ag','p5q3k')

# Anxiety/Depression 15y 
anxdep15_cols <- c('p6b36','p6b40','p6b52','p6b53','p6b54','p6b68')


## CBCL Withdrawn

# Withdrawn 3y
withdr3_cols <- c('p3m1','p3m2','p3m29','p3m31','p3m35','p3m36','p3m50','p3m9','p3m10','p3m11','p3m13','p3m39','p3m45')

# Withdrawn 5y 
withdr5_cols <- c('p4l25','p4l38','p4l42','p4l46','p4l61','m4b4b17','p4l52')

# Withdrawn 9y
withdr9_cols <- c('p5q3ao','p5q3bk','p5q3bo','p5q3bu','p5q3cu','p5q3da','p5q3cv','p5q3e','p5q3ch','p5q3by')

# Withdrawn 15y 
withdr15_cols <- c('p6b65','p6b66')


## CBCL Internalizing

  # Int 3y 
int3_cols <- c('p3m1','p3m2','p3m3','p3m3a','p3m9','p3m10','p3m11','p3m13',
               'p3m16','p3m19','p3m22','p3m25','p3m26','p3m29','p3m31','p3m32',
               'p3m35','p3m36','p3m37','p3m39','p3m42','p3m45','p3m46','p3m50')

int3_common_cols <- c('p3m25','p3m3a','p3m42','p3m45','p3m45')

  # Int 5y
int5_cols <- c('m4b4b4','m4b4b14','m4b4b15','m4b4b17','m4b4b18','p4l5','p4l17',
              'p4l18','p4l19','p4l20','p4l25','p4l29','p4l38','p4l42','p4l43',
              'p4l46','p4l52','p4l53','p4l61','p4l65')

int5_common_cols <- c('m4b4b14','m4b4b15','m4b4b18','m4b4b4','m4b4b4','p4l29',
                      'p4l61','p4l65')

  # Int 9y
int9_cols <- c('p5q3e','p5q3k','p5q3m','p5q3ab','p5q3ac','p5q3ad','p5q3ae',
               'p5q3af','p5q3ag','p5q3ah','p5q3ao','p5q3aq','p5q3av','p5q3ax',
               'p5q3bk','p5q3bo','p5q3bq','p5q3bu','p5q3ck','p5q3cu','p5q3cv',
               'p5q3da','p5q3db')

int9_common_cols <- c('p5q3ah','p5q3aq','p5q3aq','p5q3aq','p5q3cu','p5q3cv',
                      'p5q3db','p5q3m')

  # Int 15y
int15_cols <- c('p6b36','p6b40','p6b52','p6b53','p6b54','p6b56','p6b65','p6b66','p6b68')

int15_common_cols <- c('p6b36','p6b40','p6b52','p6b53','p6b54','p6b65','p6b66','p6b68')


## CBCL Externalizing

  # Ext 3y 
ext3_cols <- c('p3m3b','p3m5','p3m6','p3m6a','p3m6b','p3m7','p3m14','p3m17','p3m18',
               'p3m21','p3m21a','p3m23','p3m26a','p3m28','p3m30','p3m33','p3m40',
               'p3m41','p3m44','p3m47','p3m49')

ext3_common_cols <- c('p3m18','p3m26a','p3m26a','p3m3b','p3m41','p3m47','p3m6a','p3m6b','p3m7')

  # Ext 5y
ext5_cols <- c('m4b4b5','m4b4b7','m4b4b11','m4b4b12','m4b4b13','m4b4b16','p4l1',
               'p4l2','p4l7','p4l9','p4l10','p4l12','p4l13','p4l16','p4l21',
               'p4l23','p4l26','p4l33','p4l36','p4l39','p4l40','p4l44','p4l45',
               'p4l49','p4l50','p4l54','p4l56','p4l57','p4l59','p4l62','p4l64')

ext5_common_cols <- c('m4b4b11','m4b4b13','m4b4b7','p4l1','p4l12','p4l13',
                      'p4l21','p4l23','p4l26','p4l33','p4l39','p4l49','p4l50',
                      'p4l54','p4l56','p4l59','p4l59','p4l64','p4l7','p4l9')

  # Ext 9y
ext9_cols <- c('p5q3b','p5q3c','p5q3f','p5q3n','p5q3o','p5q3r','p5q3s','p5q3t','p5q3u',
              'p5q3v','p5q3x','p5q3y','p5q3aj','p5q3al','p5q3ap','p5q3bc','p5q3bi',
              'p5q3bm','p5q3bn','p5q3br','p5q3bt','p5q3bz','p5q3ca','p5q3cf','p5q3cg',
              'p5q3ci','p5q3cj','p5q3cm','p5q3cn','p5q3co','p5q3cp','p5q3cq','p5q3cr',
              'p5q3ct','p5q3cw','p5q3cx','p5q3cy')

ext9_common_cols <- c('p5q3aj','p5q3al','p5q3ap','p5q3bc','p5q3bm','p5q3bz',
                      'p5q3c','p5q3ca','p5q3cf','p5q3cj','p5q3cm','p5q3co','p5q3cq',
                      'p5q3cw','p5q3cy','p5q3o','p5q3t','p5q3u','p5q3v','p5q3x')

  # Ext 15y
ext15_cols <- c('p6b35','p6b37','p6b38','p6b39','p6b41','p6b42','p6b43','p6b44',
              'p6b45','p6b49','p6b50','p6b51','p6b57','p6b58','p6b59','p6b60',
              'p6b61','p6b62','p6b63','p6b64','p6b67')

ext15_common_cols <- c('p6b35','p6b37','p6b38','p6b39','p6b41','p6b42','p6b43','p6b44',
                'p6b45','p6b49','p6b50','p6b51','p6b57','p6b58','p6b59','p6b60',
                'p6b61','p6b62','p6b63','p6b64','p6b67')

mydf.rec <- mydf.rec %>%
  mutate(
    Adhd_3y    = rowSums(across(all_of(adhd3_cols)),     na.rm = FALSE),
    Adhd_5y    = rowSums(across(all_of(adhd5_cols)),     na.rm = FALSE),
    Adhd_9y    = rowSums(across(all_of(adhd9_cols)),     na.rm = FALSE),
    Adhd_15y   = rowSums(across(all_of(adhd15_cols)),    na.rm = FALSE),
    Int_3y     = rowSums(across(all_of(int3_cols)),     na.rm = FALSE),
    Int_5y     = rowSums(across(all_of(int5_cols)),     na.rm = FALSE),
    Int_9y     = rowSums(across(all_of(int9_cols)),     na.rm = FALSE),
    Int_15y    = rowSums(across(all_of(int15_cols)),    na.rm = FALSE),
    Ext_3y     = rowSums(across(all_of(ext3_cols)),     na.rm = FALSE),
    Ext_5y     = rowSums(across(all_of(ext5_cols)),     na.rm = FALSE),
    Ext_9y     = rowSums(across(all_of(ext9_cols)),     na.rm = FALSE),
    Ext_15y    = rowSums(across(all_of(ext15_cols)),    na.rm = FALSE)
  )

mydf.rec <- mydf.rec %>%
  mutate(
    Int_3y_com     = rowSums(across(all_of(int5_common_cols)),     na.rm = FALSE),
    Int_5y_com     = rowSums(across(all_of(int5_common_cols)),     na.rm = FALSE),
    Int_9y_com     = rowSums(across(all_of(int9_common_cols)),     na.rm = FALSE),
    Int_15y_com    = rowSums(across(all_of(int15_common_cols)),    na.rm = FALSE),
    Ext_3y_com     = rowSums(across(all_of(ext3_common_cols)),     na.rm = FALSE),
    Ext_5y_com     = rowSums(across(all_of(ext5_common_cols)),     na.rm = FALSE),
    Ext_9y_com     = rowSums(across(all_of(ext9_common_cols)),     na.rm = FALSE),
    Ext_15y_com    = rowSums(across(all_of(ext15_common_cols)),    na.rm = FALSE)
  )


###########
### SES ###
###########

mydf.rec <- mydf.rec %>%
  mutate(across(c(cm3povco,cm4povco,cm5povco,cp6povco), ~ replace(., . < 0, NA)))
mydf.rec$ses_3y <- mydf.rec$cm3povco
mydf.rec$ses_5y <- mydf.rec$cm4povco
mydf.rec$ses_9y <- mydf.rec$cm5povco
mydf.rec$ses_15y <- mydf.rec$cp6povco


####################
### sex of child ###
####################

mydf.rec$sex <- mydf.rec$cm1bsex - 1 # 0 = boys ; 1 = girls


#######################
### Attrition check ###
#######################

mydf.rec$attr <- ifelse((mydf.rec$m2a3 == -9) | (mydf.rec$m3a2 == -9) | (mydf.rec$m4a2 == -9) | (mydf.rec$m5a2 == -9) | (mydf.rec$cp6ylpcg == -9),1,0)

mydf.rec <- mydf.rec[mydf.rec$attr == 0,]


write.csv(mydf.rec,"mydf.rec.csv", row.names = F)


########################
#### Write master df ###
########################

mydf.select <- mydf.rec %>%
  select(idnum,
         attr,
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
         freq.drunk,
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
         Ext_15y,
         insecure.school,
         police.off.school,
         secure.off.school,
         trouble.students,
         school.bullying.1,
         school.bullying.2,
         school.bullying.3,
         school.bullying.4,
         school.bullying.5,
         school.bullying.6,
         neighborhood.safety.1,
         neighborhood.safety.2,
         arms.home,
         family.safety.1,
         family.safety.2,
         family.safety.3,
         family.safety.4,
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
         mom_depress_ch_5y,
         ses_15y)

mydf.select.com <- mydf.rec %>%
  select(idnum,
         attr,
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
         freq.drunk,
         Adhd_3y,
         Adhd_5y,
         Adhd_9y,
         Adhd_15y,
         Int_3y_com,
         Int_5y_com,     
         Int_9y_com,
         Int_15y_com,
         Ext_3y_com,
         Ext_5y_com,
         Ext_9y_com,
         Ext_15y_com,
         insecure.school,
         police.off.school,
         secure.off.school,
         trouble.students,
         school.bullying.1,
         school.bullying.2,
         school.bullying.3,
         school.bullying.4,
         school.bullying.5,
         school.bullying.6,
         neighborhood.safety.1,
         neighborhood.safety.2,
         arms.home,
         family.safety.1,
         family.safety.2,
         family.safety.3,
         family.safety.4,
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
         mom_depress_ch_5y,
         ses_15y)


#########################
### Percentage of NAs ###
#########################

missing_summary <- mydf.select %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "percent.NA"
  ) %>%
  arrange(desc(percent.NA))

view(missing_summary)

write.csv(mydf.select,"mydf.select.csv",row.names = F)
write.csv(mydf.select.com,"mydf.select.com.csv",row.names = F)

