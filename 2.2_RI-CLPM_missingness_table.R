library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

############################################################
################### 0. Load missingness table ##############
############################################################

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM")) {
  setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM")
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM")) {
  setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise Young/Child Development/Short-term mindset/RI-CLPM")
} else {
  stop("Working directory not found.")
}

missing_tab <- read.csv2("percentages_missing_cases_by_indicator.csv", stringsAsFactors = FALSE)

# Safety check
stopifnot(exists("missing_tab"))
stopifnot(all(c("variable", "percent.NA") %in% names(missing_tab)))

missing_tab <- missing_tab %>%
  mutate(
    variable = as.character(variable),
    percent.NA = as.numeric(gsub(",", ".", percent.NA))
  )

############################################################
################### 1. Define indicator groups #############
############################################################

group_map <- list(
  
  ##########################################################
  # Core model indicators
  ##########################################################
  
  adhd = c(
    "Adhd_3y", "Adhd_5y", "Adhd_9y", "Adhd_15y"
  ),
  
  internalizing = c(
    "Int_3y", "Int_5y", "Int_9y", "Int_15y"
  ),
  
  externalizing = c(
    "Ext_3y", "Ext_5y", "Ext_9y", "Ext_15y"
  ),
  
  mating_effort_indicators = c(
    "age.1st.date", "num.date", "age.1st.sex", "num.sex"
  ),
  
  risky_health_behaviour_indicators = c(
    "age.1st.cig", "freq.smoke.month", "quant.smoke.day",
    "age.1st.drank", "freq.alc.month", "freq.drunk",
    "freq.alc.year", "quant.alc.month", "quant.alc.year",
    "quant2.alc.year"
  ),
  
  ##########################################################
  # Early deprivation
  ##########################################################
  
  early_deprivation_3y = c(
    "act_3y", "toys_3y", "interact_3y"
  ),
  
  early_deprivation_5y = c(
    "act_5y", "toys_5y", "interact_5y"
  ),
  
  ##########################################################
  # Early threat
  ##########################################################
  
  early_threat_3y = c(
    "phys_agg_3y", "psych_agg_3y", "violence_3y"
  ),
  
  early_threat_5y = c(
    "phys_agg_5y", "psych_agg_5y", "violence_5y"
  ),
  
  ##########################################################
  # Early stochasticity
  ##########################################################
  
  early_stochasticity_3y = c(
    "arrang_3y", "bedtime_3y", "bed_routine_3y", "mom_depress_3y"
  ),
  
  early_stochasticity_5y = c(
    "arrang_5y", "bedtime_5y", "bed_routine_5y", "mom_depress_5y"
  ),
  
  ##########################################################
  # Early volatility
  ##########################################################
  
  early_volatility_3y = c(
    "sep_3y", "move_3y", "jobs_3y", "mom_depress_ch_3y"
  ),
  
  early_volatility_5y = c(
    "sep_5y", "move_5y", "jobs_5y", "mom_depress_ch_5y"
  ),
  
  ##########################################################
  # Auxiliary / contextual indicators present in your table
  ##########################################################
  
  school_safety = c(
    "secure.off.school", "police.off.school", "insecure.school",
    "school.bullying.1", "school.bullying.2", "school.bullying.3",
    "school.bullying.4", "school.bullying.5", "school.bullying.6",
    "trouble.students", "arms.home"
  ),
  
  neighborhood_safety = c(
    "neighborhood.safety.1", "neighborhood.safety.2"
  ),
  
  family_safety = c(
    "family.safety.1", "family.safety.2", "family.safety.3", "family.safety.4"
  ),
  
  household_chaos = c(
    "chaos_9y"
  ),
  
  ses = c(
    "ses_15y"
  ),
  
  id_vars = c(
    "idnum", "attr", "sex"
  ),
  
  other = c(
    "mom_depress_1y"
  )
)

############################################################
################### 2. Convert map to lookup table #########
############################################################

group_lookup <- imap_dfr(
  group_map,
  ~ tibble(
    variable = .x,
    indicator_group = .y
  )
)

############################################################
################### 3. Merge and reorganize ################
############################################################

missing_grouped <- missing_tab %>%
  left_join(group_lookup, by = "variable") %>%
  mutate(
    indicator_group = if_else(is.na(indicator_group), "unmapped", indicator_group),
    developmental_wave = case_when(
      str_detect(variable, "_3y$") ~ "3y",
      str_detect(variable, "_5y$") ~ "5y",
      str_detect(variable, "_9y$") ~ "9y",
      str_detect(variable, "_15y$") ~ "15y",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(
    factor(
      indicator_group,
      levels = c(
        "adhd",
        "internalizing",
        "externalizing",
        "mating_effort_indicators",
        "risky_health_behaviour_indicators",
        "early_deprivation_3y",
        "early_deprivation_5y",
        "early_threat_3y",
        "early_threat_5y",
        "early_stochasticity_3y",
        "early_stochasticity_5y",
        "early_volatility_3y",
        "early_volatility_5y",
        "school_safety",
        "neighborhood_safety",
        "family_safety",
        "household_chaos",
        "ses",
        "id_vars",
        "other",
        "unmapped"
      )
    ),
    developmental_wave,
    desc(percent.NA)
  )

############################################################
################### 4. Group-level summary #################
############################################################

missing_group_summary <- missing_grouped %>%
  group_by(indicator_group) %>%
  summarise(
    n_indicators = n(),
    mean_percent_NA = mean(percent.NA, na.rm = TRUE),
    median_percent_NA = median(percent.NA, na.rm = TRUE),
    min_percent_NA = min(percent.NA, na.rm = TRUE),
    max_percent_NA = max(percent.NA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_percent_NA))

###########################################################
################### 5. Table ready for export #############
###########################################################

missing_grouped_ready <- missing_grouped %>%
  mutate(
    percent.NA = round(percent.NA, 2)
  ) %>%
  select(indicator_group, developmental_wave, variable, percent.NA)

missing_group_summary_ready <- missing_group_summary %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

############################################################
################### 6. Optional wide view ##################
############################################################

missing_grouped_wide <- missing_grouped_ready %>%
  mutate(row_id = row_number()) %>%
  group_by(indicator_group) %>%
  mutate(within_group_id = row_number()) %>%
  ungroup() %>%
  select(indicator_group, within_group_id, variable, percent.NA) %>%
  pivot_wider(
    names_from = c(variable),
    values_from = percent.NA
  )

############################################################
################### 7. Export ##############################
############################################################

write.csv2(missing_grouped_pretty, "missingness_by_indicator_group.csv", row.names = FALSE)
write.csv2(missing_group_summary_pretty, "missingness_group_summary.csv", row.names = FALSE)

############################################################
################### 8. Print ###############################
############################################################

cat("\n=== Missingness reorganized by indicator group ===\n")
print(missing_grouped_pretty, n = Inf)

cat("\n=== Missingness summary by indicator group ===\n")
print(missing_group_summary_pretty, n = Inf)
