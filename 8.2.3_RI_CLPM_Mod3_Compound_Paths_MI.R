############################################################
###### Model-implied compound paths (indirect effects) #####
###### Model 3 — script autonome                       #####
############################################################

library(blavaan)
library(lavaan)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

############################################################
################### 0. Working directories #################
############################################################

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects")) {
  fit_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects")) {
  fit_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Objects"
} else {
  stop("Fit_Objects directory not found.")
}

if (dir.exists("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables")) {
  table_dir <- "/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables"
} else if (dir.exists("C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables")) {
  table_dir <- "C:/Users/PJacquet/Dropbox/DGAEFS/AXIS 0_COLLABORATORS/Heloise Young/Child Development/Short-term mindset/RI-CLPM/Model_3/Model Fit_Tables"
} else {
  stop("Fit_Tables directory not found.")
}

fig_dir <- table_dir

############################################################
######################## 1. Settings #######################
############################################################

n_imp         <- 20
save_draws    <- FALSE
output_prefix <- "model_3_RI_CLPM"
beta_family   <- "bet_sign"
mapping_file  <- file.path(fit_dir, "model_3_empirical_mapping_all_imputations.rds")
fit_file      <- function(i) file.path(fit_dir, paste0("model_3_fit_imp_", i, ".rds"))

D   <- c("Adhd", "Ext", "Int")   # 1 = Adhd, 2 = Ext, 3 = Int
OUT <- c("mating.behaviour.z", "risky.health.behaviour.z")
ADV <- list(
  threat        = c(w3 = "early.threat.3y.z",        w5 = "threat5_resid"),
  deprivation   = c(w3 = "early.deprivation.3y.z",   w5 = "depr5_resid"),
  stochasticity = c(w3 = "early.stochasticity.3y.z", w5 = "stoch5_resid"),
  volatility    = c(w3 = "early.volatility.3y.z",    w5 = "vol5_resid")
)

stopifnot(file.exists(mapping_file))
param_map_emp_all <- readRDS(mapping_file)
cat("Mapping charge :", nrow(param_map_emp_all), "lignes\n")

beta_map <- param_map_emp_all %>%
  filter(family == beta_family, op == "~", !is.na(lhs)) %>%
  select(imputation, stan_name, group, parameter)

############################################################
############### 2. Fonction de calcul ######################
############################################################

compound_paths <- function(dat) {
  n   <- nrow(dat)
  get <- function(p) if (p %in% names(dat)) dat[[p]] else rep(0, n)
  
  Bl <- function(t1, t2)
    lapply(seq_along(D), function(i)
      lapply(seq_along(D), function(j)
        get(sprintf("w%s_%d ~ w%s_%d", D[i], t2, D[j], t1))))
  
  B35 <- Bl(3, 5); B59 <- Bl(5, 9); B915 <- Bl(9, 15)
  
  propagate <- function(v, B)
    sapply(seq_along(D), function(i)
      Reduce(`+`, lapply(seq_along(D), function(j) B[[i]][[j]] * v[, j])))
  
  # propagation restreinte a un sous-ensemble de domaines
  propagate_sub <- function(v, B, keep)
    sapply(keep, function(i)
      Reduce(`+`, lapply(seq_along(keep), function(jj)
        B[[i]][[ keep[jj] ]] * v[, jj])))
  
  PAIRS <- list(no_adhd = c(2, 3),   # Ext + Int  -> evite l'attention
                no_ext  = c(1, 3),   # Adhd + Int -> evite l'externalisation
                no_int  = c(1, 2))   # Adhd + Ext -> evite l'internalisation
  
  res <- list()
  for (adv in names(ADV)) {
    
    a3 <- sapply(D, function(d) get(sprintf("w%s_3 ~ %s", d, ADV[[adv]]["w3"])))
    a5 <- sapply(D, function(d) get(sprintf("w%s_5 ~ %s", d, ADV[[adv]]["w5"])))
    
    s15_from3 <- propagate(propagate(propagate(a3, B35), B59), B915)
    s15_from5 <- propagate(propagate(a5, B59), B915)
    
    for (o in OUT) {
      
      cc <- sapply(D, function(d) get(sprintf("%s ~ w%s_15", o, d)))
      
      w3_tot <- rowSums(cc * s15_from3)
      w5_tot <- rowSums(cc * s15_from5)
      
      sub3 <- function(keep) {
        s <- propagate_sub(propagate_sub(propagate_sub(
          a3[, keep, drop = FALSE], B35, keep), B59, keep), B915, keep)
        rowSums(cc[, keep, drop = FALSE] * s)
      }
      sub5 <- function(keep) {
        s <- propagate_sub(propagate_sub(
          a5[, keep, drop = FALSE], B59, keep), B915, keep)
        rowSums(cc[, keep, drop = FALSE] * s)
      }
      
      w3_no_adhd <- sub3(PAIRS$no_adhd); w5_no_adhd <- sub5(PAIRS$no_adhd)
      w3_no_ext  <- sub3(PAIRS$no_ext);  w5_no_ext  <- sub5(PAIRS$no_ext)
      w3_no_int  <- sub3(PAIRS$no_int);  w5_no_int  <- sub5(PAIRS$no_int)
      
      res[[length(res) + 1]] <- tibble(
        adversity = adv,
        outcome   = o,
        
        from_age3_total         = w3_tot,
        from_age5_total         = w5_tot,
        
        from_age3_via_adhd15    = cc[, 1] * s15_from3[, 1],
        from_age3_via_ext15     = cc[, 2] * s15_from3[, 2],
        from_age3_via_int15     = cc[, 3] * s15_from3[, 3],
        from_age5_via_adhd15    = cc[, 1] * s15_from5[, 1],
        from_age5_via_ext15     = cc[, 2] * s15_from5[, 2],
        from_age5_via_int15     = cc[, 3] * s15_from5[, 3],
        
        from_age3_without_adhd  = w3_no_adhd,
        from_age3_through_adhd  = w3_tot - w3_no_adhd,
        from_age3_without_ext   = w3_no_ext,
        from_age3_through_ext   = w3_tot - w3_no_ext,
        from_age3_without_int   = w3_no_int,
        from_age3_through_int   = w3_tot - w3_no_int,
        
        from_age5_without_adhd  = w5_no_adhd,
        from_age5_through_adhd  = w5_tot - w5_no_adhd,
        from_age5_without_ext   = w5_no_ext,
        from_age5_through_ext   = w5_tot - w5_no_ext,
        from_age5_without_int   = w5_no_int,
        from_age5_through_int   = w5_tot - w5_no_int
      )
    }
  }
  bind_rows(res)
}

############################################################
######## 3. Boucle sequentielle sur les imputations ########
############################################################

ind_list <- vector("list", n_imp)

for (i in seq_len(n_imp)) {
  
  f <- fit_file(i)
  if (!file.exists(f)) stop("Fit introuvable : ", f)
  cat("Imputation ", i, " : lecture...\n", sep = "")
  
  fit_i  <- readRDS(f)
  post_i <- as.data.frame(fit_i@external[["mcmcout"]])
  rm(fit_i); gc()
  
  map_i    <- beta_map %>% filter(imputation == i)
  keep_col <- intersect(map_i$stan_name, names(post_i))
  if (length(keep_col) == 0) stop("Aucun parametre beta pour l'imputation ", i)
  
  draws_i <- post_i[, keep_col, drop = FALSE]
  rm(post_i); gc()
  
  ind_i <- map_dfr(sort(unique(map_i$group)), function(g) {
    mg <- map_i %>% filter(group == g, stan_name %in% keep_col)
    dg <- draws_i[, mg$stan_name, drop = FALSE]
    names(dg) <- mg$parameter
    compound_paths(dg) %>% mutate(group = g, .before = 1)
  })
  
  ind_list[[i]] <- ind_i %>% mutate(imputation = i, .before = 1)
  rm(draws_i, ind_i); gc()
  cat("  ok\n")
}

indirect_draws <- bind_rows(ind_list); rm(ind_list); gc()
cat("Tirages composes :", nrow(indirect_draws), "lignes x",
    ncol(indirect_draws), "colonnes\n")

############################################################
############ 4. Controle de coherence ######################
############################################################

chk <- indirect_draws %>%
  transmute(
    d_via3  = from_age3_total - (from_age3_via_adhd15 + from_age3_via_ext15 + from_age3_via_int15),
    d_via5  = from_age5_total - (from_age5_via_adhd15 + from_age5_via_ext15 + from_age5_via_int15),
    d_adhd3 = from_age3_total - (from_age3_without_adhd + from_age3_through_adhd),
    d_ext3  = from_age3_total - (from_age3_without_ext  + from_age3_through_ext),
    d_int3  = from_age3_total - (from_age3_without_int  + from_age3_through_int),
    d_adhd5 = from_age5_total - (from_age5_without_adhd + from_age5_through_adhd),
    d_ext5  = from_age5_total - (from_age5_without_ext  + from_age5_through_ext),
    d_int5  = from_age5_total - (from_age5_without_int  + from_age5_through_int)
  )
cat("Ecart max de decomposition (doit etre ~1e-16) :",
    max(abs(as.matrix(chk))), "\n")
rm(chk); gc()

############################################################
##################### 5. Resume ############################
############################################################

n_imp_used <- n_distinct(indirect_draws$imputation)

indirect_summary <- indirect_draws %>%
  group_by(group, adversity, outcome) %>%
  summarise(
    n_draws = n(),
    across(starts_with("from_age"),
           list(median = ~median(.x),
                q2.5   = ~unname(quantile(.x, .025)),
                q97.5  = ~unname(quantile(.x, .975)),
                p_pos  = ~mean(.x > 0),
                p_neg  = ~mean(.x < 0)),
           .names = "{.col}@@{.fn}"),
    .groups = "drop"
  ) %>%
  pivot_longer(contains("@@"), names_to = c("path", "stat"), names_sep = "@@") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    sex       = ifelse(group == 1, "boys", "girls"),
    entry     = ifelse(grepl("^from_age3", path),
                       "Entry at age 3", "Entry at age 5 (residualized)"),
    component = sub("^from_age[35]_", "", path),
    n_imp     = n_imp_used
  ) %>%
  select(group, sex, adversity, outcome, entry, component, path,
         median, q2.5, q97.5, p_pos, p_neg, n_draws, n_imp) %>%
  arrange(adversity, outcome, component, entry, group)

print(indirect_summary %>%
        filter(adversity == "threat", outcome == "mating.behaviour.z",
               entry == "Entry at age 3"), n = 30)

write.csv(indirect_summary,
          file.path(table_dir, paste0(output_prefix, "_indirect_compound_paths.csv")),
          row.names = FALSE)

if (save_draws) {
  saveRDS(indirect_draws,
          file.path(fit_dir, paste0(output_prefix, "_indirect_compound_draws.rds")),
          compress = FALSE)
}

############################################################
##################### 6. Figures ###########################
############################################################

ADV_LEVELS <- rev(c("threat", "deprivation", "stochasticity", "volatility"))
OUT_LABS   <- c("mating.behaviour.z"       = "Mating effort",
                "risky.health.behaviour.z" = "Risky health behaviour")
ENTRIES    <- c(age3 = "Entry at age 3",
                age5 = "Entry at age 5 (residualized)")

base_forest <- function(dat) {
  ggplot(dat, aes(x = median, y = adversity, colour = sex, shape = sex)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(aes(xmin = q2.5, xmax = q97.5), height = 0,
                   position = position_dodge(0.6), linewidth = 0.6) +
    geom_point(position = position_dodge(0.6), size = 2.4, fill = "white") +
    scale_shape_manual(values = c(boys = 16, girls = 21)) +
    scale_colour_manual(values = c(boys = "#F58518", girls = "#14D9BB")) +
    labs(x = "Compound path (SD units)", y = NULL, colour = NULL, shape = NULL) +
    theme_bw(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey95", colour = NA),
          legend.position  = "top")
}

make_fig <- function(entry_label, comps, labs_vec) {
  dat <- indirect_summary %>%
    filter(component %in% names(labs_vec), entry == entry_label) %>%
    mutate(adversity = factor(adversity, levels = ADV_LEVELS),
           outcome   = recode(outcome, !!!OUT_LABS),
           panel     = factor(recode(component, !!!labs_vec),
                              levels = unname(labs_vec)))
  base_forest(dat) + facet_grid(outcome ~ panel, scales = "free_x")
}

## --- Figure 1 : par domaine d'arrivee (dernier pas) ---------
VIA_LABS <- c(total      = "All routes",
              via_adhd15 = "Arriving via\nattentional state",
              via_ext15  = "Arriving via\nexternalizing state",
              via_int15  = "Arriving via\ninternalizing state")

## --- Figure 2 : routes evitant chaque domaine ---------------
AVOID_LABS <- c(total         = "All routes",
                without_adhd  = "Avoiding\nattentional",
                without_ext   = "Avoiding\nexternalizing",
                without_int   = "Avoiding\ninternalizing")

## --- Figure 3 : routes impliquant chaque domaine ------------
THROUGH_LABS <- c(total         = "All routes",
                  through_adhd  = "Involving\nattentional",
                  through_ext   = "Involving\nexternalizing",
                  through_int   = "Involving\ninternalizing")

FIGS <- list(via = VIA_LABS, avoid = AVOID_LABS, through = THROUGH_LABS)

for (nm in names(FIGS)) {
  for (e in names(ENTRIES)) {
    p <- make_fig(ENTRIES[[e]], comps = NULL, labs_vec = FIGS[[nm]])
    ggsave(file.path(fig_dir,
                     paste0(output_prefix, "_fig_", nm, "_", e, ".png")),
           p, width = 11, height = 5, dpi = 300)
  }
}

## --- Export PDF (vectoriel) ---
for (nm in names(FIGS)) {
  for (e in names(ENTRIES)) {
    p <- make_fig(ENTRIES[[e]], comps = NULL, labs_vec = FIGS[[nm]])
    ggsave(file.path(fig_dir,
                     paste0(output_prefix, "_fig_", nm, "_", e, ".pdf")),
           p, width = 11, height = 5)   # device PDF par defaut, pas de cairo
  }
}

cat("\nTable ecrite dans   :", table_dir, "\n")
cat("Figures ecrites dans :", fig_dir, "\n")
cat("Fichiers :", paste0(output_prefix, "_fig_{via,avoid,through}_{age3,age5}.png"), "\n")


############################################################
######## 7. Visualisation dans RStudio (panneau Plots) #####
############################################################

figs <- list()
for (nm in names(FIGS)) {
  for (e in names(ENTRIES)) {
    figs[[paste0(nm, "_", e)]] <- make_fig(ENTRIES[[e]], NULL, FIGS[[nm]])
  }
}

names(figs)

figs[["avoid_age3"]]      # routes evitant chaque domaine, entree a 3 ans
figs[["avoid_age5"]]
figs[["via_age3"]]      # domaine d'arrivee (dernier pas)
figs[["via_age5"]]
figs[["through_age3"]]  # routes impliquant chaque domaine
figs[["through_age5"]]

# --- Ou toutes a la suite (flechees < > dans le panneau Plots) ---
# for (f in figs) print(f)

# --- Fenetre dediee, plus lisible que le panneau Plots ---
# dev.new(width = 11, height = 5); print(figs[["avoid_age3"]])