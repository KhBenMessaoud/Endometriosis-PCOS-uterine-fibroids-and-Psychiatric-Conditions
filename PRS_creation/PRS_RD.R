library(dplyr)
library(pROC)
library(rcompanion)

### --- Set working directory --- ###
setwd("~/Downloads/hormones/PRS")

### --- Traits and phenotype files --- ###
traits      <- c("PCOS",            "UF",                     "EM")
pheno_files <- c("eids_PCOS.csv",   "eids_uterine_fibriods.csv", "eids_endometriosis.csv")
thresholds  <- c("p5e8", "p00001", "p0001", "p001", "p005", "p01", "p05", "p1")

### --- Load and process PCs --- ###
pcs <- read.csv("~/Documents/UKB/PC_PRS.csv")
colnames(pcs)[colnames(pcs) == "eid"] <- "IID"

pc_cols      <- paste0("p22009_a", 1:10)
new_pc_names <- paste0("PC", 1:10)

colnames(pcs)[match(pc_cols, colnames(pcs))] <- new_pc_names
pcs <- pcs[complete.cases(pcs[, new_pc_names]), ]

### --- Function to load PRS .sscore files --- ###
load_sscore <- function(file, label) {
  df <- read.table(file, header = TRUE, comment.char = "")
  colnames(df)[colnames(df) == "#FID"] <- "FID"
  colnames(df)[colnames(df) == "#IID"] <- "IID"
  
  if (!("IID" %in% colnames(df)) | !("SCORE1_AVG" %in% colnames(df))) {
    stop(paste("Missing expected columns in", file))
  }
  
  df <- df[, c("IID", "SCORE1_AVG")]
  colnames(df)[2] <- paste0("PRS_", label)
  return(df)
}

### --- Helper: Nagelkerke R2 for logistic models (full vs null) --- ###
nagelkerke_R2 <- function(full_ll, null_ll, n) {
  # Cox & Snell
  r2_cs <- 1 - exp((2 / n) * (null_ll - full_ll))
  # Nagelkerke
  r2_n  <- r2_cs / (1 - exp((2 / n) * null_ll))
  return(r2_n)
}

### --- Loop through each trait --- ###
for (i in seq_along(traits)) {
  trait     <- traits[i]
  pheno_path <- pheno_files[i]
  
  cat("\n=== Evaluating PRS for:", trait, "===\n")
  
  ## Load phenotype file
  pheno <- read.csv(pheno_path)
  colnames(pheno)[colnames(pheno) == "eid"] <- "IID"
  colnames(pheno)[2] <- "Outcome"  # assumes 2nd column is the phenotype
  pheno <- pheno[, c("IID", "Outcome")]
  
  ## Load PRS scores across thresholds
  prs_list <- list()
  for (t in thresholds) {
    fname <- paste0("prs_scores_", trait, ".", t, ".sscore")
    if (file.exists(fname)) {
      prs_list[[t]] <- load_sscore(fname, t)
    } else {
      warning(paste("Missing file:", fname))
    }
  }
  
  if (length(prs_list) < 2) {
    warning(paste("Not enough PRS files found for", trait, "- skipping."))
    next
  }
  
  ## Merge PRS scores (wide, one column per threshold)
  prs_merged <- Reduce(function(x, y) merge(x, y, by = "IID"), prs_list)
  
  ## Merge phenotype + PRS + PCs
  merged_data <- pheno %>%
    inner_join(prs_merged, by = "IID") %>%
    inner_join(pcs,        by = "IID")
  
  ## Baseline model: PCs only (for incremental Nagelkerke R2)
  pc_formula <- as.formula(paste("Outcome ~", paste0("PC", 1:10, collapse = " + ")))
  model_pc   <- glm(pc_formula, data = merged_data, family = "binomial")
  ll_pc      <- as.numeric(logLik(model_pc))
  n_obs      <- nobs(model_pc)
  
  ## Run logistic regression and compute AUC for each threshold
  results <- data.frame(
    threshold = names(prs_list),
    AUC       = NA_real_
  )
  
  for (j in seq_along(prs_list)) {
    prs_name <- names(prs_list)[j]
    prs_col  <- paste0("PRS_", prs_name)
    
    formula <- as.formula(
      paste("Outcome ~", prs_col, "+", paste0("PC", 1:10, collapse = " + "))
    )
    
    model <- glm(formula, data = merged_data, family = "binomial")
    pred  <- predict(model, type = "response")
    
    roc_obj      <- roc(merged_data$Outcome, pred)
    results$AUC[j] <- auc(roc_obj)
  }
  
  ## Choose best threshold by AUC
  best <- results[which.max(results$AUC), ]
  
  ## Fit full model only for the best threshold and compute Nagelkerke R2
  best_prs_col <- paste0("PRS_", best$threshold)
  best_formula <- as.formula(
    paste("Outcome ~", best_prs_col, "+", paste0("PC", 1:10, collapse = " + "))
  )
  best_model <- glm(best_formula, data = merged_data, family = "binomial")
  ll_full    <- as.numeric(logLik(best_model))
  
  nag_r2 <- nagelkerke_R2(full_ll = ll_full, null_ll = ll_pc, n = n_obs)
  
  cat("Best PRS threshold for", trait, "=",
      best$threshold,
      "with AUC =", round(best$AUC, 4),
      "\n   Nagelkerke R2 (PRS+PCs vs PCs-only) =", round(nag_r2, 4), "\n")
}


UF <- read.table("prs_scores_UF.p05.sscore", header = T, , comment.char = "")
UF <- UF[, c("IID", "SCORE1_AVG")]
colnames(UF)[colnames(UF) == "SCORE1_AVG"] <- "Score"

write.table(UF,'UF_PRS.txt', quote = FALSE, row.names = FALSE, col.names = TRUE)

PCOS <- read.table("prs_scores_PCOS.p0001.sscore", header = T, , comment.char = "")
PCOS <- PCOS[, c("IID", "SCORE1_AVG")]
colnames(PCOS)[colnames(PCOS) == "SCORE1_AVG"] <- "Score"

write.table(PCOS,'PCOS_PRS.txt', quote = FALSE, row.names = FALSE, col.names = TRUE)


EM <- read.table("prs_scores_EM.p00001.sscore", header = T, , comment.char = "")
EM <- EM[, c("IID", "SCORE1_AVG")]
colnames(EM)[colnames(EM) == "SCORE1_AVG"] <- "Score"

write.table(EM,'EM_PRS.txt', quote = FALSE, row.names = FALSE, col.names = TRUE)


