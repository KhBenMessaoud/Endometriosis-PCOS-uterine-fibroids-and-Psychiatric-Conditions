# Load necessary libraries
library(dplyr)
library(tidyr)
library(broom)
library(openxlsx)
library(writexl)
library(tictoc)
library(future.apply)
library(data.table)
library(beepr)
library(survival)
library(parallel)
library(future)



# Load the dataset

load(file = "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro & Hormones X Mental Health/Data intermediate/sensitivity Conservative/matched_datasetFibro_CRD.Rdata")
load(file = "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro & Hormones X Mental Health/Data intermediate/sensitivity Conservative/matched_datasetEndo_CRD.Rdata")
load(file = "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro & Hormones X Mental Health/Data intermediate/sensitivity Conservative/matched_datasetPCOS_CRD.Rdata")

# Rename columns to replace "-" with "_"
colnames(matched_datasetEndo_CRD) <- gsub("-", "_", colnames(matched_datasetEndo_CRD))
colnames(matched_datasetFibro_CRD) <- gsub("-", "_", colnames(matched_datasetFibro_CRD))
colnames(matched_datasetPCOS_CRD) <- gsub("-", "_", colnames(matched_datasetPCOS_CRD))


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define the general function
process_dataset <- function(dataset) {
  # Ensure that Metabolic_Date and index_date are in Date format
  dataset <- dataset %>%
    mutate(
      Metabolic_Date = as.Date(Metabolic_Date),
      index_date = as.Date(index_date)
    )
  
  # Rename Metabolic_overall to Metabolic_old (keeping it as factor)
  dataset <- dataset %>%
    mutate(
      Metabolic_old = as.factor(Metabolic_overall)  # Rename and retain as factor
    )
  
  # Create the new factor variable Metabolic_overall
  dataset <- dataset %>%
    mutate(
      Metabolic_overall = as.factor(ifelse(Metabolic_old == 1 & Metabolic_Date < index_date, 1, 0))
    )
  
  return(dataset)  # Return the processed dataset
}

# Apply the function to `matched_datasetFibro` and `matched_datasetEndo`
matched_datasetFibro_CRD <- process_dataset(matched_datasetFibro_CRD)
matched_datasetEndo_CRD <- process_dataset(matched_datasetEndo_CRD)
matched_datasetPCOS_CRD <- process_dataset(matched_datasetPCOS_CRD)

# check outcome  binary 0/1 or return NA with a reason
coerce_binary <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.factor(x)) {
    if (nlevels(x) == 2) return(as.integer(x == levels(x)[2]))
    return(structure(rep(NA_integer_, length(x)), reason = "Outcome has >2 levels"))
  }
  
  if (is.character(x)) {
    x_trim <- trimws(tolower(x))
    # map common strings to 0/1
    map1 <- x_trim %in% c("1","yes","YES")
    map0 <- x_trim %in% c("0","no","NO")
    if (all(map1 | map0 | is.na(x_trim))) return(ifelse(map1, 1L, ifelse(map0, 0L, NA_integer_)))
    return(structure(rep(NA_integer_, length(x)), reason = "Character outcome not clearly binary"))
  }
  
  if (is.numeric(x)) {
    # allow 0/1 (and NA); if other values present, not binary
    if (!all(x %in% c(0,1,NA))) return(structure(rep(NA_integer_, length(x)), reason = "Numeric outcome not 0/1"))
    return(as.integer(x))
  }
  structure(rep(NA_integer_, length(x)), reason = "Unsupported outcome type")
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set global options to handle large data
options(future.globals.maxSize = 1 * 1024^3)

# Outcome variables
outcome_variables <- c("DX_F20_F29_overall", "DX_F30_F39_overall", "DX_F40_F48_overall", "DX_F50_F59_overall")

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# ENDOMETRIOSIS

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

formulas <- list(
  
  f_3 = "DX_endometriosis + Ethnicity + Migration + Incomes + Degree + Metabolic_overall + Primarycare + Year.of.birth + IMD_Q"
)

tic()
plan(multisession, workers = max(1, detectCores() - 1))

process_outcome_optimized <- function(i, outcome_variables, matched_dataset, formulas) {
  outcome_variable <- outcome_variables[i]
  outcome_results <- list()
  notes <- list()
  
  # copy so we can add a temporary binary response
  dat <- matched_dataset
  
  # 1) Coerce outcome to binary 0/1
  y_raw <- dat[[outcome_variable]]
  y_bin <- coerce_binary(y_raw)
  
  if (is.null(attr(y_bin, "reason"))) {
    dat$..y <- y_bin
  } else {
    notes[[outcome_variable]] <- paste0("Skipped: ", attr(y_bin, "reason"))
    return(list(.notes = notes))
  }
  
  # 2) Drop rows with missing outcome or subclass
  dat <- dat[!is.na(dat$..y) & !is.na(dat$subclass), , drop = FALSE]
  
  # 3) Ensure each subclass has at least one case and one control
  tab <- table(dat$subclass, dat$..y)
  valid_sets <- as.integer(rownames(tab)[rowSums(tab > 0) == 2])
  dat <- dat[dat$subclass %in% valid_sets, , drop = FALSE]
  
  if (nrow(dat) == 0L) {
    notes[[outcome_variable]] <- "Skipped: no strata with both case(s) and control(s) after cleaning."
    return(list(.notes = notes))
  }
  
  # 4) Fit models
  for (formula_name in names(formulas)) {
    ftxt <- paste("..y ~", formulas[[formula_name]], "+ strata(subclass)")
    form <- as.formula(ftxt)
    
    fit <- try(clogit(form, data = dat), silent = TRUE)
    if (inherits(fit, "try-error")) {
      outcome_results[[formula_name]] <- data.frame(
        Variable = character(), OR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
        stringsAsFactors = FALSE
      )
      notes[[paste0(outcome_variable, "_", formula_name)]] <- paste("Model failed:", conditionMessage(attr(fit, "condition")))
      next
    }
    
    # OR & CI
    est <- coef(fit)
    if (is.null(est) || length(est) == 0L) {
      outcome_results[[formula_name]] <- data.frame(
        Variable = character(), OR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
        stringsAsFactors = FALSE
      )
      next
    }
    exp_coef <- exp(est)
    ci <- try(exp(confint(fit)), silent = TRUE)
    if (inherits(ci, "try-error")) {
      # fallback to Wald CIs if profile fails
      se <- sqrt(diag(vcov(fit)))
      z <- qnorm(0.975)
      ci <- cbind(exp(est - z*se), exp(est + z*se))
      colnames(ci) <- c("2.5 %","97.5 %")
    }
    
    outcome_results[[formula_name]] <- data.frame(
      Variable = names(exp_coef),
      OR = as.numeric(exp_coef),
      CI_Lower = as.numeric(ci[,1]),
      CI_Upper = as.numeric(ci[,2]),
      row.names = NULL,
      check.names = FALSE
    )
  }
  
  if (length(notes)) outcome_results$.notes <- notes
  outcome_results
}

# Run for each outcome
results_all_outcomes <- future_lapply(seq_along(outcome_variables), function(i) {
  process_outcome_optimized(i, outcome_variables, matched_datasetEndo_CRD, formulas)
})

# Name results and strip NULLs
names(results_all_outcomes) <- outcome_variables
results_all_outcomes <- lapply(results_all_outcomes, function(x) x[!sapply(x, is.null)])

# Write to Excel: one sheet per outcome_formula; add a NOTES sheet if any skips/errors
wb <- createWorkbook()
has_notes <- FALSE

for (outcome in names(results_all_outcomes)) {
  res_list <- results_all_outcomes[[outcome]]
  if (is.null(res_list)) next
  
  # pull any notes
  if (!is.null(res_list$.notes)) {
    has_notes <- TRUE
    addWorksheet(wb, paste0(outcome, "_NOTES"))
    note_df <- data.frame(Message = unlist(res_list$.notes), stringsAsFactors = FALSE)
    writeData(wb, sheet = paste0(outcome, "_NOTES"), note_df)
    res_list$.notes <- NULL
  }
  
  for (formula_name in names(res_list)) {
    df <- res_list[[formula_name]]
    if (!is.data.frame(df) || nrow(df) == 0L) next
    sheet_name <- paste(outcome, formula_name, sep = "_")
    # Excel sheet names max 31 chars; shorten if needed
    if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df)
  }
}


# Save the Excel file with results
saveWorkbook(wb, file = "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro & Hormones X Mental Health/Output/sensitivity Conservative/overall Endo_CRD.xlsx", overwrite = TRUE)


# Stop timing and provide feedback
cat("Results successfully saved to", output_file, "\n")
toc()
beep(3)



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# PCOS

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



formulas <- list(
 
  f_3 = "DX_PCOS + Ethnicity + Migration + Incomes + Degree + Metabolic_overall + Primarycare + Year.of.birth + IMD_Q"
)

tic()
plan(multisession, workers = max(1, detectCores() - 1))

process_outcome_optimized <- function(i, outcome_variables, matched_dataset, formulas) {
  outcome_variable <- outcome_variables[i]
  outcome_results <- list()
  notes <- list()
  
  # copy so we can add a temporary binary response
  dat <- matched_dataset
  
  # 1) Coerce outcome to binary 0/1
  y_raw <- dat[[outcome_variable]]
  y_bin <- coerce_binary(y_raw)
  
  if (is.null(attr(y_bin, "reason"))) {
    dat$..y <- y_bin
  } else {
    notes[[outcome_variable]] <- paste0("Skipped: ", attr(y_bin, "reason"))
    return(list(.notes = notes))
  }
  
  # 2) Drop rows with missing outcome or subclass
  dat <- dat[!is.na(dat$..y) & !is.na(dat$subclass), , drop = FALSE]
  
  # 3) Ensure each subclass has at least one case and one control
  tab <- table(dat$subclass, dat$..y)
  valid_sets <- as.integer(rownames(tab)[rowSums(tab > 0) == 2])
  dat <- dat[dat$subclass %in% valid_sets, , drop = FALSE]
  
  if (nrow(dat) == 0L) {
    notes[[outcome_variable]] <- "Skipped: no strata with both case(s) and control(s) after cleaning."
    return(list(.notes = notes))
  }
  
  # 4) Fit models
  for (formula_name in names(formulas)) {
    ftxt <- paste("..y ~", formulas[[formula_name]], "+ strata(subclass)")
    form <- as.formula(ftxt)
    
    fit <- try(clogit(form, data = dat), silent = TRUE)
    if (inherits(fit, "try-error")) {
      outcome_results[[formula_name]] <- data.frame(
        Variable = character(), OR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
        stringsAsFactors = FALSE
      )
      notes[[paste0(outcome_variable, "_", formula_name)]] <- paste("Model failed:", conditionMessage(attr(fit, "condition")))
      next
    }
    
    # OR & CI
    est <- coef(fit)
    if (is.null(est) || length(est) == 0L) {
      outcome_results[[formula_name]] <- data.frame(
        Variable = character(), OR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
        stringsAsFactors = FALSE
      )
      next
    }
    exp_coef <- exp(est)
    ci <- try(exp(confint(fit)), silent = TRUE)
    if (inherits(ci, "try-error")) {
      # fallback to Wald CIs if profile fails
      se <- sqrt(diag(vcov(fit)))
      z <- qnorm(0.975)
      ci <- cbind(exp(est - z*se), exp(est + z*se))
      colnames(ci) <- c("2.5 %","97.5 %")
    }
    
    outcome_results[[formula_name]] <- data.frame(
      Variable = names(exp_coef),
      OR = as.numeric(exp_coef),
      CI_Lower = as.numeric(ci[,1]),
      CI_Upper = as.numeric(ci[,2]),
      row.names = NULL,
      check.names = FALSE
    )
  }
  
  if (length(notes)) outcome_results$.notes <- notes
  outcome_results
}

# Run for each outcome
results_all_outcomes <- future_lapply(seq_along(outcome_variables), function(i) {
  process_outcome_optimized(i, outcome_variables, matched_datasetPCOS_CRD, formulas)
})

# Name results and strip NULLs
names(results_all_outcomes) <- outcome_variables
results_all_outcomes <- lapply(results_all_outcomes, function(x) x[!sapply(x, is.null)])

# Write to Excel: one sheet per outcome_formula; add a NOTES sheet if any skips/errors
wb <- createWorkbook()
has_notes <- FALSE

for (outcome in names(results_all_outcomes)) {
  res_list <- results_all_outcomes[[outcome]]
  if (is.null(res_list)) next
  
  # pull any notes
  if (!is.null(res_list$.notes)) {
    has_notes <- TRUE
    addWorksheet(wb, paste0(outcome, "_NOTES"))
    note_df <- data.frame(Message = unlist(res_list$.notes), stringsAsFactors = FALSE)
    writeData(wb, sheet = paste0(outcome, "_NOTES"), note_df)
    res_list$.notes <- NULL
  }
  
  for (formula_name in names(res_list)) {
    df <- res_list[[formula_name]]
    if (!is.data.frame(df) || nrow(df) == 0L) next
    sheet_name <- paste(outcome, formula_name, sep = "_")
    # Excel sheet names max 31 chars; shorten if needed
    if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df)
  }
}


# Save the Excel file with results
saveWorkbook(wb, file = "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro & Hormones X Mental Health/Output/sensitivity Conservative/overall PCOS_CRD.xlsx", overwrite = TRUE)


# Stop timing and provide feedback
cat("Results successfully saved to", output_file, "\n")
toc()
beep(3)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# FIBROIDIS

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Formula list for each condition
formulas <- list(

  f_3 = "DX_Fibroids + Ethnicity + Migration + Incomes + Degree + Metabolic_overall + Primarycare + Year.of.birth + IMD_Q"
)
tic()
plan(multisession, workers = max(1, detectCores() - 1))

process_outcome_optimized <- function(i, outcome_variables, matched_dataset, formulas) {
  outcome_variable <- outcome_variables[i]
  outcome_results <- list()
  notes <- list()
  
  # copy so we can add a temporary binary response
  dat <- matched_dataset
  
  # 1) Coerce outcome to binary 0/1
  y_raw <- dat[[outcome_variable]]
  y_bin <- coerce_binary(y_raw)
  
  if (is.null(attr(y_bin, "reason"))) {
    dat$..y <- y_bin
  } else {
    notes[[outcome_variable]] <- paste0("Skipped: ", attr(y_bin, "reason"))
    return(list(.notes = notes))
  }
  
  # 2) Drop rows with missing outcome or subclass
  dat <- dat[!is.na(dat$..y) & !is.na(dat$subclass), , drop = FALSE]
  
  # 3) Ensure each subclass has at least one case and one control
  tab <- table(dat$subclass, dat$..y)
  valid_sets <- as.integer(rownames(tab)[rowSums(tab > 0) == 2])
  dat <- dat[dat$subclass %in% valid_sets, , drop = FALSE]
  
  if (nrow(dat) == 0L) {
    notes[[outcome_variable]] <- "Skipped: no strata with both case(s) and control(s) after cleaning."
    return(list(.notes = notes))
  }
  
  # 4) Fit models
  for (formula_name in names(formulas)) {
    ftxt <- paste("..y ~", formulas[[formula_name]], "+ strata(subclass)")
    form <- as.formula(ftxt)
    
    fit <- try(clogit(form, data = dat), silent = TRUE)
    if (inherits(fit, "try-error")) {
      outcome_results[[formula_name]] <- data.frame(
        Variable = character(), OR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
        stringsAsFactors = FALSE
      )
      notes[[paste0(outcome_variable, "_", formula_name)]] <- paste("Model failed:", conditionMessage(attr(fit, "condition")))
      next
    }
    
    # OR & CI
    est <- coef(fit)
    if (is.null(est) || length(est) == 0L) {
      outcome_results[[formula_name]] <- data.frame(
        Variable = character(), OR = numeric(), CI_Lower = numeric(), CI_Upper = numeric(),
        stringsAsFactors = FALSE
      )
      next
    }
    exp_coef <- exp(est)
    ci <- try(exp(confint(fit)), silent = TRUE)
    if (inherits(ci, "try-error")) {
      # fallback to Wald CIs if profile fails
      se <- sqrt(diag(vcov(fit)))
      z <- qnorm(0.975)
      ci <- cbind(exp(est - z*se), exp(est + z*se))
      colnames(ci) <- c("2.5 %","97.5 %")
    }
    
    outcome_results[[formula_name]] <- data.frame(
      Variable = names(exp_coef),
      OR = as.numeric(exp_coef),
      CI_Lower = as.numeric(ci[,1]),
      CI_Upper = as.numeric(ci[,2]),
      row.names = NULL,
      check.names = FALSE
    )
  }
  
  if (length(notes)) outcome_results$.notes <- notes
  outcome_results
}

# Run for each outcome
results_all_outcomes <- future_lapply(seq_along(outcome_variables), function(i) {
  process_outcome_optimized(i, outcome_variables, matched_datasetFibro_CRD, formulas)
})

# Name results and strip NULLs
names(results_all_outcomes) <- outcome_variables
results_all_outcomes <- lapply(results_all_outcomes, function(x) x[!sapply(x, is.null)])

# Write to Excel: one sheet per outcome_formula; add a NOTES sheet if any skips/errors
wb <- createWorkbook()
has_notes <- FALSE

for (outcome in names(results_all_outcomes)) {
  res_list <- results_all_outcomes[[outcome]]
  if (is.null(res_list)) next
  
  # pull any notes
  if (!is.null(res_list$.notes)) {
    has_notes <- TRUE
    addWorksheet(wb, paste0(outcome, "_NOTES"))
    note_df <- data.frame(Message = unlist(res_list$.notes), stringsAsFactors = FALSE)
    writeData(wb, sheet = paste0(outcome, "_NOTES"), note_df)
    res_list$.notes <- NULL
  }
  
  for (formula_name in names(res_list)) {
    df <- res_list[[formula_name]]
    if (!is.data.frame(df) || nrow(df) == 0L) next
    sheet_name <- paste(outcome, formula_name, sep = "_")
    # Excel sheet names max 31 chars; shorten if needed
    if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df)
  }
}


# Save the Excel file with results
saveWorkbook(wb, file = "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro & Hormones X Mental Health/Output/sensitivity Conservative/overall Fibro_CRD.xlsx", overwrite = TRUE)


# Stop timing and provide feedback
cat("Results successfully saved to", output_file, "\n")
toc()
beep(3)
