#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(openxlsx)


#___________________________________
load(file = "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro D X Mental Health/Data intermediate/UKBB.Rdata")
PC <- read.table("R:/janecm01lab/janecm01labspace/KBM-ZA/PRS_Scores/PC_final.txt", header = TRUE, sep = " ")
UF <- read.table("R:/janecm01lab/janecm01labspace/KBM-ZA/PRS_Scores/UF/UF_PRS.txt", header = TRUE, sep = " ")
EM <- read.table("R:/janecm01lab/janecm01labspace/KBM-ZA/PRS_Scores/EM/EM_PRS.txt", header = TRUE, sep = " ")
PCOS <- read.table("R:/janecm01lab/janecm01labspace/KBM-ZA/PRS_Scores/PCOS/PCOS_PRS.txt", header = TRUE, sep = " ")
#___________________________________


# DATA MANAGMENT
#________________

UKBB<- UKBB%>%rename(IID=Participant.ID)

UF<- UF%>%rename(UF_score=Score)
EM<- EM%>%rename(EM_score=Score)
PCOS<- PCOS%>%rename(PCOS_score=Score)

UKBB <- UKBB %>%
  inner_join(PC, by = "IID") %>%
  inner_join(UF, by = "IID") %>%
  inner_join(EM, by = "IID") %>%
  inner_join(PCOS, by = "IID")

keep<- c("IID","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10",
               "UF_score","EM_score","PCOS_score","DX_F20_F29", "DX_F20_F29_source", "DX_F20_F29_Date", "DX_F20_F29_age", "DX_F30_F39",
              "DX_F30_F39_source", "DX_F30_F39_Date", "DX_F30_F39_age", "DX_F40_F48", "DX_F40_F48_source",
              "DX_F40_F48_Date", "DX_F40_F48_age", "DX_F50_F59", "DX_F50_F59_source", "DX_F50_F59_Date",
              "DX_F50_F59_age")

UKBB <- UKBB[, keep]

UKBB <- UKBB %>%
  mutate(across(c(DX_F20_F29, DX_F30_F39, DX_F40_F48, DX_F50_F59), as.factor))



# NORMALISATION
#______________



# List of continuous variables to normalize
variables_to_normalize <- c("EM_score", "PCOS_score", "UF_score")

# Define function for Min-Max Normalization
min_max_normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Define function for Z-Score Normalization
z_score_normalize <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Normalize specified continuous variables
UKBB_normalized <- UKBB %>%
  mutate(across(all_of(variables_to_normalize), 
                list(
                  MinMax = ~ min_max_normalize(.),
                  ZScore = ~ z_score_normalize(.)
                ),
                .names = "{.col}_{.fn}"))  # Naming: "Variable_MinMax", "Variable_ZScore"

# View the normalized dataset
head(UKBB_normalized)









# ANALYSIS
#_________


# Define your outcomes and predictors
outcomes <- c("DX_F20_F29", "DX_F30_F39", "DX_F40_F48", "DX_F50_F59")
scores <- c("EM_score_ZScore", "PCOS_score_ZScore", "UF_score_ZScore")

# Create an empty list to store model results
model_results <- list()

# Fit models and store results
for (score in scores) {
  model_results[[score]] <- list()
  for (outcome in outcomes) {
    formula <- as.formula(paste(outcome, "~", score, "+ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10"))
    model <- glm(formula, data = UKBB_normalized, family = binomial)
    model_results[[score]][[outcome]] <- model
  }
}

# Function to extract odds ratios and confidence intervals
get_odds_ratios_specific <- function(model, score) {
  coeffs <- coef(summary(model))
  specific_coeffs <- coeffs[c("(Intercept)", score), ]
  
  odds_ratios <- exp(specific_coeffs[, "Estimate"])
  ci_lower <- exp(specific_coeffs[, "Estimate"] - 1.96 * specific_coeffs[, "Std. Error"])
  ci_upper <- exp(specific_coeffs[, "Estimate"] + 1.96 * specific_coeffs[, "Std. Error"])
  
  result <- data.frame(
    Term = rownames(specific_coeffs),
    Odds_Ratio = odds_ratios,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper
  )
  
  return(result)
}

# Create a workbook to store results
wb <- createWorkbook()

# Add each score's results to a separate sheet
for (score in scores) {
  # Create a new sheet for each score
  addWorksheet(wb, score)
  
  all_results <- data.frame()
  
  for (outcome in outcomes) {
    # Get odds ratios for the specific score and outcome
    odds_ratios <- get_odds_ratios_specific(model_results[[score]][[outcome]], score)
    
    # Add a column to indicate the outcome for clarity
    odds_ratios$Outcome <- outcome
    
    # Combine results
    all_results <- rbind(all_results, odds_ratios)
  }
  
  # Write results to the sheet
  writeData(wb, score, all_results)
}

# Save the workbook
saveWorkbook(wb, "C:/Users/BENMEK01/OneDrive - NYU Langone Health/Documents/Research project/Infertility UKBB/Repro & Hormones X Mental Health/Output/PRS/Models_Results.xlsx", overwrite = TRUE)

