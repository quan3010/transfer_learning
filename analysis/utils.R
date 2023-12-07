## Shared utilities for data loading and preprocessing.

library(tidyverse)
# install.packages('latex2exp')
library(latex2exp)
# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
library(patchwork)

library(glue)




#' Compute standard error of the AUC statistic.
#' 
#' @param auc The AUC or A' statistic (area under the Receiver Operating Characteristic Curve).
#' @param np The number of positive elements in the dataset.
#' @param nn The number of negative elements in the dataset.
#' @return The standard error of the AUC.
se_auc <- function(auc, np, nn){
  D_p = (np - 1) * (auc / (2 - auc) - auc^2)
  D_n = (nn - 1) * ((2 * auc^2 / (1 + auc)) - auc^2)
  se = sqrt((auc * (1 - auc) + D_p + D_n) / (np * nn))
  return(se)
}

zero_if_nan <- function(x){
  return(ifelse(is.nan(x), 0, x))
}

######################################################
#### Pre-process
######################################################

# Get all metrics. The variable METRICS_DIR should be defined in the source script.
results = list()
for (f in list.files(METRICS_DIR, pattern = "metrics_tgt.*\\.csv", full.names = TRUE)){
  message(glue("reading results from {f}"))
  results[[f]] = read.csv(f)
}

d = bind_rows(results)

extract_model_type_from_uid <- function(x){
  return(str_match(x, "model_type(\\w+)_src")[2])
}

extract_lambda_from_uid <- function(x){
  return(as.numeric(str_match(x, "lambda(\\d+\\.*\\d*)")[2]))
}

clean_up_names <- function(df){
  # Clean up names
  df$src_institution = toupper(df$src_institution)
  df$target_institution = toupper(df$target_institution)
  df$lambda = sapply(df$uid, extract_lambda_from_uid)
  # TODO(jpgard): revalue to match codes used for model type previously.
  df$model_type = sapply(df$uid,extract_model_type_from_uid)
  df$is_ensemble = !is.na(d$ensemble_type)
  # df$model_type[df$model_type=="EN" & nchar(df$src_institution)==15] = "ENs"
  # df$model_type[df$model_type=="EN" & nchar(df$src_institution)<15] = "ENm"
  # df$model_type_lng = factor(df$model_type, labels=c("AdaBoost", "Ensemble: Majority Voting", "Ensemble: Stacked", "Logistic Regression", "Logistic Regression: Lambda", "Random Forest"))
  df$overall = df$subgroup == "full_test"
return(df)
}

d = clean_up_names(d)


# Subgroup names for the marginal/non-intersectional groups.
# Groups which do not appear in the test data (usually due to no dropouts in test set)
# are removed to reduce the number of groups in the legend.
MARGINAL_GROUPS = c("sexMale", 
                    "sexFemale", 
                    # "sexNotIndicated", "sexOther",
                    "urm_statusNon-Underrepresented Minority",                
                    "urm_statusUnderrepresented Minority"                    
                    # "urm_statusInternational"
)

INTERSECTIONAL_GROUPS = c("sexMale_urm_statusNon-Underrepresented Minority", 
                          "sexMale_urm_statusUnderrepresented Minority", 
                          "sexFemale_urm_statusNon-Underrepresented Minority",
                          "sexFemale_urm_statusUnderrepresented Minority")


# Anon institutions
# ASU-> A; ASUO -> B; UCI -> C; UM -> D
d$target_institution = factor(d$target_institution, labels = c("A", "B", "C", "D"))
d$src_institution = sub("UM", "D", d$src_institution)
d$src_institution = sub("UCI", "C", d$src_institution)
d$src_institution = sub("ASUO", "B", d$src_institution)
d$src_institution = sub("ASU", "A", d$src_institution)

# Add column for seauc, standard error of the AUC.
d %<>% 
  mutate(np = cm_true1_pred0 + cm_true1_pred1) %>%
  mutate(nn = cm_true0_pred0 + cm_true0_pred1) %>%
  mutate(seauc = se_auc(auc, np, nn))

