#' Loan Dataset
#'
#' A synthetic dataset inspired by the original Credit Risk dataset on Kaggle,
#' enriched with additional variables related to financial risk in loan approval.
#' The dataset was expanded using the SMOTENC algorithm to generate new instances,
#' and it contains a mix of categorical and continuous features, and the version
#' used in this package is a subset of the original source.
#'
#' @format A data frame with 10000 rows and 14 columns:
#' \describe{
#'   \item{person_age}{Age of the person. (numeric)}
#'   \item{person_gender}{Gender of the person. (character, "male" or "female")}
#'   \item{person_education}{Highest education level. (character, "High School", "Bachelor", "Master", "Doctorate", or "Associate")}
#'   \item{person_income}{Annual income. (numeric)}
#'   \item{person_emp_exp}{Years of employment experience. (numeric)}
#'   \item{person_home_ownership}{Home ownership status. (character, "OTHER", "RENT", "MORTGAGE", or "OWN"))}
#'   \item{loan_amnt}{Loan amount requested. (numeric)}
#'   \item{loan_intent}{Purpose of the loan. (character)}
#'   \item{loan_int_rate}{Loan interest rate. (numeric)}
#'   \item{loan_percent_income}{Loan amount as a percentage of annual income. (numeric)}
#'   \item{cb_person_cred_hist_length}{Length of credit history in years. (numeric)}
#'   \item{credit_score}{Credit score of the person. (numeric)}
#'   \item{previous_loan_defaults_on_file}{Indicator of previous loan defaults. (character, "No" or "Yes")}
#'   \item{loan_status}{Loan approval status. (numeric, 0 or 1)}
#' }
#'
#' @source https://www.kaggle.com/datasets/taweilo/loan-approval-classification-data
#'
"loan_dataset"