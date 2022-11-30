#' shap_scores_long_sample
#'
#' Sample output from get_shap_scores()
#'
#' @format A data frame with 20375 rows and 8 variables:
#' \describe{
#'   \item{ID}{ID label 1:1208 (not Household ID HHID)}
#'   \item{code}{ feature code}
#'   \item{shap}{contribution of feature to utility}
#'   \item{u_predicted}{modelled total utility}
#'   \item{u_actual}{actual utility}
#'   \item{answercode}{the survey response}
#'   \item{question}{the survey question in long form}
#'   \item{answer}{the survey answer in long form}
#' }
#' @source \url{}
"shap_scores_long_sample"
