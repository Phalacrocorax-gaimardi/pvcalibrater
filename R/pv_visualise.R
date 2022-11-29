#visualiser

#features q9_1

#' featureImportance
#'
#' makes a boxplot of shap score dependences on feature values
#'
#' @param shap_scores_long shap scores in long format (output of get_shap_scores)
#' @param feature_code feature code partial utility to be plotted
#'
#' @return a plot
#' @export
#'
#' @examples
featureImportance <- function(shap_scores_long, feature_code){

  shap <- shap_scores_long %>% dplyr::filter(code==feature_code) %>% dplyr::select(ID,all_of(feature_code),shap)
  shap %>% ggplot2::ggplot(ggplot2::aes(factor(.data[[feature_code]]), .data[["shap"]])) + ggplot2::geom_boxplot()
  #if(feature_code == "q9_1") shap %>% ggplot2::ggplot(ggplot2::aes(factor(bill_vals[.data[[feature_code]]]), .data[["shap"]])) + ggplot2::geom_boxplot()
}


#' createTable2
#'
#' creates a contingency table for a pair of features from pv_data
#'
#' @param q vector of features e.g. c("q1","q3")
#' @param format return the table in long or wide format
#'
#' @return n x m contingency table
#' @export
#'
#' @examples
createTable2 <- function(q,format="long"){
  #contingency table for n variables
  non_multiple_choice_question_codes <- NULL
  q1 <- q[!(q %in% non_multiple_choice_question_codes)]
  q2 <- q[(q %in% non_multiple_choice_question_codes)]
  n <- length(q)
  ev <- pv_data %>% dplyr::group_by_at(dplyr::vars(dplyr::one_of(q))) %>% dplyr::summarise(n=dplyr::n())
  for(qq in q1)
    ev <- ev %>% dplyr::inner_join(dplyr::filter(pv_qanda,code==qq)[,3:4],by=setNames("answercode",qq))# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  for(qq in q2)
    ev$qq <- "number"# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  names(ev)[1:n] <- q
  names(ev)[seq(n+2,2*n+1)] <- toupper(q)
  if(format=="long") return(ev)
  if(format=="wide") {
    ev <- ev[,1:3] %>% tidyr::pivot_wider(names_from=q[[2]],values_from=n)
    names(ev)[1] <- paste(as.character(q[[2]]),as.character(q[[1]]),sep="/")
    return(ev)
  }
}


#' plot_weights
#'
#' @param shap_scores_long partial utilities
#' @param clip fraction of observations to clip (i.e. reset to clip/2, 1-clip/2 quantile values). thetas are unclipped.
#'
#' @return
#' @export
#' @importFrom psych pairs.panels
#'
#' @examples
plot_weights <- function(shap_scores_long, clip=0.05){

  weights_abm_wide <- get_model_weights(shap_score_long)
  weights_abm_wide <- weights_abm_wide %>% select(-ID)
  clipper <- function(x){
    q <- quantile(x,probs=c(clip/2,1-clip/2))
    x <- ifelse(x > q[2],q[2],x)
    x <- ifelse(x < q[1],q[1],x)
    return(x)
  }
  weights_abm_wide$q9_1 <- clipper(weights_abm_wide$q9_1)
  weights_abm_wide$qsp21 <- clipper(weights_abm_wide$qsp21)
  #weights_abm_wide$theta <- clipper(weights_abm_wide$theta)
  pairs.panels(weights_abm_wide,ellipses=F,smooth=F)
}


