####################################
#PV survey data
######################################

#' transform_to_utils
#'
#' Transforms survey Likert scores for likelihood of pv adoption (1..5) to utilities in range -1,1.
#' The transformation depends on s (utility uncertainty) and epsilon (survey hypothetical bias estimate).
#'
#' @param pv_data survey data including qsp22_7 (Likert scores for adopting pv)
#' @param s utility uncertainty (default 0.15)
#' @param epsilon degree of hypothetical bias (default 0.7, 1 = no bias). Further hypothetical bias correction is applied at ABM tuning stage.
#'
#' @return modified pv survey dataframe
#' @export
#'
#' @examples
transform_to_utils <-function(pv_data,s=0.15,epsilon=0.7){

  #pv_data1 <- pv_data %>% dplyr::select(-ID)
  util_mapping <- map_likertscores_to_utilities(s,epsilon)
  pv_data1 <- pv_data %>% dplyr::rowwise() %>% dplyr::mutate(u = util_mapping[[qsp22_7,"dU"]]) %>% dplyr::select(-qsp22_7)
  return(pv_data1)
  }

#pv_data1 <- pv_util(pv_data)

#' find_optimum_rounds_from_crossvalidation
#'
#' helper function to find optimum learning complexity for for given learning rate and tree depth
#'
#' @param pv_data solar PV data
#' @param learning_rate eta parameter (default 0.02)
#' @param tree_depth maximum tree depth (default 5)
#' @param k_crossvalidation k-fold cross validation
#'
#' @return n_opt
#' @export
#'
#' @examples
find_optimum_rounds_from_crossvalidation <- function(pv_data, learning_rate=0.02, tree_depth=5, k_crossvalidation=5){

  #pv_data1 <- pv_data %>% dplyr::select(-ID)
  pv_util <- transform_to_utils(pv_data)
  pv.train <- xgboost::xgb.DMatrix(as.matrix(pv_util[,-dim(pv_util)[2]]),label=as.vector(pv_util$u), missing=NA)

  #if(!train_on_utilities) #train on Likert scores
  # pv.train <- xgboost::xgb.DMatrix(as.matrix(pv_data1[,-dim(pv_data1)[2]]),label=as.vector(pv_data1$qsp22_7-1), missing=NA)
  #
  paramlist <- list(booster="gbtree",
                    tree_method = "exact",
                    eta=learning_rate,
                    max_depth=tree_depth,
                    gamma=0,
                    subsample=0.9,
                    colsample_bytree = 0.9,
                    objective="reg:squarederror",
                    eval_metric="rmse"
                    #objective="multi:softprob",
                    #eval_metric = "mlogloss"
  )

  bst <- xgboost::xgb.cv(params=paramlist,pv.train,nrounds=500,nfold=k_crossvalidation)

  cv_data <- bst["evaluation_log"] %>% as.data.frame() %>% tibble::as_tibble()
  nopt <- cv_data[,"evaluation_log.test_rmse_mean"][[1]] %>% as.numeric() %>% which.min()
  print(paste("optimal nrounds",nopt))
  return(nopt)

}


#' getBoostedTreeModel
#'
#' creates a cross-validated boosted tree regression model from pv survey data
#'
#' @param pv_data_in input survey data
#' @param learning_rate eta. typical value 0.02
#' @param tree_depth tree depth typical value 5
#' @param k_crossvalidation k-fold cross validatin typical value 5
#' @param complexity_factor "over-fitting" enhancement relative to optimal model complexity from cross-validation. Values in range 1-1.5.
#'
#'
#' @return xgboost model
#' @export
#'
#' @examples
#'
get_boosted_tree_model <- function(pv_data_in, learning_rate=0.02, tree_depth=5, k_crossvalidation=5,complexity_factor = 1){

  pv_util <- transform_to_utils(pv_data_in)
  pv.train <- xgboost::xgb.DMatrix(as.matrix(pv_util[,-dim(pv_util)[2]]),label=as.vector(pv_util$u), missing=NA)

  #if(!train_on_utilities) #train on Likert scores
   # pv.train <- xgboost::xgb.DMatrix(as.matrix(pv_data1[,-dim(pv_data1)[2]]),label=as.vector(pv_data1$qsp22_7-1), missing=NA)

  paramlist <- list(booster="gbtree",
                    tree_method = "exact",
                    eta=learning_rate,
                    max_depth=tree_depth,
                    gamma=0,
                    subsample=0.9,
                    colsample_bytree = 0.9,
                    objective="reg:squarederror",
                    eval_metric="rmse"
                    #objective="multi:softprob",
                    #eval_metric = "mlogloss"
  )
  n_opt <- find_optimum_rounds_from_crossvalidation(pv_data,learning_rate,tree_depth,k_crossvalidation)
  bst <- xgboost::xgboost(data=pv.train,params=paramlist,pv.train,nrounds=complexity_factor*n_opt)
  return(bst)
}


#e.g restrict to owner-occupier houses i.e. q1 in 2,3,4 q3 in 1,2
#pv_data_oo <- pv_data %>% dplyr::filter(q1 %in% 2:4,q3 %in% 1:2)

#bst <- get_boosted_tree_model(pv_data_oo,complexity_factor = 1.5)

#' get_shap_scores
#'
#' returns partial utilities (shap scores) for each agent
#'
#' @param pv_data_in input survey data (training). Use pv_data for full survey data or pv_data_oo for owner-occupiers
#' @param bst boosted tree model from xgboost
#'
#' @return shap scores for all agents and features, in long format and including BIAS (same for all agents)
#' @export
#'
#' @examples
get_shap_scores <- function(pv_data_in,bst){

  pv_util <- transform_to_utils(pv_data_in)
  pv_util_long <- pv_util
  pv_util_long$ID <- 1:dim(pv_util)[1]
  pv_util_long <- pv_util_long %>% tidyr::pivot_longer(-ID,names_to="code",values_to="answercode")
  pv_util_long <- pv_util_long %>% dplyr::inner_join(pv_qanda,by=c("code","answercode"))

  shap_scores <- predict(bst, as.matrix(pv_util[,-dim(pv_util)[2]]), predcontrib = TRUE, approxcontrib = F) %>% tibble::as_tibble()
  shap_scores$ID <- 1:dim(shap_scores)[1]
  shap_scores_long <- tidyr::pivot_longer(shap_scores,-ID,values_to="shap","names_to"="code")
  #add predictions
  preds <- shap_scores_long %>% dplyr::group_by(ID) %>% dplyr::summarise(u_predicted=sum(shap)) #includes BIAS
  #preds$actual <- pv_data$qsp22_7
  shap_scores_long1 <- shap_scores_long  %>% dplyr::inner_join(preds,by="ID")
  shap_scores_long1$u_actual <- sapply(pv_util$u, rep, dim(pv_util)[2]) %>% as.vector()
  #shap_scores_long1$pred <- shap_scores_long1$pred + 1 #+ dplyr::filter(shap_scores,name=="BIAS")$value
  #pv_data1$ID <- 1:dim(pv_data1)[1]
  #shap_scores_long1 <- shap_scores_long1 %>% dplyr::inner_join(pv_data1)
  shap_scores_long1 <-  shap_scores_long1 %>% dplyr::left_join(pv_util_long)
  return(shap_scores_long1)
  }

#shap_scores_long <- get_shap_scores(pv_data_oo,bst)
#shap_scores_long %>% ggplot(aes(factor(round(u_actual,2)),u_predicted)) + geom_boxplot()

#u_econ <- shap_scores_long %>% filter(code == "q9_1") %>% group_by(ID) %>% summarise(u_econ=shap)
#u_social <- shap_scores_long %>% filter(code == "qsp21") %>% group_by(ID) %>% summarise(u_social=shap)

#test <- u_econ %>% inner_join(u_social) %>% inner_join(u_theta)
#test <- test %>% mutate(u_tot=u_econ+u_social+u_theta)

#' get_abm_calibration
#'
#' returns abm partial utilities for the selected features (q9_1 and qsp21) and barrier (theta) terms in ABM model for each agent. Results are expressed as mean
#' partial utilities corresponding to each survey response and individual weights for each agent.
#'
#' @param shap_scores_long individual shap scores by feature (output from get_shap_scores)
#'
#' @return data frame giving partial utilities for abstracted model features and residual (theta) terms and individual weights
#' @export
#'
#' @examples
get_abm_calibration <- function(shap_scores_long){

  shap_scores_long <- shap_scores_long %>% dplyr::select(-question,-answer)
  u_theta <- shap_scores_long %>% dplyr::filter(!(code %in% c("q9_1","qsp21"))) %>% dplyr::group_by(ID) %>% dplyr::summarise(shap=sum(shap))
  u_theta$code <- "theta"
  u_theta$answercode <- NA

  shap_scores_abm <- shap_scores_long %>% dplyr::filter(code %in% c("q9_1","qsp21")) %>% dplyr::select(-u_predicted,-u_actual)
  shap_scores_abm <- shap_scores_abm %>% dplyr::bind_rows(u_theta) %>% dplyr::arrange(ID)
  shap_scores_abm <- shap_scores_abm %>% rename("du"=shap)
  shap_scores_mean <- shap_scores_abm %>% dplyr::group_by(code,answercode) %>% dplyr::summarise(du_mean=mean(du))
  shap_scores_abm <- shap_scores_abm %>% inner_join(shap_scores_mean)
  shap_scores_abm <- shap_scores_abm %>% mutate(weight=du/du_mean)
  return(shap_scores_abm)
}

#shap_scores_abm <- get_abm_calibration(shap_scores_long)

#shap_scores_abm %>% dplyr::filter(code=="q9_1") %>% ggplot2::ggplot(ggplot2::aes(factor(answercode),du)) + ggplot2::geom_boxplot()
#shap_scores_abm %>% dplyr::filter(code=="qsp21") %>% ggplot2::ggplot(ggplot2::aes(factor(answercode),du)) + ggplot2::geom_boxplot()
#shap_scores_abm %>% dplyr::filter(code=="q9_1") %>% ggplot2::ggplot(ggplot2::aes(weight)) + ggplot2::geom_histogram() + facet_grid(.~answercode)


#' get_model_weights
#'
#' The agent weights for financial, social and barrier terms
#'
#' @param shap_scores_long shaps scores (partial utilities)
#'
#' @return a dataframe with colums ID w_q9_1  w_qsp21 W_theta
#' @export
#'
#' @examples
get_model_weights <- function(shap_scores_long){

  shap_scores_abm <- get_abm_calibration(shap_scores_long)
  weights_abm <- shap_scores_abm %>% tidyr::pivot_wider(c(-du,-answercode,-du_mean),values_from=weight,names_from=code)
  return(weights_abm)
}

#' get_empirical_partial_utilities
#'
#' partial (dis) utilities for pv adoption derived from survey
#'
#'
#' @param shap_scores_long shap scores (parial utilities)
#'
#' @return dataframe
#' @export
#'
#' @examples
get_empirical_partial_utilities <- function(shap_scores_long){

  shap_scores_abm <- get_abm_calibration(shap_scores_long)
  partial_utils <- shap_scores_abm %>% group_by(code,answercode) %>% summarise(du_mean=mean(du))
  return(partial_utils)
}


#' pbeta_util
#'
#' Beta function probability generalised to range -1,1
#'
#' @param x real in range -1 to 1
#' @param shape1 "a" shape parameter
#' @param shape2 "b" shape parameter
#'
#' @return generalised beta function value
#' @export
#'
#' @examples
pbeta_util <- function(x,shape1,shape2){
   #beta function generalised to -2,1
   return(pbeta((x+1)/2,shape1,shape2))
}

#' dbeta_util
#'
#' Beta function distribution generalised to interval -1,1
#'
#' @param x in -1,1
#' @param shape1 "a" shape parameter
#' @param shape2 "b" shape parameter
#'
#' @return function value
#' @export
#'
#' @examples
dbeta_util <- function(x,shape1,shape2){
   #beta function generalised to -2,1
   return(dbeta((x+1)/2,shape1,shape2))
}

#' probs_from_shape_params
#'
#' mapping between shape parameters of generalised Beta function and total probability that value > 0
#'
#' @param s standard deviation of Beta distribution
#'
#' @return dataframe
#' @export
#'
#' @examples
probs_from_shape_params <- function(s){

df <- tidyr::tibble()
for( a in seq(0.15,300, by=0.1)){
 f <- function(b)  {4*a*b-((a+b+1)*(a+b)^2)*s^2} #its a polynomial
 f1 <- function(b) {4*a*b-b^3*s^2-3*a*b^2*s^2-b^2*s^2-3*a^2*b*s^2-2*a*b*s^2-a^3*s^2-a^2*s^2}
#  #find roots of cubic polynomial in b given s (sd) and a
 f.roots <- polyroot(c(-a^3*s^2-a^2*s^2,-3*a^2*s^2-2*a*s^2+4*a,-3*a*s^2-s^2,-s^2)) %>% Re()
#  #f.roots
 i.roots <- polyroot(c(-a^3*s^2-a^2*s^2,-3*a^2*s^2-2*a*s^2+4*a,-3*a*s^2-s^2,-s^2)) %>% Im() %>% round(5)
#
#  #real roots only
 b <- f.roots[which(i.roots==0)]
  #positive real roots only
  b <- b[b>0]
for(b1 in b)
#  #print(paste("b=",b1,"mean=", 2*a/(a+b1)-1, "  sd=",2*sqrt(a*b1/((a+b1+1)*(a+b1)^2)),"prob=",round(1-pbeta_util(0,a,b1),2) ))
   df <- dplyr::bind_rows(df,tidyr::tibble(s=s,a=a,b=b1,mean= 2*a/(a+b1)-1,prob=round(1-pbeta_util(0,a,b1),3) ))
}
return(df)
}


#' shape_params_from_prob
#'
#' returns shape parameters corresponding to an adoption probability (i.e. area of generalised beta distribution > 0)
#'
#' @param df.in dataframe produced by probs_from_shape_params()
#' @param prob value in range 0,1
#'
#' @return shape paramaters (a and b)
#' @export
#'
#' @examples
shape_params_from_prob <- function(df.in,prob){
  #
     return(c(approx(df.in$prob,df.in$a,prob,ties="mean")$y,approx(df.in$prob,df.in$b,prob,ties="mean")$y))
}

#' map_likertscores_to_utilities
#'
#' maps likert scores 1..5 to adoption probabilities and then to expected utility.
#' Epsilon controls the degree and sign of the hypothetical bias correction needed when tuning the ABM
#'
#' @param s agent utility uncertainty (standard deviation of utility at time of survey)
#' @param epsilon probability scale factor (to allow for hypothetical bias) 1= no hypothetical bias. default 0.75.
#'
#' @return a dataframe of shape parameters and expected utility values corresponging to likert probabilities
#' @export
#'
#' @examples
map_likertscores_to_utilities <- function(s=0.15,epsilon=0.75){

  probs <- epsilon*c(0.1,0.3,0.5,0.7,0.9) #hard-wired
  df <- probs_from_shape_params(s)
  distrib_params <- tidyr::tibble()
 for(prob in probs)
   distrib_params <- distrib_params %>% dplyr::bind_rows(tidyr::tibble(s=s,epsilon=epsilon,prob=prob,a=shape_params_from_prob(df,prob)[1],b=shape_params_from_prob(df,prob)[2]))
   distrib_params <- distrib_params %>% dplyr::mutate(dU = 2*a/(a+b)-1)
   return(distrib_params)
   }


#shap_scores_long <- getSHAPscores(bst)

