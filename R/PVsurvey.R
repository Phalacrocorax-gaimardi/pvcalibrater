####################################
#EV survey data 
######################################
library(tidyverse)
library(ggthemes)
library(sf)
library(xgboost)
library(SHAPforxgboost)
library(verification)
library(knitr)
library(kableExtra)
#library(reticulate)
library(fastshap)
#demographic, personality

survey_raw <- read_csv("~/Policy/AgentBasedModels/Survey/ESB Final data +LCA.csv")
survey_raw <- survey_raw %>% mutate_each(funs(as.integer))

qanda <- read_csv("~/Policy/AgentBasedModels/solarPV/Survey/qanda_pv.csv" )
qanda$code <- tolower(qanda$code)
questions <- distinct(qanda[,c(1,2,5)])
#
survey_pv <- survey_raw %>% dplyr::select(tolower(qanda$code))

##########################
# contingency tables
##########################

createTable <- function(q1,q2){
  #
  ev <- ev_owners %>% group_by(q1=get(q1),q2=get(q2)) %>% summarise(n=n()) %>% inner_join(filter(qanda,code==q1)[,3:4],by=c(q1="answercode")) %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  names(ev)[1:2] <- c(q1,q2)
  names(ev)[4:5] <- toupper(c(q1,q2))
  return(ev)
}
#questions that require numerical answer
non_multiple_choice_question_codes <- c("q16","qe1b_1","qe1b_2","q9_1","q9_2")

createTable2 <- function(survey = survey_pv, q){
  #cross table for n variables
  q1 <- q[!(q %in% non_multiple_choice_question_codes)]
  q2 <- q[(q %in% non_multiple_choice_question_codes)]
  n <- length(q)
  ev <- survey %>% group_by_at(vars(one_of(q))) %>% summarise(n=n()) 
  for(qq in q1)
    ev <- ev %>% inner_join(filter(qanda,code==qq)[,3:4],by=setNames("answercode",qq))# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  for(qq in q2)
    ev$qq <- "number"# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  names(ev)[1:n] <- q
  names(ev)[seq(n+2,2*n+1)] <- toupper(q)
  return(ev)
}

createTable3 <- function(survey = survey_ev, q,r,fun="median"){
  #cross table for variables q
  #returns median category values of variables r
  q1 <- q[!(q %in% non_multiple_choice_question_codes)]
  q2 <- q[(q %in% non_multiple_choice_question_codes)]
  n <- length(q)
  m <- length(r)
  if(fun=="median")
   ev <- survey %>% group_by_at(vars(one_of(q))) %>% summarise_at(r,median,na.rm=T) 
  if(fun=="mean")
    ev <- survey %>% group_by_at(vars(one_of(q))) %>% summarise_at(r,mean,na.rm=T) 
  for(qq in q1)
    ev <- ev %>% inner_join(filter(qanda,code==qq)[,3:4],by=setNames("answercode",qq))# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  for(qq in q2)
    ev$qq <- "number"# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  if(fun=="median") for(rr in r)
    ev <- ev %>% inner_join(filter(qanda,code==rr)[,3:4],by=setNames("answercode",rr))# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  #names(ev)[seq(1,n+m)] <- q
  if(fun == "median") names(ev)[seq(n+m+1,2*(n+m))] <- toupper(c(q,r))
  if(fun == "mean") names(ev)[seq(n+m+1,2*n+m)] <- toupper(c(q))
  return(ev)
}

createTable4 <- function(survey=survey_ev,q,r,fun="median"){
  
  return(inner_join(createTable3(survey_ev,q,r,fun=fun), createTable2(survey_ev,q),by=c(q,toupper(q))))
}


################################################
#factors influencing willingness to adopt
#################################################
#qsp22_7 willingness to consider
#q3 home ownership

test <- survey_pv %>% createTable2(c("q3","qsp22_7")) # %>% mutate(freq=n/sum(n)) #%>% ggplot() + geom_tile(aes(qc,qev33,fill=freq))
#
test %>% pivot_wider(c(-Q3,-QSP22_7),values_from=n,names_from=q3)
#regional & transport
test <- createTable2(c("qc","qev35")) # %>% mutate(freq=n/sum(n))


#########################################################################
##########################################################################
#survey stochastic gradient boosting: ordinal classification problem
##########################################################################
############################################################################

# predict willingness to "consider adopting" qsp22_7 on

#recats
survey_pv$q12a <- survey_pv$q12a + 1 #1 is no 2 ies ye
survey_pv$q12b <- survey_pv$q12b + 1
survey_pv$q12c <- survey_pv$q12c + 1
survey_pv$q12d <- survey_pv$q12d + 1
survey_pv$q12e <- survey_pv$q12e + 1
survey_pv$q12f <- survey_pv$q12f + 1
survey_pv$q12b %>% range() 
#social
q16_recat <- function(i){
  ifelse( i < 5,return(i+1),return(6))
}
q16_recat <- Vectorize(q16_recat)
#qsp21 : knowing someone who owns a PV
qsp21_recat <-function(i){
  #recategorises qev29 (know ev driver) to ordinal
  if(i==3) return(1)
  if(i==1) return(2)
  if(i==2) return(3)
}
qsp21_recat <-  Vectorize(qsp21_recat)

q9_1_recat <-function(x){
  #recategorises qev29 (know ev driver) to ordinal
  cut(x,c(-10,90,110,150,200,2000)) %>% as.integer() %>% return
}
q9_1_recat <-  Vectorize(q9_1_recat)

pv_data <- survey_pv %>% mutate(q16=q16_recat(q16), qsp21=qsp21_recat(qsp21), q9_1=q9_1_recat(q9_1))

dim(pv_data) #1208 households
#exclude q17a_4 because it is ambiguous and overlaps q17a_1
#exclude qev25 because it overlaps qev34
pv_features <- c("age","gender","class","region","qc1","q1","q3","q5","q7","q9_1","q10b","q15","q16","q17b","q17c","q17a_1","q17a_2","q17a_3","q17a_5","qsp20","qsp21","qsp22_7","qj","qk","qh","qg","qf","qsp22_7")
pv_data <- pv_data[,pv_features] 
dim(pv_data)
pv_data <- pv_data %>% drop_na("qsp22_7")
dim(pv_data)

#recategorise qanda 
# ONLY OF NECESSARY
qanda[which(qanda$code=="qsp21"),]$answercode <- qsp21_recat(1:3)
qanda <- qanda %>% arrange(code,answercode) #order
questions <- distinct(qanda[,c(1,2,5)])

#split q15 into two categories - technophile/non-technophile  &  follower/non-follower 
ev_drivers <- ev_drivers %>% mutate(q16=q16_recat(q16), qev23 = qev23_recat(qev23), qev29 = qev29_recat(qev29), qev32=qev32recat(qev32))

#ev_data$qev33 <- threecat(ev_data$qev33)
dim(survey_pv)
#write_csv(ev_data,"~/Policy/AgentBasedModels/modeldata_nonEVcarowners.csv")
#write_csv(ev_drivers,"~/Policy/AgentBasedModels/modeldata_EVcarowners.csv")
#write_csv(ev_data,"~/Policy/AgentBasedModels/modeldata_allcarbuyers.csv")
pv.train <- xgb.DMatrix(as.matrix(pv_data[,-dim(pv_data)[2]]),label=as.vector(pv_data$qsp22_7-1), missing=NA)
#
paramlist <- list(booster="gbtree",
                  tree_method = "exact",
                  eta=0.1,
                  max_depth=4,
                  gamma=0,
                  subsample=0.9,
                  colsample_bytree = 0.9,
                  #objective="reg:squarederror",
                  #eval_metric="rmse"
                  objective="multi:softprob"
                  #feval = amm.mae
)

bst <- xgb.cv(params=paramlist,pv.train,nrounds=1000,nfold=5,num_class=5)

test <- bst["evaluation_log"] %>% as.data.frame() %>% as_tibble()
test[,4] %>% min()
nopt <- test[,4][[1]] %>% as.numeric() %>% which.min()
print(paste("optimal nrounds",nopt))


bst.df <- bst["evaluation_log"] %>% as_tibble()
bst.df <- bst.df[[1]]
names(bst.df) <- c("iter","train_rmse_mean","train_rmse_std","test_rmse_mean","test_rmse_std")
ggplot(bst.df, aes(iter,train_rmse_mean)) + geom_point() + geom_point(aes(iter,test_rmse_mean),colour="red") + geom_vline(xintercept = nopt,linetype="dotted")

bst <- xgboost(params=paramlist,pv.train,nrounds=1*nopt,num_class=5)
#xgb.save(bst,fname="model.xgboost")
#bst <- xgboost(params=list(eta=0.05,max_depth=6,subsample=1),as.matrix(ev_data[,-20]),label=as.vector(ev_data$qev33-1),nrounds=200)
pred.bst <- predict(bst,as.matrix(pv_data[,-dim(pv_data)[2]]),reshape=T) %>% as_tibble()
pred.bst <- pred.bst %>% rowwise() %>% mutate(prediction= which.max(c(V1,V2)))

pred <- bind_cols(tibble(n = 1:dim(ev_data)[1],actual=ev_data$q12a), pred.bst)
unique(pred$prediction)
#ggplot(pred, aes(prediction)) + geom_histogram() + geom_histogram(aes(actual),fill="red",alpha=0.5)
g <- ggplot(pred, aes(factor(actual),V2)) + geom_boxplot(fill="orange")
g <- g + theme_economist() + scale_x_discrete(breaks=1:2,labels=filter(qanda,code=="q12a")$answer)
g <- g+labs(x="",y="prediction",title="model skill",subtitle="Do you own an electric car")
g + theme(axis.title = element_text(size=14))
#ggsave("~/Policy/AgentBasedModels/xgboost_qev33_skill.png")

verify(pred$prediction,pred$actual,frcst.type = "cat", obs.type="cat")$gs

questions$code <- tolower(questions$code)
#xgb.plot.tree(model=bst)
xgb.plot.shap(model=bst,as.matrix(pv_data[,-dim(pv_data)[2]] ),top_n=2)

#SHAP importance
questions$code <- tolower(questions$code)
shap_values <- shap.values(bst,as.matrix(pv_data[,-dim(pv_data)[2]]))
shap_scores <- shap_values$shap_score %>% as_tibble()
shap <- enframe(shap_values$mean_shap_score) %>% left_join(questions,c("name"="code"))
#shap[8,"question"] <- "How willing are you to take risk?" 
#shap[7,"question"] <- "How willing are you to defer reward?"
#shap[3,"question"] <- "How willing are you to adopt unfamiliar technology?"
#shap[12,"question"] <- "How concerned are you about the environment?"
shap$question <- factor(shap$question, levels=rev(shap$question))
shap$name <- factor(shap$name, levels=rev(shap$name))

g <- ggplot(shap, aes(name,value/sum(value),fill=type)) + coord_flip() + geom_col() + theme_economist()# + scale_x_discrete(limits=rev(shap$question[1:14])) #+ geom_text(aes(label=round(value,3)),hjust=1)
g <- g + ylim(0,0.18) + ggtitle("\"How likely is it that your next car is an electric car?\"") + xlab("") +ylab("") + theme(plot.title = element_text(hjust=1.2),plot.subtitle = element_text(size=12,face="bold",hjust=-2.5))
g
#ggsave("~/Policy/AgentBasedModels/adoption_importance.png")


shap %>% group_by(type) %>% summarise(shap=sum(value)) %>% mutate(shap_norm = shap/sum(shap)) %>% arrange(-shap)

#shap_values_ev <- shap_values$shap_score
#shap_values_ev <- shap.prep(shap_contrib = shap_values_ev, X_train = as.matrix(ev_data[,-dim(ev_data)[2]]),top_n=10)
#shap_long_ev <- shap.prep(shap_contrib = test, X_train=as.matrix(ev_data[,-dim(ev_data)[2]]))
#shap.plot.summary(shap_values_ev)

#shap_long_ev1 <- shap_long_ev %>% inner_join(questions,c("variable"="code")) %>% as_tibble()


shap_scores1 <- predict(bst, as.matrix(pv_data[,-dim(pv_data)[2]]), predcontrib = TRUE, approxcontrib = F)[[1]] %>% as_tibble()
#importance
shap_scores1$ID <- 1:dim(shap_scores1)[1]
shap_scores_long <- pivot_longer(shap_scores1,-ID)

preds <- shap_scores_long %>% group_by(ID) %>% summarise(pred=sum(value))
preds$actual <- pv_data$qsp22_7
shap_scores_long <- shap_scores_long  %>% inner_join(preds,by="ID")
shap_scores_long$pred <- shap_scores_long$pred + 1+shap_scores1$BIAS[1]
names(shap)[2] <- "mean"
shap_scores_long <- shap_scores_long %>% inner_join(shap,by="name")
pv_data_long <- pv_data
pv_data_long$ID <- 1:dim(pv_data)[1]
pv_data_long <- pivot_longer(pv_data_long,-ID)
names(pv_data_long)[3] <- "answercode"
pv_data_long <- pv_data_long %>% inner_join(qanda,by=c("name"="code","answercode"))

shap_scores_long <- shap_scores_long %>% inner_join(pv_data_long,c("ID","name","question","type"))
#shap_scores_long <- shap_scores_long %>% inner_join(ev_data_long)
#summary plot
shap_scores_long <- shap_scores_long %>% arrange(ID,-mean)
u_cutoff <- 0.035
shap_main <- shap_scores_long %>% filter(abs(mean)>u_cutoff)
shap_main$question <- factor(shap_main$question, levels=filter(shap,mean>u_cutoff)$question)
shap_main$name <- factor(shap_main$name, levels=filter(shap,mean>u_cutoff)$name)
std1 <- function(x) {
  return((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                       min(x, na.rm = TRUE)))
}

shap_main <- shap_main %>% group_by(name) %>% mutate(std_feature = std1(answercode))

g <- shap_main %>% ggplot(aes(name,value,colour=std_feature)) + ggforce::geom_sina(method="counts",size=0.2,alpha=0.5,maxwidth = 0.5) + coord_flip() + theme_minimal()
g <- g + scale_x_discrete(labels=str_wrap(levels(shap_main$question),30))
g <- g + scale_color_gradient(name="feature value",low = "#FFCC33", high = "#6600CC", 
                         breaks = c(0, 1), labels = c(" Low", "High "), 
                         guide = guide_colorbar(barwidth = 12, barheight = 0.3)) + theme_minimal()
g <- g + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
             legend.position = "bottom", legend.title = element_text(size = 10), 
             legend.text = element_text(size = 8), axis.title.x = element_text(size = 10))
g + geom_text(data = filter(shap_main, ID==2), aes(name,-Inf, label = sprintf("%.3f", mean)), size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold",colour="black")
#ggsave("~/Policy/AgentBasedModels/ev_model.summary.png")

###########################
#dependency plots
# empirical utility functions
##############################

feature_dep <- shap_scores_long %>% filter(name %in% c("qev25"))
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:14)
feature_dep$answer <- factor(feature_dep$answercode , levels = filter(qanda,code=="qev25")$answer)

g <- feature_dep %>% ggplot(aes(answercode,value)) + geom_point(,size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam") + facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_x_discrete(labels=str_wrap(levels(feature_dep$answer),30))
g + theme(axis.text.x = element_text(angle =45))
#
#decision plot for individuals
id <- 1 #interesting cases 540,9,  300
print(paste("predicted", sum(filter(shap_scores_long,ID==id)$value)+1.28+1))
test <- filter(shap_scores_long,ID==id) %>% arrange(-value)
#test <- test %>% inner_join(ev_data_long)

#test <- test %>% inner_join(qanda,c("name"="code","answercode"="answercode","question"="question","type"="type"))
test$name <- factor(test$name,levels=test$name)
test$question <- factor(test$question,levels=test$question)

#test %>% ggplot(aes(1,value,colour=factor(sign(value)))) + geom_col(position="stack") + coord_flip()
test1 <- filter(test, abs(value) > 0.02)
test1$index <- 1:dim(test1)[1]

g <- test1 %>% ggplot(aes(index,value,fill=factor(sign(value)))) + geom_col()+ coord_flip() + theme_minimal()
g <- g + scale_x_continuous(breaks=1:dim(test1)[1],labels=str_wrap(test1$question,60),sec.axis=dup_axis(labels=str_wrap(test1$answer,60))) #+ geom_text(aes(label=answer),size=4)
g <- g + labs(x="",y="SHAP score",title=paste("ID",id,"How likely is it that your next car is an electric car?"), subtitle = paste(filter(qanda,code=="qev33",answercode==round(test1$pred[1]))$answer,":",round(test1$pred[1],1)))
g + theme(legend.position="none",plot.title = element_text(hjust=0.5)) 
ggsave(paste("~/Policy/AgentBasedModels/ID=",id,"_decisionPlot.png",sep=""))
#filter(shap_scores_long,ID==id) %>% ggplot(aes(name,value,fill=type),colour="white") + geom_col() + coord_flip()

shap_scores_long %>% group_by(type) %>% summarise(mean=sum(value))

test <- shap_scores_long %>% filter(name!="BIAS") %>% group_by(name) %>% summarise(importance=mean(abs(value))) %>% arrange(-importance)

test %>% group_by(type) 

#########################
#interaction effects
###########################

shap_scores_int <- predict(bst, as.matrix(ev_data[,-dim(ev_data)[2]]), predcontrib = F, predinteraction=TRUE,approxcontrib = F) %>% as_tibble()


#importance
shap_scores_int$ID <- 1:dim(shap_scores_int)[1]
shap_scores_int_long <- pivot_longer(shap_scores_int,-ID)

shap_scores_int_long$f1 <- sapply(strsplit(shap_scores_int_long$name,"[.]"),"[",1)
shap_scores_int_long$f2 <- sapply(strsplit(shap_scores_int_long$name,"[.]"),"[",2)

shap_scores_int_long <- shap_scores_int_long  %>% inner_join(preds,by="ID")
shap_scores_long$pred <- shap_scores_long$pred + 1

shap_scores_int_long <- shap_scores_int_long %>% inner_join(ev_data_long,c("ID","f1"="name"))
names(shap_scores_int_long)[8:11] <- paste("f1",names(shap_scores_int_long)[8:11],sep="." )
shap_scores_int_long <- shap_scores_int_long %>% inner_join(ev_data_long,c("ID","f2"="name"))
names(shap_scores_int_long)[12:15] <- paste("f2",names(shap_scores_int_long)[12:15],sep="." )


shap_int <- shap_scores_int_long %>% group_by(name,f1,f2) %>% summarise(mean_score=mean(abs(value))) %>% arrange(-mean_score)




filter(shap_scores_int_long, ID==3)

filter(shap_int, f1 %in% c("q15","q16","qev29"), f2 %in% c("q15","q16","qev29") )

##########################################################################
#create news features from the data representing the "utility" function and thresold
# empirical utility function 
##########################################################################
#same question can appear in multiple utility functions/thresholds functions
financial_utility <- function(price_of_car,budget,cost_sensitivity){
  #
  1/(1+exp((price_of_car-budget)/cost_sensitivity))
  
}
lambda_cost_sensitivity <- 0.1 #leeway 10% of budget
fossil_price <- 20
ev_prices <- seq(fossil_price*0.5,fossil_price*1.5,length.out = 10)
u <- sapply(seq(0,20), function(p) {financial_utility(p,budget,cost_sensitivity)})

ev_premia <- seq(-10,10,length.out=10) #fixed euro premium
dfs <- vector("list",10)
for( i in 1:10){
  ev_p <- ev_premia[i]
  df <- tibble(fossil_price=20,ev_price=ev_p, budget=10:40)
  df$switch_util <- sapply(seq(10,40), function(b) {financial_utility(b+ev_p,b,lambda_cost_sensitivity*b)-financial_utility(1.1*b,b,lambda_cost_sensitivity*b)})
  dfs[[i]] <- df
}
dfs <- bind_rows(dfs)

ggplot(dfs, aes(budget,switch_util,colour=factor(sign(ev_price)))) + geom_point()

u_s <- sapply(seq(15,40), function(b) {financial_utility(ev_price,b,lambda_cost_sensitivity*b)-financial_utility(b,b,lambda_cost_sensitivity*b)})
plot(seq(15,40),u_s)
#price model for EVs 


functional_utility <- function(charging_options, daily_distance)
environmental

theta_threshold <- function(risk_appetite,technophobia, threshold_scale = 1.0){
  #technophobia q15 risk_appetite q17b
  #this version take 
  #if(technophoba==6) return(threshold_scale*((5-risk_appetite)*0.2+0.1))
  #if(technophobia != 6) return( )
  return(threshold_scale*((5-risk_appetite)*0.2+0.1))
}

survey_ev %>% createTable2(c("q15","q17b"))
#risk aversion #direct calculation of theta
#
filter(questions,type=="risk")
survey_ev %>% createTable2(c("q15","q17b")) %>% filter(q15 != 6) %>% ggplot() + geom_tile(aes(q15,q17b,fill=n))
#map 
#P(dU) > 0 -> 5 levels epsilon * 0.1,0.3,0.5,0.7,0.9 (can jitter)
#Du-theta in [-1,1]
#fix subjective error in dU and adjust expected value to meet
#use generalised beta functions
a <- 30 #shape1
b <- 20  #shape2
print(paste("mean=", 2*a/(a+b)-1, "  sd=",2*sqrt(a*b/((a+b+1)*(a+b)^2)) ))
#generalise beta function to -1,1
pbeta_util <- function(x,shape1,shape2){
  #beta function generalised to -2,1
  return(pbeta((x+1)/2,shape1,shape2))
}
dbeta_util <- function(x,shape1,shape2){
  #beta function generalised to -2,1
  return(dbeta((x+1)/2,shape1,shape2))
  
}
rbeta_util <- function(n,shape1,shape2){
  #beta function generalised to -2,1
  return(2*rbeta(n,shape1,shape2)-1)
  
}

#generalised beta distribution parameters 
# fix a values solve b for fixed sd
# fix standard deviation
s <- 0.15
#solve for b as a function of a
df <- tibble()
for( a in seq(0.15,300, by=0.1)){
 f <- function(b)  {4*a*b-((a+b+1)*(a+b)^2)*s^2} #its a polynomial
 f1 <- function(b) {4*a*b-b^3*s^2-3*a*b^2*s^2-b^2*s^2-3*a^2*b*s^2-2*a*b*s^2-a^3*s^2-a^2*s^2}
 #g <- function(b)  {3*sqrt(a*b/((a+b+1)*(a+b)^2))-targ_sd}
 #find roots of cubic polynomial in b given s (sd) and a
 f.roots <- polyroot(c(-a^3*s^2-a^2*s^2,-3*a^2*s^2-2*a*s^2+4*a,-3*a*s^2-s^2,-s^2)) %>% Re()
 #f.roots
 i.roots <- polyroot(c(-a^3*s^2-a^2*s^2,-3*a^2*s^2-2*a*s^2+4*a,-3*a*s^2-s^2,-s^2)) %>% Im() %>% round(5)

 #real roots only
 b <- f.roots[which(i.roots==0)]
 #positive real roots only
 b <- b[b>0]
 #plot(f,-50,20); abline(v=f.roots); abline(h=0,col=2)

for(b1 in b)
 #print(paste("b=",b1,"mean=", 2*a/(a+b1)-1, "  sd=",2*sqrt(a*b1/((a+b1+1)*(a+b1)^2)),"prob=",round(1-pbeta_util(0,a,b1),2) ))
  df <- bind_rows(df,tibble(s=s,a=a,b=b1,mean= 2*a/(a+b1)-1,prob=round(1-pbeta_util(0,a,b1),3) ))
}

ggplot(df, aes(prob,b)) + geom_point()
#calculate a,b from p (for fixed s)
get_params_from_prob <- function(df.in,prob){
  
  return(c(approx(df.in$prob,df.in$a,prob)$y,approx(df.in$prob,df.in$b,prob)$y))
} 
get_params_from_prob(df.in=df,0.7)
#bias adjusted adoption probabilities
epsilon <- 0.5
probs <- epsilon*c(0.1,0.3,0.5,0.7,0.9)
distrib_params <- tibble()
for(prob in seq(0.005,0.95,by=0.01))
 distrib_params <- distrib_params %>% bind_rows(tibble(s=s,epsilon=epsilon,prob=prob,a=get_params_from_prob(df,prob)[1],b=get_params_from_prob(df,prob)[2]))

distrib_params <- distrib_params %>% mutate(dU = 2*a/(a+b)-1)
#write_csv(distrib_params,"~/Policy/AgentBasedModels/utility_distribution_params.csv")
ggplot(distrib_params,aes(prob,a)) + geom_line() + geom_line(aes(prob,b),colour="red")


dutildf <- function(a,b){
  return(tibble(x=seq(-1,1,by=0.01), p=sapply(seq(-1,1,by=0.01),dbeta_util,shape1=a,shape2=b)))
}
beta_df <- distrib_params %>% group_by(prob) %>% do(dutildf(.$a,.$b))

beta_df <- beta_df %>% filter(prob %in% probs)

g <- ggplot(beta_df) + geom_area(aes(x,p,fill=factor(prob)),alpha=0.25,colour="black",position="identity")
#g <- g + geom_line(aes(x,p,colour=factor(prob)),alpha=0.5)#
g <- g + geom_vline(xintercept=0,linetype='dashed',colour="grey50") + theme_tufte()#+ geom_text(aes(label=round(1-pbeta_util(0,a,b),2))) + g
#g <- g + scale_y_continuous(limits=c(0,7)) + scale_fill_discrete(name="probability")
#g <- g + geom_segment(data = distrib_params, aes(x=mean,xend = mean, y= rep(6,5), yend=rep(7,5)),colour="grey50",size=1.5)
#g <- g + geom_text(data=distrib_params, aes(x=mean,y=rep(6.5,5),label=round(mean,2)),vjust=-0.8,angle=90)  
g <- g+ scale_fill_brewer(name="adoption probability",type="qual")
g <- g + labs(x=TeX('$u$'),y=TeX('$P\\left(u\\right)$'),title="")
g <- g + theme(axis.text.y=element_blank())
#ggsave("~/Policy/AgentBasedModels/switchingutilityprobability.png")

test <- beta_df %>% group_by(prob) %>% summarise(u_bar=weighted.mean(x,p))

test$n <- 1:5
g2 <- ggplot(test, aes(n,u_bar)) + geom_point() + theme_few() + labs(x="Likert score",y=TeX('$\\bar{u}$')) + theme(axis.title.y = element_text(angle=90))

g + annotation_custom(ggplotGrob(g2), xmin = 0.25, xmax = 1, ymin = 2, ymax = 5)
#ggsave("~/Policy/AgentBasedModels/switchingutilityprobability.png")
#ML prediction => dU-theta
#specification  dU
#theta => w*risk attitide


######################################################################
#fit a model with financial, social, environmental, functional factors
# rebuild the model using .. for ABM
#####################################################################
#methodology: predict qev33 (n=1:5)
#convert n_predict into probabilistics dU prediction

sd1 <- 0.15
distrib_params <- read_csv("~/Policy/AgentBasedModels/utility_distribution_params.csv")
distrib_params <- distrib_params %>% arrange(prob)
#distrib_params$qev33 <- 1:5 #%>% ggplot(aes(1:5,mean)) + geom_point()+geom_smooth(method="lm")
ev_data <- read_csv("~/Policy/AgentBasedModels/modeldata_allcarbuyers.csv")

#
ev_data1 <- ev_data %>% mutate(p=probs[qev33]) #%>% filter(qev31 == 1)
# new or don't now
ev_data1 <- dplyr::select(ev_data1,-qev33)

training_sample <- sample(1:dim(ev_data1)[1], 1*dim(ev_data1)[1])
test_sample <- which(!(1:dim(ev_data1)[1] %in% training_sample))

ev_data1_train <- ev_data1[training_sample,]
ev_data1_test <- ev_data1[test_sample,]

#ev.train <- xgb.DMatrix(as.matrix(ev_data1_train[,-dim(ev_data1)[2]]),label=as.vector(ev_data1_train$qev33)-1, missing=NA)
ev.train <- xgb.DMatrix(as.matrix(ev_data1_train[,-dim(ev_data1)[2]]),label=as.vector(ev_data1_train$p), missing=NA)

paramlist <- list(booster="gbtree",
                  tree_method = "exact",
                  eta=0.01,
                  max_depth=5,
                  gamma=0,
                  subsample=0.8,
                  colsample_bytree = 0.8,
                  #objective="reg:squarederror"
                  #eval_metric="rmse"
                  objective=amo.fairobj2
                  #feval = amm.mae
)



bst <- xgb.cv(params=paramlist,ev.train,nrounds=1000,nfold=3)
test <- bst["evaluation_log"] %>% as.data.frame() %>% as_tibble()
test[,4] %>% min()
nopt <- test[,4][[1]] %>% as.numeric() %>% which.min()
print(paste("optimal nrounds",nopt))


bst.df <- bst["evaluation_log"] %>% as_tibble()
bst.df <- bst.df[[1]]
names(bst.df) <- c("iter","train_rmse_mean","train_rmse_std","test_rmse_mean","test_rmse_std")
ggplot(bst.df, aes(iter,train_rmse_mean)) + geom_point() + geom_point(aes(iter,test_rmse_mean),colour="red") + geom_vline(xintercept = nopt,linetype="dotted")

bst <- xgboost(params=paramlist,ev.train,nrounds=nopt,feval=amm_mae)
#saveRDS(bst,"~/Policy/AgentBasedModels/xgb_all.model")
#bst <- xgboost(params=paramlist,ev.train,nrounds=1.5*nopt)
#xgb.save(bst,fname="model.xgboost")
#bst <- xgboost(params=list(eta=0.05,max_depth=6,subsample=1),as.matrix(ev_data[,-20]),label=as.vector(ev_data$qev33-1),nrounds=200)
insample.pred.bst <- predict(bst,as.matrix(ev_data1_train[,-dim(ev_data1_train)[2]]),reshape=T)#
insample.pred.int <- sapply(insample.pred.bst, function(x) which.min((x-0:4)^2))

outsample.pred.bst <- predict(bst,as.matrix(ev_data1_test[,-dim(ev_data1_test)[2]]),reshape=T)#
outsample.pred.int <- sapply(outsample.pred.bst, function(x) which.min((x-0:4)^2))


pred.outsample <- tibble(n = 1:dim(ev_data1_test)[1],prediction.actual=outsample.pred.bst+1,prediction=outsample.pred.int,actual=ev_data1_test$qev33)
#pred.insample <- tibble(n = 1:dim(ev_data1_train)[1],prediction.actual=insample.pred.bst+1,prediction=insample.pred.int,actual=ev_data1_train$qev33)
pred.insample <- tibble(n = 1:dim(ev_data1_train)[1],prediction.actual=insample.pred.bst,actual=ev_data1_train$p)

unique(pred.outsample$prediction)
#ggplot(pred, aes(prediction)) + geom_histogram() + geom_histogram(aes(actual),fill="red",alpha=0.5)
g <- ggplot(pred.insample, aes(factor(actual),prediction.actual, group=actual)) + geom_boxplot(fill="orange")
g <- g + theme_economist() + scale_x_discrete(breaks=1:5,labels=filter(qanda,code=="qev33")$answer)
g <- g+labs(x="",y="prediction",title="model skill",subtitle="How likely is it that your next car is an electric car?")
g + theme(axis.title = element_text(size=14))
#ggsave("~/Policy/AgentBasedModels/xgboost_qev33_skill.png")

verify(pred.outsample$prediction.actual,pred.outsample$actual,frcst.type = "cont", obs.type="cont")$MSE
verify(pred.insample$prediction.actual,pred.insample$actual,frcst.type = "cont", obs.type="cont")$MSE
cor(pred.outsample$prediction.actual,pred.outsample$actual)^2 #R2
cor(pred.insample$prediction.actual,pred.insample$actual)^2

#prediction on ev drivers
pred.bst <- predict(bst,as.matrix(ev_drivers[,-dim(ev_drivers)[2]]),reshape=T)#
pred.int <- sapply(pred.bst, function(x) which.min((x-0:4)^2))

pred <- tibble(n = 1:dim(ev_drivers)[1],prediction.actual=pred.bst,prediction=pred.int,actual=ev_drivers$qev33)
unique(pred$prediction)
g <- ggplot(pred, aes(factor(actual),prediction.actual, group=actual)) + geom_boxplot(fill="orange")
g <- g + theme_economist() + scale_x_discrete(breaks=1:5,labels=filter(qanda,code=="qev33")$answer)
g <- g+labs(x="",y="prediction",title="model skill",subtitle="How likely is it that your next car is an electric car?")
g + theme(axis.title = element_text(size=14))

#shap scores
shap_scores1 <- predict(bst, as.matrix(ev_data1[,-dim(ev_data1)[2]]), predcontrib = TRUE, approxcontrib = F) %>% as_tibble()
#importance
shap_scores1$ID <- 1:dim(shap_scores1)[1]
shap_scores_long <- pivot_longer(shap_scores1,-ID)

preds <- shap_scores_long %>% group_by(ID) %>% dplyr::summarise(pred=sum(value))
#preds$actual <- ev_data1$qev33
preds$actual <- ev_data1$p
shap_scores_long <- shap_scores_long  %>% inner_join(preds,by="ID")
#shap_scores_long$pred <- shap_scores_long$pred + shap_scores1$BIAS[1]
#
shap <- shap_scores_long %>% group_by(name) %>% dplyr::summarise(SHAP=mean(abs(value))) %>% arrange(-SHAP)
shap <- shap %>% left_join(questions,c("name"="code"))

shap_scores_long <- shap_scores_long %>% inner_join(shap,by="name")
ev_data_long <- ev_data1
ev_data_long$ID <- 1:dim(ev_data1)[1]
ev_data_long <- pivot_longer(ev_data_long,-ID)
names(ev_data_long)[3] <- "answercode"
ev_data_long <- ev_data_long %>% inner_join(qanda,by=c("name"="code","answercode"))

shap_scores_long <- shap_scores_long %>% inner_join(ev_data_long,c("ID","name","question","type"))
#shap_scores_long <- shap_scores_long %>% inner_join(ev_data_long)
#
shap_scores_long <- shap_scores_long %>% arrange(ID,-SHAP)
#
bias0 <- shap[1,]$SHAP

#write_csv(shap,"~/Policy/AgentBasedModels/shap_summary_alldrivers.csv")
#write_csv(shap_scores_long,"~/Policy/AgentBasedModels/shap_scores_alldrivers.csv")
shapn <- read_csv("~/Policy/AgentBasedModels/shap_summary_alldrivers.csv")
shap_scores_long <- read_csv("~/Policy/AgentBasedModels/shap_scores_alldrivers.csv")
#mean dependence plots
question_codes <- shapn$name[c(2)]
feature_dep <- shap_scores_long %>% filter(name %in% question_codes)
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code %in% question_codes)$answer)
#individual plots
g <- feature_dep %>% ggplot(aes(answercode,value,fill=factor(answer))) + geom_boxplot(size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam", k=1) + facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="Set2")
g <- g + theme(axis.ticks.x = element_blank(),axis.line.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_text(size=14))
g + labs(x="",y="switch utility",title=filter(questions,code==question_codes)$question,subtitle="adoption utility contribution") #+ facet_grid(question~.)

#individual features
question_codes <- shapn$name[3]
feature_dep <- shap_scores_long %>% filter(name %in% question_codes)
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code %in% question_codes)$answer)
feature_dep1 <- feature_dep
feature_dep1$answercode <- as.integer(feature_dep1$answercode)
#feature_dep1 <- feature_dep1  %>% filter( answercode < 8)
g <- feature_dep1 %>% filter(answercode <= 55) %>% ggplot() + geom_boxplot(aes(as.integer(answercode),value,fill=factor(answer)),size=0.05,alpha=0.2) 
g <- g  + geom_smooth(aes(answercode, value),method="gam",formula =  y ~ s(x, bs = "cs", k=5), span=1,colour="grey50",alpha=0.25) #+ facet_grid(question~., scales="free_x") #+ coord_flip()
#g <- g  + geom_smooth(aes(answercode, value),method="loess", span=1,colour="grey50",alpha=0.25)
g <- g + theme_tufte() + scale_fill_viridis_d() + scale_x_continuous(breaks=1:10) #+ scale_y_continuous(limits=c(-0.04,0.04))
#g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="Set2")
g <- g + labs(x="",y="f",title=str_wrap(filter(questions,code==question_codes)$question,60)) + theme(axis.title.y=element_text(size=14), axis.text.y = element_text(size=16),legend.title=element_blank(),plot.title = element_text(size=16))
g <- g  + theme(legend.text=element_text(size=14),legend.direction="vertical",axis.text=element_text(size=14),legend.position = "top")#+ facet_grid(question~.) + theme
g + labs(y="")
ggsave("~/Policy/AgentBasedModels/PHEVs/DECC_report/social_shap.png")

test <- feature_dep %>% filter(answer %in% c("Toyota","Nissan","Volkswagen","Hyundai","Ford","Skoda","BMW","Volvo","Audi","KIA","Renault","Peugeot","Dacia")) %>% group_by(answer) %>% summarise(median=median(value)) %>% arrange(-median)
#write_csv(test,"~/Policy/AgentBasedModels/Paper/carmakes_shap.csv")


mean_shap <- shap_scores_long %>% group_by(name, answercode,answer) %>% dplyr::summarise(shap = mean(value))
#filter variables for model building
var_names <- c("qev32","qev29","q17a_2","qev34")
mean_shap <- mean_shap %>% filter(name %in% var_names)
mean_shap$name <- factor(mean_shap_sub$name, levels = var_names)
g <- ggplot(mean_shap,aes(answercode,shap,colour=name)) + geom_point() + geom_smooth() + theme_economist() + facet_wrap(name~.,ncol=2)
g + ggtitle("ABM model ")
#empirical fits
#"base model" economic and social only
filter(mean_shap,name=="qev29")$shap

prob_econ <- function(answer){
  s <- filter(mean_shap,name=="qev32")$shap
  vals <- c(s[1:3],rep(s[4],7))
  if(!(answer %in% 1:10)) return("bad budget value")
  return(vals[answer])
}
prob_social <- function(answer){
  #
  s <- filter(mean_shap,name=="qev29")$shap
  vals <- s
  ifelse(answer %in% 1:3,return(vals[answer]),return("return bad social value"))
}
prob_distance <- function(answer){
  s <- filter(mean_shap,name=="qev34")$shap
  vals <- s
  ifelse(answer %in% 1:7,return(vals[answer]),return("return bad social value"))
}
prob_enviro <- function(answer){
  s <- filter(mean_shap,name=="q17a_2")$shap
  vals <- s
  ifelse(answer %in% 1:7,return(vals[answer]),return("return bad social value"))
}
prob_calc <- function(question,answer){
  #list utility models
  if(question=="qev32") return(prob_econ(answer))
  if(question=="qev29") return(prob_social(answer))
  if(question=="qev34") return(prob_distance(answer))
  if(question=="q17a_2") return(prob_enviro(answer))
  if(question=="theta") return(NA)
}
prob_calc <- Vectorize(prob_calc)
#theta calculations for base model
shap_thetas <- shap_scores_long %>% filter(!(name %in% var_names)) %>% group_by(ID) %>% dplyr::summarise(name="theta",value=sum(value))
shap_thetas <- shap_thetas %>% mutate(value=value+bias0)
ggplot(shap_thetas) + geom_col(aes(ID,value)) + geom_hline(yintercept=bias0)
shap_thetas$answercode <- NA
#create reduced shap scores 
prob_scores <- shap_scores_long %>% filter(name %in% var_names)
prob_scores <- prob_scores[,c("ID","name","value","answercode")]
prob_scores <- bind_rows(prob_scores,shap_thetas) %>% arrange(ID)
prob_scores <- prob_scores %>% rowwise() %>% mutate(model_value = prob_calc(name,answercode))
prob_scores <- prob_scores %>% mutate(w = value/model_value)
#compute probability distribution parameters a, b 
test0 <- prob_scores %>% group_by(ID) %>% dplyr::summarise(p=sum(value))
prob_scores1 <- prob_scores[,1:3] %>% pivot_wider(values_from = value,names_from = name)
prob_scores1 <- prob_scores1 %>% inner_join(test0)
names(prob_scores1) <- c("ID","p_econ","p_social","p_enviro","p_distance","p_theta","p")

prob_scores1 <- prob_scores1 %>% mutate(a=approx(distrib_params$prob,distrib_params$a,xout=p)$y, b = approx(distrib_params$prob,distrib_params$b,xout=p)$y)
prob_scores1 <- prob_scores1 %>% mutate(a_theta=approx(distrib_params$prob,distrib_params$a,xout=p_theta)$y, b_theta = approx(distrib_params$prob,distrib_params$b,xout=p_theta)$y)

distrib_params <- distrib_params %>% mutate(da_dp = 100*(a-lag(a)), db_dp = 100*(b-lag(b))  )
prob_scores1 <- prob_scores1 %>% mutate(a_econ=p_econ*approx(distrib_params$prob,distrib_params$da_dp,xout=p_theta)$y, b_econ = p_econ*approx(distrib_params$prob,distrib_params$db_dp,xout=p_theta)$y)
prob_scores1 <- prob_scores1 %>% mutate(a_social=p_social*approx(distrib_params$prob,distrib_params$da_dp,xout=p_theta)$y, b_social = p_social*approx(distrib_params$prob,distrib_params$db_dp,xout=p_theta)$y)
prob_scores1 <- prob_scores1 %>% mutate(a_enviro=p_enviro*approx(distrib_params$prob,distrib_params$da_dp,xout=p_theta)$y, b_enviro = p_enviro*approx(distrib_params$prob,distrib_params$db_dp,xout=p_theta)$y)
prob_scores1 <- prob_scores1 %>% mutate(a_distance=p_distance*approx(distrib_params$prob,distrib_params$da_dp,xout=p_theta)$y, b_distance = p_distance*approx(distrib_params$prob,distrib_params$db_dp,xout=p_theta)$y)

#mean of util =  2*a/(a+b)-1
#convert probabilities to expected utilities
prob_scores1 <- prob_scores1 %>% mutate(dU = 2*a/(a+b)-1,theta = 2*a_theta/(a_theta+b_theta)-1, dU_econ = 2/(a_theta+b_theta)*a_econ -2*a_theta/(a_theta+b_theta)^2*b_econ,
                          dU_social = 2/(a_theta+b_theta)*a_social -2*a_theta/(a_theta+b_theta)^2*b_social,
                          dU_enviro = 2/(a_theta+b_theta)*a_enviro -2*a_theta/(a_theta+b_theta)^2*b_enviro,
                          dU_distance = 2/(a_theta+b_theta)*a_distance -2*a_theta/(a_theta+b_theta)^2*b_distance)

utility0 <- prob_scores1[,c("ID","dU_econ","dU_social","dU_enviro","dU_distance","theta","dU")]
names(utility0) <- c("ID","qev32","qev29","q17a_2","qev34","theta","dU")

g <- ggplot(utility0, aes(-theta)) + geom_histogram(alpha=0.5,binwidth = 0.005,fill="grey50") + geom_vline(xintercept = bias0,linetype="dotted") + geom_histogram(aes(dU),binwidth = 0.005,fill="orange",alpha=0.7)
g <- g + geom_histogram(aes(dU-theta),fill="red" , alpha=0.5,binwidth = 0.005) + theme_economist()
g <- g + labs(x="",y="Count",title="EV adoption utilities")
g
#ggsave("~/Policy/AgentBasedModels/adoption_util_distribution_from_survey.png")

utility_long <- utility0 %>% pivot_longer(-ID)
names(utility_long)[3] <- "util"

ev_data2 <- ev_data1[,var_names]
ev_data2$ID <- 1:dim(ev_data2)[1]
ev_data2 <- ev_data2 %>% pivot_longer(-ID,values_to="answercode")
utility_long <- left_join(utility_long, ev_data2)

mean_utilities <- utility_long %>% group_by(name,answercode) %>% dplyr::summarise(mean_util=mean(util))
mean_util <- mean_utilities %>% filter(name %in% var_names)
mean_util$name <- factor(mean_util$name, levels = var_names)
g <- ggplot(mean_util,aes(answercode,mean_util,colour=name)) + geom_point() + geom_smooth() + theme_economist() + facet_wrap(name~.,ncol=2)
g + ggtitle("ABM model ")

utility_long <- utility_long %>% inner_join(mean_utilities,by=c("name","answercode"))
utility_long <- utility_long %>% mutate(w=util/mean_util)
utility_long <- utility_long %>% filter(name != "dU")
#
g <- utility_long %>% ggplot(aes(w,fill=name)) + geom_histogram(alpha=0.5) + facet_grid(.~name)+scale_x_continuous(limits=c(-0.5,2.5))
g <- g + theme_economist() + labs(x="weight",y="Count",title="Distribution of agent weights") + scale_fill_brewer(type="qual")
g
ggsave("~/Policy/AgentBasedModels/abm_weight_distribution_from_survey.png")
#
utility %>% filter(name != "qev34") %>% ggplot(aes(w)) + geom_histogram() + facet_grid(name~., scales="free_x")
#
range(filter(utility, name=="q17a_2")$w)
#u_tot <- utility %>% group_by(ID) %>% dplyr::summarise(utot=sum(util)) 
#du_tot <- utility %>% filter(name != "theta") %>% group_by(ID) %>% dplyr::summarise(du=sum(util))
#thetas <- utility %>% filter(name == "theta")
#thetas <- thetas[,c(1,3)]
#names(thetas)[2] <- "theta"
#inner_join(u_tot,du_tot) %>% inner_join(thetas) %>% ggplot() + geom_histogram(aes(utot),fill="orange",alpha=0.5) + geom_histogram(aes(utot-du),fill="red",alpha=0.5) + geom_histogram(aes(theta+du+0.1),fill="grey50",alpha=0.5)

#write_csv(utility_long, "~/Policy/AgentBasedModels/utility_model_params_long.csv")
utility_wide <- dplyr::select(utility_long,-answercode,-util) %>% pivot_wider(names_from =c(name),values_from = c(w,mean_util))

utility_wide <- bind_cols(utility_wide,ev_data1)
names(utility_wide)[7:11] <- c("dU_qev32","dU_qev29","dU_q17a_2","dU_qev34","theta")
#write_csv(utility_wide, "~/Policy/AgentBasedModels/utility_model_params_wide.csv")

utility_wide %>% ggplot(aes(qev32,dU_qev32)) + geom_line()
utility_wide %>% ggplot(aes(qev29,dU_qev29)) + geom_line()
utility_wide %>% ggplot(aes(q17a_2,dU_q17a_2)) + geom_line()
utility_wide %>% ggplot(aes(qev34,dU_qev34)) + geom_line()

#utility plots


#filter variables for model building
var_names <- c("qev32","qev29","q17a_2","qev34")
mean_shap <- mean_shap %>% filter(name %in% var_names)
mean_shap$name <- factor(mean_shap_sub$name, levels = var_names)
#write_csv(mean_shap,"~/Policy/AgentBasedModels/empirical_utilities.csv")
g <- ggplot(mean_shap,aes(answercode,shap,colour=name)) + geom_point() + geom_smooth() + theme_economist() + facet_wrap(name~.,ncol=2)
g + ggtitle("ABM model ")




ggplot(test1,aes(ID,dU_enviro)) + geom_point()

test2 <- test1
#compute probabilty that dU_rand > 0 
test2 <- test2 %>% mutate(adopt_prob = 1-pbeta_util(0,a,b),adopt_prob_theta = 1-pbeta_util(0,a_theta,b_theta))
test2 <- test2 %>% mutate(adopt_prob = 1-pbeta_util(0,a,b),adopt_prob_norm = 1-pnorm(0,mean=theta,sd=0.15))

ggplot(test2, aes(p,adopt_prob_norm)) + geom_point()
ggplot(test2, aes(ID,theta)) + geom_col()

x0 <- rbeta_util(10000,17.0,24.9)
x1 <- rbeta_util(10000,17.0-0.526,24.9+0.202)

ggplot(test2)  + geom_point(aes(ID,dU)) + geom_point(aes(ID,dU_rand),colour="red")

u_cutoff <- 0.035
shap_main <- shap_scores_long %>% filter(abs(SHAP)>u_cutoff)
shap_main$question <- factor(shap_main$question, levels=filter(shap,SHAP>u_cutoff)$question)
shap_main$name <- factor(shap_main$name, levels=filter(shap,SHAP>u_cutoff)$name)
std1 <- function(x) {
  return((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                       min(x, na.rm = TRUE)))
}

shap_main <- shap_main %>% group_by(name) %>% mutate(std_feature = std1(answercode))

g <- shap_main %>% ggplot(aes(name,value,colour=std_feature)) + ggforce::geom_sina(method="counts",size=0.2,alpha=0.5,maxwidth = 0.5) + coord_flip() + theme_minimal()
g <- g + scale_x_discrete(labels=str_wrap(levels(shap_main$question),30))
g <- g + scale_color_gradient(name="feature value",low = "#FFCC33", high = "#6600CC", 
                              ```breaks = c(0, 1), labels = c(" Low", "High "), 
                              guide = guide_colorbar(barwidth = 12, barheight = 0.3)) + theme_minimal()
g <- g + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
               legend.position = "bottom", legend.title = element_text(size = 10), 
               legend.text = element_text(size = 8), axis.title.x = element_text(size = 10))
g + geom_text(data = filter(shap_main, ID==2), aes(name,-Inf, label = sprintf("%.3f", mean)), size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold",colour="black")

financial.features <- c("qev32","qev34","q17a_5","qev25","qev30","qev31","qev25")
social.features <- c("qev29","q15")
risk.features <- c("q17b","q15")
environmental.features <- c("q17a_2","q17a_1")
funcional.features <- 
  
#feature contributions to switching utility
#mileage
feature_dep <- shap_scores_long %>% filter(name %in% c("qev34"))
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code=="qev34")$answer)

g <- feature_dep %>% ggplot(aes(answercode,value,fill=factor(answer))) + geom_boxplot(size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam", k=1) + facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="Reds")
g <- g + theme(axis.text.x = element_blank())
g + labs(x="",y="",title=filter(questions,code=="qev34")$question,subtitle="adoption utility contribution")
#ggsave("~/Policy/AgentBasedModels/mileage_switching_utility.png")
#budget
feature_dep <- shap_scores_long %>% filter(name %in% c("qev32"))
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code=="qev32")$answer)

g <- feature_dep %>% ggplot(aes(answercode,value,fill=factor(answer))) + geom_boxplot(size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam", k=1) + facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="RdGy")
g <- g + theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())
g1 <- g + labs(x="",y="",title=filter(questions,code=="qev32")$question,subtitle="adoption utility contribution")
#ggsave("~/Policy/AgentBasedModels/budget_switching_utility.png")
budget_distrib <- ev_data %>% createTable2("qev32")
budget_distrib <- budget_distrib %>% mutate(p=n/sum(n))
g2 <- budget_distrib %>% ggplot(aes(qev32,100*p)) + geom_col(fill="grey30",alpha=0.5,width=0.7) 
g2 <- g2 + theme_economist() + labs(y="%",x="") + theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())
g1 + g2 + plot_layout(nrow = 2, heights = c(3, 1))
#time to purchase
feature_dep <- shap_scores_long %>% filter(name %in% c("qev30"))
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code=="qev30")$answer)

g <- feature_dep %>% ggplot(aes(answercode,value,fill=factor(answer))) + geom_boxplot(size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam", k=1) #+ facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="RdGy")
g <- g + theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())
g1 <- g + labs(x="",y="",title=filter(questions,code=="qev30")$question,subtitle="adoption utility contribution")

timing_distrib <- ev_data %>% createTable2("qev30")
timing_distrib <- timing_distrib %>% mutate(p=n/sum(n))
g2 <- timing_distrib %>% ggplot(aes(qev30,100*p)) + geom_col(fill="grey30",alpha=0.5,width=0.7) 
g2 <- g2 + theme_economist() + labs(y="%",x="") + theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())
g1 + g2 + plot_layout(nrow = 2, heights = c(3, 1))
#ggsave("~/Policy/AgentBasedModels/timetopurchase_switching_utility.png")

#social
feature_dep <- shap_scores_long %>% filter(name %in% c("qev29"))
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code=="qev29")$answer)

g <- feature_dep %>% ggplot(aes(answercode,value,fill=factor(answer))) + geom_boxplot(size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam", k=1) + facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="Set2")
g <- g + theme(axis.ticks.x = element_blank(),axis.line.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_text(size=14))
g + labs(x="",y="switch utility",title=filter(questions,code=="qev29")$question,subtitle="adoption utility contribution")
#ggsave("~/Policy/AgentBasedModels/social_switching_utility.png")

#environmental
feature_dep <- shap_scores_long %>% filter(name %in% c("q17a_2"))
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code=="q17a_2")$answer)

g <- feature_dep %>% ggplot(aes(answercode,value,fill=factor(answer))) + geom_boxplot(size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam", k=1) #+ facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="Set2")
g <- g + theme(axis.ticks.x = element_blank(),axis.line.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_text(size=14))
g <- g + labs(x="",y="switch utility",title=filter(questions,code=="q17a_2")$question,subtitle="adoption utility contribution")

enviro_distrib <- ev_data %>% createTable2("q17a_2")
enviro_distrib <- enviro_distrib %>% mutate(p=n/sum(n))
g2 <- enviro_distrib %>% ggplot(aes(q17a_2,100*p)) + geom_col(fill="grey30",alpha=0.5,width=0.7) 
g2 <- g2 + theme_economist() + labs(y="%",x="") + theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank())
g + g2 + plot_layout(nrow = 2, heights = c(3, 1))
#ggsave("~/Policy/AgentBasedModels/enviro_switching_utility.png")


#risk avesrsion and threshold
risk_distrib <- ev_data %>% createTable2("q17b")
risk_distrib <- risk_distrib %>% mutate(p=n/sum(n))
risk_distrib$Q17B <- factor(risk_distrib$Q17B,levels=risk_distrib$Q17B)
risk_distrib %>% ggplot(aes(Q17B,p)) + geom_col()
#risk_aversion_scores s = 0.1,0.3,0.5,0.7,0.9
#model theta = a+ (1-a)*s 

#individual utility

id <- 300 #interesting cases 540,9,  300
print(paste("predicted", sum(filter(shap_scores_long,ID==id)$value)))
test <- filter(shap_scores_long,ID==id) %>% arrange(-value)
#test <- test %>% inner_join(ev_data_long)

#test <- test %>% inner_join(qanda,c("name"="code","answercode"="answercode","question"="question","type"="type"))
test$name <- factor(test$name,levels=test$name)
test$question <- factor(test$question,levels=test$question)

#test %>% ggplot(aes(1,value,colour=factor(sign(value)))) + geom_col(position="stack") + coord_flip()
test1 <- filter(test, abs(value) > 0.002)
test1$index <- 1:dim(test1)[1]

g <- test1 %>% ggplot(aes(index,value,fill=factor(sign(value)))) + geom_col()+ coord_flip() + theme_minimal()
g <- g + scale_x_continuous(breaks=1:dim(test1)[1],labels=str_wrap(test1$question,60),sec.axis=dup_axis(labels=str_wrap(test1$answer,60))) #+ geom_text(aes(label=answer),size=4)
g <- g + labs(x="",y="SHAP score",title=paste("ID",id,"How likely is it that your next car is an electric car?"), subtitle = paste(filter(qanda,code=="qev33",answercode==round(test1$pred[1]))$answer,":",round(test1$pred[1],1)))
g + theme(legend.position="none",plot.title = element_text(hjust=0.5)) 
ggsave(paste("~/Policy/AgentBasedModels/ID=",id,"_decisionPlot.png",sep=""))
#filter(shap_scores_long,ID==id) %>% ggplot(aes(name,value,fill=type),colour="white") + geom_col() + coord_flip()

#interactions
shap_scores2 <- predict(bst, as.matrix(ev_data[,-dim(ev_data)[2]]), predcontrib = FALSE,predinteraction=TRUE, approxcontrib = F) %>% as_tibble()

shap_scores2$ID <- 1:dim(shap_scores2)[1]
shap_scores2 <- shap_scores2 %>% pivot_longer(-ID)

shap2 <- shap_scores2 %>% group_by(name) %>% summarise(SHAP=mean(abs(value))) %>% arrange(-SHAP)

#dominant interaction effects is qev30-qev29 i.e. when do you expect overlaps with do you know people
# and q17c-qev32 - budget & willingness to defer grat
shap2 %>% filter(str_detect(name,"qev30"))

shap_scores2 %>% filter(name %in% c("qev29.qev29","qev29.qev30","qev30.qev30")) %>% ggplot(aes(name,value)) + geom_point()


dplyr::select(shap_scores2,dplyr::contains("qev30")) %>% dplyr::select(contains("qev32"))


###########################################
# contingency table based adoption curve
# adoption curve from survey
# based on new cars only
########################################

#probabilty model
epsilon <- 0.4 #less than 1
dp_min <- 0.1 #between 0 and 0.1
probs <- epsilon*(c(0.1,0.3,0.5,0.7,0.9)-dp_min)
#probs <- c(0,0,0.05,0.1,0.2) #matched 2019
#calibrate probabilites using new car buyers only
#new cars buyers only
q <- c("qev31","qev30","q12a","qev33")
filter(questions, code %in% q)

test <- createTable2(q=q) %>% filter(qev31==1)
sum(test$n)
adopt <- filter(test,q12a==0)
adopt <- adopt %>% mutate(p=probs[qev33])
#EV share of new car market


adoptcurve <- adopt %>% group_by(qev30,QEV30) %>% summarise(dN = sum(p*n))
adoptcurve$N <- cumsum(adoptcurve$dN)
adoptcurve$t <- c(0.5,2,4,7.5)
adoptcurve <- pivot_longer(dplyr::select(adoptcurve,-"dN"), -c("qev30","QEV30","t"),values_to = "N",names_to = "model")


g <- ggplot(adoptcurve, aes(t+2019,50/sum(adopt$n)*100+N/sum(adopt$n)*100)) + geom_col(width=c(1,2,2,5),colour="white",position="identity",alpha=0.3) 
g <- g + theme_economist() + scale_x_continuous(breaks=c(2019,2021,2023)) + geom_hline(yintercept=50/924*100,linetype="dotted")
g <- g + scale_fill_manual(values=c("red","orange","pink")) + labs(x="year",y="%", title="EV penetration from survey")
g
#ggsave("~/Policy/AgentBasedModels/implied_adoption.png")

#EV new car market share
#fraction of ev buyers from all new cars sold
ev_share <- adopt %>% group_by(qev30,QEV30) %>% summarise(ev_fraction = sum(p*n)/sum(n))
ev_share$year <- c(2018.5,2021,2023,2025)
ev_share1 <- tibble(year=2019:2025, ev_share=approx(x=ev_share$year,y=ev_share$ev_fraction,xout=2019:2025)$y)

library(csodata)
toc <- cso_get_toc()
cso_search_toc("vehicles lic") %>% as_tibble()

newcars <- cso_get_data("TEA17") %>% as_tibble() %>% filter(Type.of.Vehicle.Registration=="New Private Cars")
newcars <- dplyr::select(newcars, -1,-2,-4,-5) %>% pivot_longer(-Type.of.Fuel,names_to="year")
newcars$year <- as.integer(newcars$year)
ggplot(newcars, aes(year,value,colour=Type.of.Fuel)) + geom_line()


newcars <- newcars %>% pivot_wider(values_from="value","names_from"="Type.of.Fuel")

g <- ggplot(newcars, aes(year,100*(Electric+`Petrol or Diesel plug-in hybrid electric`)/`All fuel types`)) + geom_point() + geom_line()
g + theme_economist()

ev_share0 <- newcars %>% group_by(year) %>% summarise(ev_share=(Electric+`Petrol or Diesel plug-in hybrid electric`)/`All fuel types`)
ev_share0$category <- "historical"
ev_share1$category <- "implied"
ev_share <- bind_rows(ev_share0,ev_share1)
g <- ggplot(ev_share, aes(year,100*ev_share,colour=category)) + geom_point(size=2) + geom_line(size=1)
g <- g + theme_economist() + labs(x="",y="%", title = "New electric cars licensed", subtitle = paste("Survey Likert probabilities",paste(probs,collapse = " ")))
g + scale_x_continuous(breaks=2015:2025) + theme(legend.title = element_blank()) + scale_colour_manual(values=c("orange","grey50"))
ggsave("~/Policy/AgentBasedModels/newevslicensed.png")
############################################
# predict who already owns an EV
#############################################

ev_data <- survey_ev %>% filter(qev30!=5)  #dimension 924 = 206+718
dim(ev_data) #924 drivers nor currently driving an ev or potential drivers
#include q12a remove q2v21 (fuel type)
ev_data <- ev_data[,c("age","gender","class","region","qc1","q7","q17b","q17c","q17a_1","q17a_2","q17a_3","q17a_4","q17a_5","qev20","qev22","qev23","qev24","qev25","qev26","qev27","qev28","qev29","qev30","qev31","qev32","qev34","qev35","q15","q16","qj","qk","qh","qg","qf","q12a")] 
dim(ev_data)
#car owners only ie. switchers
#ev_data <- filter(ev_data, qev20==1)
dim(ev_data)
ev_data <- ev_data %>% drop_na("qev33")
dim(ev_data)
#ev_data <- ev_data %>% filter(qi != 15)
#dim(ev_data)
#ev_data <- ev_data %>% select(-"qev20")

#recategorisations qev23, qev29, q16
qev29_recat <-function(i){
  #recategorises qev29 (know ev driver) to ordinal
  if(i==3) return(1)
  if(i==1) return(2)
  if(i==2) return(3)
}
qev29_recat <-  Vectorize(qev29_recat)
q16_recat <- function(i){
  ifelse( i < 5,return(i+1),return(6))
}
q16_recat <- Vectorize(q16_recat)

#quantile(survey_ev$qev23,na.rm=T)
qev23_recat <- function(i){
  
  if(is.na(i)) return(NA)
  if( i <= 2006) return(1)
  if( i <= 2010) return(2)
  if( i <= 2015) return(3)
  if( i <= 2018) return(4)
  
}
qev23_recat <- Vectorize(qev23_recat)
#recategorise qanda
qanda[which(qanda$code=="qev29"),]$answercode <- qev29_recat(1:3)
qanda <- qanda %>% arrange(code,answercode) #order
questions <- distinct(qanda[,c(1,2,5)])

qev32recat <- function(i){
  if(i < 10) return(i)
  if(i >= 10) return(10)
  
}
qev32recat <- Vectorize(qev32recat)

#split q15 into two categories - technophile/non-technophile  &  follower/non-follower 
ev_data <- ev_data %>% mutate(q16=q16_recat(q16), qev23 = qev23_recat(qev23), qev29 = qev29_recat(qev29), qev32=qev32recat(qev32))


#ev_data$qev33 <- threecat(ev_data$qev33)
dim(ev_data)
ev.train <- xgb.DMatrix(as.matrix(ev_data[,-dim(ev_data)[2]]),label=as.vector(ev_data$q12a), missing=NA)

paramlist <- list(booster="gbtree",
                  tree_method = "exact",
                  eta=0.02,
                  max_depth=5,
                  gamma=0,
                  subsample=1,
                  colsample_bytree = 0.8,
                  #objective="reg:squarederror",
                  #eval_metric="rmse"
                  objective=amo.fairobj2
                  #feval = amm.mae
)

bst <- xgb.cv(params=paramlist,ev.train,nrounds=400,nfold=5)

test <- bst["evaluation_log"] %>% as.data.frame() %>% as_tibble()
test[,4] %>% min()
nopt <- test[,4][[1]] %>% as.numeric() %>% which.min()
print(paste("optimal nrounds",nopt))


bst.df <- bst["evaluation_log"] %>% as_tibble()
bst.df <- bst.df[[1]]
names(bst.df) <- c("iter","train_rmse_mean","train_rmse_std","test_rmse_mean","test_rmse_std")
ggplot(bst.df, aes(iter,train_rmse_mean)) + geom_point() + geom_point(aes(iter,test_rmse_mean),colour="red") + geom_vline(xintercept = nopt,linetype="dotted")

bst <- xgboost(params=paramlist,ev.train,nrounds=1.5*nopt,feval=amm_mae)
#xgb.save(bst,fname="model.xgboost")
#bst <- xgboost(params=list(eta=0.05,max_depth=6,subsample=1),as.matrix(ev_data[,-20]),label=as.vector(ev_data$qev33-1),nrounds=200)
pred.bst <- predict(bst,as.matrix(ev_data[,-dim(ev_data)[2]]),reshape=T)#
pred.int <- sapply(pred.bst, function(x) which.min((x-0:1)^2))

pred <- tibble(n = 1:dim(ev_data)[1],prediction.actual=pred.bst,prediction=pred.int-1,actual=ev_data$q12a)
unique(pred$prediction)
#ggplot(pred, aes(prediction)) + geom_histogram() + geom_histogram(aes(actual),fill="red",alpha=0.5)
g <- ggplot(pred, aes(factor(actual),prediction.actual, group=actual)) + geom_boxplot(fill="orange")
g <- g + theme_economist() + scale_x_discrete(breaks=0:1,labels=filter(qanda,code=="q12a")$answer)
g <- g+labs(x="",y="prediction",title="model skill",subtitle="Do you own an EV?")
g + theme(axis.title = element_text(size=14))
#ggsave("~/Policy/AgentBasedModels/xgboost_qev33_skill.png")

verify(pred$prediction,pred$actual,frcst.type = "cat", obs.type="cat")$gs

questions$code <- tolower(questions$code)


#xgb.plot.tree(model=bst)
questions$code <- tolower(questions$code)
shap_values <- shap.values(bst,as.matrix(ev_data[,-dim(ev_data)[2]]))
shap_scores <- shap_values$shap_score %>% as_tibble()
shap <- enframe(shap_values$mean_shap_score) %>% left_join(questions,c("name"="code"))
#shap[8,"question"] <- "How willing are you to take risk?" 
#shap[7,"question"] <- "How willing are you to defer reward?"
#shap[3,"question"] <- "How willing are you to adopt unfamiliar technology?"
#shap[12,"question"] <- "How concerned are you about the environment?"
shap$question <- factor(shap$question, levels=rev(shap$question))
shap$name <- factor(shap$name, levels=rev(shap$name))

g <- ggplot(shap, aes(name,value/sum(value),fill=type)) + coord_flip() + geom_col() + theme_economist()# + scale_x_discrete(limits=rev(shap$question[1:14])) #+ geom_text(aes(label=round(value,3)),hjust=1)
g <- g + ylim(0,0.18) + ggtitle("\"Do you drive an EV\"") + xlab("") +ylab("") + theme(plot.title = element_text(hjust=1.2),plot.subtitle = element_text(size=12,face="bold",hjust=-2.5))
g

shap_scores1 <- predict(bst, as.matrix(ev_data[,-dim(ev_data)[2]]), predcontrib = TRUE, approxcontrib = F) %>% as_tibble()
#importance
shap_scores1$ID <- 1:dim(shap_scores1)[1]
shap_scores_long <- pivot_longer(shap_scores1,-ID)

preds <- shap_scores_long %>% group_by(ID) %>% summarise(pred=sum(value))
preds$actual <- ev_data$q12a
shap_scores_long <- shap_scores_long  %>% inner_join(preds,by="ID")
shap_scores_long$pred <- shap_scores_long$pred + 0.05349302
names(shap)[2] <- "mean"
shap_scores_long <- shap_scores_long %>% inner_join(shap,by="name")
ev_data_long <- ev_data
ev_data_long$ID <- 1:dim(ev_data)[1]
ev_data_long <- pivot_longer(ev_data_long,-ID)
names(ev_data_long)[3] <- "answercode"
ev_data_long <- ev_data_long %>% inner_join(qanda,by=c("name"="code","answercode"))

shap_scores_long <- shap_scores_long %>% inner_join(ev_data_long,c("ID","name","question","type"))
#shap_scores_long <- shap_scores_long %>% inner_join(ev_data_long)
#summary plot
shap_scores_long <- shap_scores_long %>% arrange(ID,-mean)


question_codes <- shap$name[9]
feature_dep <- shap_scores_long %>% filter(name %in% question_codes)
feature_dep$answercode <- factor(feature_dep$answercode , levels = 1:length(unique(feature_dep$answercode)))
feature_dep$answer <- factor(feature_dep$answer , levels = filter(qanda,code %in% question_codes)$answer)

g <- feature_dep %>% ggplot(aes(answercode,value,fill=factor(answer))) + geom_boxplot(size=0.05,alpha=0.5) + geom_smooth(data=feature_dep, aes(as.integer(answercode),value),method="gam", k=1) + facet_grid(name~., scales="free_x") #+ coord_flip()
g <- g + theme_economist() + scale_fill_brewer(name="",labels=str_wrap(levels(feature_dep$answer),30),palette="Set2")
g <- g + theme(axis.ticks.x = element_blank(),axis.line.x=element_blank(),axis.text.x=element_blank(),axis.title.y=element_text(size=14))
g + labs(x="",y="switch utility",title=filter(questions,code==question_codes)$question,subtitle="adoption utility contribution") #+ facet_grid(question~.)

####################################################################
# model new car buyers only
####################################################################


library(fastshap)
library(xgboost)
library(kableExtra)
bst <- xgb.load("C:/Users/Joe/Documents/model.xgboost")


ev_data <- read_csv("~/Policy/AgentBasedModels/modeldata_allcarbuyers.csv")
X <- as.matrix(ev_data[,-dim(ev_data)[2]])
shap <- explain(bst,X=X,pred_wrapper = predict)
dat <- force_plot(shap[674,],display="html")

save_html(dat,file="~/Policy/AgentBasedModels/forceplot.html")

kable(filter(society, ID==674)) %>% save_kable(file="~/Policy/AgentBasedModels/agent674.png")


#NUTS3
admin <- read_csv("~/Policy/AgentBasedModels/ireland_admin2.csv")
survey_ev %>% createTable2(c("qc")) %>% inner_join(admin) %>% group_by(NUTS3) %>% summarise(n_nuts=sum(n)) %>% mutate(freq=n_nuts/sum(n_nuts))
#the survey is geographically balanced
survey_ev %>% createTable2(c("qc")) %>% inner_join(admin) %>% ggplot(aes(population,n)) + geom_point()

#######################################
# demographics
#####################################
#occupation
survey_ev %>% createTable2(c("qh")) %>% mutate(freq=n/sum(n)) %>% arrange(-freq) #only 20 (1.5%) are farmers!

#######################################
# car ownership characteristics
######################################
#dominant mode of transport
survey_ev %>% createTable2(c("qev35")) %>% mutate(freq=n/sum(n))
#car ownership
survey_ev %>% createTable2(c("qev20")) %>% mutate(freq=n/sum(n))
#car ownvership vs dominant mode of transport: 71% own a car but only for 71% of those is it the primary model of transport
survey_ev %>% createTable2(c("qev20","qev35")) %>% group_by(qev20) %>% mutate(freq=n/sum(n))
#ownnership vs likelihood next car is electric
survey_ev %>% createTable2(c("qev20","qev33")) %>% group_by(qev20) %>% mutate(freq=n/sum(n))
#fuel type vs likelihood next car is electric
survey_ev %>% createTable2(c("qev21","qev33")) %>% group_by(qev21) %>% mutate(freq=n/sum(n)) %>% filter(qev21==4)
#new vs old car buyers  30% are new car buyers
survey_ev %>% createTable2(c("qev31")) %>% mutate(freq=n/sum(n))
#old vs new and likliehood next car is electric 
survey_ev %>% createTable2(c("qev31","qev33")) %>% group_by(QEV31) %>% mutate(freq=n/sum(n))
#year of reg
survey_ev %>% createTable2(c("qev23")) %>% ggplot(aes(qev23,n)) + geom_col()
#age of car when disposed by current owner
survey_ev %>% mutate(age_at_disposal = 2018+qev30-qev23) %>% filter(age_at_disposal <=40) %>% ggplot(aes(age_at_disposal)) + geom_histogram()
#
#new/old vs socio-economic class
survey_ev %>% createTable2(c("qh","qev31")) %>% group_by(QH) %>% mutate(freq=n/sum(n)) %>% ggplot(aes(qh,n,fill=QEV31)) + geom_col()
#car brands
test <- survey_ev %>% createTable2(c("qev22")) %>% mutate(freq=n/sum(n)) %>% arrange(-freq)
top_brands <- test$qev22[1:20] #91% of all cars
#car brand vs occupation heatmap NOT USEFUL
survey_ev %>% filter(qev22 %in% top_brands) %>% createTable2(c("class","qev22")) %>% group_by(QEV22) %>% mutate(freq=n/sum(n)) %>% ggplot(aes(QEV22,CLASS,fill=freq)) + geom_tile() + scale_fill_viridis()
#car registration vs social class
survey_ev %>% filter(qev23> 2000) %>% createTable2(c("qh","qev23")) %>% group_by(QH) %>% mutate(freq=n/sum(n)) %>% ggplot(aes(qev23,freq,fill=QH)) + geom_col() + facet_grid(QH~.)
#mileage
test <- survey_ev %>% createTable2(c("qev25")) %>% mutate(freq=n/sum(n))
test <- test %>% mutate(QEV25 = str_remove(QEV25,"km"))
test <- test %>% rowwise() %>% mutate(QEV25mid = 0.5*(as.numeric(strsplit(QEV25,"-")[[1]][1])+as.numeric(strsplit(QEV25,"-")[[1]][2]) ))
test$QEV25mid[14] <- 100
test %>% ggplot(aes(QEV25mid,n)) + geom_col()
#driving distance vs likelihood of adoption
survey_ev %>% createTable2(c("qev25","qev33")) %>% group_by(QEV25) %>% mutate(freq=n/sum(n)) %>% ggplot(aes(QEV25,freq,fill=QEV33)) + geom_col(position = "stack")

#average 13,700km mean
#stated future 9,150km mean

#############################
#technology risk aversion
#################################
questions %>% filter(code=="qev33")
#
survey_ev %>% createTable2(c("q17a_1","q15")) %>% filter(q15 != 6)  #%>% mutate(freq=n/sum(n)) %>% ggplot(aes(q17a_1,q15)) + geom_tile(aes(fill=freq))

createTable2(c("qev20","qev33")) %>% mutate(freq=n/sum(n))# %>% filter(qev33 == 4)

#practicality
createTable2(q=c("qev34","qev33")) %>% mutate(freq=n/sum(n)) %>% filter(qev33 == 4)

########################################
#social network/influence structure
#########################################
#global degree distrubution
test <- survey_ev %>% createTable2(c("q16")) %>% mutate(freq=n/sum(n)) #%>% ggplot(aes(q16,freq)) + geom_col()
#fit to exponential
fdegree <- function(p){
  df <- test %>% summarise(error=sum((freq-p[1]*exp(-p[2]*q16))^2))
  return(as.numeric(df[1,1]))
}
fitf <- nlm(fdegree,p=c(1,1))
test <- test %>% mutate(fit = fitf$estimate[1]*exp(-fitf$estimate[2]*q16))
test %>% ggplot(aes(q16,freq)) + geom_col() + geom_point(aes(q16,fit))
test %>% summarise(degree=weighted.mean(q16,freq), degree.fit = weighted.mean(q16,fit))

#network degree distribution by area type:
survey_ev %>% createTable2(c("qc1","q16")) %>% mutate(freq=n/sum(n)) %>% ggplot() + geom_col(aes(q16,freq,fill=factor(QC1))) + facet_grid(QC1~.)
#network degree distribution by region
survey_ev  %>% createTable2(c("region","q16")) %>% group_by(region) %>% summarise(REGION=REGION[1],degree=weighted.mean(q16,n))
#network degree by NUTS region: DUblin is most connected, Border is least
nuts_degrees <- survey_ev %>% createTable2(c("qc","q16")) %>% inner_join(admin) %>% group_by(NUTS3,q16) %>% summarise(n=sum(n)) %>% summarise(degree=weighted.mean(q16,n))
nuts_degrees %>% ggplot(aes(NUTS3,degree)) + geom_col()
#network degree distribution by gender: no significant difference
survey_ev %>% createTable2(c("gender","q16")) %>% mutate(freq=n/sum(n)) %>% ggplot() + geom_col(aes(q16,freq,fill=factor(GENDER))) + facet_grid(GENDER~.)
survey_ev  %>% createTable2(c("gender","q16")) %>% summarise(degree=weighted.mean(q16,n))
#network degree distribution by age
survey_ev  %>% createTable2(c("age","q16")) %>% summarise(AGE=AGE[1],degree=weighted.mean(q16,n))
#mean network degree by age and class
survey_ev  %>% createTable2(c("age","class","q16")) %>% summarise(AGE=AGE[1],CLASS=CLASS[1],degree=weighted.mean(q16,n))
#network degree distribution by profession
survey_ev  %>% createTable2(c("qh","q16")) %>% summarise(QH=QH[1],degree=weighted.mean(q16,n))


g <- survey_ev %>% createTable2(c("region","qev20","q16")) %>% mutate(freq=n/sum(n)) %>% ggplot() + geom_col(aes(q16,freq,fill=factor(REGION))) + facet_grid(REGION~QEV20)  + scale_fill_viridis(name="",discrete=T)
g + xlab("") + ggtitle("social influence network degree distribution vs car ownership")
#ggsave("~/Policy/AgentBasedModels/degree_distribution.png")
g <- survey_ev %>% createTable2(c("qc1","qev20","q16")) %>% mutate(freq=n/sum(n)) %>% ggplot() + geom_col(aes(q16,freq,fill=factor(QC1))) + facet_grid(QC1~QEV20)  + scale_fill_viridis(name="",discrete=T)
g + xlab("") + ggtitle("social influence network degree distribution vs car ownership")

#%>% summarise(Fuel=QEV21[1],mean=weighted.mean(q16,n)) %>% ggplot(aes(Fuel,mean))
#

#size of social network vs no of people qev29
#no relationship
createTable2(c("qev29","q16")) %>% ggplot() + geom_boxplot(aes(factor(QEV29),q16))
#
createTable2(c("qev29","qev33"))[,1:3] %>% pivot_wider(names_from="qev33",values_from = n)
#logistic regress

#26% of people know at least one person who has an ev
createTable2(c("qev29")) %>% mutate(freq=n/sum(n))
#1.5% of people own an ev
createTable2(c("qev21")) %>% mutate(freq=n/sum(n))


######################
# spatial
######################

irl <-getData("GADM", country="IE", level=1) %>% st_as_sf()
names(irl)[4] <- "QC"
irl[which(irl$QC == "Laoighis"),"QC"] <- "Laois"

admin <- read_csv("~/Policy/AgentBasedModels/ireland_admin2.csv")
irl <- irl %>% inner_join(admin,by="QC")
#create NUTS3 map
#irl1 <- irl %>% group_by(NUTS3) %>% summarise(geometry=st_union(geometry))
#st_write(irl1,"~/Policy/AgentBasedModels/nuts3.shp")
irl1 <- st_read("~/Policy/AgentBasedModels/nuts3.shp")
test <- test %>% inner_join(admin) #%>% summarise(n=sum(n))
test1 <- test %>% group_by(QEV33,NUTS3) %>% summarise(n=sum(n))
#
test1 <- test %>% group_by(QEV35,NUTS3) %>% summarise(n=sum(n))


ggplot(irl2) + geom_sf()

irl1 <- irl1 %>% inner_join(test1,by="NUTS3")
#create new admin map
irl1 <- irl1 %>% group_by(NUTS3) %>% mutate(freq=n/sum(n))
irl1$QEV33 <- factor(irl1$QEV33, levels=c("Very unlikely","Unlikely","Neutral","Likely","Very likely"))
irl1$QEV35 <- factor(irl1$QEV35, levels=c("Private car or  van","Bus","Train or DART or LUAS","Cycling","Walking"))

g <- ggplot(irl1) + geom_sf(aes(fill=freq)) + facet_grid(.~QEV35) + scale_fill_viridis_c() + theme_map()
g + ggtitle("Primary mode of transport") + theme(legend.position="top")
#ggsave("~/Policy/AgentBasedModels/regionalmodeoftransport.png")


