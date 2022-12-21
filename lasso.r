library(LKT)
library(glmnet)
library(tidyverse)
library(fastDummies)
val<-largerawsample

#clean it up
val$KC..Default.<-val$Problem.Name
# make it a datatable
val = setDT(val)

#make unstratified folds for crossvaldiations
val$fold<-sample(1:5,length(val$Anon.Student.Id),replace=T)

# get the times of each trial in seconds from 1970
val$CF..Time.<-as.numeric(as.POSIXct(as.character(val$Time),format="%Y-%m-%d %H:%M:%S"))

#make sure it is ordered in the way the code expects
val<-val[order(val$Anon.Student.Id, val$CF..Time.),]

#create a binary response column to predict and extract only data with a valid value
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
val<-val[val$CF..ansbin==0 | val$CF..ansbin.==1,]



# create durations
val$Duration..sec.<-(val$CF..End.Latency.+val$CF..Review.Latency.+500)/1000

# this function needs times and durations but you don't need it if you don't want to model time effects
val <- computeSpacingPredictors(val, "KC..Default.") #allows recency, spacing, forgetting features to run


modelob <- LKT(
  data = val, interc=TRUE,
  components = c("Anon.Student.Id", "KC..Default.", "KC..Default.", "KC..Default.", "KC..Default.", "KC..Default.", "KC..Default.", "KC..Default.", "KC..Default.", "KC..Default."),
  features = c("intercept", "intercept", "base4","logitdec","propdec","linesuc","linefail","logsuc","logfail","recency"),
  fixedpars=c(0.1890747,0.6309054,0.05471752,.5,0.2160748,.3,.5))

#dummy code intercepts
#https://stats.stackexchange.com/questions/136085/can-glmnet-logistic-regression-directly-handle-factor-categorical-variables-wi
#dummies https://www.marsja.se/create-dummy-variables-in-r/
#Exclude item intercepts with exclude = .. or nah?
data = as.data.frame(modelob$newdata)
data$KC..Default. = as.factor(data$KC..Default.)
data$KC..Cluster. = as.factor(data$KC..Cluster.)
dummies = dummy_cols(data,select_columns = c("KC..Default.","KC..Cluster."))
dummies = dummies[,-which(colnames(dummies)=="pred")] # cant use pred column
train_x = makeX(dummies[,76:191])
train_y  = dummies$CF..ansbin.
train_xx=unname(train_x)
fit  <- glmnet(train_x,train_y,family="binomial",intercept = TRUE) #See issue #1 in repo for why I think keeping global makes sense (tldr; lasso will drop it if not useful)
plot(fit,label=TRUE)
print(fit)
coef(fit, s = 0.1)
cvfit <- cv.glmnet(train_x, train_y,family="binomial",intercept=TRUE)
plot(cvfit)
coef(cvfit, s = "lambda.min")

cv_features = coef(cvfit, s = "lambda.1se")

rownames(cv_features)[1:9]
cv_features[1:9]

#Larger lambda (to the right) results in fewer features
plot(fit, xvar = "lambda", label = TRUE)
