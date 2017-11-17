library(data.table)
library(xgboost)
library(lubridate)

# 
# packages_list=c("knitr","codetools","devtools","data.table","tm","SnowballC","xml2","rvest","tidyverse","stringr","magrittr","hexView","httr","jsonlite","pbapply","wordcloud","text2vec","xgboost","LDAvis","topicmodels")
# for (pkg in packages_list){
#   print(paste0("check: ",pkg))
#   if(!require(pkg,character.only = T)){
#     print(paste0("need to install: ",pkg))
#     install.packages(pkg)
#   }
#   library(pkg,character.only = T)
# }


#############" Data reading "##################
setwd("C:/documents/xq.do/Desktop/Hackathon/team_5_model_comparisons")


x.train = readRDS("./x.train.RDS")
x.test  = readRDS("./x.test.RDS")
y.train = readRDS("./y.train.RDS")
y.test  = readRDS("./y.test.RDS")


set.seed(1024)
SumModelGini <- function(solution, submission) {
  df = data.frame(solution = solution, submission = submission)
  df <- df[order(df$submission, decreasing = TRUE),]
  df$random = (1:nrow(df))/nrow(df)
  totalPos <- sum(df$solution)
  df$cumPosFound <- cumsum(df$solution) # this will store the cumulative number of positive examples found (used for computing "Model Lorentz")
  df$Lorentz <- df$cumPosFound / totalPos # this will store the cumulative proportion of positive examples found ("Model Lorentz")
  df$Gini <- df$Lorentz - df$random # will store Lorentz minus random
  return(sum(df$Gini))
}

NormalizedGini <- function(solution, submission) {
  SumModelGini(solution, submission) / SumModelGini(solution, solution)
}

# wrap up into a function to be called within xgboost.train
evalgini <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- NormalizedGini(as.numeric(labels),as.numeric(preds))
  return(list(metric = "Gini", value = err))
}


param_xgb <- list(objective        = "reg:linear",
                  booster          = "gbtree",
                  eta              = 0.01,
                  max_depth        = 8,
                  min_child_weight = 50,
                  subsample        = .7,
                  colsample_bytree = .9,
                  nthread          = 4
)

xgbMod_ <- xgboost::xgboost(data           = x.train,
                            label          = y.train,
                            params         = param_xgb,
                            feval          = evalgini,
                            nrounds        = 1000,#train.xgbCV_$best_iteration,
                            verbose        = TRUE,
                            maximize       = F,
                            print_every_n  = 50)


#predict on test set
value_pred <- predict(xgbMod_, newdata = x.test, type='response')


tab=data.frame("value_pred" = value_pred,
               "Value" = y.test)
threshold=100
keep_fraction=500

library(dplyr)
gain_tab <- tab %>%
  filter(value_pred>threshold)%>%
  arrange(-value_pred) %>%
  mutate(index=1:nrow(.)/nrow(.),log_value=Value, gain=cumsum(Value-min(Value))/sum(Value-min(Value)),sub_sample=sample(1:round(nrow(.)/keep_fraction),replace=T,size=nrow(.)))
sprintf("the model Gini index is %s pct",round(100*(mean(gain_tab$gain,na.rm=T)*2-1),1))

gain_tab <- tab %>%
  filter(Value>threshold)%>%
  arrange(-Value) %>%
  mutate(index=1:nrow(.)/nrow(.),log_value=Value, gain=cumsum(Value-min(Value))/sum(Value-min(Value)),sub_sample=sample(1:round(nrow(.)/keep_fraction),replace=T,size=nrow(.)))
sprintf("the best Gini index is %s pct",round(100*(mean(gain_tab$gain,na.rm=T)*2-1),1))
