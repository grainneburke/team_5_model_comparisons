library(data.table)
library(xgboost)
library(lubridate)


packages_list=c("knitr","codetools","devtools","data.table","tm","SnowballC","xml2","rvest","tidyverse","stringr","magrittr","hexView","httr","jsonlite","pbapply","wordcloud","text2vec","xgboost","LDAvis","topicmodels")
for (pkg in packages_list){
  print(paste0("check: ",pkg))
  if(!require(pkg,character.only = T)){
    print(paste0("need to install: ",pkg))
    install.packages(pkg)
  }
  library(pkg,character.only = T)
}


#############" Data reading "##################
setwd("C:/documents/xq.do/Desktop/Hackathon/team_5_model_comparisons")


x.train = readRDS("./x.train.RDS")
x.test  = readRDS("./x.test.RDS")
y.train = readRDS("./y.train.RDS")
y.test  = readRDS("./y.test.RDS")

set.seed(1024)
param_xgb <- list(objective        = "reg:linear",
                  booster          = "gbtree",
                  eta              = 0.01,
                  max_depth        = 8,
                  min_child_weight = 50,
                  subsample        = .7,
                  colsample_bytree = .7,
                  nthread          = 4)

library(xgboost)
x.train = xgb.DMatrix(x.train)
train.xgbCV_ <- xgboost::xgb.cv(data= x.train,
                                label = y.train,
                                params=param_xgb,
                                nrounds=5000,
                                nfold=4,
                                stratified=F,
                                verbose=1,
                                early_stopping_rounds=100,
                                print_every_n=50,
                                maximize=F)

