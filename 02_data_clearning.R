library(data.table)
library(xgboost)
library(lubridate)

#############" Data reading "##################
setwd("C:/documents/xq.do/Desktop/Hackathon/team_5_model_comparisons")
data = readRDS("./final_data_2_with_des.con_mined.RDS")

#filter on value > 0
data = data[value > 0, ]

#drop high cardinality variables
drop.var_ = c("key", "application.permit.number", "address", "description",
              "applicant.name", "contractor", "permit.and.complaint.status.url",
              "location", "des.con", "case.type", "case.group", "status.y")
for(i in drop.var_){data[, (i) := NULL]}
rm(drop.var_)



#date variables
date.var_ = c("application.date", "issue.date", "final.date", "expiration.date")
for(i in date.var_){data[, (i) := as.numeric(as.Date(get(i), "%Y-%m-%d"))]}
rm(date.var_)

data = data.frame(data)

for(i in 1:ncol(data)){
  if (class(data[, i]) == "factor"){
    data[is.na(data[, i]), i] <- "missing"
  } 
  else {
    data[is.na(data[, i]), i] <- -999
  }
}

sum(is.na(data))





######################################################################
##############              Modelling                #################
######################################################################

dt.size = nrow(data)
set.seed(1337)
train_sample = sample(1:dt.size,round(.7*dt.size))

train = data[train_sample, ]
test = data[-train_sample, ]


y.train = train$value
x.train = subset(train, select= -c(value))
x.train = as.matrix(x.train)

y.test = test$value
x.test = subset(test, select= -c(value))
x.test = as.matrix(x.test)
rm(data)
rm(train)
rm(test)
gc()

# saveRDS(x.train, "./x.train.RDS")
# saveRDS(x.test, "./x.test.RDS")
# saveRDS(y.train, "./y.train.RDS")
# saveRDS(y.test, "./y.test.RDS")
