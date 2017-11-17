library(axasr)
library(axaml)

set.seed(123)

#Loading of initial DB (merge of building permits, fire calls and violation code)

data <- readRDS(file="final_data_base_1.RDS")

#Exploring of the initial DB

str(data)
hist(data$value)
plot(ecdf(data$value))

table(data$permit.type) #as we want to model the value, keep only the permit.type="Construction"

#Cleansing of the DB

  #1) Remove of zero values as we will use a Gamma distribution

data <- data[data$value > 0,]

  #2) Keep only the building permits relating to construction

#data <- data[data$permit.type == "Construction",]

  #3) Recoding of number of violation : NA means no violation (=> 0)

data$nb.violation <- ifelse(is.na(data$nb.violation),0,data$nb.violation)


#Modelling phase

  #Special attribute names

label <- 'value'
id <- 'application.permit.number'
distribution <- 'gamma'
train.index <- 'train'
features <- names(data)[!(names(data)%in%c(label,train.index,id))]

  #Screen of the DB

obj.screen <- VarScreenStep(data           = data,
                            label          = label,
                            features       = features,
                            keep.data      = FALSE,
                            round.estimate = FALSE)

  #Remove of one-level variable : permit.type (as we have kept only the construction ones) and nb.call

data <- data[, -c("permit.type","nb.call")]

  #Remove of variables with missing values

data <- data[, -c("longitude","latitude","application.date","issue.date","final.date","expiration.date","master.use.permit","des.con","case.type","case.group","status.y" )]

  #Remove of variables with a lot of levels

data <- data[, -c("address","description","applicant.name","contractor","permit.and.complaint.status.url","location")]

  #Re-screen of the DB to see if all is OK

features <- names(data)[!(names(data)%in%c(label,train.index,id))]
obj.screen <- VarScreenStep(data           = data,
                            label          = label,
                            features       = features,
                            NAReplace      = TRUE)
data <- obj.screen$data

  #Split in train/test samples

index.split <- sample(seq_len(nrow(data)), size = floor(0.8 * nrow(data)))
data[index.split,train.index] <- TRUE
data[-index.split,train.index] <- FALSE

  #Selection of variables

obj.selection <- VarSelectionStep(label = label,
                                  features = features,
                                  data=data[data$train==TRUE,],
                                  distribution = distribution,
                                  type = 1)
obj.selection$var.select.final 

  #Fitting a GBM

obj.stump <- StumpModelingStep(label = label,
                               features = obj.selection$var.select.final,
                               data=data[data$train==TRUE,],
                               distribution = distribution,
                               shrinkage.start=obj.selection$gbm.model$shrinkage,
                               round.table=obj.screen$round.stump)
  
  #Predict on validation sample

data$pred.axasr <- PredictLinearStump(model=obj.stump$GBMExtract,
                                    testbase=data,
                                    round.table=obj.screen$round.stump)$response

  #Performance of the model : Gini

axaml::plotgain(predrisk = list( test= data[data$train==F,'pred.axasr'],
                                 train = data[data$train==T,'pred.axasr'])
                ,
                truerisk = list(data[data$train==F,label],
                                data[data$train==T,label]),
                return_val = T, 
                with_optimal = F,
                normalize=F,
                significance = 3)

#Theoretical Gini

tab <- data[data$train == FALSE,c("pred.axasr","value")]
threshold <- 100
keep_fraction <- 500

gain_tab <- tab %>%
  filter(pred.axasr>threshold)%>%
  arrange(-value) %>%
  mutate(index=1:nrow(.)/nrow(.),value=value,gain=cumsum(value-min(value))/sum(value-min(value)),sub_sample=sample(1:round(nrow(.)/keep_fraction),replace=T,size=nrow(.)))
sprintf("The best theoretical Gini index on test sample is %s pct",round(100*(mean(gain_tab$gain,na.rm=T)*2-1),1))
