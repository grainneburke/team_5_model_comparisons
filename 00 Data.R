library(data.table)
library(lubridate)

#Necessary functions for further uses
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


############################################################
#########              DATA          #######################
############################################################
setwd("C:/documents/xq.do/Desktop/Hackathon/Hack/data/data")
building.permits = fread("./Building_Permits___Current.csv")
code.violation = fread("./Code_Violation_Cases.csv")
data.911 = fread("./Seattle_Real_Time_Fire_911_Calls (2).csv")


#########        columns cleaning  ########################
#rename
names(building.permits) = tolower(make.names(names(building.permits), unique=TRUE))
names(code.violation) = tolower(make.names(names(code.violation), unique=TRUE))
names(data.911) = tolower(make.names(names(data.911), unique=TRUE))

#date formatting
date.var_ = c("date.case.created", "last.inspection.date", "last.inspection.result")
for(i in date.var_){code.violation[, (i) := as.Date(get(i), "%d/%m/%Y")]}

date.var_ = c("application.date", "issue.date", "final.date", "expiration.date")
for(i in date.var_){building.permits[, (i) := mdy(get(i))]}

#formatting numerical variables
building.permits[, value := as.numeric(gsub("\\$", "", value))]


############################################################
######                  MERGING       ######################
############################################################
#add tracking variables
code.violation[, violation := 1]
data.911[, call.911 := 1]


#######################    Merge with code.violation    ########################

#NA deletion
code.violation = code.violation[is.na(longitude) == F 
                                & is.na(latitude) == F, ]

code.violation.group = code.violation[, list(nb.violation     = sum(violation),
                                             case.type        = mode(case.type),
                                             case.group       = mode(case.group),
                                             status           = mode(status)),
                                      by = c("longitude", "latitude")]

data = merge(building.permits, code.violation.group, 
             by = c("longitude", "latitude"), 
             all.x = T)



#######################    Merge with data.911    ########################
#data.911
data.911 = data.911[is.na(longitude) == F 
                    & is.na(latitude) == F, ]

data.911.group = data.911[, list(type    = mode(type),
                                 nb.call = sum(call.911)),
                            by = c("longitude", "latitude")]

data = merge(data, data.911.group, 
             by = c("longitude", "latitude"), 
             all.x = T)



#Write the final db
fwrite(data, "./final_data_base_1.csv")
