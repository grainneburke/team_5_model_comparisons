library(data.table)


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


date.var_ = c("application.date", "issue.date", "final.date", "expiration.date")
for(i in date.var_){building.data[, (i) := mdy(get(i))]}
rm(date.var_)
names(code.violation)



############################################################
######                  MERGING       ######################
############################################################
code.violation[, violation := 1]
data.911[, call.911 := 1]

#NA deletion
code.violation = code.violation[is.na(longitude) == F 
                                & is.na(latitude) == F, ]

head(code.violation[, list(nb.violation    = sum(violation)),
                    , .SD[which.max(pt)],
               by = c("longitude", "latitude")])


data.911 = data.911[is.na(longitude) == F 
                    & is.na(latitude) == F, ]


data = merge(building.permits, code.violation, 
             by = c("longitude", "latitude"), 
             all.x = T)

nrow(building.permits) #56881
nrow(data) #72582

data = merge(data, data.911, 
             by = c("longitude", "latitude"), 
             all.x = T)

