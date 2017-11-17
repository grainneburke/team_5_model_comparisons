location <- ("C:/Hck 2017/Team 5 Hackathon/")

#Load data
bld_permits <- read.csv(paste0(location, "Building_Permits___Current.csv"), sep = ",")
cde_violations <- read.csv("C:/Hck 2017/data/data/Code_Violation_Cases.csv", sep = ",")
calls_911 <- read.csv("C:/Hck 2017/data/data/Seattle_Real_Time_Fire_911_Calls (2).csv", 
                          sep = ",")


colnames(bld_permits)    <- gsub(".","_",colnames(bld_permits),fixed=TRUE) 
colnames(cde_violations) <- gsub(".","_",colnames(cde_violations),fixed=TRUE) 
colnames(calls_911)      <- gsub(".","_",colnames(calls_911),fixed=TRUE) 