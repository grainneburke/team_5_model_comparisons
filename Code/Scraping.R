
library(dplyr)


# Figure out concatenation ------------------------------------------------

#setwd("C:/Users/formationaxc/Downloads/data (1)/data")
data<-read.csv("Code_Violation_Cases.CSV")
data2<-data%>%mutate(lon=round(data$Longitude,4),lat=round(data$Latitude,4))

test1<-data2[!is.na(data2$Longitude) & data2$lon==-122.2600 & data2$lat==47.4959,c("lon","lat","Description")]
#test1
test2 <- test1 %>% group_by(lat, lon) %>% mutate(description_conc = paste0(Description, collapse = "")) 
#test2[1,]


# Webscraping 1 ------------------------------------------------------------
setwd("C:/Users/formationaxc/Downloads/team_5_model_comparisons-master/team_5_model_comparisons-master")

data <- readRDS("final_data_base_1.rds")
#head(data)
#test1 <- data[data$application.permit.number==6599155,]

library(dplyr)
library(xml2)
library(rvest)

alteration1    <- rep(NA,nrow(data))
alteration2    <- rep(NA,nrow(data))
new            <- rep(NA,nrow(data))
new_calculated <- rep(NA,nrow(data))
new_other      <- rep(NA,nrow(data))
declared_value <- rep(NA,nrow(data))
actual_value   <- rep(NA,nrow(data))
text           <- rep(NA,nrow(data))

for (i in seq(1:nrow(data))){ #seq(1:nrow(data))
  webpage <- xml2::read_html(data$permit.and.complaint.status.url[i]) #http://web6.seattle.gov/dpd/PermitStatus/Project.aspx?id=6599155
  
  
  if (length(webpage %>% html_nodes(xpath = '//*[@id="cph_plcProjectTabControl"]') %>% html_text())!=0){
    text[i] <- webpage %>% html_nodes(xpath = '//*[@id="cph_plcProjectTabControl"]') %>% html_text()
  }
  else {text[i]=" "}
  
  if ( unlist(gregexpr(pattern="Issuance Valuation"  ,text[i]))[1]!=-1){
    x_split <- substr(text[i],unlist(gregexpr(pattern="Issuance Valuation"  ,text[i]))[1],nchar(text[i]))
    x_split <- unlist(strsplit(x_split, split = "\r\n"))
    x_split <- gsub("^\\s+|\\s+$", "", x_split)
    x_split <- x_split[x_split != ""]
    alteration1[i]    <- x_split[3]
    alteration2[i]    <- x_split[5]
    new[i]            <- x_split[7]
    new_calculated[i] <- x_split[9]
    new_other[i]      <- x_split[11]
    declared_value[i] <- x_split[13]
    actual_value[i]   <- x_split[15]  
  }
  else{
    print("Error")
    alteration1[i]    <- NA
    alteration2[i]    <- NA
    new[i]            <- NA
    new_calculated[i] <- NA
    new_other[i]      <- NA
    declared_value[i] <- NA
    actual_value[i]   <- NA
  } 
  print(i)
}
data2 <- cbind(data,alteration1,alteration2,new,new_calculated,new_other,declared_value,actual_value)

# divided into 4 parallel sessions for faster scraping
data2A <- readRDS("data2A.rds")[    1:10000,]
data2B <- readRDS("data2B.rds")[10001:20000,]
data2C <- readRDS("data2C.rds")[20001:30000,]
data2D <- readRDS("data2D.rds")[30001:38424,]
data2 <- rbind(data2A,data2B,data2C,data2D)
saveRDS(data2,"data2.rds")

summary(data2)


# Webscraping 2 -----------------------------------------------------------

#webpage2 <- xml2::read_html("http://web6.seattle.gov/dpd/PermitStatus/Project.aspx?id=6599155")

#text2 <- webpage2 %>% html_nodes(xpath = '//*[@id="content"]') %>% html_text()
#text2 <- webpage2 %>% html_nodes(xpath = '//*[@id="form1"]') %>% html_text()

#x_split <- unlist(strsplit(text2, split = "\r\n"))
#x_split <- gsub("^\\s+|\\s+$", "", x_split)
#x_split <- x_split[x_split != ""]
#x_split



# Merging data ------------------------------------------------------------

#read the data 
setwd("C:/Users/formationaxc/Downloads/team_5_model_comparisons-master/team_5_model_comparisons-master")
data.scrap = readRDS("./data2_scrape.rds") 
data.scrap = data.scrap[, c("alteration1", "alteration2", "new", "new_calculated", "new_other", 
                            "declared_value", "actual_value", "application.permit.number")] 
#remove the $  
library(data.table) 
data.scrap = setDT(data.scrap) 
format.col_ = c("alteration1", "alteration2", "new", "new_calculated", "new_other", 
                "declared_value", "actual_value") 
for (i in format.col_){ 
  data.scrap[, (i) := as.numeric(gsub(",", "", gsub("\\$", "", get(i))))] #formatting 
  data.scrap[, (i) := ifelse(is.na(get(i)), median(get(i), na.rm = T), get(i))] #formatting 
} 


#### Merge with the original db 
#data = readRDS("./final_data_2_with_des.con_mined.RDS") 
data <- readRDS("final_data_base_1.rds")
data = merge(data, data.scrap, by = "application.permit.number", all.x = T) 

saveRDS(data, "./data_text_scrap.RDS") 






