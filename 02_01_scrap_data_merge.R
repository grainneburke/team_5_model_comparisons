#read the data
setwd("C:/documents/xq.do/Desktop/Hackathon/team_5_model_comparisons")
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
data = readRDS("./final_data_2_with_des.con_mined.RDS")
data = merge(data, data.scrap, by = "application.permit.number", all.x = T)

saveRDS(data, "./data_text_scrap.RDS")