#Packages
packages_list=c("knitr","codetools","devtools","data.table","tm","SnowballC","xml2","rvest","tidyverse","stringr","magrittr","hexView","httr","jsonlite","pbapply","wordcloud","text2vec","xgboost","LDAvis","topicmodels")
for (pkg in packages_list){
  print(paste0("check: ",pkg))
  if(!require(pkg,character.only = T)){
    print(paste0("need to install: ",pkg))
    install.packages(pkg)
  }
  library(pkg,character.only = T)
}

#functions
whitespace_rm <- function(vector){
  count=1
  while(length(grep("  ",vector))>0){
    print(count)
    vector <- str_replace_all(vector, "  ", " ")
    count=count+1
  }
  return(vector)
}

###################################################################
##########             Data Reading                  ##############
###################################################################
setwd("C:/documents/xq.do/Desktop/Hackathon/team_5_model_comparisons")
data = readRDS("./final_data_base_1.RDS")
data[, key := seq(1, nrow(data))]


#convert character variables to factors
data = setDT(data.frame(unclass(data)))

###################################################################
###################################################################
###################################################################
##########             Text Mining                   ##############
###################################################################
###################################################################
###################################################################
des.data = data[des.con != "", c("key", "des.con")]

###################################################################
##########             1. Cleaning                   ##############
###################################################################
#remove double quote
des.data[, des.con := gsub(des.con, pattern = '"', replacement = " ")]

#remove simple quote
des.data[, des.con := gsub(des.con, pattern = "'", replacement = " ")]

#remove comma
des.data[, des.con := gsub(des.con, pattern = ",", replacement = " ")]

#remove dot
des.data[, des.con := gsub(des.con, pattern = ".", replacement = " ", fixed = T)]

#remove backslash
des.data[, des.con := gsub(des.con, pattern = "////", replacement = " ")]

#remove digits and ponctuations
des.data[, des.con := gsub(des.con, pattern = "[[:digit:]]|[[:punct:]]", replacement = " ")]

#keep only alphabet characters
des.data[, des.con := str_replace_all(des.con, "[^[:alpha:]]", " ")]

#remove tabulations
des.data[, des.con := gsub(des.con, pattern = "/t", replacement = " ")]

#remove white spaces
des.data[, des.con := whitespace_rm(des.con)]



###################################################################
##########              2. Corpus                    ##############
###################################################################
text_corpus = tm::Corpus(tm::VectorSource(x = des.data$des.con))
tm::inspect(text_corpus[sample(1:length(text_corpus),10)])







