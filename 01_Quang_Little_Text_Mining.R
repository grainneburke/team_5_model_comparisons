#Packages
packages_list=c("knitr","codetools","devtools","data.table","tm",
                "SnowballC","xml2","rvest","tidyverse","stringr",
                "magrittr","hexView","httr","jsonlite","pbapply",
                "wordcloud","text2vec","xgboost","LDAvis",
                "topicmodels", "hunspell")
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

#lower case
text_corpus <- tm::tm_map(text_corpus, tolower)
tm::inspect(text_corpus[sample(1:length(text_corpus),10)])

#stop words
stop_words <- tm::stopwords(kind = "en")
text_corpus <- tm::tm_map(text_corpus,function(x)
  tm::removeWords(x,c(stop_words,
                      "the","house", "permit", "violation"))) #add "per" word
tm::inspect(text_corpus[sample(1:length(text_corpus),10)])

#remove white space because stop words are replaced by white spaces
text_corpus <- tm::tm_map(text_corpus, tm::stripWhitespace)
tm::inspect(text_corpus[sample(1:length(text_corpus),10)])

#Extract a sentence and read it raw ascii
# set.seed(1)
# sentence <- text_corpus[[sample(1:length(text_corpus),1)]][1]
# print(sentence)
# write_lines(sentence, path = "./sentence_example.txt")
# sentence_raw <- hexView::readRaw("./sentence_example.txt")
# sentence_raw



###################################################################
##########         3. Tokenization                   ##############
###################################################################
#simply split the sentences to words
token <- tm::tm_map(text_corpus, tm::scan_tokenizer)


######## Focus on scarce token (observed once)
token_char <- unlist(lapply(token, as.character))
token_table <- table(token_char)
token_table <- token_table[order(token_table, decreasing = T)]
token_all <- names(token_table)
token_scarce <- names(token_table[token_table == 1])


#Save tokens to a file to use external app for spell checking
write_lines(x = token_char, path = "./little_tokens.txt")



###################################################################
##########         4. Spell checking                 ##############
###################################################################

# https://www.wordsapi.com/pricing you need to create an account and can access 2500 queries a day for free.
# http://app.aspell.net/lookup?dict=en_US;words=no%0D%0A%0D%0A%0D%0A
# http://wordnet.princeton.edu/wordnet/download/current-version/
# Quang: https://www.r-bloggers.com/hunspell-spell-checker-and-text-parser-for-r/

# word_examples = c("hackathon", "world", "Google", "deep learning")
# sapply(word_examples, hunspell_find)
token_all = unique(token_char)
#########          spelling check using hunspell_check function        ###########
word.check = sapply(token_all, hunspell_check)
token.all = data.table("word" = token_all,
                       "check" = word.check)
View(token.all[word.check == F])
########           Correcting the bad words          ################
corrected.word = sapply(token.all[word.check == F]$word, hunspell_suggest)
x = head(corrected.word)
corrected.word[836] = "code zone garage"
corrected.word = corrected.word[corrected.word != "character(0)"]
test = unlist(lapply(corrected.word, `[[`, 1), use.names = T)
bad.word.correction = data.table("old.word" = names(test),
                                 "new.word" = test)
fwrite(bad.word.correction, "./bad_word_correction.csv")

###################################################################
##########               4. Stemming                 ##############
###################################################################
# explanation.s, explain.s, explained => explain

# stemmed_tokens <- tm::tm_map(token,tm::stemDocument)
# save(list = "stemmed_tokens",file = "stemmed_tokens.RData")
load("stemmed_tokens.RData")
head_stemmed <- sapply(stemmed_tokens[1:1000],as.character)
head_token <- sapply(token[1:1000],as.character)
differences_index <- which(!head_stemmed==head_token)
head(cbind(head_token,head_stemmed)[differences_index,],10)