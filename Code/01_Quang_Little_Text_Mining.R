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


#formatting & save the results
corrected.word[836] = "code zone garage"
corrected.word = corrected.word[corrected.word != "character(0)"]
test = unlist(lapply(corrected.word, `[[`, 1), use.names = T)
bad.word.correction = data.table("old.word" = names(test),
                                 "new.word" = test)
fwrite(bad.word.correction, "./bad_word_correction.csv")
correction = data.table("old.word" = token.all$word,
                        "check" = token.all$check)
correction = merge(correction, bad.word.correction, by = "old.word", all.x = T)
correction[, new.word := ifelse(check == TRUE, old.word, new.word)]
correction = correction[!is.na(new.word), ]
correction[, check := NULL]
fwrite(correction, "./correction.csv")
View(head(correction))

###################################################################
##########               4. Stemming                 ##############
###################################################################
# explanation.s, explain.s, explained => explain
token.vector = sapply(token, as.character)
head(token.vector)
token.table = setDT(data.frame(token.vector))
token.table = merge(token.table, correction, by.x = "token.vector", by.y = "old.word")
token.vector = token.table$new.word
token.corpus = tm::Corpus(tm::VectorSource(x = unique(token.vector)))


stemmed_tokens <- tm::tm_map(token.corpus, tm::stemDocument)
save(list = "stemmed_tokens", file = "./stemmed_tokens.RData")
load("./stemmed_tokens.RData")
head_stemmed <- sapply(stemmed_tokens[1:1000],as.character)
head_token <- sapply(token.corpus[1:1000],as.character)
differences_index <- which(!head_stemmed==head_token)
head(cbind(head_token,head_stemmed)[differences_index,],10)


b = sapply(stemmed_tokens, as.character)
names(b) = sapply(token.corpus, as.character)

sentence <- des.data$des.con
sentence <- str_replace_all(string = sentence, b)
head(sentence)

text_corpus = tm::Corpus(tm::VectorSource(x = sentence))


#String distance
tdm <- tm::TermDocumentMatrix(text_corpus,
                              control = list(removePunctuation = TRUE,
                                             stopwords = TRUE))

#Highest frequency words
prune_top_words=5
num_words=1000
terms <- findFreqTerms(tdm)[prune_top_words:num_words]

text_tf <- tdm[terms,] %>%
  as.matrix() %>%
  rowSums()  %>%
  data.frame(Term = terms, Frequency = .) %>%
  arrange(desc(Frequency))%>%
  mutate(rank=1:nrow(.),wordlen=str_length(terms)) %>%
  arrange(desc(wordlen))


# https://stackoverflow.com/questions/19424709/r-gsub-pattern-vector-and-replacement-vector
a <- paste0(" ",text_tf$Term," ")
b <- paste0(" ",as.character(text_tf$rank)," ")
names(b) <- a
sentence <- str_replace_all(string = sentence, b)

sentence_hashed_ranked <- gsub("[[:alpha:]]","",sentence)
sentence_hashed_ranked <- gsub("//s+"," ",sentence_hashed_ranked)

data_prepared <- data.frame(Value=des.data$key, Description=sentence_hashed_ranked)
save(list="data_prepared",file="data_prepared_for_keras_lstm.RData")
dim(data_prepared)

##############   Create vocab   #############
feature="Description"
id="Value"
label="Value"

train.token <- data_prepared[, feature] %>%tolower %>%word_tokenizer

NLP.train <- text2vec::itoken(train.token,ids = data_prepared[, id], progressbar = FALSE)

NLP.dictionary <- create_vocabulary(NLP.train,stopwords = c(tm::stopwords(kind = 'en'),'construct','constrution','per'), ngram = c(ngram_min=1L,ngram_max=4L))
tail(NLP.dictionary)




########## Prune vocab       ##################
NLP.dictionary.pruned <- prune_vocabulary(NLP.dictionary,
                                          term_count_min = 20,
                                          doc_proportion_max = 0.5)
nrow(NLP.dictionary.pruned)

NLP.vectorizer <- vocab_vectorizer(NLP.dictionary.pruned)





NLP.matrix.train <- text2vec::create_dtm(NLP.train, NLP.vectorizer)
dim(NLP.matrix.train)
final.data = data.frame(as.matrix(NLP.matrix.train))
final.data$id = as.integer(NLP.train$ids)

data = merge(data, final.data, by.x = "key", by.y = "id", all.x = T)
saveRDS(data, "./final_data_2_with_des.con_mined.RDS")
