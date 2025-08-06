# title: "PridePrejudice_Corpus_Tokens_Semantic"
# output: html_document


#load required packages
require(readtext)
require(quanteda)
require(quanteda.textplots)
require(topicmodels)
require(dplyr)
require(tidytext)
require (quanteda.textstats)

include=FALSE
knitr::opts_chunk$set(echo = TRUE)

#Read into R and transform into a corpus
PridePrejudice <- readtext::readtext ("~/Desktop/MATLIT/Tese/vizualizacoes/RStudio/PridePrejudice/PridePrejudice.txt*") 
PridePrejudice
PridePrejudice_corpus <- corpus (PridePrejudice)

#Summarize the full corpus.
summary(PridePrejudice_corpus)

#Tokenize the corpus, remove stopwords and punctuation, and get a list of the most common words:
toks_PridePrejudice <- tokens(PridePrejudice_corpus, remove_punct = TRUE)
dfmat_PridePrejudice <- dfm(toks_PridePrejudice)
dfmat_PridePrejudice <- dfm_remove(dfmat_PridePrejudice, pattern = c(stopwords("en"), "can", "???","though", "without", "therefore", "yet", "whether", "said", "say", "use", "since", "may", "two",  "still", "upon", 'great', "every", "even", "one?", "must", "?", "one", "us","s"))
dfmat_PridePrejudice <- dfm_trim(dfmat_PridePrejudice, min_termfreq = 100)
topfeatures(dfmat_PridePrejudice)
nfeat(dfmat_PridePrejudice)

#Semantic network
set.seed(100)
toks <- PridePrejudice_corpus %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern =  c(stopwords("en"), "can", "???", "though", "without", "therefore", "yet", "whether", "said", "say", "use", "since", "may", "two",  "still", "upon", 'great', "every", "even", "one?", "must", "?", "one", "us","s"), padding = FALSE)
fcmat <- fcm(toks, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 30))
fcm_select(fcmat, pattern = feat) %>%
  textplot_network(
    min_freq = 0.7,
    omit_isolated = TRUE,
    edge_color = "#b793a5",
    edge_alpha = 0.2,
    edge_size = 3,
    vertex_color = "#643f32",
    vertex_size = 1,
    vertex_labelcolor = "#643f32",
    vertex_labelfont = NULL,
    vertex_labelsize = 4,
    offset = NULL,
  )


# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score: 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# It also counts the number of positive and negative emotions found in each row
d<-get_nrc_sentiment(text)
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)


#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")




