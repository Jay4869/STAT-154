filename = file.path("E:", "GitHub", "machine_learning", "data")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

library(NLP)
library(tm)

data = Corpus(DirSource(filename))
data = tm_map(data, removePunctuation)
data = tm_map(data, tolower)
data = tm_map(data, removeWords, common)

library(SnowballC)
data = tm_map(data, stemDocument) 
data = tm_map(data, stripWhitespace)
data = tm_map(data, PlainTextDocument)

dtm = DocumentTermMatrix(data)
dtms = removeSparseTerms(dtm, 0.6)

freq = colSums(as.matrix(dtms))
freq
#---------------------------------------------------
library(NLP)
library(tm)
library(splines)
library(MASS)
library(ppls)
data = Corpus(DirSource(filename))
data = tm_map(data, removeWords, common)

dtm.weighted <- DocumentTermMatrix(data,
                                   control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                  stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                  stemming=TRUE))
dtms = removeSparseTerms(dtm.weighted, 0.95)
dim(dtms)

freq = colSums(as.matrix(dtms))
length(freq)

freq.s1 = normalize.vector(freq)
CI = c(mean(freq.s) - sd(freq.s), mean(freq.s) + sd(freq.s))
best_word = names(freq.s)[which(freq.s >= CI[1] & freq.s <= CI[2])]

error = qnorm(0.84)*sd(freq)/sqrt(3068)
best_word = names(freq)[which(freq >= mean(freq) - error & freq <= mean(freq) + error)]
length(best_word)
