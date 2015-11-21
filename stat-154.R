filename = file.path("E:", "GitHub", "machine_learning", "data")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

library(NLP)
library(tm)
library(SnowballC)
library(NLP)
library(tm)
library(splines)
library(MASS)
library(ppls)

data = Corpus(DirSource(filename))
data = tm_map(data, removeWords, common)

dtm.weighted2 <- DocumentTermMatrix(data,
                                   control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                  stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                  stemming=TRUE, stripWhitespace = TRUE, PlainTextDocument = TRUE))
dtms = removeSparseTerms(dtm.weighted2, 0.95)
dim(dtms)

freq = colSums(as.matrix(dtms))
length(freq)

a = boxplot(freq)
best_words = names(freq[which(freq >= a$stats[2] & freq <= a$stats[4])])
length(best_words)

write.table(best_words, "best_words.txt", sep='\n')

#freq.s = normalize.vector(freq)
#CI = c(mean(freq.s) - sd(freq.s)/sqrt(3068), mean(freq.s) + sd(freq.s)/sqrt(3068))
#best_word = names(freq.s)[which(freq.s >= CI[1] & freq.s <= CI[2])]

#error = qnorm(0.998)*sd(freq)/sqrt(3068)
#best_word = names(freq)[which(freq >= mean(freq) - error & freq <= mean(freq) + error)]
#length(best_word)
