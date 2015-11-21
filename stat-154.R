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

dtm.weighted = DocumentTermMatrix(data,
                                   control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                  stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                  stemming=TRUE))
dtms = removeSparseTerms(dtm.weighted, 0.95)
dim(dtms)

freq = colSums(as.matrix(dtms))
length(freq)

a = boxplot(freq)
best_words = names(freq[which(freq >= a$stats[2] & freq <= a$stats[4])]) #get middle of low and high bound
length(best_words)
best_words = best_words[-c(1524, 1525)] #remove "wwwgutenbergnet" "wwwgutenbergorg"

write.table(best_words, "best_words.txt", sep='\n')

table = as.matrix(dtms)
dim(table)
word.mat = table[,best_words]
write.csv(word.mat, file="word_matrix.csv")

#--------------------------------------------------------------------------------------
filename = file.path("E:", "GitHub", "machine_learning", "Child(0)")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

child = Corpus(DirSource(filename))
child = tm_map(child, removeWords, common)

child.weighted = DocumentTermMatrix(child,
                                  control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                 stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                 stemming=TRUE))
childs = removeSparseTerms(child.weighted, 0.6)
dim(childs)

freq = colSums(as.matrix(childs))
length(freq)



#--------------------------------------------------------------------------------------
filename = file.path("E:", "GitHub", "machine_learning", "History(1)")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

history = Corpus(DirSource(filename))
history = tm_map(history, removeWords, common)

history.weighted = DocumentTermMatrix(history,
                                    control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                   stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                   stemming=TRUE))
historys = removeSparseTerms(history.weighted, 0.6)
dim(historys)

freq = colSums(as.matrix(historys))
length(freq)

b = tail(sort(freq), 300)
write.table(names(b), "history_300.txt", sep='\n')

table = as.matrix(historys)
dim(table)
word.mat = table[,names(b)]
write.csv(word.mat, file="history_300_matrix.csv")


#freq.s = normalize.vector(freq)
#CI = c(mean(freq.s) - sd(freq.s)/sqrt(3068), mean(freq.s) + sd(freq.s)/sqrt(3068))
#best_word = names(freq.s)[which(freq.s >= CI[1] & freq.s <= CI[2])]

#error = qnorm(0.998)*sd(freq)/sqrt(3068)
#best_word = names(freq)[which(freq >= mean(freq) - error & freq <= mean(freq) + error)]
#length(best_word)
