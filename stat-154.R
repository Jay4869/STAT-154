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
data = tm_map(data, removeWords, c("gutenbergtm", "project"))
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

write.table(best_words, "best_words.txt", sep='\n')

table = as.matrix(dtms)
dim(table)
word.mat = table[,best_words]
write.csv(word.mat, file="word_matrix.csv")

#--------------------------------------------------------------------------------------
filename = file.path("E:", "GitHub", "machine_learning", "Child_0")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

child = Corpus(DirSource(filename))
child = tm_map(child, removeWords, common)
child = tm_map(child, removeWords, 'gutenbergtm')

child.weighted = DocumentTermMatrix(child,
                                  control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                 stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                 stemming=TRUE))
childs = removeSparseTerms(child.weighted, 0.8)
dim(childs)

freq_child = colSums(as.matrix(childs))
length(freq_child)

b = head(sort(freq_child, T), 200)
write.table(b, "child_400.txt", sep='\n')

table = as.matrix(childs)
dim(table)
word.mat = table[,names(b)]
write.csv(word.mat, file="child_400_matrix.csv")

#--------------------------------------------------------------------------------------
filename = file.path("E:", "GitHub", "machine_learning", "History_1")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

history = Corpus(DirSource(filename))
history = tm_map(history, removeWords, common)
history = tm_map(history, removeWords, 'gutenbergtm')


history.weighted = DocumentTermMatrix(history,
                                    control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                   stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                   stemming=TRUE))
historys = removeSparseTerms(history.weighted, 0.8)
dim(historys)

freq_hist = colSums(as.matrix(historys))
length(freq_hist)

b = head(sort(freq_hist, T), 200)
write.table(b, "history_400.txt", sep='\n')

table = as.matrix(historys)
dim(table)
word.mat = table[,names(b)]
write.csv(word.mat, file="history_400_matrix.csv")

#--------------------------------------------------------------------------------------
filename = file.path("E:", "GitHub", "machine_learning", "Religion_2")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

religion = Corpus(DirSource(filename))
religion = tm_map(religion, removeWords, common)

religion.weighted = DocumentTermMatrix(religion,
                                      control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                     stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                     stemming=TRUE))
religions = removeSparseTerms(religion.weighted, 0.8)
dim(religions)

freq_reg = colSums(as.matrix(religions))
length(freq_reg)

b = head(sort(freq_reg, T), 200)
write.table(b, "religion_400.txt", sep='\n')

table = as.matrix(religions)
dim(table)
word.mat = table[,names(b)]
write.csv(word.mat, file="religion_400_matrix.csv")

#---------------------------------------------------------------------------------
filename = file.path("E:", "GitHub", "machine_learning", "Science_3")
dir(filename)
setwd("E:/GitHub/machine_learning")

common = scan('common_word.txt', what='', sep=',')
common

science = Corpus(DirSource(filename))
science = tm_map(science, removeWords, common)


science.weighted = DocumentTermMatrix(science,
                                       control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
                                                      stopwords = TRUE, tolower = TRUE, removePunctuation = TRUE, 
                                                      stemming=TRUE))
sciences = removeSparseTerms(science.weighted, 0.8)
dim(sciences)

freq_sci = colSums(as.matrix(sciences))
length(freq_sci)

b = head(sort(freq_sci, T), 200)
write.table(b, "science_400.txt", sep='\n')

table = as.matrix(sciences)
dim(table)
word.mat = table[,names(b)]
write.csv(word.mat, file="science_400_matrix.csv")

#freq.s = normalize.vector(freq)
#CI = c(mean(freq.s) - sd(freq.s)/sqrt(3068), mean(freq.s) + sd(freq.s)/sqrt(3068))
#best_word = names(freq.s)[which(freq.s >= CI[1] & freq.s <= CI[2])]

#error = qnorm(0.998)*sd(freq)/sqrt(3068)
#best_word = names(freq)[which(freq >= mean(freq) - error & freq <= mean(freq) + error)]
#length(best_word)

##--------------------------------------------------------
##--------------------------------------------------------
best_words = scan('best_words.txt', what='', sep=',')

#remove repeats in science
science = scan('science_400.txt', what='', sep=',')
science = science[-1]
science = science[seq(1,length(science),2)]
science = science[-c(1,2,3)]
bloon = rep(NA, length(science))
for(i in 1:length(science))
{
  if(is.na(match(science[i], best_words)))
    bloon[i] = TRUE
  else
    bloon[i] = FALSE
}
science = science[bloon]

religion = scan('religion_400.txt', what='', sep=',')
religion = religion[-1]
religion = religion[seq(1, length(religion),2)]
bloon = rep(NA, length(religion))
for(i in 1:length(religion))
{
  if(is.na(match(religion[i], best_words)))
    bloon[i] = TRUE
  else
    bloon[i] = FALSE
}
religion = religion[bloon]

history = scan('history_400.txt', what='', sep=',')
history = history[-1]
history = history[seq(1, length(history),2)]
history = history[-3]
bloon = rep(NA, length(history))
for(i in 1:length(history))
{
  if(is.na(match(history[i], best_words)))
    bloon[i] = TRUE
  else
    bloon[i] = FALSE
}
history = history[bloon]

child = scan('child_400.txt', what='', sep=',')
child = child[-1]
child = child[seq(1, length(child),2)]
child = child[-1]
bloon = rep(NA, length(child))
for(i in 1:length(child))
{
  if(is.na(match(child[i], best_words)))
    bloon[i] = TRUE
  else
    bloon[i] = FALSE
}
child = child[bloon]

#--------------------------------------------------------
powermat = scan('power-word.txt', what='', sep=',')
powermat = powermat[-1]
powermat = powermat[seq(2, length(powermat),2)]
length(powermat) #246

powermat = unique(c(science, religion, child, history))
write.table(powermat, "power-word.txt", sep='\n')

#---------------------------------------------------------
y = rep(0, 22308)
name = rownames(word.mat)
tmp1 = dir(file.path("E:", "GitHub", "machine_learning", "History_1"))
y[match(tmp1, name)] = 1
tmp2 = dir(file.path("E:", "GitHub", "machine_learning", "Religion_2"))
y[match(tmp2, name)] = 2
tmp3 = dir(file.path("E:", "GitHub", "machine_learning", "Science_3"))
y[match(tmp3, name)] = 3

word.matrix = cbind(word.mat, y)
write.csv(word.matrix, file="word_matrix.csv")

#----------------------------------------------------------
y_p = rep(0, 22308)
power_table = as.matrix(inspect(dtm.weighted[,powermat]))
y_p[match(tmp1, name)] = 1
y_p[match(tmp2, name)] = 2
y_p[match(tmp3, name)] = 3

power_table = cbind(power_table, y)
write.csv(power_table, file="power_matrix.csv")

data_table = read.table('word_matrix.csv', header = T)
