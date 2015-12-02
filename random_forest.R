library(randomForest)

# Loading data
word = read.csv("word_matrix.csv") # last column is "y"
power = read.csv("power_matrix.csv")
filenames = word[,1]
word = word[,-c(1,2)]
power = power[,-1]
word$y = as.factor(word$y)
power$y = as.factor(power$y)
combine = cbind(word[,-ncol(word)], power)

# Classification on the filtered Word Feature Matrix
# parameters to be tuned: mtry, ntree
set.seed(1)
k=10
fold=sample(1:k, nrow(word), replace=TRUE)
ntree = seq(100,2500,100)

# cross validation on word
mtry = c(seq(50,ncol(word), 100), ncol(word))
parameters = expand.grid(ntree, mtry)
colnames(parameters) = c("ntree","mtry")
cv.acc.word = matrix(NA,k,nrow(parameters))
for (i in 1:k){
  train = word[fold!=i,]
  test = word[fold==i,]
  test.y = word$y[fold==i]
  for (j in 1:nrow(parameters)){
    rf.word = randomForest(y~., data=train, ntree=parameters[j,1], 
                           mtry=parameters[j,2])
    rf.pred= predict(rf.word, newdata=test,type="class")
    confusion.matrix = table(rf.pred, test.y)
    cv.acc.word[i,j] = (confusion.matrix[1,1]+confusion.matrix[2,2]+
                        confusion.matrix[3,3]+confusion.matrix[4,4])/sum(confusion.matrix)
  }
}

# cross validation on power
mtry2 = c(seq(50,ncol(power), 100), ncol(power))
parameters2 = expand.grid(ntree, mtry2)
colnames(parameters2) = c("ntree","mtry")
cv.acc.power = matrix(NA,k,nrow(parameters2))
for (i in 1:k){
  train = power[fold!=i,]
  test = power[fold==i,]
  test.y = power$y[fold==i]
  for (j in 1:nrow(parameters2)){
    rf.power = randomForest(y~., data=train, ntree=parameters2[j,1], 
                           mtry=parameters2[j,2])
    rf.pred= predict(rf.power, newdata=test,type="class")
    confusion.matrix = table(rf.pred, test.y)
    cv.acc.power[i,j] = (confusion.matrix[1,1]+confusion.matrix[2,2]+
                          confusion.matrix[3,3]+confusion.matrix[4,4])/sum(confusion.matrix)
  }
}

# cross validation on combination
mtry3 = c(seq(50,ncol(combine), 100), ncol(combine))
parameters3 = expand.grid(ntree, mtry3)
colnames(parameters3) = c("ntree","mtry")
cv.acc.combine = matrix(NA,k,nrow(parameters3))
for (i in 1:k){
  train = combine[fold!=i,]
  test = combine[fold==i,]
  test.y = combine$y[fold==i]
  for (j in 1:nrow(parameters3)){
    rf.combine = randomForest(y~., data=train, ntree=parameters3[j,1], 
                            mtry=parameters3[j,2])
    rf.pred= predict(rf.combine, newdata=test,type="class")
    confusion.matrix = table(rf.pred, test.y)
    cv.acc.combine[i,j] = (confusion.matrix[1,1]+confusion.matrix[2,2]+
                           confusion.matrix[3,3]+confusion.matrix[4,4])/sum(confusion.matrix)
  }
}
