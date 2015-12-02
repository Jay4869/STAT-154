library(foreach)
library(randomForest)
library(doParallel)
library(doMC)
library(caret)

n = 8

# Loading data
word = read.csv("word_matrix.csv") # last column is "y"
word = word[,-c(1,2)]
word$y = as.factor(word$y)
levels(word$y) = c("Child", "History", "Religion", "Science")

#=======================================================
cl <- makeCluster(n)
registerDoParallel(cl,cores=n)
registerDoMC(cores=n)
getDoParWorkers()

#=================caret=================================
# tune mtry
start = proc.time()
grid = expand.grid(mtry = c(10,40,750,1531))

tc = trainControl(method="cv",number=10,classProbs = TRUE)
rf_model = train(y~.,data=word, method="rf", trControl=tc,
                prox=TRUE,allowParallel=TRUE,tuneGrid=grid, ntree=500,nodesize=100)
end = proc.time()
end - start

print(rf_model)
print(rf_model$finalModel)
print(rf_model$resample)

save.image()
