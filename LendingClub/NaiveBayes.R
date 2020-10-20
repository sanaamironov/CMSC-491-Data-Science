library(mlbench)
library(e1071)

setwd("/Users/sanaamironov/Desktop/Assignment 3")
load("LendingClub")
sapply(LendingClub,class)
#Naive Bayes
## 75% of the sample size
smp_size <- floor(0.75 * nrow(LendingClub))

## set the seed to make your partition reproducible
set.seed(99668)

train_ind <- sample(seq_len(nrow(LendingClub)), size = smp_size)
##it choose the row and then train_ind is rows
train <- LendingClub[train_ind, ]
##choose all the rows any of the train_ind
test <- LendingClub[-train_ind, ]

NVmodel <- naiveBayes(loan_default ~ ., data = train)
##print  NVmodel
preds <- predict(NVmodel, newdata = test)
##print preds  to see what it predicted
conf_matrix <- table(preds, test$loan_default)
##print conf_matrix
confusionMatrix(conf_matrix)
##printk the NVmodel
NVmodel$tables

