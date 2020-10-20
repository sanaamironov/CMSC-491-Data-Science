library(tidyverse)
library(caret)
library(e1071)

setwd("/Users/sanaamironov/Desktop/Assignment 3")
load("LendingClub")
sapply(LendingClub,class)
set.seed(99679)


# Logistic Regression
LendingClub = rbind(sample_n(filter(LendingClub, loan_default==1), 1000), sample_n(filter(LendingClub, loan_default==0), 1000))
#Separating Test and Training Data
TrainIndex = sample(1:nrow(LendingClub), round(0.7*nrow(LendingClub)))
LendTrain = LendingClub[TrainIndex, ] 
LendTest = LendingClub[-TrainIndex, ] 

LendLogit = glm(loan_default ~ .,
                 data= LendTrain, family = "binomial") # for logistic, this is always set to "binomial"


LendTest = LendTest %>% 
  mutate(EstimatedProb = predict(LendLogit,
                                 newdata = LendTest,
                                 type = "response"))

# Now let's predict Y = 1 if P(Y = 1) > 0.4
LendTest = LendTest %>% mutate(LendLogitPredicited = I(EstimatedProb > 0.4) %>% as.numeric())

confusionMatrix(table(LendTest$LendLogitPredicited ,LendTest$loan_default))



