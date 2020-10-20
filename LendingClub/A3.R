library(tidyverse)
library(caret)
library(e1071)
library(mlbench)
library(psych)
library(readxl)
library(caret)


#load in the data
setwd("/Users/sanaamironov/Desktop/Assigment 3")
load("LendingClub")
describe(LendingClub)
sapply(LendingClub,class)
summary(LendingClub)
View(LendingClub)
glimpse(LendingClub)

#########################################################################################################################\
#Historgram
FullModel = lm(loan_default ~ . , data = LendingClub)
summary(FullModel)
anova(FullModel)
coef(FullModel)

confint(FullModel, level = 0.95)
            
#Historgram 
ggplot(data = LendingClub, mapping = aes(x = loan_amnt)) +
  geom_histogram(fill = "Blue", color = "white", bins = 100) +
  labs(title = "Histogram of Loan Amount",
       x = "Loan Amount", 
       y = "Frequency of Loan Amount")

loan = LendingClub$loan_amnt
mean(loan)
median(loan)
#########################################################################################################################
# Multiple Linear Regression
# Model number 1
# Dependent Variable: Loan_default 
# Independent Variables: All other set
# Fit the full model, using all predictor variables
FullModel = lm(loan_default ~ . , data = LendingClub)
 
## F-Test: Is At Least One Predictor Related to the Response Variable?

summary(FullModel)

## Partial F-Test: Are a Subset of the Predictor Variables Related to the Response?

FullModel = lm(loan_default ~ . , data = LendingClub)
ReducedModel = lm(loan_default ~ loan_amnt + pct_loan_income + dti +residence_property + inq_last_6mths + bc_util, data = LendingClub)

# Now pass them into the anova function, with the reduced model first
anova(ReducedModel, FullModel)

# View the Full Model Once Again
summary(FullModel)
summary(ReduceModel)

TempModel = lm(loan_default ~  loan_amnt, data = LendingClub)
summary(TempModel)

# Beta3 Test
FullModel = lm(loan_default ~ . , data = LendingClub)
ReducedModelBeta3 = lm(loan_default ~dti + pub_rec_bankruptcies+ num_accts_ever_120_pd +
                         adjusted_annual_inc + open_acct, data = LendingClub) 

# All
anova(ReducedModelBeta3, FullModel) # Remember, reduced model first

## Multicolinearity
LoanDefault = lm(loan_default ~loan_amnt, data = LendingClub)

summary(LoanDefault)

AdjustedIncLR = lm(loan_default ~ open_acc + adjusted_annual_inc, data = LendingClub)

summary(AdjustedIncLR)

#########################################################################################################################
# Multiple Linear Regression
# Model number 2
# Fit the full model, using all predictor variables
FullModel = lm(loan_default ~ loan_amnt + adjusted_annual_inc + dti + open_acc + inq_last_6mths, data = LendingClub)
#inquire means they are activityl looking for money, might mean bad $ position
#dti low and home owner then equity can be offered as collateral
ReducedModel = lm(loan_default ~ loan_amnt, data = LendingClub)

anova(ReducedModel, FullModel)

#########################################################################################################################
# Logistic Regression Model 1

#Sample  1
LendingClub = rbind(sample_n(filter(LendingClub, loan_default==1), 1000), sample_n(filter(LendingClub, loan_default==0), 1000))


#Separating Test and Training Data
TrainIndex = sample(1:nrow(LendingClub), round(0.7*nrow(LendingClub)))
LendTrain = LendingClub[TrainIndex, ] 
LendTest = LendingClub[-TrainIndex, ] 

# Let's predict lendingClub with the multiple Predictors
LendLogit = glm(loan_default ~ .,
                data= LendTrain, 
                family = "binomial") # for logistic, this is always set to "binomial"

summary(LendLogit)

#creates a new column called EstimatedProb in LendTest
LendTest = LendTest %>% 
  mutate(EstimatedProb = predict(LendLogit,
                                 newdata = LendTest,
                                 type = "response"))

##############Probability 1
# Now let's predict Y = 1 if P(Y = 1) > 0.4
LendTest = LendTest %>% mutate(LendLogitPredicited = I(EstimatedProb > 0.4) %>% as.numeric())

lendTable = table(LendTest$LendLogitPredicited ,LendTest$loan_default)

confusionMatrix(lendTable)



#########################################################################################################################
# Logistic Regression Model 2
#Sample 2
LendingClub = rbind(sample_n(filter(LendingClub, loan_default==1), 500), sample_n(filter(LendingClub, loan_default==0), 500))

#Separating Test and Training Data
TrainIndex = sample(1:nrow(LendingClub), round(0.7*nrow(LendingClub)))
LendTrain = LendingClub[TrainIndex, ] 
LendTest = LendingClub[-TrainIndex, ] 

# Let's predict lendingClub with the defere variable
LendLogit = glm(loan_default ~ .,
                data= LendTrain, 
                family = "binomial") # for logistic, this is always set to "binomial"

summary(LendLogit)

#creates a new column called EstimatedProb in LendTest
LendTest = LendTest %>% 
  mutate(EstimatedProb = predict(LendLogit,
                                 newdata = LendTest,
                                 type = "response"))

##############Probability 2
# Now let's predict Y = 1 if P(Y = 1) > 0.6
LendTest = LendTest %>% mutate(LendLogitPredicited = I(EstimatedProb > 0.4) %>% as.numeric())

lendTable = table(LendTest$LendLogitPredicited ,LendTest$loan_default)

confusionMatrix(lendTable)


#########################################################################################################################
#Naive Bayes
## 65% of the sample size
smp_size <- floor(0.65 * nrow(LendingClub))

## set the seed to make your partition reproducible
set.seed(99679)

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


