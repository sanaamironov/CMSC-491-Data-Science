library(tidyverse)
setwd("/Users/sanaamironov/Desktop/Assigment 3")
load("LendingClub")

# Let's take a look at the data set
glimpse(LendingClub )


# Scatter plot of Speed vs Stopping Distance
# Note that you can pass hexidecimal codes for your 
# favorite colors into geom functions

ggplot(data = LendingClub, mapping = aes(x = loan_amnt, y = adjusted_annual_inc)) +
  geom_point(color = "#00a0e1") +
  #ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Loan Amount vs. Annual Income",
       x = "Loan Amount",
       y = "Annual Income") +
  theme_light()

# Fit the linear regression model and assign it to "MyModel"
MyModel = lm(adjusted_annual_inc ~ loan_amnt , data = LendingClub)


# Coefficient Estimates, t statistics, p-values, R-squared, 
# and residual standard error
summary(MyModel)

## ANOVA (Analysis of Variance) Table
anova(MyModel)

## Obtaining the Parameter Estimates
coef(MyModel)

## 95% Confidence Intervals for the Parameters
confint(MyModel, level = 0.95)

## 95% Confidence Interval for the Mean Value of Y Given X = x
predict(MyModel,
        newdata = data.frame(loan_amnt = 80), # x value entered as a data frame
        interval = "confidence")

# We can also obtain CIs for multiple values of dist
# Now dist is entered as a vector inside of a data frame
predict(MyModel,
        newdata = data.frame(loan_amnt = c(80, 82, 90)),
        interval = "confidence")

## 95% Confidence Interval for an Individual Value of Y Given X = x
predict(MyModel,
        newdata = data.frame(loan_amnt = 80), # x value entered as a data frame
        interval = "prediction")

# We can also obtain CIs for multiple values of dist
# Now dist is entered as a vector inside of a data frame
predict(MyModel,
        newdata = data.frame(loan_amnt = c(80, 82, 90)),
        interval = "prediction")

## Plotting the Results

# Plot the data with the linear regression line
# This includes 95% confidence bands for the mean of adjusted_annual_inc
ggplot(data = LendingClub, mapping = aes(x = loan_amnt, y = adjusted_annual_inc)) +
  geom_point(color = "orange") + 
  geom_smooth(method = "lm") +
  theme_light()

# Let's see what's available in our lm object, "MyModel"
# The names() function returns the names 
names(MyModel)

# We can extract these terms with the $ operator
# The important terms for us will be the "residuals" and "fitted.values"
MyModel$coefficients


# We build a data frame with the original data, the residuals (errors) and
# predictions from our linear model
ModelResults = data.frame(adjusted_annual_inc = LendingClub$adjusted_annual_inc,
                          loan_amnt = LendingClub$loan_amnt,
                          residuals = MyModel$residuals,
                          predictions = MyModel$fitted.values)

# View the results
glimpse(ModelResults)


## Histogram of Residuals

ggplot(data = ModelResults, mapping = aes(x = residuals)) +
  geom_histogram(fill = "blue", color = "white", bins = 15) +
  labs(title = "Histogram of Residuals",
       x = "Residual Value",
       y = "Number of Observations") +
  theme_light()

## Scatter Plot of Residuals
ggplot(data = ModelResults, mapping = aes(x = predictions, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red") + # adds red horizontal line at y = 0
  theme_light()



ggplot(data = LendingClub, aes(x=loan_amnt)) + 
  geom_histogram(binwidth=1)

p<-ggplot(data = LendingClub, aes(x=loan_amnt)) + 
  geom_histogram(color="blue", fill="white") +labs(title = "Histogram of Loan Amount",
                                                   x = "Loan Amount", 
                                                   y = "Frequency of Loan Amount")
p

