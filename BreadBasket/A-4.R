#install and load package arules
library(arules)
#install and load arulesViz
library(arulesViz)
#install and load tidyverse
library(tidyverse)
#install and load readxml
library(readxl)
#install and load knitr
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
library(lubridate)
#install and load plyr
library(plyr)
library(dplyr)

#read excel into R dataframe
setwd("/Users/sanaamironov/Desktop/491-Assigment 4")
#Name of dataset
breadBasket <- read_csv("BreadBasket_DMS.csv")
#How many rows do we have
nrow(breadBasket)

#complete.cases(data) will return a logical vector indicating which rows 
#have no missing values. Then use the vector to get only rows that 
#are complete using retail[,].
breadBasket <- breadBasket[complete.cases(breadBasket), ]
#view the data set
view(breadBasket)

#remove all rows with "NONE" to clean up the dataset a bit
breadBasket <- breadBasket[!(breadBasket$Item == "NONE"),]

#mutate function is from dplyr package. It is used to edit or add new columns 
#to dataframe. Here Description column is being converted to 
#factor column. as.factor converts column to factor column. 
#%>% is an operator with which you may pipe values to another function or 
#expression
breadBasket %>% mutate(Item = as.factor(Item))

view(breadBasket)

#Convert and edit TransNum into numeric
TransNum <- as.numeric(as.character(breadBasket$Transaction))

#Bind new columns TransNum into dataframe retail
cbind(breadBasket,TransNum)

#get a glimpse of your data
glimpse(breadBasket)

library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(breadBasket,c("TransNum"),
                         function(df1)paste(df1$Item,
                                            collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used
transactionData

#set column TransNum of dataframe transactionData  
transactionData$TransNum <- NULL
#set column Date of dataframe transactionData
#transactionData$Item <- NULL
#Rename column to items
#colnames(transactionData) <- c("items")
#Show Dataframe transactionData
transactionData

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
#transactionData: Data to be written
#"D:/Documents/market_basket.csv": location of file with file name to be written to
#quote: If TRUE it will surround character or factor column with double quotes. If FALSE nothing will be quoted
#row.names: either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written.

tr <- read.transactions("market_basket_transactions.csv", format = 'basket', sep=',')
#sep tell how items are separated. In this case you have separated using ','

tr
summary(tr)
# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, minlen = 2))

################################################################################################
count(breadBasket$Item)
itemsHere <- unique(breadBasket$Item)
view(itemsHere)
count(itemsHere)


#see the association rules set for the dataset
summary(association.rules)

inspect(association.rules[1:7])

