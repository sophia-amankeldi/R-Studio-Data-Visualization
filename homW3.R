########################################
# ENMA 646  Spring 2022
# Homework 3
# Student name: Sophia A
# email: saman001@odu.edu
# Date: 
########################################

install.packages("ggplot2")
library(ggplot2)
install.package("dplyr")
library(dplyr)
library(tidyverse)
library(caret)
library(tidyr)


#Preparation 
## read in data
df <- read.csv("shipCBM2014_3k", stringsAsFactors = TRUE)
df <- read.csv(file.choose(), stringsAsFactors = TRUE) 
# select data file "shipCBM2014_3k"
df <- shipCBM2014_3k

str(df)
View(df)
summary(df)
summary(df$kMt)
summary(df$kMt.cat)

summarise(df, avg=mean(kMt, na.rm=TRUE))
which(is.na(df))


## GT Turbine decay state coefficient distribution.
#plotting Histogram

ggplot(data = df, aes(x=kMt, fill = kMt.cat)) +
  geom_bar(position = position_dodge()) +
  labs(x="KMT", y="KMT categories", title = "Relationship between GT Turbine decay state coefficient and its categories")+
  theme_minimal()

## Plotting a  scatterplot to observe KMC distribution by speed

df %>%
  ggplot(aes(x=kMt, speed, color=kMt.cat)) +
  geom_point() +
  labs(title = "GT Turbine decay state coefficient distribution by Speed of ships",
       x = "KMt", 
       y= "Speed") 

#Splitting data into training and test set
qplot(kMt.cat, data = df, fill = I("darkgreen") )

set.seed(5)
ndx <- createDataPartition(y=df$kMt.cat, p=0.7, list = FALSE)
df.train <- df[ndx, ]
df.test <- df[-ndx, ]

# Random grid search by default
train.ctrl.random <- trainControl(method="repeatedcv", number=5, repeats=3, 
                                  search = "random",
                                  classProbs = TRUE,
                                  summaryFunction = multiClassSummary)
# Modeling with  models 
# SVM linear classifier 
install.packages('MLmetrics')
library(MLmetrics)
install.packages("doParallel")
library(doParallel)

start.time <- proc.time()
Svml <- train(kMt.cat ~., data = df.train, 
              method = "svmLinear", 
              preProcess = c("center","scale"),
              tuneLength = 3, 
              metric = "AUC",
              trControl = train.ctrl.random)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)


# View the model
svml
