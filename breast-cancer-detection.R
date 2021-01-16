
#installed packages

library(dplyr)
library(tidyr)
library(caTools)
library(lubridate)
library(lattice)
library(caret)
library(zoo)
library(ggplot2)

# Reading data from downloaded CSV file of my breast cancer detection

breast_cancer_detection <- read.csv("Breast_cancer_data.csv")
head(breast_cancer_detection)
View(breast_cancer_detection)
str(breast_cancer_detection) #structure of the data
summary(breast_cancer_detection)

# Since diagnosis is in binary the results from binary are inaccurate so we need to convert it to factors

breast_cancer_detection$diagnosis = as.factor(breast_cancer_detection$diagnosis)
str(breast_cancer_detection) #structure of the data
summary(breast_cancer_detection)
bc_detection <- breast_cancer_detection
bc_detection

# logistic regression models 

# model 1 

glm_model = glm(diagnosis ~ . , data = bc_detection, family = "binomial")
summary(glm_model)

# AIC level is 181.22 so I'll try the other model to see which one yields the lower AIC value 

# model 2

#splitting the data 

sample.split(bc_detection$diagnosis, SplitRatio = 0.65)->split_index


# using the subset method to divide the model into training and testing 

subset(bc_detection, split_index == T )->train
subset(bc_detection, split_index == F)-> test



# logistic model for train data 

train_data = glm(diagnosis ~ . , train, family = binomial)
summary(train_data) # AIC is 121.98 
predicted = (predict(train_data, newdata = test, type = "response"))
summary(predicted)

# converting it to factors
predictFactor = factor(ifelse(predicted < 0.5, 0, 1)) 
test = cbind(test,predictFactor)
head(test)
View(test)
str(test)
#levels(test) <- c('diagnosis', 'predictFactor')

# using confusion matrix

confusionMatrix(test$diagnosis, test$predictFactor)
# accuracy is 0.9347 
library(yardstick)
library(ggplot2)


# The confusion matrix from a single assessment set (i.e. fold)


breat_cancer_model_plot <- as.table(matrix(c(65, 4, 9, 121), nrow = 2, byrow = TRUE))
fourfoldplot(breat_cancer_model_plot, color = c("pink", "#e75480"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
breat_cancer_model_plot
