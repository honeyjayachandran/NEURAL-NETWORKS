library(neuralnet)  # regression
library(nnet) # classification 
library(NeuralNetTools)
library(plyr)

Forest <- read.csv(file.choose())
View(Forest)
class(Forest)

str(Forest)

FF<- Forest[,1:11]
View(FF)

# Convert month and day string variables into numeric values
FF$month <- as.numeric(as.factor(FF$month))
FF$day <- as.numeric(as.factor(FF$day))

# The area value has lots of zeros

hist(FF$area)
rug(FF$area)

# Transform the Area value to Y 

FF1 <- mutate(FF, y = log(area + 1))  # default is to the base e, y is lower case
hist(FF1$y)

summary(FF1) # Confirms on the different scale and demands normalizing the data.


# Prediction of Forest fires requires only prediction from 

# Apply Normalization technique to the whole dataset :

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

FF_norm<-as.data.frame(lapply(FF1,FUN=normalize))
#FF_norm = cbind(FF[,c(1,2)], FF_norm)
summary(FF1$area) # Normalized form of area

summary(FF_norm) # Orginal  value

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(FF_norm), replace = TRUE, prob = c(0.7,0.3))
FF_train <- FF_norm[ind==1,]
FF_test  <- FF_norm[ind==2,]
# to train model

# Creating a neural network model on training data
FF_model <- neuralnet(area~.,data = FF_train)
str(FF_model)

plot(FF_model, rep = "best")

summary(FF_model)
#Error is 0.0161. SSE sum of squared errors . least SSE best model

# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(FF_model,FF_test)
str(model_results)
predicted_strength <- model_results$net.result

cor(predicted_strength,FF_test$area)
plot(predicted_strength,FF_test$area)


mean(predicted_strength==FF_test$area)


#Building Model 2
model_5<-neuralnet(area~.,data= FF_train,hidden = 5,linear.output = T)

plot(model_5)

#Evaluating model performance
model_5_res<-compute(model_5,FF_test)
model_5_res$net.result
str(model_5_res)

pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,FF_test$area)
plot(pred_strn_5,FF_test$area)

length(pred_strn_5)
length(FF_test$area)
mean(pred_strn_5)
mean(FF_test$area)

mean(pred_strn_5==FF_test$area)

