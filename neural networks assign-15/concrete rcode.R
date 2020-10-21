concrete <- read.csv(file = file.choose())
View(concrete)
str(concrete)
concrete_scale<-as.data.frame(scale(concrete))
#### creating test and train data set
sam <- sample(2,size = nrow(concrete_scale), replace =TRUE, prob = c(0.7,0.3))
concrete_train <- concrete_scale[sam==1,]
concrete_test <- concrete_scale[sam==2,]

#### neural network
library(neuralnet)  # regression
library(nnet) # classification 
concrete_model <- neuralnet(strength~.,data = concrete_train,hidden = c(5,2))

summary(concrete_model)
str(concrete_model)
plot(concrete_model)

model_results <- compute(concrete_model,concrete_test[,1:8])

attributes(model_results)

str(model_results)

p <- model_results$net.result

p

cor(p,concrete_test$strenght)

plot(p,concrete_test$strenght)


