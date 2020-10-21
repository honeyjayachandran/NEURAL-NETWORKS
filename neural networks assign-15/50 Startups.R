
start_up <- read.csv("C:\\Users\\Sony\\Downloads\\neural networks assign-15\\50_Startups.csv")
View(start_up)
str(start_up)
start_up <- start_up[,c("R.D.Spend","Administration",
                        "Marketing.Spend","Profit","State")]

#### Normalization

normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x)))
}
start_up_norm <- as.data.frame(lapply(start_up[,-5], normalize))
start_up_norm$State <- as.integer(start_up$State)
str(start_up_norm)

#### creating test and train data set

sam <- sample(x = 2,size = nrow(start_up),replace = TRUE,prob = c(0.7,0.3))
train <- start_up_norm[sam==1,]
test <- start_up_norm[sam==2,]

#### neural network

library(neuralnet)
NN <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend,
                data = train, 
                hidden = c(2,1),
                threshold = 0.01,
                linear.output = TRUE
                )
plot(NN)

#### finding the accuracy
nn.results <- compute(NN , test[,-4])
results <- data.frame(Actual = test$Profit, Prediction = nn.results$net.result)
rounded.results.df <- as.data.frame(sapply(results,round,digits=0))
attach(rounded.results.df)
tab <- table(Actual, Prediction)
library(caret)
confusionMatrix(tab) #accuracy =92.86%

detach(rounded.results.df)

cor(nn.results$net.result,test$Profit) #correlation=0.9547

