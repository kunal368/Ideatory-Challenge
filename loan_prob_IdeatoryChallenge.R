#Ideatory Practice challenge
#Bad Loans Prediction from Loan Application Dataset
#Date 22/06/2016

#Loading Datasets
loan <- read.csv("Practice_Training.csv")
testloan <- read.csv("Practice_Evaluation.csv")

#Inspecting Dataset
str(loan1)
str(loan)

#Converting Dependent Variable to Factor type  
loan$Loan.Status <- as.factor(loan$Loan.Status)
summary(loan$Loan.Status)
testloan$Loan.Status <- NULL
summary(loan$Loan.Duration)

#Converting Loan Duration Variable to Numeric Type and removing char months from it
loan$Loan.Duration <- as.numeric(loan$Loan.Duration)
summary(loan$Loan.Duration)
loan$Loan.Duration  <-  as.character(loan$Loan.Duration)

strsplit(loan$Loan.Duration[1], split='[ ]')
strsplit(loan$Loan.Duration[1], split='[ ]')[[1]][1]
loan$Loan.Duration <- sapply(loan$Loan.Duration, FUN=function(x) {strsplit(x, split='[ ]')[[1]][1]})
loan$Loan.Duration[1]
loan$Loan.Duration  <-  as.numeric(loan$Loan.Duration)

#Doing the same for test dataset
testloan$Loan.Duration <- as.character(testloan$Loan.Duration)
testloan$Loan.Duration <- sapply(testloan$Loan.Duration, FUN=function(x) {strsplit(x, split='[ ]')[[1]][1]})
testloan$Loan.Duration <- as.numeric(testloan$Loan.Duration)

#Converting Interest Rate Variable to Numeric Type and removing char '%' from it
loan$Interest.Rate[1]
loan$Interest.Rate <- as.character(loan$Interest.Rate)
strsplit(loan$Interest.Rate[1], split='[%]')[[1]][1]
loan$Interest.Rate <- sapply(loan$Interest.Rate, FUN=function(x) {strsplit(x, split='[%]')[[1]][1]})
loan$Interest.Rate <- as.numeric(loan$Interest.Rate)

#Doing same for test dataset
testloan$Interest.Rate <- as.character(testloan$Interest.Rate)
strsplit(testloan$Interest.Rate[1], split='[%]')[[1]][1]
testloan$Interest.Rate <- sapply(testloan$Interest.Rate, FUN=function(x) {strsplit(x, split='[%]')[[1]][1]})
testloan$Interest.Rate <- as.numeric(testloan$Interest.Rate)
testloan$Interest.Rate[1]

summary(loan$Interest.Rate)

#Introducing Null Loan Status Column in Test DataSet
testloan$Loan.Status <- NA

#Combining training and testing dataset 
combi <- rbind(loan, testloan)
str(combi)

#Converting Loan Issue Date to Date Variable from factor variable 
combi$Loan.Issue.Date[1]
combi$Loan.Issue.Date <- as.character(combi$Loan.Issue.Date)
combi$Loan.Issue.Date <- as.Date(combi$Loan.Issue.Date, format = "%d-%m-%y" )

#Converting Date.of.Borrower.s.First..Loan to Date Variable from factor variable 
combi$Date.of.Borrower.s.First..Loan[2]
combi$Date.of.Borrower.s.First..Loan <- as.character(combi$Date.of.Borrower.s.First..Loan)
combi$Date.of.Borrower.s.First..Loan <- as.Date(combi$Date.of.Borrower.s.First..Loan, format = "%d-%m-%y" )

#Converting Last.Month.Payment.was.Received to Date Variable from factor variable 
combi$Last.Month.Payment.was.Received[1]
combi$Last.Month.Payment.was.Received <- as.character(combi$Last.Month.Payment.was.Received)
combi$Last.Month.Payment.was.Received <- as.Date(combi$Last.Month.Payment.was.Received, format = "%d-%m-%y" )

#Splitting combined dataset again into testing and training dataset
train_loan <- combi[1:24000,]
test_loan <- combi[24001:40538, ]

#Loading libraries for CART model
library(rpart)
library(rpart.plot)

#Making CART model
classificationtrees <- rpart(Loan.Status ~ .-Loan.Application.Number, 
                             data = train_loan, method = "class")

#Plotting Classification tree
prp(classificationtrees)

#Making Predictions on test dataset 
loanstatus <- predict(classificationtrees, newdata = test_loan, type = "class")
table(loanstatus)

#Writing to output csv file
submit <- data.frame(test_loan$Loan.Application.Number, loanstatus)
write.csv(submit, file = "loan_status1.csv", row.names = FALSE)

#Loading libraries for cross validation Method
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid( .cp = (0:10)*0.001)

#Cross Validation Method
tr = train(Loan.Status ~ .-Loan.Application.Number, data= train_loan, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

# Cross Validation gives Optimal value of 0.001
tr

#Making CART model with optimal cp =0.001
classificationtrees1 <- rpart(Loan.Status ~ .-Loan.Application.Number, 
                              data = train_loan, method = "class", cp = 0.001)
#Making Predictions
loanstatus_0.001 <- predict(classificationtrees1, newdata = test_loan, type = "class")

#Writing to output csv file
submit <- data.frame(test_loan$Loan.Application.Number, loanstatus_0.001)
write.csv(submit, file = "loan_statuscp_0.001.csv", row.names = FALSE)