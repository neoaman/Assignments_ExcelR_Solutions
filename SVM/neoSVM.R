"_______________________________________________SUPPORT VECTOR MACHINE___________________________________________"

library(kernlab)
library(caret)

# Question no 1 ----
# Prepare support vector machines model for classifying the area under fire for foresfires data



normalize_dummy <- function(x){
  col <- ncol(x)
  row <- nrow(x)
  y <- 1:nrow(x)
  for (i in 1:col){
    if(class(x[,i])=="numeric" | class(x[,i])=="integer")
    {
      minx <- min(x[,i])
      maxx <- max(x[,i])
      for(j in 1:row)
      {
        x[j,i] <- ifelse((x[j,i] - minx) != 0,yes =((x[j,i] - minx) / (maxx - minx)),no = 0)
      }
    }
    
  }
  f <- c()
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor"){
      dummies <- data.frame(dummies::dummy(x[,i]))
      y <- data.frame(y,dummies)
      f <- c(f,i)
    }
    else{
      next
    }
  }
  if(is.null(f)){
    output <- x
  }
  else{output <- data.frame(x[,-f],y[,-1])}
  return(output)
}



forestfire <- read.csv("forestfires.csv")
head(forestfire)
str(forestfire)
dim(forestfire)
norm_ff <- normalize_dummy(forestfire[,-31])
dim(norm_ff)
table(forestfire$size_category)

# Train and Test Splitting 
set.seed(101)
split <- sample(nrow(norm_ff), nrow(norm_ff)*0.7, replace = TRUE)
train_ff <- norm_ff[split,]
test_ff  <- norm_ff[-split,]

# All possible karnel models "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", "besseldot", "anovadot", "splinedot", "matrix"

modelF1<-ksvm(forestfire$size_category[split]~temp+rain+wind+RH, 
             data= train_ff,kernel = "vanilladot")

PredArea1 <- predict(modelF1, test_ff)
table(Predicted=PredArea1,Actual=forestfire$size_category[-split])
methodss <- c( "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", "besseldot", "anovadot", "splinedot", "matrix")

SVMinfo = list();SVMtable = list();SVMAccu = list()
for (i in methodss){
  modelF<-ksvm(forestfire$size_category[split]~temp+rain+wind+RH, 
                data= train_ff,kernel = i)
   PredArea <- predict(modelF, test_ff)
  SVMinfo[[i]] <- PredArea
  SVMtable[[i]] <- table(Predicted=PredArea,Actual=forestfire$size_category[-split])
  SVMAccu[[i]] <- mean(PredArea==forestfire$size_category[-split])
}
SVMinfo
SVMtable
SVMAccu

MLmetrics::F1_Score(y_pred = SVMinfo$vanilladot,y_true =forestfire$size_category[-split] )
MLmetrics::F1_Score(y_pred = SVMinfo$rbfdot,y_true =forestfire$size_category[-split] )
MLmetrics::F1_Score(y_pred = SVMinfo$tanhdot,y_true =forestfire$size_category[-split] )


"_____________________________________________________________________________________________________________"

# Prepare a classification model using SVM for salary data 

train_salary <- read.csv("SalaryData_Train(1).csv")
test_salary <- read.csv("SalaryData_Test(1).csv")

head(train_salary)

salarytrain_norm <- normalize_dummy(train_salary)

modelS1 <- ksvm(Salary~.,data=train_salary)
predS1 <- predict(modelS1,test_salary)
table(test_salary$Salary,predS1)
mean(test_salary$Salary==predS1) # 0.8541


SVMinfo2 <- list()
SVMtable2 <- list()
SVMAccu2 <- list()
for(i in methodss){
  modelS <- ksvm(Salary~.,data=train_salary,kernel = i)
  predS <- predict(modelS,test_salary)
  SVMinfo2[[i]] <- predS
  SVMtable2[[i]] <- table(test_salary$Salary,predS)
  SVMAccu2[[i]] <- mean(test_salary$Salary==predS)
  
}

SVMAccu2 # Here I am getting my most accuracy using my karnel method as vanilladot i.e. 0.8562815
SVMinfo2
SVMtable2

SVMinfo2$vanilladot
SVMinfo2$vanilladot
SVMinfo2$vanilladot

SVMtable2$vanilladot
SVMtable2$rbfdot
SVMtable2$laplacedot

MLmetrics::F1_Score(y_pred = SVMinfo2$rbfdot,y_true = test_salary$Salary)
MLmetrics::F1_Score(y_pred = SVMinfo2$vanilladot,y_true = test_salary$Salary)
MLmetrics::F1_Score(y_pred = SVMinfo2$laplacedot,y_true = test_salary$Salary)
