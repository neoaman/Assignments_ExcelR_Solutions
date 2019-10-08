# Question No 2
# Importing Library 
library(e1071)

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

# getting the data ----
SAl_train = read.csv("SalaryData_Train.csv")
SAL_test = read.csv("SalaryData_Test.csv")
head(SAl_train)
head(SAL_test)
str(SAl_train)
barplot(table(SAL_test$Salary),col=c("pink","lightblue"),main="Salary in our Test Set",xlab = "Salary")
df_train <- normalize_dummy(SAl_train[,-14])
head(df_train) # Awsome we comeup with our required table
df_test = normalize_dummy(SAL_test)[,-14];
barplot(table(SAl_train$Salary),col=c("pink","lightblue"),main="Salary in our Train Set",xlab = "Salary")
boxplot(df_train[1:5],pch=20,cex=0.7)
head(df_train)


# Model 1 Building----
model_1 = naiveBayes(df_train,y = SAl_train$Salary)
summary(model_1)

# Predict our salary for Test Data Set
predict1 = predict(model_1,df_test)
mean(predict1==SAL_test$Salary)

table(Actual=SAL_test$Salary,Predicted=predict1)


# Model 2 Building with laplase smoothing ----
model_2 = naiveBayes(x=df_train,y = SAl_train$Salary,laplace = 1)
summary(model_2)

# Predict our salary for Test Data Set
predict2 = predict(model_2,df_test)
mean(predict2==SAL_test$Salary)

table(Actual=SAL_test$Salary,Predicted=predict2)



