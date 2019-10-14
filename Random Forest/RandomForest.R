setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Decission Tree")
#install.packages("randomForest")
library(randomForest)
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

#______________________________Random Forest in Company data set__________________________________-

company <- read.csv("Company_Data.csv")
head(company)
nrow(company)
str(company)
boxplot(company)$out # 9 data point as outliers
boxplot(scale(company[,c(1:6,8,9)])) # Sales, CompPrice and Price contains outlier.

# Outlier treatment
boxplot(company)$out
outlie1 <- which(company$Sales >= min(boxplot(company$Sales)$out))#317,377
outlie2 <- which(company$CompPrice == max(boxplot(company$CompPrice)$out)
                 |company$CompPrice == min(boxplot(company$CompPrice)$out) )#43,311
outlie3 <- which(company$Price <= 53 | company$Price >= 185)
outlier <- c(outlie1,outlie2,outlie3)
company_o <- company[-outlier,]
boxplot(company_o)$out
boxplot(company_o$Price)$out
company_final <- company_o[company_o$Price >55,]
nrow(company_final)

SaleC <- ifelse(company_final$Sales > 8.5,"High","Low")
df_c <- data.frame(company_final[,-1],SaleC)
# Presence of missing values
colSums(is.na(df_c)) # No missing data in my data set

# Train Test Split of Data.
set.seed(101);splitC <- sample(nrow(company_final),nrow(company_final)*.7,F)
trainC <- df_c[splitC,]
testC <- df_c[-splitC,]

trainC_n <- normalize_dummy(trainC[,-11])
cn <- c("CompPrice","Income","Advertising","Population","Price","Age","Education","CBad","CGood","CMedium","CNo","CYes","CNo.1","CYes.1")
colnames(trainC_n) <- cn
testC_n <- normalize_dummy(testC[,-11])
colnames(testC_n) <- cn


# Model Building Model no 1
set.seed(101);modelC1 <- randomForest(trainC$SaleC~.,data = trainC[,-11])
summary(modelC1)
predC1 <- predict(modelC1,testC) 
table(Actual=testC$SaleC,Predicted=predC1)
mean(testC$SaleC==predC1) # 82.9% accuracy
plot(modelC1)
varImpPlot(modelC1, pch = 20, main = "Importance of Variables")
# Model 2 Building with normalised values
set.seed(101);modelC2 <- randomForest(trainC$SaleC~.,data = trainC_n)
summary(modelC2)
predC2 <- predict(object = modelC2,newdata = testC_n) # If throughs Error Re run the whole code

table(Actual=testC$SaleC,Predicted=predC2)
mean(testC$SaleC==predC2) # 79.487% accuracy
plot(modelC2)

# Tuning the random Forest
set.seed(101);tune <- tuneRF(trainC[,-11], trainC[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 400,
               trace = TRUE, improve = 0.05)

# Model 3
set.seed(101);model3C <- randomForest(trainC$SaleC~.,data = trainC[,-11], ntree = 400, mtry = 3, importance = TRUE,proximity = TRUE)
predC3 <- predict(model3C,testC[,-11])
table(Actual=testC$SaleC,Predicted=predC3)
mean(testC$SaleC==predC3) # 84.6%

#_________________________________________Random Forest in Fraud Check data__________________________________________________________________

# Question no 2 Fraud_Check

df = read.csv("Fraud_check.csv")
head(df)
# Encoding Risky and Good
GR <- ifelse(df$Taxable.Income<30000,"Risky","Good")
Fraud <- data.frame(df[,-3],GR) # Removed the column Taxable income and adding the Column GR i.e. categorical.
# Here we will remove the column Taxable income from our data frame, as If we consider the column 
# using the Random forest in this model will be useless, as we can classify the whole data with the single 
# variable itself. So Our main focus is here to see how the Risky And Good is related to other vatiables in our data


boxplot(Fraud) #no sign of outliers in the data
barplot(table(Fraud$GR))
str(Fraud)
colSums(is.na(Fraud)) # No outliers in my data
# Train Test Splitting
set.seed(101);splitF <- sample(nrow(Fraud),nrow(Fraud)*.7,F)
Train_F <- Fraud[splitF,]
Test_F <- Fraud[-splitF,]

set.seed(101);ModelF1 <- randomForest(Train_F$GR~.,data = Train_F[,-6])
predF1 <- predict(ModelF1,Test_F[,-6])
mean(Test_F$GR==predF1) # accuracy is 805556
table(Actual=Test_F$GR,Predicted=predF1)
plot(ModelF1)


set.seed(101);ModelF2 <- randomForest(Train_F$GR~.,ntree = 400,mtry = 5,data=Train_F)
predF2 <- predict(ModelF2,Test_F[,-6])
mean(Test_F$GR==predF2) #0.73333
plot(ModelF2)

# Tuning of random forest
set.seed(101);tune <- tuneRF(Train_F[,-6], Train_F[,6], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
                             trace = TRUE, improve = 0.5)

# We are getting least error when Mtry is 1 
set.seed(101);ModelF3 <- randomForest(Train_F$GR~.,ntree = 400,mtry = 1,data=Train_F)
predF3 <- predict(ModelF3,Test_F[,-6])
mean(Test_F$GR==predF3) #0.8166667
table(Actual = Test_F$GR,Predicted=predF3)
plot(ModelF3)
