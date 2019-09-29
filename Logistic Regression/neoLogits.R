setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\Logistic Regression")
# Importing Packages ----
library(car)
library(ROCR)
#install.packages("MLmetrics")
#install.packages("caret")
library(caret)
library(MLmetrics)

# Question 1 ----
"Whether the client has subscribed a term deposit or not "
# Reading the bank data
Bank <- read.csv("bank-full.csv",sep = ";")
table(Bank$y)
head(Bank)
colnames(Bank)
str(Bank)
summary(Bank);
boxplot(Bank)
boxplot(scale(Bank[,c(1,6,10,12:15)]),ylim = c(-10,20))

#write.csv(summary(Bank),"BankSummary.csv")
  # Train and Test Split and Model Evaluation ----
  set.seed(101)
  Test_Spl <- as.integer(sample(x = rownames(Bank),size = round(nrow(Bank)*(30/100)),replace = F))
  Train_B <- Bank[-c(Test_Spl),]
  Test_B <- Bank[c(Test_Spl),]
  nrow(Train_B);nrow(Test_B)


  # Model Building 1 ----
    model_B1 <- glm(y~.,data = Train_B,family = binomial(link = "logit"))
    summary(model_B1) # AIC = 15017 and Insignificant variables are age pdays balance default age
  #Test Efficiency
      Y_B1 <- predict(model_B1,Test_B)
      prob_B1 <-predict(model_B1,Test_B,type = "response")
      
      plot(prob_B1,Test_B$y,
           col=ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),"green","red")
           # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
      )
      # In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
      plot(Y_B1,col=ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),"green","red"))
      confu_B1 <- table(prob_B1>0.5,Test_B$y) ;confu_B1
      effi_B1 <- sum(diag(confu_B1))/sum(confu_B1);effi_B1 # Efficiency of my model is 0.900317
      # F_1 Score
      pB1 <- ifelse(prob_B1>0.5,1,0)
      ytrueB1 <- ifelse(Test_B$y=="yes",1,0)
      F1_Score(y_true = ytrueB1,y_pred = pB1) # 0.945201
  #Train Efficiency
      Y_B1.1 <- predict(model_B1,Train_B)
      prob_B1.1 <-predict(model_B1,Train_B,type = "response")
      plot(prob_B1.1,Train_B$y,
           col=ifelse((prob_B1.1<0.5 & Train_B$y =="no") | (prob_B1.1>0.5 & Train_B$y =="yes"),"green","red")
           # ,pch = ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),1,4)
      )
      # In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
      plot(Y_B1.1,col=ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),"green","red"))
      confu_B1.1 <- table(prob_B1.1>0.5,Train_B$y) ;confu_B1.1
      effi_B1.1 <- sum(diag(confu_B1.1))/sum(confu_B1.1);effi_B1.1 # Efficiency of my model is 0.9010996
  
  influenceIndexPlot(model_B1,id=list(col="red"))    
  influence_B1 <- as.integer(rownames(influencePlot(model_B1,id=list(n=5,col="red"))))
  length(influence_B1)
  
  # ROC Curve
  rocrpred1<-prediction(prob_B1,Test_B$y)
  rocrperf1<-performance(rocrpred1,'tpr','fpr')
  str(rocrperf1)
  rocrperf1@x.values
  plot(rocrperf1,colorize=T)
  
  rocr_cutoff <- data.frame(cut_off = rocrperf1@alpha.values[[1]],fpr=rocrperf1@x.values,tpr=rocrperf1@y.values)
  colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
  View(rocr_cutoff)
  
  library(dplyr)
  
  rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
  # Sorting data frame with respect to tpr in decreasing order 
  rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
  View(rocr_cutoff)

  
  
  # Model Building 2 ----
  head(Train_B)
  model_B2 <- glm(y~.,data = Train_B[-influence_B1,-c(1,14,5)],family = "binomial")
  summary(model_B2) # AIC = 15010 Insignificant variables are age pdays balance default
  #Test Efficiency
      Y_B2 <- predict(model_B2,Test_B)
      prob_B2 <-predict(model_B2,Test_B,type = "response")
      plot(prob_B2,Test_B$y,
           col=ifelse((prob_B2<0.5 & Test_B$y =="no")|(prob_B2>0.5 & Test_B$y =="yes"),"green","red")
           # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
      )
      # In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
      plot(Y_B2,col=ifelse((prob_B2<0.5 & Test_B$y =="no")|(prob_B2>0.5 & Test_B$y =="yes"),"green","red"))
      confu_B2 <- table(prob_B2>0.5,Test_B$y) ;confu_B2
      effi_B2 <- sum(diag(confu_B2))/sum(confu_B2);effi_B2 # Efficiency of my model is 0.9000958
      
      # F_1 Score
      pB2 <- ifelse(prob_B2>0.5,1,0)
      ytrueB2 <- ifelse(Test_B$y=="yes",1,0)
      F1_Score(y_true = ytrueB2,y_pred = pB2) # 0.9450817
      
  #Train Efficiency
      Y_B2.1 <- predict(model_B2,Train_B[-influence_B1,-c(1,14,5)])
      prob_B2.1 <-predict(model_B2,Train_B[-influence_B1,],type = "response")
      plot(prob_B2.1,Train_B[-influence_B1,]$y,
           col=ifelse((prob_B2.1<0.5 & Train_B[-influence_B1,]$y =="no") | (prob_B2.1>0.5 & Train_B[-influence_B1,]$y =="yes"),"green","red")
           # ,pch = ifelse((prob_B1.1<0.5 & Train_B$y =="no")|(prob_B1.1>0.5 & Train_B$y =="yes"),1,4)
      )
      # In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
      plot(Y_B2.1,col=ifelse((prob_B2.1<0.5 & Train_B[-influence_B1,]$y =="no")|(prob_B2.1>0.5 & Train_B[-influence_B1,]$y =="yes"),"green","red"))
      confu_B2.1 <- table(prob_B2.1>0.5,Train_B[-influence_B1,]$y) ;confu_B2.1
      effi_B2.1 <- sum(diag(confu_B2.1))/sum(confu_B2.1);effi_B2.1 # Efficiency of my model is 0.9012073
  
  # ROC Curve
  rocrpred2<-prediction(prob_B2,Test_B$y)
  rocrperf2<-performance(rocrpred2,'tpr','fpr')
  str(rocrperf2)
  rocrperf2@x.values
par(mfrow=c(1,2));plot(rocrperf2,colorize=T,main = "ROC for model 2")
plot(rocrperf1,colorize=T,main = "ROC for model 1");par(mfrow=c(1,1)) # Must run this code
  
#cutoff value
rocr_cutoff2 <- data.frame(cut_off = rocrperf2@alpha.values[[1]],fpr=rocrperf2@x.values,tpr=rocrperf3@y.values)
colnames(rocr_cutoff2) <- c("cut_off","FPR","TPR")
View(rocr_cutoff2)

library(dplyr)

rocr_cutoff2$cut_off <- round(rocr_cutoff2$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff2 <- arrange(rocr_cutoff2,desc(TPR))
View(rocr_cutoff2)


# Question 2 ----
"I have a dataset containing family information of married couples, which have around 10 variables & 600+ observations. 
Independent variables are ~ gender, age, years married, children, religion etc.
I have one response variable which is number of extra marital affairs.
Now, I want to know what all factor influence the chances of extra marital affair.
Since extra marital affair is a binary variable (either a person will have or not), 
so we can fit logistic regression model here to predict the probability of extra marital affair."

df <- read.csv("affairs.csv")
EMA <- ifelse(test = df$affairs > 0 ,yes = 'yes',no = 'no')
Affair <- data.frame(df[,-c(2)],EMA)

head(Affair)
colnames(Affair)
str(Affair)
write.csv(summary(Affair),"AffSummary.csv")
boxplot(Affair)
boxplot(data.frame(scale(Affair[,-c(2,5,10)]),Affair[,c(2,5,10)]))
boxplot(Affair$age)$out
boxplot(Affair$X)$out
Affair <- Affair[Affair$X<9000,]
str(Affair)
table(Affair$EMA)


  # Train and Test Split ----
  set.seed(101)
  Split_A <- sample(as.integer(rownames(Affair)),size = round(nrow(Affair)*0.3),replace = F )
  Train_A <- Affair[-Split_A,]
  Test_A <- Affair[Split_A,]
  nrow(Train_A);nrow(Test_A)
  # Model 1 ----
  model_A1 <- glm(EMA~.,data=Train_A,family = 'binomial')
  summary(model_A1) # AIC = 429.34 indignificant variables are X,children,education,occupation
  pred_A1 <- predict(model_A1,Test_A,type = "response")
  confusion_A1 <- table(Test_A$EMA,pred_A1>0.5);confusion_A1
  effi_A1 <- sum(diag(confusion_A1))/sum(confusion_A1);effi_A1 # 0.740113
  plot(pred_A1,Test_A$EMA,
       col=ifelse((pred_A1<0.5 & Test_A$EMA =="no")|(pred_A1>0.5 & Test_A$EMA =="yes"),"green","red")
       # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
  )
  # In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
  plot(pred_A1,col=ifelse((pred_A1<0.5 & Test_A$EMA =="no")|(pred_A1>0.5 & Test_A$EMA =="yes"),"green","red"))
  
  rocrpredA1<-prediction(pred_A1,Test_A$EMA)
  rocrperfA1<-performance(rocrpredA1,'tpr','fpr')
  str(rocrperfA1)
  rocrperfA1@x.values
  plot(rocrperfA1,colorize=T)
  
  influenceIndexPlot(model_A1,id = list(n = 5,col = "red"))
  in_1 <-as.integer(rownames(influencePlot(model_A1,id = list(col = "red"))))
  
  rocr_cutoffA1 <- data.frame(cut_off = rocrperfA1@alpha.values[[1]],fpr=rocrperfA1@x.values,tpr=rocrperfA1@y.values)
  colnames(rocr_cutoffA1) <- c("cut_off","FPR","TPR")
  View(rocr_cutoffA1)
  
  # F_1 Score
  pA1 <- ifelse(pred_A1>0.5,1,0)
  ytrueA1 <- ifelse(Test_A$EMA=="yes",1,0)
  F1_Score(y_true = ytrueA1,y_pred = pA1) # 0.8413793
  
  
  library(dplyr)
  
  rocr_cutoffA1$cut_off <- round(rocr_cutoffA1$cut_off,6)
  # Sorting data frame with respect to tpr in decreasing order 
  rocr_cutoffA1 <- arrange(rocr_cutoffA1,desc(TPR))
  View(rocr_cutoffA1)
  
  
  # Model 2 ----
  model_A2 <- glm(EMA~.,data=Train_A[-in_1,-c(8,7,1,5)],family = 'binomial')
  summary(model_A2) # AIC = 421.8 indignificant variables are X,children,education,occupation
  pred_A2 <- predict(model_A2,Test_A,type = "response")
  confusion_A2 <- table(Test_A$EMA,pred_A2>0.5);confusion_A2
  effi_A2 <- sum(diag(confusion_A2))/sum(confusion_A2);effi_A2 # 0.7570621
  plot(pred_A2,Test_A$EMA,
       col=ifelse((pred_A2<0.5 & Test_A$EMA =="no")|(pred_A2>0.5 & Test_A$EMA =="yes"),"green","red")
       # ,pch = ifelse((prob_B1<0.5 & Test_B$y =="no")|(prob_B1>0.5 & Test_B$y =="yes"),1,4)
  )
  # In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
  plot(pred_A2,col=ifelse((pred_A2<0.5 & Test_A$EMA =="no")|(pred_A2>0.5 & Test_A$EMA =="yes"),"green","red"))
  
  rocrpredA2<-prediction(pred_A2,Test_A$EMA)
  rocrperfA2<-performance(rocrpredA2,'tpr','fpr')
  str(rocrperfA2)
  rocrperfA2@x.values
  plot(rocrperfA2,colorize=T)
  
  rocr_cutoffA2 <- data.frame(cut_off = rocrperfA2@alpha.values[[1]],fpr=rocrperfA2@x.values,tpr=rocrperfA2@y.values)
  colnames(rocr_cutoffA2) <- c("cut_off","FPR","TPR")
  View(rocr_cutoffA2)
  
  # F_1 Score
  pA2 <- ifelse(pred_A2>0.5,1,0)
  ytrueA2 <- ifelse(Test_A$EMA=="yes",1,0)
  F1_Score(y_true = ytrueA2,y_pred = pA2) # 0.8512111
  
  
  library(dplyr)
  
  rocr_cutoffA2$cut_off <- round(rocr_cutoffA2$cut_off,6)
  # Sorting data frame with respect to tpr in decreasing order 
  rocr_cutoffA2 <- arrange(rocr_cutoffA2,desc(TPR))
  View(rocr_cutoffA2)
  
  par(mfrow=c(1,2));plot(rocrperfA2,colorize=T,main="model2");plot(rocrperfA1,colorize=T,main="model1");par(mfrow=c(1,1))
  