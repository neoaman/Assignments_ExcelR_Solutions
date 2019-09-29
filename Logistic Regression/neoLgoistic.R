setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\Logistic Regression")
library(car)
library(ROCR)
# Question 1 ----
"Whether the client has subscribed a term deposit or not "
# Reading the bank data
Bank <- read.csv("bank-full.csv",sep = ";")
head(Bank)
colnames(Bank)
str(Bank)

# Clean unnecessary columns 
table(Bank$job);table(Bank$marital);table(Bank$education);table(Bank$default);table(Bank$housing);table(Bank$loan);table(Bank$contact);table(Bank$day);table(Bank$campaign);table(Bank$previous);table(Bank$poutcome);table(Bank$y);class(Bank$y)

df_bank <- Bank[,-c(9)]
df_bank$y

# Model 1 __________________________________________________________
model_B_1 <- glm(y~.,data = df_bank,family = "binomial")
summary(model_B_1) # AIC= 22184
Y_B_1 <- predict(model_B_1,df_bank)
plot(Y_B_1,df_bank$y)
plot(Y_B_1)
prob_B_1 <-predict(model_B_1,df_bank,type = "response")
confu_B_1 <- table(prob_B_1>0.5,df_bank$y)
effi_B_1 <- sum(diag(confu_B_1))/sum(confu_B_1);effi_B_1 # Efficiency of my model is 0.9011303 

# Train and Test Split model
set.seed(101)
Test_Spl <- sample(x = 1:nrow(df_bank),size = round(nrow(df_bank)*(30/100)),replace = F)
Train_B_1 <- df_bank[-c(Test_Spl),]
Test_B_1 <- df_bank[c(Test_Spl),]
model_BT_1 <- glm(y~.,data = Train_B_1,family = "binomial")
summary(model_BT_1) # AIC = 15393
Y_BT_1 <- predict(model_BT_1,Test_B_1)
prob_BT_1 <-predict(model_B_1,Test_B_1,type = "response")
plot(prob_BT_1,Test_B_1$y,
     col=ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),"green","red")
     ,pch = ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),1,4) # Uncomment it (Ctrl+Shift+C) if you want different character also
     )
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_BT_1,col=ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),"green","red"))

confu_BT_1 <- table(prob_BT_1>0.5,Test_B_1$y) ;confu_B_1
effi_BT_1 <- sum(diag(confu_BT_1))/sum(confu_BT_1);effi_BT_1 # Efficiency of my model is 0.9005382 


influencePlot(model_B_1)
influencePlot(model_BT_1)
influ <-as.integer(intersect(rownames(influencePlot(model_B_1)),rownames(influencePlot(model_BT_1))));influ
# ROC Curve
rocrpred<-prediction(prob_BT_1,Test_B_1$y)
rocrperf<-performance(rocrpred,'tpr','fpr')


str(rocrperf)
rocrperf@x.values
colorize(x=rocrperf,colors = c(1,2,3))
plot(rocrperf,colorize=T) #text.adj=c(-0.2,1.7)

# Model2 _______________________________________________________
df_bank2 <- df_bank[-c(influ),]
model_B_2 <- glm(y~.,data = df_bank2,family = "binomial")
summary(model_B_2) # AIC= 22177
Y_B_2 <- predict(model_B_2,df_bank2)
plot(Y_B_2,df_bank2$y)
plot(Y_B_2)
prob_B_2 <-predict(model_B_2,df_bank2,type = "response")
confu_B_2 <- table(prob_B_2>0.5,df_bank2$y)
effi_B_2 <- sum(diag(confu_B_2))/sum(confu_B_2);effi_B_2 # Efficiency of my model is 0.901148 

# Train and Test Split and Model Evaluation
set.seed(101)
Test_Spl_2 <- as.integer(sample(x = rownames(df_bank2),size = round(nrow(df_bank2)*(30/100)),replace = F))
Train_B_2 <- df_bank2[-c(Test_Spl_2),]
Test_B_2 <- df_bank2[c(Test_Spl_2),]
model_BT_2 <- glm(y~.,data = Train_B_2,family = "binomial")
summary(model_BT_2) # AIC = 15456
Y_BT_2 <- predict(model_BT_2,Test_B_2)
prob_BT_2 <-predict(model_B_2,Test_B_2,type = "response")

plot(prob_BT_2,Test_B_2$y,
     col=ifelse((prob_BT_2<0.5 & Test_B_2$y =="no")|(prob_BT_2>0.5 & Test_B_2$y =="yes"),"green","red")
     #,pch = ifelse((prob_BT_1<0.5 & Test_B_1$y =="no")|(prob_BT_1>0.5 & Test_B_1$y =="yes"),1,4)
)
# In this plot the red circle represents wrong prediction and the gree circle represents correct prediction
plot(Y_BT_2,col=ifelse((prob_BT_2<0.5 & Test_B_2$y =="no")|(prob_BT_2>0.5 & Test_B_2$y =="yes"),"green","red"))

confu_BT_2 <- table(prob_BT_2>0.5,Test_B_2$y) ;confu_BT_2
effi_BT_2 <- sum(diag(confu_BT_2))/sum(confu_BT_2);effi_BT_2 # Efficiency of my model is 0.9005382 


#Cutoff value

str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)



# Question 2 ----
"I have a dataset containing family information of married couples, which have around 10 variables & 600+ observations. 
Independent variables are ~ gender, age, years married, children, religion etc.
I have one response variable which is number of extra marital affairs.
Now, I want to know what all factor influence the chances of extra marital affair.
Since extra marital affair is a binary variable (either a person will have or not), 
so we can fit logistic regression model here to predict the probability of extra marital affair."

#importing the data
Affair <- read.csv("affairs.csv")
head(Affair)
str(Affair)

#Column cration
ExtraMA <- ifelse(Affair$affairs>0,"yes","no")
df_Affair <- data.frame(Affair[,-2],ExtraMA)

head(df_Affair)
str(df_Affair)

#Model-1 fitting
model_A_1 <- glm(ExtraMA~.,data = df_Affair,family = "binomial")
summary(model_A_1) # AIC value is 628.52
pred_A_1 <- predict(model_A_1,newdata = df_Affair,type = "response")

conf_A_1 <- table(pred_A_1>0.5,df_Affair$ExtraMA)
effi_A_1 <- sum(diag(conf_A_1))/sum(conf_A_1);effi_A_1 # Efficiency is 0.765391

influenceIndexPlot(model_A_1,id=list(col="red",n=5))
inf_A_1 <- as.integer(rownames(influencePlot(model_A_1,id=list(n=10,col="red"))))
length(inf_A_1)
df_Affair[inf_A_1,]
head(df_Affair)

# ROC Curve
rocrpred<-prediction(pred_A_1,df_Affair$ExtraMA)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
rocrperf@x.values
plot(rocrperf,colorize=T) #text.adj=c(-0.2,1.7)

# Testing the significance of all insignificant variables in our previous model
summary(glm(df_Affair$ExtraMA~df_Affair$education,family = 'binomial'))
summary(glm(df_Affair$ExtraMA~df_Affair$gender,family = 'binomial'))
summary(glm(df_Affair$ExtraMA~df_Affair$age,family = 'binomial'))
summary(glm(df_Affair$ExtraMA~df_Affair$education,family = 'binomial'))
summary(glm(df_Affair$ExtraMA~df_Affair$occupation,family = 'binomial'))
summary(glm(df_Affair$ExtraMA~df_Affair$X,family = 'binomial'))
# Removing influencing rows
df_Affair2 <- df_Affair[-c(inf_A_1),]
model_A_2 <- glm(ExtraMA~.,data = df_Affair2,family = "binomial")
summary(model_A_2) # AIC value is 609.74
pred_A_2 <- predict(model_A_2,newdata = df_Affair2,type = "response")
conf_A_2 <- table(pred_A_2>0.5,df_Affair2$ExtraMA);conf_A_2
effi_A_2 <- sum(diag(conf_A_2))/sum(conf_A_2);effi_A_2 # Efficiency is 0.7802768
# ROC CURVE
rocrpred_2<-prediction(pred_A_2,df_Affair2$ExtraMA)
rocrperf_2<-performance(rocrpred_2,'tpr','fpr')

str(rocrperf_2)
rocrperf_2@x.values
plot(rocrperf_2,colorize=T) #text.adj=c(-0.2,1.7)


        # TRAIN TEST SPLIT
set.seed(1)
Test_A_2 <- as.integer(sample(x = rownames(df_Affair2),size = round(nrow(df_Affair2)*(30/100)),replace = F))
Train_A_2 <- df_Affair2[-c(Test_A_2),]
Test_A_2 <- df_Affair2[c(Test_A_2),]
model_AT_2 <- glm(ExtraMA~.,data = Train_A_2,family = "binomial")
summary(model_AT_2) # AIC = 404.27
pred_AT_2 <- predict(model_AT_2,newdata = Test_A_2,type = "response")
conf_AT_2 <- table(pred_AT_2>0.5,Test_A_2$ExtraMA);conf_AT_2
effi_AT_2 <- sum(diag(conf_AT_2))/sum(conf_AT_2);effi_AT_2 # Efficiency is 0.0.8012048

                # ROC CURVE
rocrpred_2T<-prediction(pred_AT_2,Test_A_2$ExtraMA)
rocrperf_2T<-performance(rocrpred_2T,'tpr','fpr')

str(rocrperf_2T)
rocrperf_2T@x.values
plot(rocrperf_2T,colorize=T) #text.adj=c(-0.2,1.7)


# Model 3
model_A_3 <- glm(ExtraMA~.-X-education,data = df_Affair2,family = "binomial")
summary(model_A_3) # AIC value is 548.15
pred_A_3 <- predict(model_A_3,newdata = df_Affair2,type = "response")
conf_A_3 <- table(pred_A_3>0.5,df_Affair2$ExtraMA);conf_A_3
effi_A_3 <- sum(diag(conf_A_3))/sum(conf_A_3);effi_A_3 # Efficiency is 0.7820868

                # ROC CURVE
rocrpred_3<-prediction(pred_A_3,df_Affair2$ExtraMA)
rocrperf_3<-performance(rocrpred_3,'tpr','fpr')

str(rocrperf_3)
rocrperf_3@x.values
plot(rocrperf_3,colorize=T) #text.adj=c(-0.2,1.7)


        # TRAIN TEST SPLIT
set.seed(1)
Test_A_3 <- as.integer(sample(x = rownames(df_Affair2),size = round(nrow(df_Affair2)*(30/100)),replace = F))
Train_A_3 <- df_Affair2[-c(Test_A_3),]
Test_A_3 <- df_Affair2[c(Test_A_3),]
model_AT_3 <- glm(ExtraMA~.-X-education,data = Train_A_3,family = "binomial")
summary(model_AT_3) # AIC = 409
pred_AT_3 <- predict(model_AT_3,newdata = Test_A_3,type = "response")
conf_AT_3 <- table(pred_AT_3>0.5,Test_A_3$ExtraMA);conf_AT_3
effi_AT_3 <- sum(diag(conf_AT_3))/sum(conf_AT_3);effi_AT_3 # Efficiency is 0.0.8072289


                # ROC CURVE
rocrpred_3T<-prediction(pred_AT_3,Test_A_3$ExtraMA)
rocrperf_3T<-performance(rocrpred_3T,'tpr','fpr')

str(rocrperf_3T)
rocrperf_3T@x.values
plot(rocrperf_3T,colorize=T) #text.adj=c(-0.2,1.7)

