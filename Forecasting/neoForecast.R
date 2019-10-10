setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\Forecasting")
#======================================================================================================================
# Question no 1: ----
# Airlines Data set

df = readxl::read_xlsx("Airlines+Data.xlsx")
plot(df$Passengers,type = "b")
plot(log(df$Passengers))

# From this plot we can see a trend line as well as seasonality in the data
table(df$Month)
str(df)
as.Date(df$Month)
df$Month
#___________________________________Data Pre Processing___________________________________
# Adding the season columns
year <- rep(1995:2002,c(rep(12,length(1995:2002))))
month<- data.frame(outer(rep(month.abb,length = nrow(df)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)

df_1 <-data.frame(year,month,rn=1:nrow(df),Passengers=df$Passengers,day=seq(1,2880,by = 30)) 

# train and test splitting

df_train <-df_1[1:80,]
df_test <- df_1[81:96,]
#_________________________1 Linear Trend model________________________

model_lt1 <- lm(df_train$Passengers~day,data = df_train[,-15])
summary(model_lt1) # Here my Rsquard value is 0.7829
pred_lt1 <- predict(model_lt1,df_test[,-15])
plot(df_test$Passengers,type = "b",col="blue")
lines(pred_lt1,type = "b",col="red")
cor(pred_lt1,df_test$Passengers) # I am getting 42% accuracy, not bad
rmse_lt1<-sqrt(mean((df_test$Passengers-pred_lt1)^2)) # 47.5462

#_________________________2 Exponential Model____________________________
model_em1 <- lm(log(df_train$Passengers)~day,data = df_train[,-15])
summary(model_em1) # Here my Rsquard value is 0.8181
pred_em1_ <- predict(model_em1,df_test[,-15])
pred_em1 <- exp(pred_em1_)
plot(df_test$Passengers,type = "b",col="blue")
lines(pred_em1,type = "b",col="red")
cor(pred_em1,df_test$Passengers) # I am getting 42% accuracy, not bad
rmse_em1<-sqrt(mean((df_test$Passengers-pred_em1)^2)) # 43.79374


#_________________________3 MODEL 1 Additive Seasonality_________________________________
model1 <- lm(df_train$Passengers~.,data = df_train[,-15])
summary(model1) # Here my Rsquard value is 0.9533
pred1 <- predict(model1,df_test[,-15])
plot(df_test$Passengers,type = "b",col="blue")
lines(pred1,type = "b",col="red")
cor(pred1,df_test$Passengers) # I am getting 98% accuracy, not bad
rmse_1<-sqrt(mean((df_test$Passengers-pred1)^2)) # 33.04

#_________________________4 MODEL 2 Multiplicative Seasonality Linear trend_________________________________

model2 <- lm(log(df_train$Passengers)~.,data = df_train[,-15])
summary(model2) # Here Rsquare is 0.9743
pred2_ <- predict(model2,newdata = df_test[,-15])
pred2 <- exp(pred2_)
plot(df_test$Passengers,type = "b",col="blue")
lines(pred2,type = "b",col="red")
cor(pred2,df_test$Passengers) # I am getting 98.8% accuracy, not bad
rmse_2<-sqrt(mean((df_test$Passengers-pred2)^2)) # 9.469

# I may consider my second model in this case

#=======================================================================================================================================
# Question no 2

df_cococola = readxl::read_excel("CocaCola_Sales_Rawdata.xlsx")
str(df_cococola)
df_cococola$Quarter
plot(df_cococola$Sales) # its increased on the basis of quarters
boxplot(df_cococola$Sales) # No outliers
nrow(df_cococola)
# __________________Pre processing my data__________________________

year <- rep(1986:1996,c(rep(4,length(1986:1996))))[1:42]
Quarters<- data.frame(outer(rep(c("Q1","Q2","Q3","Q4"),length = nrow(df_cococola)),c("Q1","Q2","Q3","Q4"),"==") + 0 )
colnames(Quarters) <- c("Q1","Q2","Q3","Q4")

df_cococola1 <-data.frame(year,Quarters,rn=1:nrow(df_cococola),Sales=df_cococola$Sales) 
head(df_cococola1)

# Train Test splitting
df_train <-df_cococola1[1:30,]
df_test <- df_cococola1[31:42,]

#_________________________1 Linear Trend model________________________

model_ltC1 <- lm(df_train$Sales~rn,data = df_train[,-7])
summary(model_ltC1) # Here my Rsquard value is 0.7079
pred_ltC1 <- predict(model_ltC1,df_test[,-7])
plot(df_test$Sales,type = "b",col="blue")
lines(pred_ltC1,type = "b",col="red")
cor(pred_ltC1,df_test$Sales) # I am getting 74.48% accuracy, not bad
rmse_ltC1<-sqrt(mean((df_test$Sales-pred_ltC1)^2)) # 714.0144

#_________________________2 Exponential Model____________________________
model_emC1 <- lm(log(df_train$Sales)~rn,data = df_train[,-7])
summary(model_emC1) # Here my Rsquard value is 0.7067
pred_emC1_ <- predict(model_emC1,df_test[,-7])
pred_emC1 <- exp(pred_emC1_)
plot(df_test$Sales,type = "b",col="blue")
lines(pred_emC1,type = "b",col="red")
cor(pred_emC1,df_test$Sales) # I am getting 74.15% accuracy, not bad
rmse_emC1<-sqrt(mean((df_test$Sales-pred_emC1)^2)) # 552.2821



#__________________________________3 MODEL 1 Additive Seasonality with linear trend________________________________
modelC1 <- lm(df_train$Sales~.,data = df_train[,-7])
summary(modelC1) # Here my Rsquard value is 0.8457
predC1 <- predict(modelC1,df_test[,-7])
plot(df_test$Sales,type = "b",col="blue")
lines(predC1,type = "b",col="red")
cor(predC1,df_test$Sales) # I am getting 97% accuracy, not bad
rmse_C1<-sqrt(mean((df_test$Sales-predC1)^2)) # 637.9405

#_____________________________________4 MODEL 2 Multiplicative Seasonality Linear trend________________________________
modelC2 <- lm(log(df_train$Sales)~.,data = df_train[,-7])
summary(modelC2) # Here my Rsquard value is 0.8586
predC2_ <- predict(modelC2,df_test[,-7])
predC2 <- exp(predC2_)
plot(df_test$Sales,type = "b",col="blue")
lines(predC2,type = "b",col="red")
cor(predC2,df_test$Sales) # I am getting 96.98% accuracy, not bad
rmse_C2<-sqrt(mean((df_test$Sales-predC2)^2)) # 410.2497

#_____________________________________5 MODEL 3 Multiplicative Seasonality Linear trend_______________________________

par(mfrow=c(1,2));plot(df_cococola$Sales)
plot(df_cococola$Sales[6:30]);par(mfrow=c(1,1))
# Neglecting the erratic compont in my data i.e. record 1 to 8
modelC3 <- lm(log(df_train$Sales[8:30])~.,data = df_train[8:30,-7])
summary(modelC3) # Here my Rsquard value is 0.9655
predC3_ <- predict(modelC3,df_test[,-7])
predC3 <- exp(predC3_)
plot(df_test$Sales,type = "b",col="blue")
lines(predC3,type = "b",col="red")
cor(predC3,df_test$Sales) # I am getting 97.36% accuracy, not bad
rmse_C3<-sqrt(mean((df_test$Sales-predC3)^2)) # 146.4575

# I am considering my Model 3 Multiplicative Seasonality Linear trend as my final model

#=======================================================================================================================================
# Question no 3

dfp=read.csv("PlasticSales.csv")
str(dfp)
plot(dfp$Sales)
nrow(dfp) # 60 records
boxplot(dfp$Sales) # No outliers
sum(is.na(dfp$Sales)) # No missing values as well

# __________________Pre processing my data__________________________

year <- rep(1949:1953,c(rep(12,length(1949:1953))))
month<- data.frame(outer(rep(month.abb,length = nrow(dfp)), month.abb,"==") + 0 )
colnames(month) <- c(month.abb)

df_plastic <-data.frame(year,month,rn=1:nrow(dfp),Sales=dfp$Sales) 
head(df_plastic)

# Train Test splitting
df_trainP <-df_plastic[1:40,]
df_testP <- df_plastic[41:60,]

#_________________________1 Linear Trend model________________________

model_ltP1 <- lm(df_trainP$Sales~rn,data = df_trainP[,-15])
summary(model_ltP1) # Here my Rsquard value is 0.1211
pred_ltP1 <- predict(model_ltP1,df_testP[,-15])
plot(df_testP$Sales,type = "b",col="blue")
lines(pred_ltP1,type = "b",col="red")
cor(pred_ltP1,df_testP$Sales) # I am getting -0.1532 negative correlations here
rmse_ltP1<-sqrt(mean((df_testP$Sales-pred_ltP1)^2)) # 248.924

#_________________________2 Exponential Model____________________________
model_emP1 <- lm(log(df_trainP$Sales)~rn,data = df_trainP[,-15])
summary(model_emP1) # Here my Rsquard value is 0.1269
pred_emP1_ <- predict(model_emP1,df_testP[,-15])
pred_emP1 <- exp(pred_emP1_)
plot(df_testP$Sales,type = "b",col="blue")
lines(pred_emP1,type = "b",col="red")
cor(pred_emP1,df_testP$Sales) # I am getting -0.1541 correlation
rmse_emP1<-sqrt(mean((df_testP$Sales-pred_emP1)^2)) # 250.1071 i.e. increased

#_________________________3 Seasonal Variation in Model____________________________
model_svP1 <- lm(df_trainP$Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = df_trainP[,-15])
summary(model_svP1) # Here my Rsquard value is 0.8405
pred_svP1 <- predict(model_svP1,df_testP[,-15])

plot(df_testP$Sales,type = "b",col="blue")
lines(pred_svP1,type = "b",col="red")
cor(pred_svP1,df_testP$Sales) # I am getting 0.9072 correlation
rmse_svP1<-sqrt(mean((df_testP$Sales-pred_svP1)^2)) # 263.2362 i.e. increased but fitted well

#__________________________________4 MODEL 1 Additive Seasonality with linear trend________________________________
modelP1 <- lm(df_trainP$Sales~.,data = df_trainP[,-15])
summary(modelP1) # Here my Rsquard value is 0.9791
predP1 <- predict(modelP1,df_testP[,-15])
plot(df_testP$Sales,type = "b",col="blue")
lines(predP1,type = "b",col="red")
cor(predP1,df_testP$Sales) # I am getting 0.88 correlation
rmse_P1<-sqrt(mean((df_testP$Sales-predP1)^2)) # 105.2468

#_____________________________________5 MODEL 2 Multiplicative Seasonality _______________________________
modelP2 <- lm(log(df_trainP$Sales)~.-rn,data = df_trainP[,-15])
summary(modelP2) # Here my Rsquard value is 0.9848
predP2_ <- predict(modelP2,df_testP[,-15])
predP2 <- exp(predP2_)
plot(predP2,type = "b",col="red")
lines(df_testP$Sales,type = "b",col="blue")
cor(predP2,df_testP$Sales) # I am getting 0.8763 correlation
rmse_P2<-sqrt(mean((df_testP$Sales-predP2)^2)) # 117.115

#_____________________________________6 MODEL 2 Multiplicative Seasonality Linear Trend_______________________________
modelP3 <- lm(log(df_trainP$Sales)~.,data = df_trainP[,-15])
summary(modelP3) # Here my Rsquard value is 0.9848
predP3_ <- predict(modelP3,df_testP[,-15])
predP3 <- exp(predP3_)
plot(predP3,type = "b",col="red")
lines(df_testP$Sales,type = "b",col="blue")
cor(predP3,df_testP$Sales) # I am getting 0.8763 correlation
rmse_P3<-sqrt(mean((df_testP$Sales-predP3)^2)) # 117.115

