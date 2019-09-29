setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Completed\\Simple Linear Regression")
#________________________________________________________________________________________________
# Importing required libraries
library(ggplot2)
library(car)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Question no 1 ----
# 1) Calories_consumed-> predict weight gained using calories consumed 
#________________________________________________________________________________________________
# Answer : 
# Imporing the data
calories_consumed <- read.csv("calories_consumed.csv")
colnames(calories_consumed) <- c("WeightGain","CaloriesConsumed")
# Target variable is Weight Gain and independent variable is Calories consumed
# Normality test
shapiro.test(calories_consumed$WeightGain); # p-value is 0.006646 so it's not Normally Distributed at 0.05 level of significance
shapiro.test(calories_consumed$CaloriesConsumed) # Normally Distributed at 0.05 level of significance
# lets go for the correlation
cor(calories_consumed$WeightGain,calories_consumed$CaloriesConsumed)
plot(calories_consumed$WeightGain~calories_consumed$CaloriesConsumed)
# Its seems like the data is linear with +ve correlation with 0.946 

# Building my first model over it
model1 <- lm(calories_consumed$WeightGain~calories_consumed$CaloriesConsumed)
summary(model1) # Rsquare = 0.896
# Here our coefficient of determination is 0.896 i.e. about 90% of variation on target variable (weight_gain) is explained by the independent variable "Calories consumed"
predict1 <- predict(object =model1,newdata = calories_consumed)
cor(calories_consumed$WeightGain,predict1) # Correlation is 0.946991 i.e. our predicted and actual value are verymuch correlated.
sqrt(sum(model1$residuals^2)/nrow(calories_consumed))  #RMSE = 103.3025

# Plotting the regression line
plot(x = calories_consumed$CaloriesConsumed,y = predict1,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = calories_consumed$CaloriesConsumed,y = calories_consumed$WeightGain)
# ggplot
ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=WeightGain))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")

# Lets do polynomial transformation
model2 <- lm(calories_consumed$WeightGain~poly(x = calories_consumed$CaloriesConsumed,degree = 2))
summary(model2) # Coefficient of determination is 0.9521 i.e. 95 percent of variation on the dependent variable is explained by the independent variable
predict2 <- predict(model2,newdata = calories_consumed)
cor(predict2,calories_consumed$WeightGain) # 0.9757 i.e. more than the previous model
sqrt(sum(model2$residuals^2)/nrow(calories_consumed))  #RMSE = 70.40752 Here we are getting less RMSE value compared to model1
ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=WeightGain))+geom_smooth(method = "lm",formula = y~poly(x,2))+geom_point()+ggtitle("Model 2",subtitle = "Second degree polynomial")

# I am doing my model with 5 degree polynomial 
model3 <- lm(calories_consumed$WeightGain~poly(x = calories_consumed$CaloriesConsumed,degree = 3))
summary(model3) # Coefficient of determination is 0.9811 i.e. 98 % of variation on y is explained by the 3 degree polynomial of x
predict3 <- predict(object = model3,newdata = calories_consumed)
cor(predict3,calories_consumed$WeightGain) # 0.9905292 highly correlated
sqrt(sum(model3$residuals^2)/nrow(calories_consumed))  #RMSE = 44.15011 Here we are getting less RMSE value compared to model1
ggplot(data = calories_consumed,mapping=aes(x = CaloriesConsumed,y=WeightGain))+geom_smooth(method = "lm",formula = y~poly(x,3))+geom_point()+ggtitle("Model 3",subtitle = "3rd degree polynomial Transformation")
plot(model3)
#_____________________________________________________________________________________________________________________
# Question no 2 ----
# 2) Delivery_time -> Predict delivery time using sorting time
#_____________________________________________________________________________________________________________________
# Answer : 
# Importing the Data
Delivery_time <- read.csv("delivery_time.csv")
# Target variable is DeliveryTime, and Independent variable is SortingTime
# Normality check
shapiro.test(Delivery_time$Sorting.Time) # p-value is 0.1881 Data is normally distributed
shapiro.test(Delivery_time$Delivery.Time) # p-value is 0.8963 Data is normally distributed
# Correlation analysis
cor(Delivery_time$Delivery.Time,Delivery_time$Sorting.Time) # Correlation between the two variable is 0.825599
plot(Delivery_time$Delivery.Time~Delivery_time$Sorting.Time)
# Its seems like the data is linear with +ve correlation with 0.8255 

# Building my first model over it
model1 <- lm(Delivery_time$Delivery.Time~Delivery_time$Sorting.Time)
summary(model1) # Rsquare = 0.6823
influencePlot(model = model) # one Data is influencing to our model i.e. observation no 10
# Here our coefficient of determination is 0.6823 i.e. about 68% of variation on target variable (Delivery_time) is explained by the independent variable "Sorting time"
predict1 <- predict(object =model1,newdata = Delivery_time)
cor(Delivery_time$Delivery.Time,predict1) # Correlation is 0.8259 i.e. our predicted and actual value are verymuch correlated.
sqrt(sum(model1$residuals^2)/nrow(Delivery_time))  #RMSE = 2.79165

# Plotting the regression line
plot(x = Delivery_time$Sorting.Time,y = predict1,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = Delivery_time$Sorting.Time,y = Delivery_time$Delivery.Time)
# ggplot
ggplot(data = Delivery_time,mapping=aes(y = Delivery.Time,x=Sorting.Time))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")

# Lets do polynomial transformation
model2 <- lm(Delivery_time$Delivery.Time~poly(x = Delivery_time$Sorting.Time,degree = 2))
summary(model2) # Coefficient of determination is 0.6934 i.e. 69 percent of variation on the dependent variable is explained by the independent variable
predict2 <- predict(model2,newdata = Delivery_time)
cor(predict2,Delivery_time$Delivery.Time) # 0.8327 i.e. more than the previous model
sqrt(sum(model2$residuals^2)/nrow(Delivery_time))  #RMSE = 2.742148 Here we are getting less RMSE value compared to model1
ggplot(data = Delivery_time,mapping=aes(x = Sorting.Time,y=Delivery.Time))+geom_smooth(method = "lm",formula = y~poly(x,2))+geom_point()+ggtitle("Model 2",subtitle = "2nd degree polynomial transformation")

# I am doing my model with 5 degree polynomial 
model3 <- lm(Delivery_time$Delivery.Time~poly(x = Delivery_time$Sorting.Time,degree = 5))
summary(model3) # Coefficient of determination is 0.7142 i.e. 71 % of variation on y is explained by the 5 degree polynomial of x
sqrt(sum(model3$residuals^2)/nrow(Delivery_time))  #RMSE = 2.6475 Here we are getting less RMSE value compared to model2
predict3 <- predict(object = model3,newdata = Delivery_time)
cor(predict3,Delivery_time$Delivery.Time) # 0.845121 highly correlated
ggplot(data = Delivery_time,mapping=aes(x = Sorting.Time,y=Delivery.Time))+geom_smooth(method = "lm",formula = y~poly(x,5))+geom_point()+ggtitle("Model 3",subtitle = "5 degree polynomial")

# Doing Log transformation on the target variable. 
model4 <- lm(log(Delivery_time$Delivery.Time)~poly(Delivery_time$Sorting.Time,2))
summary(model4) # Coefficient of determination is 0.7649 i.e. 76 % of variation on y is explained by the 3 degree polynomial of x
sqrt(sum(model4$residuals^2)/nrow(Delivery_time))  #RMSE = 0.1505 Here we are getting less RMSE value compared to model2
predict4 <-  predict(object = model4,newdata = Delivery_time) #log of predicted value

pred4_act <- exp(predict4) # Predicted value
Delivery_time$Delivery.Time # Actyal value
sqrt(sum((Delivery_time$Delivery.Time-pred4_act)^2)/nrow(Delivery_time)) # RMSE = 2.799042

cor(exp(predict4),Delivery_time$Delivery.Time) # 0.825883 highly correlated
ggplot(data = Delivery_time,mapping=aes(x = Sorting.Time,y=log(Delivery.Time)))+geom_smooth(method = "lm",formula = y~poly(x,5))+geom_point()+ggtitle("Model 4",subtitle = "log(y) and x is of 2 degree polynomial")


#_____________________________________________________________________________________________________________________
# Question no 3 ----
# 3) Emp_data -> Build a prediction model for Churn_out_rate
#_____________________________________________________________________________________________________________________
# Answer : 
# Importing the Data
Emp_data <- read.csv("emp_data.csv")
# Target variable is "Churn out rate", and Independent variable is "Salary hike"
# Normality check
shapiro.test(Emp_data$Salary_hike) # p-value is 0.5018 Data is normally distributed
shapiro.test(Emp_data$Churn_out_rate) # p-value is 0.7342 Data is normally distributed
# Correlation analysis
cor(Emp_data$Salary_hike,Emp_data$Churn_out_rate) # Correlation between the two variable is -0.9117216
plot(Emp_data$Salary_hike,Emp_data$Churn_out_rate)
# Its seems like the data is linear with -ve correlation with -0.9117216 

# Building my first model over it
model1 <- lm(Emp_data$Churn_out_rate~Emp_data$Salary_hike)
summary(model1) # Rsquare = 0.8312
# Here our coefficient of determination is 0.8312 i.e. about 83% of variation on target variable "Churn out rate" is explained by the independent variable "Salary hike"
predict1 <- predict(object =model1,newdata = Emp_data)
cor(Emp_data$Churn_out_rate,predict1) # Correlation is 0.9117216 i.e. our predicted and actual value are verymuch correlated.
sqrt(sum(model1$residuals^2)/nrow(Emp_data))  #RMSE = 3.997528

# Plotting the regression line
plot(x = Emp_data$Salary_hike,y = predict1,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = Emp_data$Salary_hike,y = Emp_data$Churn_out_rate)
# ggplot
ggplot(data = Emp_data,mapping=aes(y = Churn_out_rate,x=Salary_hike))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")

# Lets do polynomial transformation
model2 <- lm(Emp_data$Churn_out_rate~poly(x = Emp_data$Salary_hike,degree = 2))
summary(model2) # Coefficient of determination is 0.9737 i.e. 97 percent of variation on the dependent variable is explained by the independent variable
predict2 <- predict(model2,newdata = Emp_data)
cor(predict2,Emp_data$Churn_out_rate) # 0.9867 i.e. more than the previous model
sqrt(sum(model2$residuals^2)/nrow(Emp_data))  #RMSE = 1.5779 Here we are getting less RMSE value compared to model1
#plotting
plot(x = Emp_data$Salary_hike,y = predict2,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = Emp_data$Salary_hike,y = Emp_data$Churn_out_rate)

ggplot(data = Emp_data,mapping=aes(x = Salary_hike,y=Churn_out_rate))+geom_smooth(method = "lm",formula = y~poly(x,2))+geom_point()+ggtitle("Model 2",subtitle = "2nd degree polynomial transformation")
#_____________________________________________________________________________________________________________________
# Question no 4 ----
# 4) Salary_hike -> Build a prediction model for Salary_hike
#_____________________________________________________________________________________________________________________
# Answer : 
# Importing the Data
SalaryData <- read.csv("Salary_Data.csv")
# Target variable is "Salary", and Independent variable is "YearsExperience"
# Normality check
shapiro.test(SalaryData$YearsExperience) # p-value is 0.1034 Data is normally distributed
shapiro.test(SalaryData$Salary) # p-value is 0.01516 Data is not normally distributed
# Correlation analysis
cor(SalaryData$YearsExperience,SalaryData$Salary) # Correlation between the two variable is 0.9782416
plot(SalaryData$YearsExperience,SalaryData$Salary)
# Its seems like the data is linear with +ve correlation with 0.9782 

# Building my first model over it
model1 <- lm(SalaryData$Salary~SalaryData$YearsExperience)
summary(model1) # Rsquare = 0.957
# Here our coefficient of determination is 0.957 i.e. about 95% of variation on target variable "Salary" is explained by the independent variable "YearsExperience"
predict1 <- predict(object =model1,newdata = SalaryData)
cor(SalaryData$Salary,predict1) # Correlation is 0.9782 i.e. our predicted and actual value are verymuch correlated.
sqrt(sum(model1$residuals^2)/nrow(SalaryData))  #RMSE = 5592.044

# Plotting the regression line
plot(x = SalaryData$YearsExperience,y = predict1,type = "b",col="red",xlab = "Calories consumed",ylab = "Weight Gain")
points(x = SalaryData$YearsExperience,y = SalaryData$Salary)
# ggplot
ggplot(data = SalaryData,mapping=aes(y = Salary,x=YearsExperience))+geom_smooth(method = "lm")+geom_point()+ggtitle("Model 1",subtitle = "Y~X")

# Lets do polynomial transformation
model2 <- lm(SalaryData$Salary~poly(x = SalaryData$YearsExperience,degree = 3))
summary(model2) # Coefficient of determination is 0.9636 i.e. 96 percent of variation on the dependent variable is explained by the independent variable
predict2 <- predict(model2,newdata = SalaryData)
cor(predict2,SalaryData$Salary) # 0.9816 i.e. more than the previous model
sqrt(sum(model2$residuals^2)/nrow(SalaryData))  #RMSE = 5142.642 Here we are getting less RMSE value compared to model1 but its negligibl. 
ggplot(data = SalaryData,mapping=aes(x = YearsExperience,y=Salary))+geom_smooth(method = "lm",formula = y~poly(x,3))+geom_point()+ggtitle("Model 2",subtitle = "3rd degree polynomial transformation")
