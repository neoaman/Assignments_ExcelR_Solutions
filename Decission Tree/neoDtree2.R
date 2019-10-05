# Question No 2 ----

library(party)
library(C50)
#install.packages("png",repos = "http://cran.us.r-project.org")
df = read.csv("Fraud_check.csv")
head(df)
str(df)
pairs(df)
# taxable_income <= 30000 as "Risky" and others are "Good"
type <- ifelse(df$Taxable.Income <= 30000,"Risky","Good")
Fraud_df <- data.frame(df,type)
barplot(table(Fraud_df$type))
table(Fraud_df$type)

# balancing the data
balance <- as.integer(sample(rownames(Fraud_df[type=="Good",]),124,replace = F))
Fraud_df_ <- rbind(Fraud_df[type=="Risky",],Fraud_df[balance,])
barplot(table(Fraud_df_$type))

# Model with Imbalanced Data

Treef1 <- ctree(Fraud_df$type~Undergrad + Marital.Status + City.Population + 
                  Work.Experience,data = Fraud_df)
predf1 <- predict(Treef1,Fraud_df)
table(Predicted=predf1,Actual=Fraud_df$type)
mean(predf1==Fraud_df$type)

# Model with Balanced Data
Treef1.B <- ctree(Fraud_df_$type~Undergrad + Marital.Status + City.Population + 
                  Work.Experience,data = Fraud_df_)
predf1.B <- predict(Treef1.B,Fraud_df_)
table(Predicted=predf1.B,Actual=Fraud_df_$type)
mean(predf1.B==Fraud_df_$type)


# Feature selection
set.seed(101)
spl <- as.integer(sample(x = nrow(Fraud_df_),size = nrow(Fraud_df_)*.7,F));length(spl)
train_F <- Fraud_df_[spl,]; 
test_F <- Fraud_df_[-spl,];


Treef2 <- ctree(train_F$type~.,data = train_F[,-7])
predf2 <- predict(Treef2,test_F[,-7])

table(Actual =test_F$type ,Predicted = predf2)
mean(test_F$type==predf2) # 0.989 i.e. 99%  accuracy we are getting
plot(Treef2)

# What if we remove the column Taxable.Income
Treef2.1 <- ctree(type~Undergrad + Marital.Status + City.Population + 
                    Work.Experience + Urban,data = train_F)
predf2.1 <- predict(Treef2.1,test_F)

table(Actual =test_F$type ,Predicted = predf2.1)
mean(test_F$type==predf2.1) # 0.797491 i.e. 80%  accuracy we are getting
plot(Treef2.1)
# Here  we are getting each and every predicted value as good.

# Model 3
Treef3 <- C5.0(train_F$type~.,data = train_F[,-c(7,3,5)],trials=50)
summary(Treef3)
predf3 <- predict(Treef3,test_F[,-c(7,3,5)])

table(Actual =test_F$type ,Predicted = predf3)
mean(test_F$type==predf3) # 0.797491 i.e. 80%  accuracy we are getting
plot(Treef3)

# Here I can conclude that in our model that the model is unrelaible untill or unless we get a relaivent variable in our data

summary(lm(Taxable.Income~.,data =df ))$coe
# even looking at the linear model summary between all the variable for predicting the Taxable Income, we cant find any of them significant.
# I am building the linear model to check whether the origin of our desired categorical variable having any variable explanable for its variation or not.

