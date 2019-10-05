library(party)
library(C50)
df = read.csv("Company_Data.csv")
str(df)
head(df)
pairs(df)
range(df$Sales)
median(df$Sales)
sort(df$Sales)
length(df$Sales)
# Less Assume 3 sales segments I may consider low, Medium and High. 
# Lets see the cutoff for High sales
sort(df$Sales)[(400/3 * 2)]
# As here cutoff we got is 8.67
# so we can condider that those sales which values are gratter than 8.5  will be considered as High sales and rest will be low sales
SaleC <- ifelse(df$Sales > 8.5,"High","Low")
prop.table(table(SaleC)) # Desired result achieved
comp <- data.frame(SaleC,df[,-1])

set.seed(101);split <- sample(nrow(comp),nrow(comp)*.7,F)
Train_comp <- comp[split,]
Test_comp <- comp[-split,]

# Model-1 with whole data
Model1 <- ctree(SaleC~.,data = Train_comp)
pred1 <- predict(Model1,Test_comp)
table(Actual=Test_comp$SaleC,Predicted=pred1)
mean(Test_comp$SaleC==pred1) # Accuracy is 75.8%
plot(Model1)

# Model-2 with whole data
Model2 <- C5.0(SaleC~.,data = Train_comp)
pred2 <- predict(Model2,Test_comp)
table(Test_comp$SaleC,pred2)
mean(Test_comp$SaleC==pred2) # Accuracy is 80%
plot(Model2)


# Model-3 with whole data
Model3 <- C5.0(SaleC~.,data = Train_comp,trials=100)
pred3 <- predict(Model3,Test_comp)
table(Test_comp$SaleC,pred3)
mean(Test_comp$SaleC==pred3) # Accuracy is 85%
plot(Model3,cex=0.5)
plot(Model3)
C5imp(Model3)
library(party)
plot(Model3_2,terminal_panel = node_terminal(Model3_2))


for( i in 1:50){
  Model3 <- C5.0(SaleC~.,data = Train_comp,trials=i)
  pred3 <- predict(Model3,Test_comp)
  print(paste( i , mean(Test_comp$SaleC==pred3)))
  
}
