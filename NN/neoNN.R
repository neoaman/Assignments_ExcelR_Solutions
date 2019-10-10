setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\NN _ Issues")

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
denormalize <- function(x,max,min){
  denormalize <- ((x*(max-min)) + min)
  return(denormalize)
}

#install.packages(c("neuralnet","nnet"))
library(nnet);library(neuralnet)
# Question  no 1 ----
"Build a Neural Network model for 50_startups data to predict profit "

statups <- read.csv("50_Startups (1).csv")
head(statups)
str(statups)
# Target is Profit
plot(statups[,-4])
corrplot::corrplot(cor(statups[,-4]))
colSums(is.na(statups)) # No NA values are present in our data

df_statups <- data.frame(normalize_dummy(statups))#,profit=statups$Profit)
colnames(df_statups)[5:7] <- c("California","Florida","NewYork")
head(df_statups)
boxplot(statups)
# Train Test Splitting
set.seed(101)
Split_s <- sample(x = 1:nrow(df_statups),size = round(nrow(df_statups)*0.3),replace = F)
Train_St <- df_statups[-Split_s,];dim(Train_St)
Test_St <- df_statups[Split_s,];dim(Test_St)

# Model Building 
head(df_statups)
set.seed(4) #4
model_St1 <- neuralnet(Profit~.,data=Train_St)
plot(model_St1,rep = "best")
pred_St1 <- compute(model_St1,Test_St)
cor(pred_St1$net.result,Test_St$Profit) # 0.9585

pred_St_n1 <- denormalize(pred_St1$net.result,max(statups$Profit[Split_s]),min(statups$Profit[Split_s]))
cor(statups$Profit[Split_s],pred_St_n1) # 0.9585436
RMSE_S1 <- sqrt(mean((statups$Profit[Split_s]-pred_St_n1)^2)) # 14478.8
plot(statups$Profit[Split_s],pred_St_n1) 


model_St2 <- neuralnet(Profit~.,data=Train_St,hidden = 5)
plot(model_St2,rep = "best")
pred_St2 <- compute(model_St2,Test_St)
cor(pred_St2$net.result,Test_St$Profit) # 0.9600603

pred_St_n2 <- denormalize(pred_St2$net.result,max(statups$Profit[Split_s]),min(statups$Profit[Split_s]))
cor(statups$Profit[Split_s],pred_St_n2) # 0.9600603
RMSE_S2 <- sqrt(mean((statups$Profit[Split_s]-pred_St_n2)^2)) # 13479.83
plot(statups$Profit[Split_s],pred_St_n2) 


# Question no 2 : ----

"PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS"

forest <- read.csv("forestfires.csv")
head(forest);dim(forest)
str(forest)
boxplot(forest)
table(forest$size_category)
boxplot(forest$area)$out
colSums(is.na(forest)) # No outliers in my data
length(boxplot(forest$area)$out)
dim(forest)
dum_fire <- normalize_dummy(forest)
head(dum_fire)
boxplot(dum_fire)
# Target is area
library(ggplot2)
ggplot(data=forest,aes(x=area,y=..density..))+geom_histogram(binwidth = 30,color="blue")+geom_density(aes(y=..density..),col="forestgreen")


set.seed(101)
split_f <- sample(nrow(forest),0.3*nrow(forest))
train_forest <- dum_fire[-split_f,] 
test_forest <- dum_fire[split_f,] 

# Model Fitting
set.seed(101);fo_model_1 <- neuralnet(area~.,data = train_forest)
str(fo_model_1)
plot(fo_model_1, rep = "best")
summary(fo_model_1)

NeuralNetTools::plotnet(fo_model_1,alpha=0.3)

Pred_fo_1 <- compute(fo_model_1,test_forest)
cor(Pred_fo_1$net.result,test_forest$area) # 0.24

Act_Pred_f1 <- denormalize(Pred_fo_1$net.result,max(forest$area),min(forest$area))
table(Act_Pred_f1<0) # no negative values
Act_Pred_f1.1 <- ifelse(Act_Pred_f1<0,0,Act_Pred_f1)# removing the negative values

cor(Act_Pred_f1.1,test_forest$area) # 0.17

# Model 2 
set.seed(101);fo_model_2 <- neuralnet(area~.,data = train_forest,hidden = 5)
str(fo_model_2)
plot(fo_model_2, rep = "best")
summary(fo_model_2)
NeuralNetTools::plotnet(fo_model_2,alpha=0.3)
Pred_fo_2 <- compute(fo_model_2,test_forest)
cor(Pred_fo_2$net.result,test_forest$area) # 0.125
Act_Pred_f2 <- denormalize(Pred_fo_2$net.result,max(forest$area),min(forest$area))
cor(forest$area[split_f],Act_Pred_f2) # 0.125
data.frame(Predicted=Act_Pred_f2,Actual=test_forest$area) -> df_f_p
ggplot2::ggplot(data=df_f_p,aes(x=Actual,y=Predicted))+geom_smooth()+geom_point()


# Removing the records with burn area = 0
head(forest)
table(forest[forest$area!=0,]$size_category) # burn area data where area is not 0
df_forest_2 <- forest[forest$area!=0,]
df_forest_2_norm <- normalize_dummy(df_forest_2)
split <- sample(nrow(df_forest_2_norm),nrow(df_forest_2_norm)*0.7,F)
train_forest <- df_forest_2_norm[-split,] 
test_forest <- df_forest_2_norm[split,] 

set.seed(101);fo_model_3 <- neuralnet(area~.,data = train_forest,hidden = 3)
str(fo_model_3)
plot(fo_model_3, rep = "best")
summary(fo_model_3)
NeuralNetTools::plotnet(fo_model_3,alpha=0.3)
Pred_fo_3 <- compute(fo_model_3,test_forest)
table(Pred_fo_3$net.result<0) # I am grtting 38 negative values in my prediction
cor(Pred_fo_3$net.result,test_forest$area) # 0.06976619
Act_Pred_f3 <- denormalize(Pred_fo_3$net.result,max(forest$area),min(forest$area))
table(Act_Pred_f3<0) # 49 negative values
cor(forest$area[forest$area!=0][split],Act_Pred_f3) # 0.0697
data.frame(Predicted=Act_Pred_f3,Actual=test_forest$area) -> df_f_p
ggplot2::ggplot(data=df_f_p,aes(x=Actual,y=Predicted))+geom_smooth()+geom_point()




# Question no 3 : ----
"Prepare a model for strength of concrete data using Neural Networks"

concret <- read.csv("concrete.csv")
# Target is strength
str(concret)
boxplot(concret)
boxplot(concret$age)
age_remove <- which(concret$age>150)
concret <- concret[-age_remove,]
boxplot(concret$age)$out

# As we can see after removing the outliers also we are getting another set of 
library(ggplot2)
# Lets viasualize the distribution of cement slag with density plot
ggplot2::ggplot(data=concret,aes(x=cement,y=..density..))+geom_histogram(binwidth = 30,color="blue")+geom_density(aes(y=..density..),col="forestgreen")
ggplot2::ggplot(data=concret,aes(x=slag,y=..density..))+geom_histogram(binwidth = 30,color="blue")+geom_density(aes(y=..density..),col="forestgreen")

# Summary of the cement
summary(concret)
# Normalizing the data
df_norm_concret <- normalize_dummy(concret)
# Splitting the data
set.seed(101)
split_c <- sample(nrow(concret),0.3*nrow(concret))
train_concret <- df_norm_concret[-split_c,] 
test_concret <- df_norm_concret[split_c,] 

# Model Fitting
set.seed(101);con_model_1 <- neuralnet(strength~.,data = train_concret)
str(con_model_1)
plot(con_model_1, rep = "best")
summary(con_model_1)

NeuralNetTools::plotnet(con_model_1,alpha=0.3)

Pred_con_1 <- compute(con_model_1,test_concret)
cor(Pred_con_1$net.result,test_concret$strength) # 0.8153639

Actual_Strength <- concret$strength[split_c]
Act_Pred_c1 <- denormalize(Pred_con_1$net.result,max(concret$strength),min(concret$strength))
RMSE_cement1 <- sqrt(mean((Actual_Strength-Act_Pred_c1)^2)) # i.e 8.731773
cor(Act_Pred_c1,Actual_Strength) # 0.860514

# Model 2 

set.seed(101);con_model_2 <- neuralnet(strength~.,data = train_concret,hidden = 5)
str(con_model_2)
plot(con_model_2, rep = "best")
summary(con_model_2)

NeuralNetTools::plotnet(con_model_2,alpha=0.3)

Pred_con_2 <- compute(con_model_2,test_concret)
cor(Pred_con_2$net.result,test_concret$strength) # 0.9361695

Act_Pred_c2 <- denormalize(Pred_con_2$net.result,max(concret$strength),min(concret$strength))
cor(concret$strength[split_c],Act_Pred_c2) # 0.9361695
RMSE_cement2 <- sqrt(mean((Actual_Strength-Act_Pred_c2)^2)) # i.e 5.649183
data.frame(Predicted=Act_Pred_c2,Actual=test_concret$strength) -> df_c_p
ggplot2::ggplot(data=df_c_p,aes(x=Actual,y=Predicted))+geom_smooth()+geom_point()

