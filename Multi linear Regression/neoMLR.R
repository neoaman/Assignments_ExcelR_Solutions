# Setting my Working dir.
setwd(dir = "D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\Multi linear Regression")
library(ggplot2)
library(car)


#Q1----
"Prepare a prediction model for profit of 50_startups data.
 Do transformations for getting better predictions of profit and
 make a table containing R^2 value for each prepared model."

#Answer:
Startups <- read.csv("50_Startups.csv")         # Read The csv file
head(Startups)
summary(Startups)
boxplot(Startups[,-4],col = c("lightblue","pink","lightgreen",'Orange'))
#write.csv(summary(Startups),"outputs/summary.csv")
attach(Startups)
pairs(Startups[,-4],pch=20,cex=1.5,col=State);par(xpd = TRUE)
# Red = Florida, Black = California, Green = New York
legend(x = "bottomr",legend = unique(State),fill = State,cex=0.63,bg="transparent",bty = "n");par(xpd = F)
cor(Startups[,-4])
# write.csv(cor(Startups[,-4]),"correlation.csv")
attach(Startups)

#First model ----

#Lets build a simple model
model.S <- lm(Profit~R.D.Spend+Administration+Marketing.Spend) 
summary(model.S) # R² = 0.9507
rmse_1 <- mean(model.S$residuals^2)^.5 # RMSE value is 8855.344
pred_S1 <- predict(model.S,newdata = Startups)
cor(pred_S1,Startups$Profit) # correlation is 0.975062

vif(model.S) # In our model no colinearity problem exists between the variables.
avPlots(model.S)
influencePlot(model.S,col='lightblue',pch=16)
influence_index = as.integer(rownames(influencePlot(model.S,id = list(n=2,col="red",cex=2),pch=20,col="green")[]));influence_index

# Checking Significance level with individual variables
summary(lm(Profit~Administration)) # Only 4 % of variation in the "Profit" is explained by the "Administration"
summary(lm(Profit~R.D.Spend)) # 94 % of variation in the "Profit" is explained by the "R.D.Spend"
summary(lm(Profit~Marketing.Spend)) # Only 55 % of variation in the "Profit" is explained by the "Marketing.Spend"
summary(lm(Profit~State)) # Only 2 % of variation in the "Profit" is explained by the "State"

# Examining Variable "State" taking only "R.D.Spend" as independent variable.
ggplot(data = Startups,aes(x=R.D.Spend,y=Profit,color=State))+geom_smooth(method = "lm")+geom_point(cex=3)
ggplot(data = Startups,aes(x=R.D.Spend,y=Profit))+geom_smooth(method = "lm")+geom_point(cex=3,aes(color=State))

# So here only 2 variable is Relevent and other are insignificant 
# Removing the variable "State" will have no impact over our model.

#Second Model ----
#Removing the Influencing observations
df_Startups <- Startups[-c(influence_index),]
model.S.2 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = df_Startups)
summary(model.S.2) # R² = 0.9626
vif(model.S.2) # In our model no colinearity problem exists between the variables.
avPlots(model.S.2,id=list(n=3,col="red"),lwd=2,col='forestgreen')
rmse_2 <- mean(model.S.2$residuals^2)^.5 # RMSE value is 6774.245
pred_S2 <- predict(model.S.2,Startups)
cor(pred_S2,Startups$Profit) # correlation is 0.9748282

#Final model ----
model.S.3 <- lm(Profit~R.D.Spend+Marketing.Spend,data = df_Startups)
summary(model.S.3) # In our model R² = 0.9612 (0.9594 Adjusted) and other variable except R.D.Spend are insignificant.
avPlots(model.S.3,id=list(n=3,col="red"),lwd=2,col='forestgreen')
pred_S3 <- predict(model.S.3,Startups) # Predicted Values
cor(pred_S3,Startups$Profit) # 0.9748121
rmse_3 <- mean(model.S.3$residuals^2)^.5 # RMSE value is 6899.99

detach(Startups)

#________________________________________________________________________________________________________________

#Question No 2 ----
colnames(Comp)
Comp <- read.csv("Computer_Data.csv")
df_comp <- Comp[,-1]
head(Comp) 
ls_comp = list("speed" = table(Comp$speed),"ram"=table(Comp$ram),"screen"=table(Comp$screen),
     "cd"=table(Comp$cd),"multi"=table(Comp$multi),table(Comp$premium),"ads"=table(Comp$ads),"trend"=table(Comp$trend))

barplot(ls_comp$speed,main="speed",xlab = "speed",ylab = "No of computers")
str(Comp)

summary(Comp) #
#write.csv(summary(Comp),'outputs/Comp.csv')

boxplot(Comp[,-c(1,7,8,9)])$out
boxplot(scale(Comp[,-c(1,7,8,9)]))$out
boxplot(scale(log(Comp[,-c(1,7,8,9)])))$out

#install.packages("rgl")
plot(Comp[,-c(1,7:9)])
cor(Comp[,-c(1,7:9)])
#write.csv(cor(Comp[,-c(1,7:9)]),'outputs/Comp_COR.csv')

# Model 1  ----
model_Comp_1 <-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data = df_comp)
summary(model_Comp_1) # R2 = 0.7756

model_Comp_1.1 <- lm(price~.,data=df_comp)
summary(model_Comp_1.1)

vif(model_Comp_1)
# No colinearity problem in our model.
avPlots(model_Comp_1)
pred_C_1 <- predict(model_Comp_1,newdata = df_comp)
cor(pred_C_1,df_comp$price) # Correlation is 0.8806631
rmse_C_1 <- mean(model_Comp_1$residuals^2)^.5 # rmse = 275.1298
influence_1 <- as.integer(rownames(influencePlot(model_Comp_1,id = list(n=5,col="blue",cex=1.2))))
influenceIndexPlot(model_Comp_1,id=list(n=10,col='red'))

# model 2 ----
# Log transformation
df_comp2 <- data.frame(scale(log(Comp[,-c(1,2,7,8,9)])),"price" = df_comp$price,"cd" = df_comp$cd,"premium" = df_comp$premium,"multi" = df_comp$multi)
model_Comp_2 <- lm(price~.,data=df_comp2)
summary(model_Comp_2) # 0.7426
influenceIndexPlot(model_Comp_2,id = list(n=20,col='red'))
influ_comp <- as.integer(rownames(influencePlot(model_Comp_2,id = list(n=20,col="blue"))))
length(influ_comp)
df_comp3 <- df_comp2[-c(influ_comp),]#head(df_comp2)
model_Comp_3 <- lm(price~.,data=df_comp3)
summary(model_Comp_3) # R2 = 0.7508

avPlots(model_Comp_3)
pred_C_3 <- predict(model_Comp_3,newdata = df_comp3)
cor(pred_C_3,df_comp3$price) # Correlation is 0.8664749
rmse_C_3 <- mean(model_Comp_3$residuals^2)^.5 # rmse = 281.3819


# model 4 ----

influence(model_Comp_1)
influenceIndexPlot(model_Comp_1)
influencing_obs <- length(which(rowSums(influence.measures(model_Comp_1)$is.inf) > 0));influencing_obs # These are the influencing observations
influence_obs <- as.integer(rownames(influencePlot(model_Comp_1,id=list(n=90,col="red"))))

length(influence_obs)

1- 186/nrow(df_comp)
df_Comp_scale <- data.frame(df_comp[,-c(6,7,8)],"premium"=df_comp$premium,"cd"=df_comp$cd,"multi"=df_comp$multi)#,"cd"=df_comp3$cd,"multi"=df_comp3$multi
df_Comp_scale <- df_Comp_scale[-c(influence_obs),]
head(df_Comp_scale)

df_comp[influence_obs,]

model_Comp_4 <- lm(price~.,data=df_Comp_scale) 
summary(model_Comp_4) # R2 = 0.804
avPlots(model_Comp_4)
pred_C_4 <- predict(model_Comp_4,newdata = df_comp)
cor(pred_C_4,df_comp$price) # Correlation is 0.879
rmse_C_4 <- mean(model_Comp_4$residuals^2)^.5 # rmse = 238.0004

#__________________________________________________________________________________________________________________

Corol <- read.csv("ToyotaCorolla.csv") 
str(Corol)
Corolla<-Corol[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

summary(Corolla)
#write.csv(summary(Corolla),"outputs/summary_toyota.csv")
boxplot(Corolla,col = 1:9)
boxplot(scale(Corolla),ylim=c(-5,5))
# Checking out the correlations and the Pair plot
cor(Corolla)
# write.csv(cor(Corolla),'outputs/cor_coro.csv')
# no pair of variables except Age and Price is highly correlated
pairs(Corolla)

#Target Price

#Building my first model

model_T_1 <- lm(Price~.,data = Corolla)
summary(model_T_1) # R2 = 0.8638 ........... cc and doors are seems to be insignificant in our model
pred_T_1 <- predict(model_T_1,Corolla)
cor(pred_T_1,Corolla$Price) # We are getting correlation  as 0.9293884
rmse_T_1 <- mean(model_T_1$residuals^2)^.5 ;rmse_T_1# and RMSE value as 1338.258
avPlots(model_T_1)
# Check for the Influence plot
influenceIndexPlot(model_T_1,grid = T,id = list(col="red",cex=1.5))
influencePlot(model_T_1,id=list(col="red"))
influence_index <- as.integer(rownames(influencePlot(model_T_1))) # Ok, here we are getting 3 influence index in our model
vif(model_T_1) # ok, in our model we can say there is no colinearity problem exists as all the vif values are less than 10
influenceIndexPlot(model_T_1,grid = T,id = list(col="red",cex=2))


#Building my Second model
df_Corola <- Corolla[-c(influence_index),]

model_T_2 <- lm(Price~.,data = df_Corola)
summary(model_T_2) # R2 = 0.8852... cc and doors are now significant
pred_T_2 <- predict(model_T_2,df_Corola)
cor(pred_T_2,df_Corola$Price) # We are getting correlation  as 0.9408425
rmse_T_2 <- mean(model_T_2$residuals^2)^.5;rmse_T_2 # and RMSE value as 1227.474

avPlots(model_T_2)
