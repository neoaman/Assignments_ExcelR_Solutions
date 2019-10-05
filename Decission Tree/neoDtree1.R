"------------------------Decission Tree Classifier -------------------------"
# Question 1 ----

# Required packages

#install.packages(c("rpart","rpart.plot","party","tree"))
library("party")
library (tree)
library(rpart);library(rpart.plot)
library(C50)

# Required Data ----
data("iris")
head(iris) #Here our target variable is Species

# Feature Selection
set.seed(1);split <- sample(nrow(iris),nrow(iris)*0.7,F)
df_iTrain <- iris[split,]
df_iTest <- iris[-split,]

# Model 1 ---- using library party.
Treei1 <- ctree(formula = df_iTrain$Species ~.,data = df_iTrain[,-5])
predi1 <- predict(Treei1,df_iTest)

table(Predicted=predi1,Actual=df_iTest$Species)
mean(predi1==df_iTest$Species) # Accuracy is 97.7%
plot(Treei1)
plot(Treei1,terminal_panel = node_terminal(Treei1,fill = "lightgreen"))

# Model 2 ---- using library tree
Treei2 <- tree(formula = df_iTrain$Species ~.,data = df_iTrain[,-5])
predi2_ <- predict(Treei2,df_iTest);
predi2 = ifelse(predi2_[,2] > 0.5,"versicolor",ifelse(predi2_[,3]>0.5,"virginica","setosa"))


table(Predicted=predi2,Actual=df_iTest$Species)
mean(predi2==df_iTest$Species) # Accuracy is 91%
plot(Treei2,lwd=2,col="gray")
text(Treei2,col=c("forestgreen","red"))

# Model 3 ---- using library rpart and rpart.plot
Treei3 <- rpart(formula = df_iTrain$Species ~.,data = df_iTrain[,-5])
predi3_ <- predict(Treei3,df_iTest);
predi3 = ifelse(predi3_[,2] > 0.5,"versicolor",ifelse(predi3_[,3]>0.5,"virginica","setosa"))


table(Predicted=predi3,Actual=df_iTest$Species)
mean(predi3==df_iTest$Species) # Accuracy is 91%
rpart.plot(Treei3,lwd=1,type = 4)

# Model 4 ---- using library C50
Treei4 <- C5.0(formula = df_iTrain$Species ~.,data = df_iTrain[,-5])
predi4 <- predict(Treei4,df_iTest);

table(Predicted=predi4,Actual=df_iTest$Species)
mean(predi4==df_iTest$Species) # Accuracy is 91%
plot(Treei4)

# Model 5 ---- Using boosting technique


Treei5 <- C5.0(formula = df_iTrain$Species ~.,data = df_iTrain[,-5],trials = 10)
predi5 <- predict(Treei5,df_iTest);
table(predi5,df_iTest$Species)
mean(predi5==df_iTest$Species) # Accuracy is 97.7%
plot(Treei5)


# Check for all 100 trials of boosting
for ( i in 1:100){
  Treei5 <- C5.0(formula = df_iTrain$Species ~.,data = df_iTrain[,-5],trials = i)
  predi5 <- predict(Treei5,df_iTest);
  # table(predi5,df_iTest$Species)
  print(mean(predi5==df_iTest$Species)) # Accuracy is 91%
  # plot(Treei5)
  if(mean(predi5==df_iTest$Species) == 1) {
    print(i)
  }
    
}

