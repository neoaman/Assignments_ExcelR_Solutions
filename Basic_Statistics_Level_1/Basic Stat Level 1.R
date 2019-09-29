setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Basic Statistics Level 1")
# Question 7 
q7 <- read.csv("Q7.csv")
df_1 = q7[,2:4]
head(df_1,2)
summary(df_1)
var(df_1[,1]);var(df_1[,2]);var(df_1[,3])
sd(df_1[,1]);sd(df_1[,2]);sd(df_1[,3])
abs(range(df_1[,1])[1]-range(df_1[,1])[2]);abs(range(df_1[,2])[1]-range(df_1[,2])[2]);abs(range(df_1[,3])[1]-range(df_1[,3])[2])
library(car)
library(nortest)
nortest::ad.test(df_1[,3])
boxplot(df_1[,1])$out
quantile(df_1[,2])
3.920+1.5*IQR(df_1[,1])
hist(df_1$Points)
hist(df_1$Score)
hist(df_1$Weigh)
install.packages("moments")
library(moments)
skewness(df_1$Points)[]
?skewness
moments::skewness(df_1$Score)[]
kurtosis(df_1$Score)[]
moments::kurtosis(df_1$Score)[]

skewness(c(2000:2010,10))

ald = c(108, 110, 123, 134, 135, 145, 167, 187, 199)
mean(ald)
library(BSDA)
library(e1071)
q_9a = read.csv("Q9_a.csv")
skewness(q_9a[,2]);skewness(q_9a[,3])

kurtosis(q_9a[,2]);kurtosis(q_9a[,3])
hist(q_9a[,2],breaks = 20)
hist(q_9a[,3],breaks = 20)


q_9b = read.csv("Q9_b.csv")
head(q_9b)
skewness(q_9b[,"SP"]);skewness(q_9b[,"WT"])
kurtosis(q_9b[,2]);kurtosis(q_9b[,3])
hist(q_9b[,2],breaks = 20)
hist(q_9b[,3],breaks = 20)

# Q11 : Average weight
qnorm(p = 0.97)
200+(1.880794*(30/sqrt(2000)))  
200-(1.880794*(30/sqrt(2000)))    

qnorm(p = 0.99)
200+(2.326348*(30/sqrt(2000)))  
200-(2.326348*(30/sqrt(2000))) 

qnorm(p = 0.98)
200+(2.053749*(30/sqrt(2000)))  
200-(2.053749*(30/sqrt(2000))) 

qnorm(0.95)
qnorm(0.97)
qnorm(0.8)

a <- c(34,36,36,38,38,39,39,40,40,41,41,41,41,42,42,45,49,56)
mean(a);median(a);var(a);sd(a)
hist(a)
shapiro.test(a)
which.max(tabulate(a))


df = read.csv("Cars.csv")
table(prop.table(df$MPG>38))
prop.table(table(df$MPG>38))
prop.table(table(df$MPG<40))
prop.table(table(df$MPG < 50 & df$MPG > 20))
table(df$MPG > 20)
table(df$MPG < 50)
shapiro.test(df$MPG)
hist(df$MPG)

df = read.csv("wc-at.csv")
head(df)

shapiro.test(df$AT)
shapiro.test(df$Waist)
hist(df$AT)
hist(df$Waist)

2*qnorm(p = 0.95)
qt(p = 0.005,df = 24,lower.tail = F)

qt(.975,24)


(260-270)/(90/sqrt(18))

pt(-0.4714045,df = 17)

qt(p = 0.95,df = 17 )

