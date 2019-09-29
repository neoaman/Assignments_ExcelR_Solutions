setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Basic Statistics Level 2")
library(readxl)

#Question 1 : -------------------------

df <-read_excel("Book01.xlsx")
colnames(df) <- c("Company","measure")
boxplot(df$measure)$out
mean(df$measure)
sd(df$measure)
var(df$measure)

#Set 2
#Q1--------------
Mu = 45
sigma = 8
60-10
X = 50
Z = (50-Mu)/sigma
1-pnorm(q = 0.625)
#Q2--------------
Mu = 38
sigma = 6
Z = (44-Mu)/sigma
pnorm(1)

Z = (30-Mu)/sigma
pnorm(Z)*400
400*9

#Q4-----------------------
Mu = 100
sigma = 20
z = qnorm(p = 0.995)
a = 100+(2.575829)

qnorm(p = 0.995,mean = 100,sd = 20,lower.tail = F);qnorm(p = 0.995,mean = 100,sd = 20)

a = 1:10
shapiro.test(a)

A = rnorm(n = 1000,mean = 5,sd = 2)
B = rnorm(n = 1000,mean = 5,sd = 2)
mean(A+B)
mean(2*A)
var(A+B)
var(A)+var(B)
sd(A+B)
sd(A)+sd(B)
var(2*A)
4*var(A)
2*var(B)
2*var(A)
var(A+B)
sd(2*A)
2*sd(A)
sd(A+A)
sd(A+B)

#Q5:----------------
mean = 7+5
sd = sqrt(25)
qnorm(p = 0.05,mean = mean,sd = sd)*45
qnorm(p = 0.975,mean = mean,sd = sd,lower.tail = F)
2.2*45
21.79*45

x = 9
mu1 = 5
mu2 = 7
s1 = 3
s2 = 4

(x-mu1)/s1
(x-mu2)/s2

qnorm(p = 0.95,mean = mu1,sd = s1)
qnorm(p = 0.95,mean = mu2,sd = s2)

#Q6:
qnorm(0.95,mean = 0,sd = 1)
qt(p = 0.95,30000000)

pnorm((55-50)/40)-pnorm((45-50)/40)


pnormGC
install.packages("tigerstats")
library(tigerstats)
pnormGC(bound = c(55,45),region = "between",mean = 50,sd = 40)
ptGC(bound = c(55,45),region = 'between',df = 99)
5/4
1-(pnorm(5/4)-pnorm(-(5/4)))
1-(pnorm(q = 55,mean = 50,sd = 40))-(pnorm(q = 45,mean = 50,sd = 40))


pnorm(5/4)
pnorm()



(qnorm(0.975)*40/5)^2
qnorm(0.975)*40
120/sqrt(40000)
