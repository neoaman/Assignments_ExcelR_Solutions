#Setting the working Directory for my project
setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Completed\\Hypothesis testing Assignments")
getwd()



#____________________________________________________________________________________________________________________________________________________
 "Question No 1:
      A F&B manager wants to determine whether there is any significant difference in the diameter of the cutlet between two units.
      A randomly selected sample of cutlets was collected from both units and measured? Analyze the data and draw inferences at 5%
      significance level. Please state the assumptions and tests that you carried out to check validity of the assumptions."
# Answer :------------------------

# Importing the Data Cutlets.csv
cutlets = read.csv("Cutlets.csv")
attach(cutlets)
head(cutlets)
# Hypoyhesis Part :----
#  Ho : µ1(Unit.A) = µ2(Unit.B) Against the Alternative Hypothesis Ha : µ1(Unit.A)  <not equal to>  µ2(Unit.B)
# Normality Test For the data
nortest::pearson.test(Unit.A);nortest::pearson.test(Unit.B) # According to pearson Normality test P = [0.3799,0.3288]
nortest::ad.test(Unit.A);nortest::ad.test(Unit.B) # According to Anderson Darling Normality test P = [0.2866,0.6869]
# Both the Data are Normally Distributed , So we will perform the Parametric Test
# Comparing the Variance, whather variance are same or not at 0.95 confidence level 
var(Unit.A);var(Unit.B);var.test(Unit.A,Unit.B) # Variance ratio i.e. F-test is 0.705 ; Ho is accepted at 31% level of significance as    p-value is 0.3136# As variance are same so we are going to perform the t-test for two samples
t.test(x = Unit.A,y =Unit.B ,alternative = "two.sided",conf.level = 0.95,var.equal = T) # P value for the t-test is found out to be 0.4722 
t.test(x = Unit.A,y =Unit.B ,alternative = "two.sided",conf.level = 0.95,correct = T)

# Conclusion : 
#   The P value for the two sample t test is found out to be 0.47 so we accept the null hypothesis i.e. there is no significant difference
#   between the length of cutlets in the two sample units "Unit.A" and "Unit.B" , proved with level of significance 0.05 i.e. confidence level 0.95





#____________________________________________________________________________________________________________________________________________________
 "Question No 2:
      A hospital wants to determine whether there is any difference in the average Turn Around Time (TAT) of reports of the laboratories 
      on their preferred list. They collected a random sample and recorded TAT for reports of 4 laboratories. 
      TAT is defined as sample collected to report dispatch.
      Analyze the data and determine whether there is any difference in average TAT among the different laboratories at 5% significance level."
# Answer :----------------------

# Importing the Data LabTAT.csv
labtat = read.csv("LabTAT.csv")
head(labtat)
library(nortest)
# Hypothesis Part
# Ho : There is no significant difference between the average TAT values in the 4 libraries 
# H1 : There is significant difference between the average TAT values in the 4 libraries 
ad.test(labtat$Laboratory.1);ad.test(labtat$Laboratory.2);ad.test(labtat$Laboratory.3);ad.test(labtat$Laboratory.4)
# All the 4 variables are normally distributed with respected p values = [0.53,0.73,0.57,0.41]
# So we are going to perform Homogenity of variance tests like Levene test
stack_labdata = stack(labtat)
library(car)
leveneTest(stack_labdata$values,stack_labdata$ind)
# The Variances of both the variables are homogenous (p value is 0.051 > 0.05) so we will go for Anova test
summary(aov(formula = values~ind,data = stack_labdata))
# Here P value < 0.05 So we will reject the null hypothesis on the basis of 0.05 level of significance.

# Conclusion : 
# There is significant difference between the TAT values of 4 different libraries

#____________________________________________________________________________________________________________________________________
"Question no 3 : 
      Sales of products in four different regions is tabulated for males and females. 
      Find if male-female buyer rations are similar across regions."

#Answer :---

#Importing required data
BuyerRatio <- read.csv("BuyerRatio.csv")
# Hypothesis Part
# H0 : male-female buyer rations are similar across regions
# H1 : male-female buyer rations are not similar across regions

chisq.test(BuyerRatio[,2:5])
chisq.test(t(BuyerRatio[,2:5]))
# Here we find the Chisqare  value to be 1.59 with 3 degree of freedom and 0.05 level of significance
# P value for our chi-square test is found out to be 0.6603 i.e. > 0.05 

# Conclusion:--
# Hence Null Hypothesis may be accepted i.e. male-female buyer rations are similar across regions.

#_______________________________________________________________________________________________________________________________________
"Question no 4 : 
      TeleCall uses 4 centers around the globe to process customer order forms. 
      They audit a certain %  of the customer order forms. 
      Any error in order form renders it defective and has to be reworked before processing. 
      The manager wants to check whether the defective %  varies by centre. 
      Please analyze the data at 5% significance level and help the manager draw appropriate inferences"
# Importing the data set:
CostomerOrderForm = read.csv("Costomer+OrderForm.csv")
attach(CostomerOrderForm)
df = as.data.frame(cbind(c(Phillippines,Indonesia,Malta,India),rep(c("Phillippines","Indonesia","Malta","India"),c(300,300,300,300))))
colnames(df) = c("Type","Location")
head(df)
df_tab1 = table(df$Type,df$Location)
# 1 is for Defective and 2 is for Error free

# Hypothesis:
# Ho : No of Defectives doesnot varies with centers
# Ha : No of Defectives varies with centers

# Chisquare test for difference in Attributes 
chisq.test(df_tab1) # P value = 0.2771

# Conclusion:
# As our null hypothesis is accepted with p value : 0.27 which is gratter than 0.05 ; 
# So we conclude that, number of Defectives doesnot varies with centers


#_______________________________________________________________________________________________________________________________________
"Question no 5 : 
      Fantaloons Sales managers commented that % of males versus females walking in to the store differ based on day of the week.
      Analyze the data and determine whether there is evidence at 5 % significance level to support this hypothesis."
# Importin the data:
Faltoons = read.csv("Faltoons.csv")
head(Faltoons)

#Data Manipulation
nrow(Faltoons) # 400 rows in the data
attach(Faltoons)
df_falt = as.data.frame(cbind(c(Weekdays,Weekend),rep(c("Weekdays","Weekend"),c(400,400)))) # 1 for Female; and 2 for Male
colnames(df_falt) = c("Gender","Day")
head(df_falt)
df_tab2 = table(df_falt);df_tab2

# Hypothesis Testing:---
# Ho : males versus females walking in to the store doesnot differ based on day of the week.
# Ha : males versus females walking in to the store differ based on day of the week.

# Chisquare test for difference in Attributes, Here one Attribute is Gender (Male and Female) and another Attribute is day (Weekedays,weekends)

chisq.test(df_tab2) # P-value is 0.00008543 < 0.05 , Hence we reject the null hypothesis

# Conclusion:--
# males versus females walking in to the store differ based on day of the week.
