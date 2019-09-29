# Q3- Three Coins are tossed, find the probability that two heads and one tail are obtained ? 
#Ans:
Coin <- c(1,0) # 1 for head 0 for tail
Toss1 <- sample(Coin, 1800000,replace = T)
Toss2 <- sample(Coin, 1800000,replace = T)
Toss3 <- sample(Coin, 1800000,replace = T)
df = cbind(Toss1,Toss2,Toss3)
table(rowSums(df))
prop.table(table(rowSums(df))) # 0.37


Bag <- c("R","R","G","G","G","B","B")
combn(x = Bag,m = 2)
1-(((2/7)*(1/6))+(2/7))
10/21
