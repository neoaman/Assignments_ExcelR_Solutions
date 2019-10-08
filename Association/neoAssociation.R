"______________________________________ASSOCIATION______________________________________________________________"
# Library required
library(arules)
library(arulesViz)

book <- read.csv("book.csv")

# Book First Apriori algorithm ----
# Apriori algorithm with support 0.02 and confidence 0.5 and minimum length as 3
Brules <- apriori(as.matrix(book),parameter=list(support=0.05, confidence = 0.5,minlen=3)) # Set of 186 rules
inspect(head(sort(Brules, by = "lift")))
inspect(head(sort(Brules, by = "confidence")))
inspect(head(sort(Brules, by = "support")))
inspect(head(sort(Brules, by = c("count","lift")))) # Maximum count for 299
# here we are getting maximum support values as 0.1495, maximum confidence value as 1 and maximum lift as 2.6009

head(quality(Brules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(Brules,method = "scatterplot",jitter=0,col=colfunc(30))
# plot methods are'matrix', 'mosaic', 'doubledecker', 'graph',
#'paracoord', 'scatterplot', 'grouped matrix', 'two-key plot', 'matrix3D', 'iplots'
set.seed(103);plot(Brules,method = "graph")
plot(Brules,method = "paracoord")
plot(Brules,method = "grouped matrix",col=colfunc(300))
plot(Brules,method = "two-key plot",jitter=0)
plot(Brules,method = "matrix",engine = "3d")# iplots
# install.packages("iplots")
plot(Brules,method = "iplots")





"____________________________________________________________________________________________________"
# Library required
library(arules)
library(arulesViz)

groceries <- read.transactions("groceries.csv",sep = ",")
itemFrequency(groceries[, 1:5])
inspect(groceries[1:5])

itemFrequencyPlot(x = groceries,topN=10)

# Book First Apriori algorithm ----
# Apriori algorithm with support 0.005 and confidence 0.05 and minimum length as 3
Grules <- apriori(groceries,parameter=list(support=0.005, confidence = 0.05,minlen=3)) # Set of 186 rules
inspect(head(sort(Grules, by = "lift")))
inspect(head(sort(Grules, by = "confidence")))
inspect(head(sort(Grules, by = "support")))
inspect(head(sort(Grules, by = c("count","lift")))) # Maximum count for 228
# here we are getting maximum support values as 0.0231, maximum confidence value as 0.7 and maximum lift as 4.085

head(quality(Grules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(Grules,method = "scatterplot",jitter=0,col=colfunc(30))
# plot methods are'matrix', 'mosaic', 'doubledecker', 'graph',
#'paracoord', 'scatterplot', 'grouped matrix', 'two-key plot', 'matrix3D', 'iplots'
set.seed(103);plot(Grules,method = "graph")
plot(Grules,method = "paracoord")
plot(Grules,method = "grouped matrix",col=colfunc(300))
plot(Grules,method = "two-key plot",jitter=0)
plot(Grules,method = "matrix",engine = "3d")# iplots
# install.packages("iplots")
plot(Grules,method = "iplots")


yogurtrules <- subset(Grules, items %in% "yogurt")
inspect(yogurtrules[1:5])

"__________________________________________________________________________________________________________"

library(arules)
library(arulesViz)

movies <- read.csv("my_movies.csv")
head(movies)


# Book First Apriori algorithm ----
# Apriori algorithm with support 0.005 and confidence 0.05 and minimum length as 3
Mrules <- apriori(as.matrix(movies[,6:15]),parameter=list(support=0.005, confidence = 0.05,minlen=3)) # Set of 186 rules
inspect(head(sort(Mrules, by = "lift")))
inspect(head(sort(Mrules, by = "confidence")))
inspect(head(sort(Mrules, by = "support")))
inspect(head(sort(Mrules, by = c("count","lift")))) # Maximum count for 4
# here we are getting maximum support values as 0.4, maximum confidence value as 1 and maximum lift as 10

head(quality(Mrules))
colfunc <- colorRampPalette(c("black","red", "yellow"))
plot(Mrules,method = "scatterplot",jitter=0,col=colfunc(30),cex=2)
# plot methods are'matrix', 'mosaic', 'doubledecker', 'graph',
#'paracoord', 'scatterplot', 'grouped matrix', 'two-key plot', 'matrix3D', 'iplots'
set.seed(103);plot(Mrules,method = "graph")
plot(Mrules,method = "paracoord")
plot(Mrules,method = "grouped matrix",col=colfunc(300))
plot(Mrules,method = "two-key plot",jitter=0,pch=20,cex=2)
plot(Mrules,method = "matrix",engine = "3d")# iplots
# install.packages("iplots")
plot(Mrules,method = "iplots",interactive = T)

inspect(Mrules)

Harrypotter <- subset(Mrules, items %in% "Harry.Potter1")
inspect(Harrypotter[1:5])
# The people who buy HarryPotter most likely to buy Lord of the ring.
