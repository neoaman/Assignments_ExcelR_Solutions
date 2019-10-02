setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\Text Mining")

# Required Packages for extracting twitter data:
library(twitteR)
library("SnowballC")
library("tm")
library("twitteR")
library("syuzhet")
library(topicmodels)
setup
# My twitter Secret and Twitter key is in my folder ---- ------------------------
"_______________________________MY TWITTER CONFIDENTIAL _________________________________________"
# key <- readLines("twitterconfidential.txt")
# appname <- key[1]
# consumer_key <- key[2]
# consumer_secret <- key[3]
# access_token <- key[4]
# access_secret <- key[5]
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
"________________________________________________________________________________________________"
# Search for Narendra Modiji -----
"________________________________Extracting the data_____________________________________________"
# tweets <- userTimeline("narendramodi", n=1000,includeRts = F)
# tweet_df <- twListToDF(tweets)
# write.csv(tweet_df,"modiTweet.csv")
# head(tweet_df)
# colnames(tweet_df) # our column of interest is column no 1. which is "text"
# head(tweet_df$text)
"_____________________________________________________________________________"
tweet_df <- read.csv("modiTweet.csv")
tweet_df$text
# Cleaning unwanted "http://" ----
tweet_df2 <- gsub(pattern = "http.*",replacement = "",x = tweet_df$text)
# again filter "https"
tweet_df2 <- tweet_df2[-which(grepl("<U+",tweet_df2))]
tweet_df2 <- gsub("https.*","",tweet_df2)
tweet_df2 <- gsub("#.*","",tweet_df2)
tweet_df2 <- gsub("@.*","",tweet_df2)

# You can see still a triple dotted character left in the line, so remove it by copuing and pasting the character inside gsub
#tweet_df2 <- gsub(".","",tweet_df2) It is a "." if possible change it to ... character and remove it

library("textcat")
# Consider only English Words cause Modi tweets in Hindi as well as other languages
table(textcat(tweet_df2))
c(which(textcat(tweet_df2)=="english"),which(textcat(tweet_df2)=="scots")) -> consider
tweet_df3 <- tweet_df2[consider]

# latent Dirichlet allocation ----

stops <- readLines("stop.txt") # stopwards
moditweet.corpus <- Corpus(VectorSource(tweet_df3))
moditweet.corpus <- tm_map(moditweet.corpus, removePunctuation) 
stop <- unique(c(stopwords('english'),stops,"the","due", "are", "not", "for","the", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then"))
moditweet.corpus <- tm_map(moditweet.corpus, removeWords, stop)
moditweet.corpus <- tm_map(moditweet.corpus, removeNumbers) 
moditweet.corpus <- tm_map(moditweet.corpus, stripWhitespace)
moditweet.tdm <- TermDocumentMatrix(moditweet.corpus)

# Convert tdm to dtm
moditweet.dtm <- t(moditweet.tdm)
rowTotals <- apply(moditweet.dtm, 1, sum)
moditweet.dtm.filter <- moditweet.dtm[rowTotals > 3,]
moditweet.dtm.filter$dimnames$Terms

# LDA 
library(topicmodels)
moditweet.LDA <- LDA(x = moditweet.dtm.filter, 10) # 10 Topics
moditweet.LDA.Terms <- terms(moditweet.LDA, 5) # first 10 terms of every topic
moditweet.LDA.Terms

# NLP -----

modi_tweet <- get_sentences(tweet_df3)
class(modi_tweet)
str(modi_tweet)
head(modi_tweet)

# there are 6 methods for sentiment analysis
sentiments <- c("syuzhet", "afinn", "bing", "nrc","stanford", "custom")
A <- NULL;Sent_List<-NULL
for(i in sentiments[1:4]){
  Sent_List[[i]] <- get_sentiment(modi_tweet, method = i)
  A[[i]] =  table(get_sentiment(modi_tweet, method = i))
  
}
A
Sent_List

# Lets analyse the data

# plot For nrc ----
plot(Sent_List$nrc, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red",lwd=2)
abline(h = 1, col = "lightgreen",lwd=1)
abline(h = 2, col = "forestgreen",lwd=1)
abline(h = 3, col = "blue",lwd=2)
# To extract the sentence with the most negative emotional valence
negative <- Sent_List$nrc[which.min(Sent_List$nrc)]
modi_tweet[which(Sent_List$nrc==negative)]
# and to extract the most positive sentence
positive <- Sent_List$nrc[which.max(Sent_List$nrc)]
modi_tweet[which(Sent_List$nrc==positive)]


# plot For bing ----
plot(Sent_List$bing, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red",lwd=2)
abline(h = 1, col = "lightgreen",lwd=1)
abline(h = 2, col = "forestgreen",lwd=1)
abline(h = 3, col = "blue",lwd=2)
# To extract the sentence with the most negative emotional valence
negative <- Sent_List$bing[which.min(Sent_List$bing)]
modi_tweet[which(Sent_List$bing==negative)]
# and to extract the most positive sentence
positive <- Sent_List$bing[which.max(Sent_List$bing)]
modi_tweet[which(Sent_List$bing==positive)]




# percentage based figures ----
percent_vals <- get_percentage_values(Sent_List$bing)
plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)
ft_values <- get_transformed_values(
  Sent_List$bing, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = ifelse(ft_values >0 ,"green","orange")
)


# categorize each sentence by eight emotions ----
nrc_data <- get_nrc_sentiment(modi_tweet)

# subset

sad_items <- which(nrc_data$sadness > 0)
head(modi_tweet[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

# Word Cloud ----
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")



moditdm <- as.matrix(moditweet.tdm)
sortmoditdm <- sort(rowSums(moditdm),decreasing=TRUE)
df <- data.frame(word = names(sortmoditdm),freq=sortmoditdm)
windows()

wordcloud(words = df$word[-2], freq = df$freq, min.freq = 3,
          max.words=150, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(20, "Dark2"))

findFreqTerms(moditweet.dtm.filter, lowfreq = 8)
findAssocs(moditweet.dtm.filter, terms = "indian", corlimit = 0.3)
head(df, 10)

barplot(df[c(1,3:20),]$freq, las = 2, names.arg = df[c(1,3:20),]$word,
        col =brewer.pal(20, "Dark2"), main ="Most frequent words",
        ylab = "Word frequencies")

tweet_df3
tweet_df3[which(grepl(pattern = "Yoga",x = tweet_df3))]
