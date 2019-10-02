# Extracting reviews from Amazon
library(rvest)
library(XML)
library(magrittr)
library(tm)
"____________________________________________________________________________________________________________________________"
url <- "https://www.amazon.in/Samsung-Metallic-Display-Storage-5000mAH/product-reviews/B07HGJFP9M/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews&pageNumber"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
SamsumM30_review <- amazon_reviews
write.table(amazon_reviews,"SamsumM30.txt",row.names = F)
"____________________________________________________________________________________________________________________________"
SM30 <- unique(readLines("SamsumM30.txt"))


# Cleaning unwanted "http://" ----
df_SM30 <- gsub(pattern = "http.*",replacement = "",x = SM30)
# again filter "https"
df_SM30 <- df_SM30[-which(grepl("<U+",df_SM30))]
df_SM30 <- gsub("https.*","",df_SM30)
df_SM30 <- gsub("#.*","",df_SM30)
df_SM30 <- gsub("@.*","",df_SM30)

# You can see still a triple dotted character left in the line, so remove it by copuing and pasting the character inside gsub


library("textcat")
# Consider only English Words cause Modi tweets in Hindi as well as other languages
table(textcat(df_SM30))
df_SM30[which(textcat(df_SM30)=="german")]
c(which(textcat(df_SM30)!="norwegian")) -> consider
df_SM30 <- df_SM30[consider]

# latent Dirichlet allocation ----

stops <- readLines("stop.txt") # stopwards
SM30_Cor <- Corpus(VectorSource(df_SM30))
SM30_Cor <- tm_map(SM30_Cor, removePunctuation) 
stop <- unique(c(stopwords('english'),stops,"the","due", "are", "not", "for","the", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then"))
SM30_Cor <- tm_map(SM30_Cor, removeWords, stop)
SM30_Cor <- tm_map(SM30_Cor, removeNumbers) 
SM30_Cor <- tm_map(SM30_Cor, stripWhitespace)
SM30_tdm <- TermDocumentMatrix(SM30_Cor)

# Convert tdm to dtm
SM30_dtm <- t(SM30_tdm)
rowTotals <- apply(SM30_dtm, 1, sum)
SM30_dtm2 <- SM30_dtm[rowTotals > 3,]
SM30_dtm2$dimnames$Terms

# LDA 
library(topicmodels)
SM30_LDA <- LDA(x = SM30_dtm2, 10) # 10 Topics
SM30_LDA_terms <- terms(SM30_LDA, 5) # first 10 terms of every topic
SM30_LDA_terms







# NLP -----
library("syuzhet")
SMM30 <- get_sentences(df_SM30)
class(SMM30)
str(SMM30)
head(SMM30)

# there are 6 methods for sentiment analysis
sentiments <- c("syuzhet", "afinn", "bing", "nrc","stanford", "custom")
A <- NULL;Sent_List<-NULL
for(i in sentiments[1:4]){
  Sent_List[[i]] <- get_sentiment(SMM30, method = i)
  A[[i]] =  table(get_sentiment(SMM30, method = i))
  
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
SMM30[which(Sent_List$nrc==negative)]
# and to extract the most positive sentence
positive <- Sent_List$nrc[which.max(Sent_List$nrc)]
SMM30[which(Sent_List$nrc==positive)]


# plot For bing ----
plot(Sent_List$bing, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red",lwd=2)
abline(h = 1, col = "lightgreen",lwd=1)
abline(h = 2, col = "forestgreen",lwd=1)
abline(h = 3, col = "blue",lwd=2)
# To extract the sentence with the most negative emotional valence
negative <- Sent_List$bing[which.min(Sent_List$bing)]
SMM30[which(Sent_List$bing==negative)]
# and to extract the most positive sentence
positive <- Sent_List$bing[which.max(Sent_List$bing)]
SMM30[which(Sent_List$bing==positive)]




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
nrc_data <- get_nrc_sentiment(SMM30)

# subset

trust <- which(nrc_data$trust > 4)
head(SMM30[trust])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

# Word Cloud ----
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")



moditdm <- as.matrix(SM30_tdm)
sortmoditdm <- sort(rowSums(moditdm),decreasing=TRUE)
df <- data.frame(word = names(sortmoditdm),freq=sortmoditdm)
windows()

wordcloud(words = df$word[-2], freq = df$freq, min.freq = 3,
          max.words=150, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(20, "Dark2"))

findFreqTerms(SM30_dtm2, lowfreq = 8)
findAssocs(SM30_dtm2, terms = "battery", corlimit = 0.3)
head(df, 10)

barplot(df[c(3:20),]$freq, las = 2, names.arg = df[c(3:20),]$word,
        col =brewer.pal(20, "Dark2"),
        ylab = "Word frequencies")
# 1 is Phone and 2 is Samsung

df_SM30[which(grepl(pattern = "buy",x = df_SM30))]



