# Question 
"Extract movie reviews for any movie from IMDB and perform sentimental analysis"

# Extracting reviews from Movie
library(rvest)
library(XML)
library(magrittr)

"____________________________________________________________________________________________________________________________"
url <- "https://www.imdb.com/title/tt5580390/reviews?sort=helpfulnessScore&dir=desc&ratingFilter"
IMDB_review <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(url,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_review <- c(IMDB_review,rev)
}

length(IMDB_review)
length(unique(IMDB_review))
IMDB_review[duplicated(IMDB_review)]
write.table(unique(IMDB_review),"ShapeofWater.txt",row.names = F)
SOW <- readLines("ShapeofWater.txt")

"____________________________________________________________________________________________________________________________"
SOW[1:2]





# Cleaning unwanted "http://" ----
df_SOW <- gsub(pattern = "http.*",replacement = "",x = SOW)
# again filter "https"
#df_SOW <- df_SOW[-which(grepl("<U+",df_SOW))]
df_SOW <- gsub("https.*","",df_SOW)
df_SOW <- gsub('\""','',df_SOW)


# You can see still a triple dotted character left in the line, so remove it by copuing and pasting the character inside gsub


library("textcat")
# Consider only English Words cause Modi tweets in Hindi as well as other languages
table(textcat(df_SOW))
df_SOW[which(textcat(df_SOW)=="norwegian")]
c(which(textcat(df_SOW)!="norwegian")) -> consider
df_SOW <- df_SOW[consider]

# latent Dirichlet allocation ----

stops <- readLines("stop.txt") # stopwards
SOW_Cor <- Corpus(VectorSource(df_SOW))
SOW_Cor <- tm_map(SOW_Cor, removePunctuation) 
stop <- unique(c(stopwords('english'),stops,"the","due", "are", "not", "for","the", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then"))
SOW_Cor <- tm_map(SOW_Cor, removeWords, stop)
SOW_Cor <- tm_map(SOW_Cor, removeNumbers) 
SOW_Cor <- tm_map(SOW_Cor, stripWhitespace)
SOW_tdm <- TermDocumentMatrix(SOW_Cor)

# Convert tdm to dtm
SOW_dtm <- t(SOW_tdm)
rowTotals <- apply(SOW_dtm, 1, sum)
SOW_dtm2 <- SOW_dtm[rowTotals > 3,]
SOW_dtm2$dimnames$Terms

# LDA 
library(topicmodels)
SOW_LDA <- LDA(x = SOW_dtm2, 10) # 10 Topics
SOW_LDA_terms <- terms(SOW_LDA, 5) # first 10 terms of every topic
SOW_LDA_terms



# NLP -----

SOW_ <- get_sentences(df_SOW)
class(SOW_)
str(SOW_)
head(SOW_)

# there are 6 methods for sentiment analysis
sentiments <- c("syuzhet", "afinn", "bing", "nrc","stanford", "custom")
A <- NULL;Sent_List<-NULL
for(i in sentiments[1:4]){
  Sent_List[[i]] <- get_sentiment(SOW_, method = i)
  A[[i]] =  table(get_sentiment(SOW_, method = i))
  
}
A
Sent_List

# Lets analyse the data

# plot For nrc ----
plot(Sent_List$nrc, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red",lwd=2)
abline(h = 2, col = "lightgreen",lwd=1)
abline(h = 4, col = "forestgreen",lwd=1)
abline(h = 6, col = "blue",lwd=2)
# To extract the sentence with the most negative emotional valence
negative <- Sent_List$nrc[which.min(Sent_List$nrc)]
SOW_[which(Sent_List$nrc==negative)]
# and to extract the most positive sentence
positive <- Sent_List$nrc[which.max(Sent_List$nrc)]
SOW_[which(Sent_List$nrc==positive)]


# plot For bing ----
plot(Sent_List$bing, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red",lwd=2)
abline(h = 2, col = "lightgreen",lwd=1)
abline(h = 4, col = "forestgreen",lwd=1)
abline(h = 6, col = "blue",lwd=2)
# To extract the sentence with the most negative emotional valence
negative <- Sent_List$bing[which.min(Sent_List$bing)]
SOW_[which(Sent_List$bing==negative)]
# and to extract the most positive sentence
positive <- Sent_List$bing[which.max(Sent_List$bing)]
SOW_[which(Sent_List$bing==positive)]




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
  main ="xTransformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = ifelse(ft_values >0 ,"green","orange")
)


# categorize each sentence by eight emotions ----
nrc_data <- get_nrc_sentiment(SOW_)

# subset

sad_items <- which(nrc_data$sadness > 0)
head(SOW_[sad_items])

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

# Word Cloud ----
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

moditdm <- as.matrix(SOW_tdm)
sortmoditdm <- sort(rowSums(moditdm),decreasing=TRUE)
df <- data.frame(word = names(sortmoditdm),freq=sortmoditdm)
windows()

wordcloud(words = df$word[-1], freq = df$freq, min.freq = 3,
          max.words=150, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(20, "Dark2"))

findFreqTerms(SOW_dtm2, lowfreq = 8)
findAssocs(SOW_dtm2, terms = "creature", corlimit = 0.3)
head(df, 10)

barplot(df[2:20,]$freq, las = 2, names.arg = df[2:20,]$word,
        col =brewer.pal(20, "Dark2"), main ="Most frequent words",
        ylab = "Word frequencies")
# 1 is Phone and 2 is Samsung
df_SOW[which(grepl(pattern = "buy",x = df_SOW))]



