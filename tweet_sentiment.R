# Load necessary libraries. 
library(rtweet)
library(tm)
library(SentimentAnalysis)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)


# Stream tweets that contain "summer" for 300 seconds (5 minutes).
query <- "summer"
streamtime <- 5 * 60
filename <- "rtelect.json"
tweets <- stream_tweets(q = query, timeout = streamtime, file_name = filename)


# Remove links from tweets.
tweets$stripped_text <- NULL
tweets$stripped_text <- gsub("http.*","", tweets$text)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)

# Convert to corpus so more pre-processing can be done.
corpus <- Corpus(VectorSource(tweets$stripped_text))   # Map tweets to corpus.
corpus <- tm_map(corpus, tolower)                      # Lowercase.
corpus <- tm_map(corpus, removePunctuation)        # Remove punctuation.
corpus <- tm_map(corpus, removeWords, stopwords("english"))   # Remove stop-words.
corpus <- tm_map(corpus, removeWords, c("rt", "re", "amp"))   # Remove other words like "rt".


# Convert tweets corpus into data frame.
tweets_df <- data.frame(tweet = get("content", corpus), 
                        stringsAsFactors = FALSE)


# Sentiment analysis on tweets and produce line plot of tweets vs sentiments.
s <- analyzeSentiment(tweets_df$tweet)
sentiment <- s[complete.cases(s),]        # Remove null rows.
plotSentiment(sentiment, xlab = "Tweets")


# Get direction (positive, negative, neutral) from sentiment values calculated.
sentiment$SentimentGICategory <- convertToDirection(sentiment$SentimentGI)
sentiment$SentimentHECategory <- convertToDirection(sentiment$SentimentHE)
sentiment$SentimentLMCategory <- convertToDirection(sentiment$SentimentLM)
sentiment$SentimentQDAPCategory <- convertToDirection(sentiment$SentimentQDAP)

# Plot tweets over sentiments, using each dictionary
gi <- qplot(x = c(1:nrow(sentiment)), y = SentimentGI, colour = SentimentGICategory, 
            data = sentiment, xlab = "Tweets")
he <- qplot(x = c(1:nrow(sentiment)), y = SentimentHE, colour = SentimentHECategory, 
            data = sentiment, xlab = "Tweets")
lm <- qplot(x = c(1:nrow(sentiment)), y = SentimentLM, colour = SentimentLMCategory, 
            data = sentiment, xlab = "Tweets")
qdap <- qplot(x = c(1:nrow(sentiment)), y = SentimentQDAP, colour = SentimentQDAPCategory, 
            data = sentiment, xlab = "Tweets")
grid.arrange(gi, he, lm, qdap, nrow = 2, 
             top = textGrob("Sentiments of Tweets per Dictionary"))

# Plot tweets over average sentiment.
sentiment_cols <- c("SentimentGI", "SentimentHE", "SentimentLM", "SentimentQDAP")
avg_sentiment <- select(sentiment, sentiment_cols)
avg_sentiment <- mutate(avg_sentiment, mean_sentiment = rowMeans(avg_sentiment[,-5]))
sentiment$MeanSentiment <- avg_sentiment$mean_sentiment
sentiment$MeanSentimentCategory <- convertToDirection(sentiment$MeanSentiment)

ggplot(data = sentiment, 
       mapping = aes(x = c(1:nrow(sentiment)), y = MeanSentiment, 
                     colour = MeanSentimentCategory)) + geom_point() +
  labs(x = "Tweets") + ggtitle("Average Sentiment of Tweets") 

# Plot counts of each sentiment category.
gi_count <- qplot(x = SentimentGICategory, fill = SentimentGICategory, 
                  data = sentiment, ylab = "Number of Tweets") + 
  geom_bar(show.legend = FALSE) + theme(legend.position = "none")
he_count <- qplot(x = SentimentHECategory, fill = SentimentHECategory, 
                  data = sentiment, ylab = "Number of Tweets") + 
  geom_bar(show.legend = FALSE) + theme(legend.position = "none")
lm_count <- qplot(x = SentimentLMCategory, fill = SentimentLMCategory, 
                  data = sentiment, ylab = "Number of Tweets") + 
  geom_bar(show.legend = FALSE) + theme(legend.position = "none")
qdap_count <- qplot(x = SentimentQDAPCategory, fill = SentimentQDAPCategory, 
                  data = sentiment, ylab = "Number of Tweets") + 
  geom_bar(show.legend = FALSE) + theme(legend.position = "none")
grid.arrange(gi_count, he_count, lm_count, qdap_count, nrow = 2,
             top = textGrob("Count of Tweets per Sentiment Category by Dictionary"))


# Plot counts of tweets per average sentiment category.
ggplot(data = sentiment, mapping = aes(x = MeanSentimentCategory, 
                                       fill = MeanSentimentCategory)) +
  geom_bar(show.legend = FALSE) + ylab("Number of Tweets") + 
  ggtitle("Count of Tweets per Average Sentiment Category")


# Plot number of tweets per WordCount along with average sentiment category.
ggplot(data = sentiment, mapping = aes(x = WordCount, fill = MeanSentimentCategory)) +
  geom_bar() + ggtitle("Number of Tweets per WordCount")

# Plots average sentiment of tweets over time. 
ggplot(mapping = aes(x = tweets$created_at[complete.cases(s)], 
                     y = sentiment$MeanSentiment,
                     colour = sentiment$MeanSentimentCategory)) + 
  geom_point() + labs(x = "Time") + 
  labs(y = "Avg Sentiment") + labs(colour = "Sentiment Cateogry")

# Plot tweet counts grouped by time (hour and minute).
# Note: all tweets are from same day so we will only look at hour and minute.
tweets$Time <- NULL
tweets$Time <- format(tweets$created_at,"%H:%M")
ggplot(mapping = aes(x = tweets$Time[complete.cases(s)], 
                     fill = sentiment$MeanSentimentCategory)) + geom_bar() +
  ggtitle("Tweet Counts per Minute") + 
  labs(x = "Time (UTC)") + labs(fill = "Sentiment Category")