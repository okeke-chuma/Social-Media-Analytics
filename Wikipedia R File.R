
# Wikipedia data collection


############################################################################################
# Retrieve contents in tables on a Wikipedia page
############################################################################################


install.packages('httr')
library(httr)

install.packages('XML')
library(XML)

# Example 1

# set URL to collect data on
url1 <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

# Get contents from URL
tabs <- GET(url1)

# Extracts contents from tables in HTML file
tabs_content <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)
head(tabs_content) # View first few rows of data
class(tabs_content) # Check class of tabs

# Get data from the first component 
tablecontent1 <- tabs_content[1]
head(tablecontent1, 10)

# Convert result to a data frame
tablecontent1DF=data.frame(tablecontent1)
View(tablecontent1DF)

# Set first row as column names
colnames(tablecontent1DF) <- tablecontent1DF[1,]

# Remove first two rows of data
tablecontent1DF <- tablecontent1DF[-c(1:2), ] 
View(tablecontent1DF)

# Set row names to NULL
rownames(tablecontent1DF)=NULL

View(tablecontent1DF)

# Save video stats to a csv file in working directory
write.csv(tablecontent1DF_df, file='wikipediaPlaylistVideoStats.csv')

# Example 2

# set URL to collect data on
url2 <- "https://en.wikipedia.org/wiki/List_of_African_countries_by_area"
 

# Get contents from URL
tabs2 <- GET(url2)

# Extracts contents from tables in HTML file
tabs2_content <- readHTMLTable(rawToChar(tabs2$content), stringsAsFactors = F)
head(tabs2_content)

tabs2_content=tabs2_content[c(7:11)] # Keep Elements 7 to 11 for data in third to sevens tables on page

# Create data frame with data on first 1000 users
tabs2DF=data.frame(tabs2_content[1])

# Loop for multiple times based on number of elements in list tabs
for (i in 1:(length(tabs2_content)-1)) {
  
  # Each time converts result from one element, i.e., 1000 users, to a data frame and row bind to existing data frame
  tabs2DF=rbind(tabs2DF, (data.frame(tabs2_content[i+1])[-1,]))

}

View(tabs2DF)

# Set first row as column names
colnames(tabs2DF) <- tabs2DF[1,]

# Remove first row of data
tabs2DF <- tabs2DF[-1, ] 

# Set first row names as NULL
rownames(tabs2DF) <- NULL

View(tabs2DF)


############################################################################################
## Collect text data using rvest package
############################################################################################

install.packages("rvest")
library(rvest)

install.packages("tidyverse")
library(tidyverse)

# Collect HTML data
html_page <- read_html("https://en.wikipedia.org/wiki/University_of_Houston%E2%80%93Downtown")

html_page # Display HTML data

# Retrieve the texts of the p elements (paragraphs)
p_element <- html_page %>% html_elements("p") %>% html_text2()
p_element # Display the data

# Convert results from list to data frame
p_DF=data.frame(p_element)
View(p_DF)

# Remove first row of empty value
p_DF <- p_DF[-1, , drop=FALSE ]
View(p_DF)

# Save results to a csv file
write.csv(p_DF, file='p_DF.csv')

#Create word cloud from the wikipedia output file

#Install the required packages
install.packages("wordcloud")
install.packages("tm")
install.packages("SnowballC")
install.packages("RColorBrewer")

#Load the above installed packages
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)

# Read the CSV file
p_DF <- read.csv("p_DF.csv", stringsAsFactors = FALSE)

# Examine the data
head(p_DF)

# Create corpus from the p_element column
corpus <- Corpus(VectorSource(p_DF$p_element))

#Clean the Text
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%    # Convert to lowercase
  tm_map(removePunctuation) %>%               # Remove punctuation
  tm_map(removeNumbers) %>%                   # Remove numbers
  tm_map(removeWords, stopwords("english")) %>% # Remove common words
  tm_map(stripWhitespace) %>%                 # Remove extra spaces
  tm_map(stemDocument)                        # Optional: stem words

#Remove custom stopwords 
custom_stopwords <- c( "the", "and" )

corpus <- tm_map(corpus, removeWords, custom_stopwords)

#Create Term Frequency Matrix

tdm <- TermDocumentMatrix(corpus)

m <- as.matrix(tdm)

word_freq <- sort(rowSums(m), decreasing = TRUE)

word_freq_df <- data.frame(
  word = names(word_freq),
  freq = word_freq
)

head(word_freq_df, 20)

#Generate Word Cloud
set.seed(123)

wordcloud(
  words = word_freq_df$word,
  freq = word_freq_df$freq,
  min.freq = 3,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.25,
  colors = brewer.pal(10, "Dark2")
)

#Create sentiment analysis of the wikipedia output file

install.packages("tidytext")
install.packages("textdata")

library(tidyverse)
library(tidytext)
library(textdata)


#Tokenize the Text, Break the paragraphs into individual word
  
# Create a tidy text dataset
  tidy_text <- p_DF %>%
  unnest_tokens(word, p_element)

# Remove stop words
data("stop_words")

tidy_text <- tidy_text %>%
  anti_join(stop_words, by = "word")

#Positive vs Negative Sentiment (Bing Lexicon)
bing_sentiments <- get_sentiments("bing")

sentiment_results <- tidy_text %>%
  inner_join(bing_sentiments, by = "word")

# Count positive and negative words
sentiment_summary <- sentiment_results %>%
  count(sentiment)

sentiment_summary

#Visualize Positive vs Negative Words
sentiment_summary %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(
    title = "Sentiment Analysis of UHD Wikipedia Page",
    x = "Sentiment",
    y = "Word Count"
  ) +
  theme_minimal()

#or

sentiment_summary %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", percentage, "%)")
  ) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(
    aes(label = label),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Sentiment Analysis of UHD Wikipedia Page",
    x = "Sentiment",
    y = "Word Count"
  ) +
  theme_minimal()

#NRC Emotion Analysis
nrc <- get_sentiments("nrc")

emotion_results <- tidy_text %>%
  inner_join(nrc, by = "word")

emotion_summary <- emotion_results %>%
  count(sentiment, sort = TRUE)


#Visualize emotions
emotion_summary %>%
  ggplot(aes(x = reorder(sentiment, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "NRC Emotion Analysis",
    x = "Emotion",
    y = "Frequency"
  ) +
  theme_minimal()

#or 
emotion_summary %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    label = paste0(percentage, "%")
  ) %>%
  ggplot(aes(x = reorder(sentiment, n), y = n)) +
  geom_col() +
  geom_text(
    aes(label = label),
    hjust = -0.1,
    size = 4
  ) +
  coord_flip() +
  expand_limits(y = max(emotion_summary$n) * 1.15) +
  labs(
    title = "NRC Emotion Analysis",
    x = "Emotion",
    y = "Frequency"
  ) +
  theme_minimal()

#Calculate sentiment by graph
paragraph_sentiment <- p_DF %>%
  mutate(paragraph = row_number()) %>%
  unnest_tokens(word, p_element) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(paragraph, sentiment) %>%
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(sentiment_score = positive - negative)

head(paragraph_sentiment)
emotion_summary


#Bar Chart of 20 most frequent words
library(ggplot2)
library(dplyr)

top_words <- word_freq_df %>%
  slice_max(freq, n = 20)

ggplot(top_words,
       aes(x = reorder(word, freq),
           y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

#or Bar Chart of 20 most frequent words with percentage label
 
 top_words <- word_freq_df %>%
   slice_max(freq, n = 20) %>%
   mutate(
     percentage = round(freq / sum(freq) * 100, 1),
     label = paste0(freq, " (", percentage, "%)")
   )
 
 ggplot(top_words,
        aes(x = reorder(word, freq),
            y = freq)) +
   geom_col() +
   geom_text(
     aes(label = label),
     hjust = -0.1,
     size = 3.5
   ) +
   coord_flip() +
   expand_limits(y = max(top_words$freq) * 1.25) +
   labs(
     title = "Top 20 Most Frequent Words",
     x = "Word",
     y = "Frequency"
   ) +
   theme_minimal()
 
 
# Pie Chart
pie(
  sentiment_summary$n,
  labels = sentiment_summary$sentiment,
  main = "Sentiment Distribution"
)

#Pie Chart: Top 10 Most Frequent Words

library(dplyr)
library(ggplot2)

top10 <- word_freq_df %>%
  slice_max(freq, n = 10)

ggplot(top10,
       aes(x = "", y = freq, fill = word)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Top 10 Most Frequent Words"
  ) +
  theme_void()

#Top 10 with percent_rank
top10 <- word_freq_df %>%
  slice_max(freq, n = 10) %>%
  mutate(
    percent = round(freq / sum(freq) * 100, 1),
    label = paste0(word, " (", percent, "%)")
  )

ggplot(top10,
       aes(x = "", y = freq, fill = label)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Top 10 Most Frequent Words") +
  theme_void()
