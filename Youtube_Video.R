# For more details on this exercise, see https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html. 
# For more information on the tuber package, see https://cran.r-project.org/web/packages/tuber/tuber.pdf 

# Install tuber package
install.packages('tuber')

# Load tuber package
library(tuber)

# Set working directory. 
setwd("C:/Users/Desktop/")

# Authenticate using your YouTube client ID and client secret key
## Then authenticate in browser
client_id="" # Enter within double quotes your own client ID
client_secret=""   # Enter within double quotes your own client secret key
yt_oauth(client_id, client_secret)

###############################################################################################################
# Collect data on a Youtube video
###############################################################################################################

# Get statistics of a video with ID f3FnCoKWmBg
get_stats(video_id="f3FnCoKWmBg")
# Views the results
View(get_stats(video_id="f3FnCoKWmBg"))

# Get details about the video
View(get_video_details(video_id="f3FnCoKWmBg"))

# Search Videos and save results in data frame called search_result
search_result <- yt_search(term = "Social Media Analytics", get_all=TRUE, max_results = 100)
# Views the results
View(search_result)

# Get comments on video and saves them to data frame called comments
comments <- get_all_comments(c(video_id="f3FnCoKWmBg"))
# View first few rows of comments
View(comments)

# Save comments to a csv file in working directory
write.csv(comments, file='YouTubeVideoComments.csv')

###########################################################################################################
# Create word cloud of the comments
###########################################################################################################

# Install tm, SnowballC and wordcloud packages for text preprocessing and word clods
install.packages('tm')
install.packages('SnowballC')
install.packages('wordcloud')

# Load the libraries into current session
library(tm)
library(SnowballC)
library(wordcloud)

# Create comments corpus
comments_corp=Corpus(VectorSource(comments$textOriginal))

# Text processing and create document-term matrix
comments_DTM=DocumentTermMatrix(comments_corp,control=list(removePunctuation=T,removeNumbers=T,stopwords=T))

# Displays first five terms in DTM
as.matrix(comments_DTM[,1:5])

# Create matrix of terms and frequency
comments_terms=colSums(as.matrix(comments_DTM))
comments_terms_matrix=as.matrix(comments_terms)
comments_terms_matrix

# Create word cloud
wordcloud(words=names(comments_terms), freq=comments_terms, vfont=c('serif', 'bold italic'), colors=1:nrow(comments_terms_matrix))


################################################################################################
# Use get_nrc_sentiment function to obtain the sentiment of video comments
################################################################################################
install.packages('syuzhet') # Install syuzhet package for sentiment analysis
library(syuzhet)

# Get raw sentiment scores for each comment
video_sentiment=get_nrc_sentiment(as.character(comments$textOriginal))
View(video_sentiment)

# Obtain transpose of data frame
video_sentimentDF=t(data.frame(video_sentiment)) 
View(video_sentimentDF)

# Calculate number of comments with each emotion >0
VideoCommentsEmotionsDFCount=data.frame(rownames(video_sentimentDF), rowSums(video_sentimentDF > 0))
View(VideoCommentsEmotionsDFCount)
rownames(VideoCommentsEmotionsDFCount)=NULL # Set row names to NULL
colnames(VideoCommentsEmotionsDFCount)=c('Emotion','Frequency') # Set column names to 'Emotion' and 'Frequency'
View(VideoCommentsEmotionsDFCount)

# Barplot of YouTube video comment sentiment
barplot(VideoCommentsEmotionsDFCount$Frequency,  names.arg = VideoCommentsEmotionsDFCount$Emotion, main="YouTube Video Comments Sentiment", xlab="Emotions", ylab="Frequency")


# Obtain a single sentiment score for each comment 
# Positive values indicate positive sentiment and negative values indicate negative sentiment
VideoCommentPolarity=data.frame(as.character(comments$textOriginal),get_sentiment(as.character(comments$textOriginal)))
colnames(VideoCommentPolarity)=c('Comments','Polarity') # Set column name to "Polarity"
View(VideoCommentPolarity)

