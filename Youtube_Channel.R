# For more details on this exercise, see https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html and https://www.r-bloggers.com/using-the-tuber-package-to-analyse-a-youtube-channel/. 
# For more information on the tuber package, see https://cran.r-project.org/web/packages/tuber/tuber.pdf 

# Install tuber package
install.packages('tuber')

# Load tuber package
library(tuber)

# Set working directory
setwd("C:/Users//Desktop/")

# Authenticate using your YouTube client ID and client secret key
## Then authenticate in browser
client_id="" # Enter within double quotes your own client ID
client_secret=""   # Enter within double quotes your own client secret key
yt_oauth(client_id, client_secret)

###############################################################################################################
# Collect stats and video list on playlist of a YouTube channel
# If you keep getting an error message "Error: HTTP failure: 401", delete your .httr-oauth in the folder where
# R script is located, use the yt_oauth function above to authenticate again, and then try the code again
###############################################################################################################

# Package for drawing the plots
install.packages('ggplot2')
library(ggplot2)

# Package for arranging the plots in grids
install.packages('gridExtra')
library(gridExtra)

# dplyr package for converting character to number
install.packages('dplyr')
library(dplyr)

# Get channel statistics
chstat = get_channel_stats("UCHPHVCq_Giziio_y8QEcHyA")

# Obtain  playlists of channel
playlists_ids <- get_playlists(filter=c(channel_id="UCHPHVCq_Giziio_y8QEcHyA"))

# Get ID of one playlist
playlistid <- playlists_ids$items[[7]]$id
playlist_videos<-get_playlist_items(filter= c(playlist_id=playlistid))
 
# Video ids
video_ids <- as.vector(playlist_videos$contentDetails.videoId)

# Function to scrape stats for all videos
get_all_stats <- function(id) {
  get_stats(id)
} 

# Get stats and convert results to data frame 
video_stats <- lapply(video_ids, get_all_stats)
video_stats_df <- do.call(rbind, lapply(video_stats, data.frame))

# Convert list to data frame
View(video_stats_df)

# Convert text in the count columns to number
video_stats_df <- video_stats_df %>% mutate_at(c('statistics_viewCount','statistics_likeCount','statistics_favoriteCount','statistics_commentCount'), as.numeric)

# View results
View(video_stats_df)

# Removes "statistics_" from column names
names(video_stats_df) <- sub("^statistics_", "", names(video_stats_df))

# View results
View(video_stats_df)

# Save video stats to a csv file in working directory
write.csv(video_stats_df, file='YouTubePlaylistVideoStats.csv')

# Create video stat plots displaying various counts (like, comments) against view count
# "[,-1]" removes first column "()"id" from the data frame
# geom_point creates a scatterplot
p1 = ggplot(data = video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, ncol = 1)

###########################################################################################################
# Collect comments on videos in playlist and plot word clouds of comments
###########################################################################################################

# Install tm, SnowballC and wordcloud packages for text preprocessing and word clods
install.packages('tm')
install.packages('SnowballC')
install.packages('wordcloud')

# Load the libraries into current session
library(tm)
library(SnowballC)
library(wordcloud)

# Create function get_video_comments that retrieves the comments based on an id
get_video_comments <- function(id){
  commentCount=get_stats(id)$statistics_commentCount # get comment count for video
  if (commentCount>0) { # collects comments if the comment count is greater than zero
    get_all_comments(c(video_id = id), max_results = 1000)
  }
}

# Call function get_video_comments on all ids in Video_ids to collect their comments
comments=lapply(as.character(video_ids), get_video_comments)

# View info on comments on first video as a data frame
View(data.frame(comments[1]))

# View info on comments on second video as a data frame
View(data.frame(comments[2]))

# Get comments text from comments data list
comments_text = lapply(comments,function(x){
  as.character(x$textOriginal)
})

# View comments text on first video as a data frame
View(data.frame(comments_text[1]))

# Use reduce function to merge all data lists on comments on different videos into one variable
text = Reduce(c, comments_text)  # text is now a data frame that has comments on all videos
View(text)

# Create text corpus using comment text
comments_corp=Corpus(VectorSource(text))

# Text processing and create document-term matrix
comments_DTM=DocumentTermMatrix(comments_corp,control=list(removePunctuation=T,removeNumbers=T,stopwords=T))

# Displays first five terms in DTM
as.matrix(comments_DTM[,1:5])

# Create matrix of terms and frequency
comments_terms=colSums(as.matrix(comments_DTM))
comments_terms_matrix=as.matrix(comments_terms)
comments_terms_matrix

# Create word cloud
wordcloud(words=names(comments_terms), freq=comments_terms, vfont=c('serif', 'bold italic'), colors=brewer.pal(8, 'Dark2'), random.order = FALSE)

