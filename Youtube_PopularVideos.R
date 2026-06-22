# For more details on this exercise, see https://cran.r-project.org/web/packages/tuber/vignettes/tuber-ex.html and https://www.r-bloggers.com/using-the-tuber-package-to-analyse-a-youtube-channel/. 
# For more information on the tuber package, see https://cran.r-project.org/web/packages/tuber/tuber.pdf 

###############################################################################################################
# Some YouTube AIP functions may be subject to data quota over a period of time. 
# If you reach an unexpected error, wait some time and try again.
###############################################################################################################

# Install tuber package
install.packages('tuber')

# Load tuber package
library(tuber)

install.packages('curl')
library(curl)

install.packages('plyr')
library(plyr)

# dplyr package for converting character to number
install.packages('dplyr')
library(dplyr)

# Set working directory
setwd("C:/Users/Desktop/")

# Authenticate using your YouTube client ID and client secret key
## Then authenticate in browser
client_id=" " # Enter within double quotes your own client ID
client_secret=" "   # Enter within double quotes your own client secret key
yt_oauth(client_id, client_secret)

###############################################################################################################
# Collect stats on most popular videos
# If you keep getting an error message "Error: HTTP failure: 401", delete your .httr-oauth in the folder where
# R script is located, use the yt_oauth function above to authenticate again, and then try the code again
###############################################################################################################

# Collect top 50 most videos in the US
# Additional region code can be found at https://www.iso.org/obp/ui/#search
popularVideos<-list_videos(max_results=50, region_code='US')

# Create blank data frame to hold popular video results
popularVideosDF=data.frame()

# Use loop to collect data on all popular videos
for (i in 1:length(popularVideos$items)) { 
  # Obtain video IDs
  popularVideoID <- data.frame(popularVideos$items[[i]]$id) # Gets ID of each popular video
  popularVideosDF<-rbind(popularVideosDF, popularVideoID) # Binds ID of popularVideoID to the result data frame as a new row
} 

# Set column name to 'ID'
colnames(popularVideosDF)=c('ID')

# Video results
View(popularVideosDF)

# Function to scrape stats for all videos
get_all_stats <- function(id) {
  get_stats(id)
} 

# Get stats and convert results to data frame 
video_stats <- lapply(as.vector(popularVideosDF$ID), get_all_stats)
video_stats_df <- rbind(ldply(video_stats, data.frame))

# Removes "statistics_" from column names
names(video_stats_df) <- sub("^statistics_", "", names(video_stats_df))

# View results
View(video_stats_df)

# Convert text in the count columns to number
video_stats_df <- video_stats_df %>% mutate_at(c('viewCount','likeCount','favoriteCount','commentCount'), as.numeric)

# View results
View(video_stats_df)

# Save video stats to a csv file in working directory
write.csv(video_stats_df, file='YouTubeVideoStats.csv')

# Package for drawing the plots
install.packages('ggplot2')
library(ggplot2)

# Package for arranging the plots in grids
install.packages('gridExtra')
library(gridExtra)

# Create video stat plots displaying various counts (like, comments) against view count
# "[,-1]" removes first column "()"id" from the data frame
# geom_point creates a scatterplot
p1 = ggplot(data = video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = likeCount))
p2 = ggplot(data = video_stats_df[, -1]) + geom_point(aes(x = viewCount, y = commentCount))
grid.arrange(p1, p2, ncol = 1)

