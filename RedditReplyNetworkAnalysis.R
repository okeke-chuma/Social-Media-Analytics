# For more information on this exercise, see https://www.rdocumentation.org/packages/RedditExtractoR/versions/3.0.6
# For more information on the RedditExtractoR package, see https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf 


#####################################################################################################################
# Collect thread data based on keywords
#####################################################################################################################

# Install package
install.packages('RedditExtractoR')
library(RedditExtractoR)

# Search Reddit for threads that contain terms "MBA" posted within the last month, sorted by hot threads
reddit_threads <- find_thread_urls(keywords = "MBA", sort_by="top",  period="month")
View(reddit_threads)

# Collect all posts under the first ten threads (URLs)
analytics_reddit_posts <- get_thread_content(reddit_threads$url[1:10]) # Collect posts on first 10 threads

analytics_reddit_threads=as.data.frame(analytics_reddit_posts$threads) # Convert  thread initiating posts to data frame
View(analytics_reddit_threads)

analytics_reddit_comments=as.data.frame(analytics_reddit_posts$comments) # Convert thread comments to data frame
analytics_reddit_comments$Post=rownames(analytics_reddit_comments) # Add row number as post ID 
View(analytics_reddit_comments)

# Merge thread posts and comment posts by URL
analytics_reddit_allposts <- merge(analytics_reddit_comments, analytics_reddit_threads, by = 'url')
View(analytics_reddit_allposts)
colnames(analytics_reddit_allposts)[2]='Comment_Author' # change author.x column to "Comment_Author'
colnames(analytics_reddit_allposts)[12]='Thread_Author' # change author.y column to "Thread_Author'
View(analytics_reddit_allposts)

# Remove comments by AutoModerator
analytics_reddit_allposts=subset(analytics_reddit_allposts, analytics_reddit_allposts$Comment_Author!="AutoModerator")

#####################################################################################################################
# Generate Data Matrix for Reply Network on Reddit
#####################################################################################################################

install.packages('stringi')
library(stringi)

# Create an empty data frame to hold the network data later on
RedditReplyNetwork=data.frame()

# Identify the position of the last underscore ('_') in the comment_id of each post. 
# If doesn't exist, result will be NA
UnderscorePosition=data.frame(stri_locate_last(analytics_reddit_allposts$comment_id, fixed = '_'))

# The for loop below generates a data frame with each row representing two users with a reply relationship
# That is, the second user replied to a post by the first user
for (i in 1:nrow(UnderscorePosition)) {  # Loop through the UnderscorePosition vector
  if (is.na(UnderscorePosition[i,]$start)) { # If underscore is not found (i.e., NA)
    PostAuthor=analytics_reddit_allposts[i,]$Thread_Author
    ReplyAuthor=analytics_reddit_allposts[i,]$Comment_Author
    RedditReplyNetwork=rbind(RedditReplyNetwork, c(ReplyAuthor, PostAuthor)) # Add one row with thread author as post author and comment author as reply author
  }
  else {
    
    # Extract the Comment ID of the replied to post before the last underscore
    ReplyToCommentID=substr(analytics_reddit_allposts[i,]$comment_id, 1, UnderscorePosition[i,]$start-1)
    # Extract the URL of the thread
    ThreadURL=analytics_reddit_allposts[i,]$url
    
    # Find the row where the replied to post is located
    ReplyToRow=which((analytics_reddit_allposts$comment_id == ReplyToCommentID) & (analytics_reddit_allposts$url==ThreadURL))
    
    # Extract the authors of the post and its reply
    PostAuthor=analytics_reddit_allposts[ReplyToRow,]$Comment_Author
    ReplyAuthor=analytics_reddit_allposts[i,]$Comment_Author
    
    # Add the pair of authors to data frame of ties/edges for network
    RedditReplyNetwork=rbind(RedditReplyNetwork, c(ReplyAuthor, PostAuthor))
  }
}

# Set column names
colnames(RedditReplyNetwork)=c('ReplyAuthor', 'PostAuthor')

# Delete rows where one of the author names has been deleted 
RedditReplyMatrix=as.matrix(RedditReplyNetwork[RedditReplyNetwork$ReplyAuthor != '[deleted]' & RedditReplyNetwork$PostAuthor != '[deleted]',]) 

View(RedditReplyMatrix)
 
######################################################################################################################
# PLOTTING THE REDDIT REPLY NETWORK
######################################################################################################################

install.packages('igraph')
library(igraph)

# Create reply network graph with each user pair as an edge of the graph
reply_graph = graph_from_edgelist(RedditReplyMatrix, directed = TRUE)

# remove self ties (i.e., people who replied to themselves)
reply_graph = simplify(reply_graph)

# Get the labels' name attribute of the vertices/nodes as the vertex labels
ver_labs = vertex_attr(reply_graph, "name", index=V(reply_graph))

# Disply first 10 users
head(ver_labs, 4)

# Sets the layout of the graph to the Fruchterman & Reingold layout
glay = layout_with_fr(reply_graph)

# Set graph background to white and margins of graph
par(bg="white", mar=c(1,1,1,1))  

# Plot the graph
plot(reply_graph, layout=glay,
     vertex.color='white', # Set vertex/node fill color to white
     vertex.size=1, # Vertex size
     frame.color='black', # Vertex border color
     vertex.label=ver_labs, # Vertex label
     vertex.label.family="sans", # Vertex label font
     vertex.shape="sphere", # Vertex shape
     vertex.label.color='blue', # Vertex label color
     vertex.label.cex=0.1, # Vertex font size
     edge.arrow.size=0.1, # Edge arrow size
     edge.arrow.width=0.1, # Edge arrow width
     edge.width=2, # Edge width
     edge.color=hsv(h=.40, s=1, v=.2, alpha=0.2)) # Edge color

# add title, font size and color
title("\nReddit Reply Network of Analytics Threads:  Who Replies to Whom",
      cex.main=1, col.main="black") 


# Collating metrics for further analysis
RNMetrics=data.frame(cbind(degree(reply_graph, mode=("in"))))   # in-degree
RNMetrics=cbind(RNMetrics, data.frame(cbind(degree(reply_graph, mode=("out")))))   # out-degree
RNMetrics=cbind(RNMetrics, data.frame(cbind(betweenness(reply_graph))))  # betweenness
RNMetrics=cbind(RNMetrics, data.frame(cbind(closeness(reply_graph))))    # closeness
RNMetrics=cbind(RNMetrics, data.frame(cbind(eigen_centrality(reply_graph)$vector)))  # eigenvector
colnames(RNMetrics)=c('In-Degree', 'Out-Degree','Betweenness', 'Closeness', 'Eigenvector')  # add column headings

# View results
View(RNMetrics)

# Save results to a csv file
write.csv(RNMetrics, file='RNMetrics.csv')


# Plot graph with vertex size proportional to Eigenvector centrality
plot(reply_graph, layout=glay,
     vertex.color='white', # Set vertex/node fill color to white
     vertex.size=eigen_centrality(reply_graph)$vector*5^1.5, # Vertex size proportional to Eigenvector centrality
     frame.color='black', # Vertex border color
     vertex.label=ver_labs, # Vertex label
     vertex.label.family="sans", # Vertex label font
     vertex.shape="sphere", # Vertex shape
     vertex.label.color='blue', # Vertex label color
     vertex.label.cex=0.5, # Vertex font size
     edge.arrow.size=0.2, # Edge arrow size
     edge.arrow.width=0.4, # Edge arrow width
     edge.width=2, # Edge width
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5)) # Edge color

title("\nReddit Reply Network of Analytics Threads:  Who Replies to Whom Vertex size proportional to Eigenvector centrality",
      cex.main=1, col.main="black") 

# Identify communities
RN_community=cluster_walktrap(reply_graph)

# Plot identified communities in network
plot(RN_community, reply_graph, vertex.size=3, vertex.label.cex=0.5, 
     vertex.label=NA, edge.arrow.size=0.2, edge.curved=TRUE, layout=layout.fruchterman.reingold)
title("\nReddit Reply Network of Analytics Threads:  Who Replies to Whom",
      cex.main=1, col.main="black") 
