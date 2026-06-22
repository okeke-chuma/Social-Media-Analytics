
#####################################################################################################################
#Reddit R file#
#####################################################################################################################
# For more information on this exercise, see https://www.rdocumentation.org/packages/RedditExtractoR/versions/3.0.6
# For more information on the RedditExtractoR package, see https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf 


#####################################################################################################################
# Collect thread data based on keywords
#####################################################################################################################

# Install package
install.packages('RedditExtractoR')
library(RedditExtractoR)

# Search Reddit for threads that contain terms "MBA" posted within the last month, sorted by hot threads
reddit_threads <- find_thread_urls(keywords = "analytics", sort_by="top",  period="month")
View(reddit_threads)

# Collect all posts under the first ten threads (URLs)
analytics_reddit_posts <- get_thread_content(reddit_threads$url[1:10]) # Collect comments on first 10 threads
analytics_reddit_comments=as.data.frame(analytics_reddit_posts$comments) # Convert to data frame
analytics_reddit_comments$Post=rownames(analytics_reddit_comments) # Add row number as post ID 
View(analytics_reddit_comments)

# Remove comments by AutoModerator
analytics_reddit_comments=subset(analytics_reddit_comments, analytics_reddit_comments$author!="AutoModerator")

# Remove comments that have been deleted
analytics_reddit_comments=subset(analytics_reddit_comments, analytics_reddit_comments$comment!="[deleted]")
View(analytics_reddit_comments)

#####################################################################################################################
# Combine thread comments and initiating posts
#####################################################################################################################

# Uses Reddit data collected earlier and keep only post ID and post text
Reddit=data.frame(analytics_reddit_comments$Post, analytics_reddit_comments$comment)
colnames(Reddit)=c('Post', 'Comment') # rename columns
View(Reddit)

# Add the ten thread initiating posts
for (i in 1:10) {
  
  Reddit=rbind(Reddit, c(as.numeric(Reddit$Post[nrow(Reddit)])+1, paste(reddit_threads$title[i], ' ', reddit_threads$text[i]))) # Add a post ID and post text
  
}

View(Reddit)


#####################################################################################################################
# Named entity extraction
#####################################################################################################################

install.packages('rJava')
install.packages('NLP')
install.packages('openNLP')

# Get timeout option
getOption('timeout')

# Set timeout option
options(timeout=300)

install.packages('openNLPmodels.en', repos='http://datacube.wu.ac.at/', type='source')

install.packages("stringi")

library(stringi)
library(NLP)
library(openNLP)
library(openNLPmodels.en)


# Set up annotators for person, organization, location, date, money and percentage
person_annotator=Maxent_Entity_Annotator(kind='person')
organization_annotator=Maxent_Entity_Annotator(kind='organization')
location_annotator=Maxent_Entity_Annotator(kind='location')
date_annotator=Maxent_Entity_Annotator(kind='date')
money_annotator=Maxent_Entity_Annotator(kind='money')
percentage_annotator=Maxent_Entity_Annotator(kind='percentage')

# Create empty data frame to hold extracted entities
RedditEntities=data.frame(Post=numeric(), Type=character(), Entity=character(), Position=numeric(), stringsAsFactors=FALSE)

#repeat for each row in dataframe
#ensure post is string
#tokenize post
#annotate  tokens
#extract portion of post tagged as  entity and 
#append to RedditEntities dataframe
for (post in 1:nrow(Reddit))  # repeat for each row in dataframe
{
  RedditText=as.String(Reddit[post,2]) # retrieve text
  RedditText=stri_trim_both(RedditText) # Remove leading and trailing white spaces
  if (RedditText=="") {}  # If RedditText is not empty then perform the functions in else 
  else  {
    RedditTokens=annotate(RedditText, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator())) # set up annotator
    RedditPersTokens=annotate(RedditText, list(person_annotator), RedditTokens) # set up annotator for persons
    RedditOrgsTokens=annotate(RedditText, list(organization_annotator), RedditTokens) # set up annotator for organizations
    RedditLocsTokens=annotate(RedditText, list(location_annotator), RedditTokens) # set up annotator for locations
    RedditDatsTokens=annotate(RedditText, list(date_annotator), RedditTokens) # set up annotator for dates
    RedditMonsTokens=annotate(RedditText, list(money_annotator), RedditTokens) # set up annotator for monies
    RedditPctsTokens=annotate(RedditText, list(percentage_annotator), RedditTokens) # set up annotator for percentages
    
    RedditPerson=subset(RedditPersTokens,RedditPersTokens$features=='list(kind = "person")') # extract persons
    RedditOrganization=subset(RedditOrgsTokens,RedditOrgsTokens$features=='list(kind = "organization")') # extract organizations
    RedditLocation=subset(RedditLocsTokens,RedditLocsTokens$features=='list(kind = "location")') # extract locations
    RedditDate=subset(RedditDatsTokens,RedditDatsTokens$features=='list(kind = "date")') # extract dates
    RedditMoney=subset(RedditMonsTokens,RedditMonsTokens$features=='list(kind = "money")') # extract monies
    RedditPercentage=subset(RedditPctsTokens,RedditPctsTokens$features=='list(kind = "percentage")') # extract percentages
    
    # Add extracted persons to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditPerson))) # repeat for each row in the persons list
    {
      if (nrow(as.data.frame(RedditPerson))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Person', substr(paste(RedditText, collapse=' '),
                                                                          RedditPerson$start[i],RedditPerson$end[i]),RedditPerson$start[i])) # add post ID, 'Person', name of person extracted, and start position in text into dataframe
      }
    }
    
    # Add extracted organizations to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditOrganization)))
    {
      if (nrow(as.data.frame(RedditOrganization))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Organization', substr(paste(RedditText, collapse=' '),
                                                                                RedditOrganization$start[i],RedditOrganization$end[i]),RedditOrganization$start[i]))
      }
    }
    
    # Add extracted locations to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditLocation)))
    {
      if (nrow(as.data.frame(RedditLocation))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Location', substr(paste(RedditText, collapse=' '),
                                                                            RedditLocation$start[i],RedditLocation$end[i]),RedditLocation$start[i]))
      }
    }
    
    # Add extracted dates to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditDate)))
    {
      if (nrow(as.data.frame(RedditDate))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Date', substr(paste(RedditText, collapse=' '),
                                                                        RedditDate$start[i],RedditDate$end[i]),RedditDate$start[i]))
      }
    }
    
    # Add extracted monies to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditMoney)))
    {
      if (nrow(as.data.frame(RedditMoney))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Money', substr(paste(RedditText, collapse=' '),
                                                                         RedditMoney$start[i],RedditMoney$end[i]),RedditMoney$start[i]))
      }
    }
    
    # Add extracted percentages to dataframe containing extracted entities
    for (i in 1:nrow(as.data.frame(RedditPercentage)))
    {
      if (nrow(as.data.frame(RedditPercentage))>0) {
        RedditEntities=rbind(RedditEntities, cbind(post, 'Percentage', substr(paste(RedditText, collapse=' '),
                                                                              RedditPercentage$start[i],RedditPercentage$end[i]),RedditPercentage$start[i]))
      }
    }
  }
}

#rename columns
colnames(RedditEntities)=c('Post', 'Type', 'Entity', 'Position')

View(RedditEntities)

#merge entity tags with posts
RedditExtratedEntities=merge(RedditEntities, Reddit, by.x='Post', by.y='Post')

View(RedditExtratedEntities)

 

#####################################################################################################################
#RedditReplyNetworkAnalysis R file
#####################################################################################################################
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

