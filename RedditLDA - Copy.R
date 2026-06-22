# For more information on this exercise, see https://www.rdocumentation.org/packages/RedditExtractoR/versions/3.0.6
# For more information on the RedditExtractoR package, see https://cran.r-project.org/web/packages/RedditExtractoR/RedditExtractoR.pdf 


#####################################################################################################################
# Collect thread data based on keywords
#####################################################################################################################

# Install package
install.packages('RedditExtractoR')
library(RedditExtractoR)

# Search Reddit for threads that contain terms "analytics" posted within the last month, sorted by hot threads
reddit_threads <- find_thread_urls(keywords = "business analytics", sort_by="top",  period="month")
View(reddit_threads)

# Collect thread URLs and texts
analytics_reddit_threads <- data.frame(cbind(reddit_threads$url, paste(reddit_threads$title, ' ', reddit_threads$text)))

colnames(analytics_reddit_threads)=c('URL','Text')
View(analytics_reddit_threads)


######################################################################################################################
# TOPIC MODELING VIA LATENT DIRICHLET ALLOCATION
######################################################################################################################

install.packages('tm')
library(tm)

install.packages('topicmodels')
library(topicmodels)

RedditCorp=Corpus(VectorSource(analytics_reddit_threads$Text))

# Text preprocessing
RedditCorp=tm_map(RedditCorp,content_transformer(tolower))

#other pre-processing tasks
RedditCorp=tm_map(RedditCorp, removeWords, stopwords('english')) # Remove stop words
RedditCorp=tm_map(RedditCorp, removePunctuation) # Remove punctuation marks 
RedditCorp=tm_map(RedditCorp, removeNumbers) # Remove numbers
RedditCorp=tm_map(RedditCorp, stripWhitespace) # Remove whitespace


# Calculate the DTM
RedditDTM=as.matrix(DocumentTermMatrix(RedditCorp))

# Find indices of rows with all zeros
row_with_all_zeros = as.integer(which(rowSums(RedditDTM) == 0))

# Remove rows with all zeros from Reddit threads
if (length(row_with_all_zeros)>0) {
analytics_reddit_threads=analytics_reddit_threads[-row_with_all_zeros,]

# Remove rows with all zeros in the DTM
RedditDTM <- RedditDTM [which(rowSums(RedditDTM) > 0), ]
}


View(RedditDTM)

# run LDA; Gibbs alternative is VEM; k is the number of topics we think we have
RedditTopics=LDA(RedditDTM, method='Gibbs', k=7, control=list(seed = 77))
terms(RedditTopics,10)
#output the terms and their betas - densities within topics - for each topic
RedditTerms=data.frame(row.names(t(as.matrix(RedditDTM))),t(as.matrix(RedditTopics@beta))) 
colnames(RedditTerms)=c('Term', 'Topic1', 'Topic2', 'Topic3', 'Topic4', 'Topic5', 'Topic6', 'Topic7')

#re-order data frame to highlight top 10 terms for 1st topic
RedditTerms=RedditTerms[order(RedditTerms$Topic1, decreasing = 'T'),]
RedditTerms[1:20,] # reports natural log of probabilities, hence negative

topics(RedditTopics)				#determine which topic dominates each document

#add the topics to original RedditPosts data frame
RedditPosts=data.frame(analytics_reddit_threads,Topic=topics(RedditTopics))
View(RedditPosts)

#add the density of each topic within each document
#to the data frame
RedditPosts=data.frame(RedditPosts,RedditTopics@gamma) # gamma is the document densities over different topics

View(RedditPosts)
