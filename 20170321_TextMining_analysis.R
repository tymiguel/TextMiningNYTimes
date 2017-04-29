# Text mining and topic analysis of New York Times articles

# Make sure you run the commands in the 
# file 20151104_TextMining_functions.R
# before you run these commands. 

# Load the required libraries
library(cluster)
library(RCurl)
library(RJSONIO)
library(rlist)
library(stringr)
library(dplyr)
library(magrittr)
library(RTextTools)
library(ngram)

source(file='~/ /20170321_TextMining_fuctions.R')

# Set this so we can see all columns 
# when printing dataframes
options(dplyr.width=Inf)

# Replace the following key with your own, 
# which you can obtain from the New York Times
# developer site http://developer.nytimes.com
articlesearch.key = "d19f24bdfbd940c69deaf0922e521eef"

# The code that you will modify to
# create your clusters starts here.

# OPTIONS: query, begin and end dates
# Create a dataframe of articles from the New York Times
get.nyt.hits(query.string="dance",     # OPTION
             begin.date="20170101",    # OPTION
             end.date  ="20170107")    # OPTION
# each page carries 10 articles

article.df = get.nyt.articles(pages = -1,                 # all articles, can also change to 1:3
                              query.string = "dance",     # OPTION
                              begin.date   = "20170101",  # OPTION
                              end.date     = "20170107")  # OPTION
dim(article.df)

# Check number of articles returned
num.articles = nrow(article.df)
num.articles

# Check a random sample of 5 articles 
doc.ndx = sample(1:num.articles,5)
article.df[doc.ndx,]

# OPTION: headline, snippet, lead_paragraph or abstract
# Create `docs` (the document vector) by
# choosing the text field that you will analyze.
docs = article.df$snippet 

# Check a few of the documents
docs[doc.ndx]
# These same documents will be checked below
# after other modifications to the documents

# Remove punctuation and numbers.
# OPTION: you may find it useful to
# change the cleaning procedure and
# modify the function `clean.documents`.
docs.clean = clean.documents(docs)

save(docs.clean, 
     file="~/Desktop/Bentley/Data Mining (MA710)/Assignment 3/docs.clean.RData")
load(file="~/Desktop/Bentley/Data Mining (MA710)/Assignment 3/docs.clean.RData")

# Check the cleaned documents
docs.clean[doc.ndx]

# OPTIONS: see code below
# Modify the words in the documents 
# with stemming, n-grams and stopwords
docs.sns = 
  modify.words(
    docs.clean,  
    stem.words=FALSE,  # OPTION: TRUE or FALSE
    ngram.vector=1:2, # OPTION: n-gram lengths
    stop.words=       # OPTION: stop words
      c(stopwords(kind="english")  
        # OPTION: "SMART" or "english" 
        # OPTION: additional stop words
      )
  )

# Be careful: some stop words from the 
# stopwords function might be important 
# For example, "new"
# "new" %in% stop.words # "new york", "new england" and "new hampshire" 

# Check documents
docs.sns[doc.ndx]

# OPTION: weighting, see below
# Create the document matrix
doc.matrix <- 
  create_matrix(docs.sns, # "sns" stem ngrams stop
                language="english",      # Do not change / we want english
                stemWords=FALSE,         # Do not change / we already did this
                removePunctuation=FALSE, # Do not change / we already did this
                weighting=tm::weightTf   # OPTION: weighting (see below)
  )
# Weighting OPTIONS:
# tm::weightTfIdf - term frequency-inverse document frequency
# tm::weightTf    - term frequency
# To use binary weighting use tm::weightTf and 
# create a "binary matrix" below.
                  
# Check the document matrix
doc.matrix

# OPTIONS: none, but this command must be run
# Create the document-term matrix
dtm = as.matrix(doc.matrix) 

# Check the matrix
dtm[1:10,1:10]
dim(dtm) 

# Check the number of words in 
# the document term matrix
colnames(dtm)
ncol(dtm)

# Check the distribution of document-word frequencies 
table(dtm) # counts the 0s and the 1s

# OPTION: create a binary matrix
# in order to use binary weighting.
# DO NOT run this code if you 
# DO NOT want to use binary weighting.
# Only use with parameter
#     weighting=tm::weightTf 
# All positive frequencies become 1,
# indicating only the presence of a word  
# in a  document. 
# Uncomment the following line to use
# this code if you decide to use it. 
# dtm[dtm>1]=1 # turn each cell that is greater than 1, to a 1

# This may not make much of a difference 
# as nearly all document-word frequencies 
# are equal to 1. Most duplicate words are
# stopwords, and those have been removed. 

# Check the distribution of document-word frequencies 
# if you created a binary document-word matrix above
table(dtm)

# Check the distribution of word frequencies 
table(colSums(dtm)) #1200 words occured in 1 document, #97 occured in 2 documents 
# This gives the distribution of word frequencies 
# for the entire collection of articles

# OPTION: frequency threshold
# Keep words from the document term matrix
# that occur at least the number of times
# indicated by the `freq.threshold` parameter 
dtm=reduce.dtm(dtm,freq.threshold=2) # only keep the columns where the column sum is greater 
# the more dimensions, the max distance is just larger between two data points 
# this is important to remember when doing cluster analysis

# Check the number of columns/words 
# remaining in the document-term matrix
ncol(dtm)

# OPTION: number of clusters to find
k = 3

# OPTION: cluster algorithm 
cluster = kmeans(dtm,k)$cluster
# cluster = pam(dtm,k)$cluster
# hclust.res = hclust(dist(dtm))
# cluster = cutree(hclust.res,k)

# EVALUATE the clusters using `table` 
# to check the cluster sizes
as.data.frame(table(cluster)) # does a frequency of the clusters # our clusters suck


# EVALUATE the clusters using `check.cluster` 
# to look at the common words in each cluster
# The second parameter is the minimum number of
# rows that a cluster must have to be displayed.
options(warn=-1)
check.clusters(cluster,5) 
# gives you cluster, how many records, then the value of the word, dance got 0.09 / 9%
# we are averaging the columns, so dance shows up in 9$
# binary, we will see proportion
# if it's term frequeny, we can get more than 1
# you don't want to look at clusters that are two small
options(warn=0)

# EVALUATE the clusters using `TopWords` 
# This is the same information as supplied
# by the `check.cluster` function, except 
# that the output is displayed vertically
options(warn=-1)
1:k %>%
  lapply(function(i) TopWords(dtm, cluster, i))
options(warn=0)

# EVALUATE the clusters by looking 
# at the documents in the clusters
view.cluster(3)

# End
