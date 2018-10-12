# This program is run after visualize_summary,  In order for this program to work "surveyresults.pdf" must be in the same folder.
# It uses a heirarchical clustering algorithm to make visualizations that quickly summarize the residents' suggestions.

# Install packages
# install.packages("pdftools")
# install.packages("dplyr")
# install.packages("purrr")
# install.packages("tm")


# Visualize residents' suggestions
library(pdftools)
library(dplyr)
library(purrr)
library(tm)

txt <- pdf_text("surveyresults.pdf") # entire text of document


library(dplyr)
library(purrr)
library(tm)

# The verbatim suggestions are on pages 57:63
suggestions <- txt[57:63]


# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(suggestions))
# Lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# Remove stopwords
myCorpus<- tm_map(myCorpus, removeWords, stopwords("english")) 
#remove additional stop words
myCorpus <- tm_map(myCorpus, removeWords, c("question", "total", "table", "number", "percent", "survey", "can", "like", "results", "page", "report", "n"))
# strip whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)

# Build TDM - experiment with bigrams
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

# Remove sparse words
tdm2 <- removeSparseTerms(tdm, sparse=0.77)
dim(tdm2)

# Heirarchical cluster analysis for word structure
set.seed(1234)
d <- dist(as.matrix(tdm2))
hc <- hclust(d)
plot(hc, main="What are residents suggesting?")
library("ape")
plot(as.phylo(hc), type = "radial")

# Cut the dendrogram into 4 clusters - http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning

clus4 = cutree(hc, 10)
plot(as.phylo(hc), type = "fan",
     label.offset = 1, cex = 0.7)

# Change the appearance
# change edge and label (tip)
plot(as.phylo(hc), type = "cladogram", cex = 0.6,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2)

plot(as.phylo(hc), cex = 0.6, label.offset = 0.5)
