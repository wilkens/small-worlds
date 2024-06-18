######## Semantic Distance Functions  ##########
######## Andrew Piper   ########################

#This script calculates "semantic distance" using Glove word embedding
#for all nonGPE sequences in a collection of books
#it calculates avg. semantic distance (the mean cosine distance between all sequential pairs)
#and total semantic distance (sum cosine distance between all sequential pairs)

#takes as input the derived data table

#read data
c<-read.csv(gzfile("conlit.csv.gz"))
#c<-read.csv(gzfile("early.csv.gz"))

#write data
write.csv(c, file="early.csv", row.names = F)
system("gzip early.csv") 

#remove words
remove_words <- function(text_vector, remove) {
  # Construct a single regex pattern that matches any word in the remove vector
  pattern <- paste0("\\b(", paste(remove, collapse="|"), ")\\b")
  
  # Replace matched words with empty string
  cleaned_text <- gsub(pattern, "", text_vector)
  
  # Remove leading and trailing whitespace
  return(trimws(cleaned_text))
}

#create function to extract all places in sequence
splitElements <- function(input_column) {
  # Remove the square brackets at the beginning and end of each string in the column
  cleaned_text <- gsub("^\\[|\\]$", "", input_column)
  
  # Split each string into a list of elements based on ', ' and remove single quotes
  elements_list <- lapply(strsplit(cleaned_text, "', '"), function(x) gsub("'", "", x))
  
  # Convert the list of elements into a data frame
  result_df <- data.frame(Element = unlist(elements_list))
  
  return(result_df)
}

#functions to compute distance of a route
distRoute <- function(adjmat, route) {
  #route<-route[route %in% colnames(adjmat)]
  d <- 0
  for(n in 2:nrow(adjmat)) {
    d <- d + adjmat[route[n-1],route[n]]
  }
  return(d)
}

distRoute2 <- function(adjmat, route) {
  route<-route[route %in% colnames(adjmat)]
  d <- 0
  for(n in 2:nrow(adjmat)) {
    d <- d + adjmat[route[n-1],route[n]]
  }
  return(d)
}

#function for getting pairwise distances for all words and phrases in a vector
library(data.table)
library(proxy)

# Function to get embeddings
# For each element of a vector this function:
#a. gets the embedding for a single word
#b. takes the average embedding for word phrases of all word vectors in the embedding
get_embedding <- function(token, glove_embeddings) {
  if (!grepl(' ', token)) {
    if (token %in% row.names(glove_embeddings)) {
      return(glove_embeddings[which(row.names(glove_embeddings) %in% token),])
    } else {
      return(NULL)
    }
  }
  
  words <- unlist(strsplit(token, ' '))
  valid_embeddings <- lapply(words, function(word) {
    if (word %in% row.names(glove_embeddings)) {
      return(glove_embeddings[which(row.names(glove_embeddings) %in% word),])
    } else {
      return(NULL)
    }
  })
  
  # Remove NULLs
  valid_embeddings <- Filter(Negate(is.null), valid_embeddings)
  if (length(valid_embeddings) == 0) {
    return(NULL)
  }
  
  avg_embedding <- colMeans(do.call(rbind, valid_embeddings))
  return(avg_embedding)
}

#Function to compute pairwise distance matrix
compute_distances <- function(word_list, glove_embeddings) {
  embeddings_list <- lapply(word_list, get_embedding, glove_embeddings=glove_embeddings)
  embeddings_list <- setNames(lapply(word_list, get_embedding, glove_embeddings=glove_embeddings), word_list)
  embeddings_list <- Filter(Negate(is.null), embeddings_list)
  
  # Convert list of embeddings to matrix
  embedding_matrix <- do.call(rbind, embeddings_list)
  
  # Compute pairwise cosine similarity
  distance_matrix <- as.matrix(dist(embedding_matrix, method="cosine"))
  
  return(distance_matrix)
}

##### Calculate Semantic Distance #######

#load stopwords
library(tm)
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)

#load embedding model
setwd("/Users/akpiper/Data/glove.6B")
glove_embeddings<-read.csv("glove.6B.100d.txt", sep=" ", quote="", header=F)
row.names(glove_embeddings)<-glove_embeddings[,1]
glove_embeddings<-glove_embeddings[,-1]

# Example usage:
#word_list <- c("apple", "fruit", "juice")
#distance_matrix <- compute_distances(word_list, glove_embeddings)

#create empty vectors
semantic_dist_total<-vector(mode="numeric", length=nrow(c))
semantic_dist_mean<-vector(mode="numeric", length=nrow(c))

for (i in 1:nrow(c)){
  
  print(i)
  
  #for every row create a vector of the sequence of GPEs
  places<-splitElements(c$nongpe_places_cleaned[i])
  
  #lowercase
  places<-tolower(places$Element)
  
  #remove punctuation
  places<-gsub("[[:punct:]]", "", places)
  
  #remove stopwords
  places<-remove_words(places, stop)
  
  #remove white spaces
  places<-gsub("\\s+", " ", places)
  
  #remove blanks
  places<-places[places != ""]
  
  #remove dupes in sequence
  places<-rle(places)$values
  
  #if there are more than 2 places
  if (length(places) > 2){
    
    #calculate distance matrix for all pairwise comparisons
    #first value is the vector of place names, second is the model
    distance_matrix <- compute_distances(places, glove_embeddings)
    
    #make sure there are more than 2 places
    if (!is.null(distance_matrix)){
      if (nrow(distance_matrix) > 2){
        
        #actual distance
        semantic_dist_total[i]<-distRoute(distance_matrix, 1:ncol(distance_matrix))
        semantic_dist_mean[i]<-semantic_dist_total[i]/ncol(distance_matrix)
        
      }
    }
  } else {
    semantic_dist_total[i]<-0
    semantic_dist_mean[i]<-0
  }
}

c$semantic_dist_total<-semantic_dist_total
c$semantic_dist_mean<-semantic_dist_mean


####### Calculate Semantic Distance between first and last GPE ###########
library(word2vec)

#create function to extract all places in sequence
splitElements2 <- function(input_column) {
  # Remove the square brackets at the beginning and end of each string in the column
  cleaned_text <- gsub("^\\[|\\]$", "", input_column)
  
  # Split each string into a list of elements based on ', ' and remove single quotes
  elements_list <- lapply(strsplit(cleaned_text, "', '"), function(x) gsub("'", "", x))
  
  # Convert the list of elements into a data frame
  result_df <- unlist(elements_list)
  
  return(result_df)
}

#read model
setwd("/Users/akpiper/Data")
model<-read.word2vec("GoogleNews-vectors-negative300.bin", normalize = T) #normalize = T!!!
emb<-as.matrix(model)

#empty vector
first_last_SemanticDist<-vector(mode="numeric", length=nrow(c))

for (i in 1:nrow(c)){
  
  print(i)
  
  #get vector of place names
  places<-splitElements2(c$gpe_sequences[i])
  
  #make sure there are minimum two places
  if (length(places) > 1){
  
  #condition on first and last
  x<-places[1]
  y<-places[max(length(places))]
  
  #reshape to match the data by inserting underscores for multi word phrases
  sep<-unlist(strsplit(x, "\\s+"))
  if (length(sep) > 1){
    x<-paste(sep, collapse="_")
  }
  sep<-unlist(strsplit(y, "\\s+"))
  if (length(sep) > 1){
    y<-paste(sep, collapse="_")
  }
  
  #if not in embedding 
  #x
  if (!x %in% rownames(emb)){
    #first check if it's a trigram
    if (length(unlist(strsplit(x, "_"))) > 2){
      #if so grep matching 2 gram
      new.x<-unlist(strsplit(x, "_"))[1:2]
      new.x<-paste(new.x, collapse="_")
      x<-new.x
    } 
  } 
  
  #y
  if (!y %in% rownames(emb)){
    #first check if it's a trigram
    if (length(unlist(strsplit(y, "_"))) > 2){
      #if so grep matching 2 gram
      new.x<-unlist(strsplit(y, "_"))[1:2]
      new.x<-paste(new.x, collapse="_")
      y<-new.x
    } 
  } 
  
  #if still not in embedding take next / prior value
  if (!y %in% rownames(emb)){
    y<-places[(max(length(places))-1)]
    sep<-unlist(strsplit(y, "\\s+"))
    if (length(sep) > 1){
      y<-paste(sep, collapse="_")
    }
  }
  if (!x %in% rownames(emb)){
    x<-places[2]
    sep<-unlist(strsplit(y, "\\s+"))
    if (length(sep) > 1){
      x<-paste(sep, collapse="_")
    }
  }
  
  #if one is still not in embedding
  if (!y %in% rownames(emb) | !x %in% rownames(emb)){
    first_last_SemanticDist[i]<-NA
  } else {
    a<-emb[x,]
    b<-emb[y,]
    first_last_SemanticDist[i]<-1-(round(word2vec_similarity(a, b)[1], digits = 3))
  }
  } else {
    first_last_SemanticDist[i]<-NA
  }
}

c$first_last_SemanticDist<-first_last_SemanticDist




