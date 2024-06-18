############ Mobility: Derived Data ############
############ Andrew Piper  #####################
library(stringr)


#load CONLIT metadata
meta<-read.csv("CONLIT_META.csv")
meta<-read.csv("EARLY_META.tsv", sep="\t")

#read/write data
#c<-read.csv(gzfile("conlit.csv.gz"))
c<-read.csv(gzfile("early.csv.gz"))

# c<-c[order(c$book_id),]
# meta<-meta[order(meta$book_id),]
# meta<-meta[!duplicated(meta$book_id),]
# c<-c[c$book_id %in% meta$book_id,]
# which(c$book_id != meta$book_id)
# c<-cbind(meta$source, c)
# colnames(c)[1]<-c("collection")
# c<-cbind(meta$pub_date, c)
# colnames(c)[1]<-c("pub_date")

write.csv(c, file="early.csv", row.names = F)
system("gzip early.csv") 

##### calculate measures on extracted data #######
a<-read.csv("book_char_mobility.tsv", sep="\t")

#condtion only on all words
b<-a[a$num_words == 320000,]

c<-NULL
for (i in 1:nlevels(factor(b$book_id))){
  sub<-b[b$book_id == levels(factor(b$book_id))[i],]
  sub<-sub[which(sub$char_count == max(sub$char_count)),]
  c<-rbind(c, sub)
}
#remove dupes
c<-c[-which(duplicated(c$book_id)),]

#match with metadata
c<-c[order(c$book_id),]
meta<-meta[order(meta$ID),]
meta$ID2<-gsub(".txt", "", meta$ID)
which(c$book_id != meta$ID2)
#add metadata
c$Category<-meta$Category
c$Genre<-meta$Genre
c$Tokens<-meta$token_count

#normalize gpe_places_total by tokens
c$num_gpe_places_norm<-c$num_gpe_places/c$Tokens

#normalize nongpe_places_total by tokens
c$num_nongpe_places_norm<-c$num_nongpe_places/c$Tokens

#normalize by char_count
c$num_gpe_places_norm_byCharacter<-c$num_gpe_places/c$char_count
c$num_nongpe_places_norm_byCharacter<-c$num_nongpe_places/c$char_count

#normalize character count by Tokens
c$char_count_norm<-c$char_count/c$Tokens

#create cleaned places (remove "here" and "there" and "it")
c$nongpe_places_cleaned<-gsub("\\bthere\\b", "", c$nongpe_places)
c$nongpe_places_cleaned<-gsub("\\bhere\\b", "", c$nongpe_places_cleaned)
c$nongpe_places_cleaned<-gsub("\\bit\\b", "", c$nongpe_places_cleaned)

#calculate total distance per book
#c$dist_miles_allChars_norm_Tokens<-unname(tapply(b$dist_miles, b$book_id, sum))/c$Tokens
#c$num_gpe_places_allChars_norm_Tokens<-unname(tapply(b$num_gpe_places, b$book_id, sum))/c$Tokens

#calculate Deixis (rate of here + there)

#function to calculate total number of unique phrases
count_strings <- function(row) {
  # Remove the single quotes and split the row by commas
  strings <- unlist(strsplit(gsub("'", "", row), ","))
  # Remove leading/trailing spaces from each string and count the non-empty ones
  return(sum(nchar(trimws(strings)) > 0))
}

# Apply the function to each row and store the result in a new column
c$nongpe_places_total <- sapply(c$nongpe_places_cleaned, count_strings)
c$gpe_places_total <- sapply(c$gpe_places, count_strings)

#calculate TTR
c$ttr_nongpe<-c$num_nongpe_places/c$nongpe_places_total
c$ttr_gpe<-c$num_gpe_places/c$gpe_places_total

#calculate AVG Distance as total distance per place
c$avg_Distance_GPE<-c$dist_miles/c$gpe_places_total

#calculate AVG Distance as total distance per token
c$avg_Distance_GPE_Tokens<-c$dist_miles/c$Tokens

#NON / GPE ratio
c$non_gpe_ratio<-c$num_nongpe_places/c$num_gpe_places

#rank by num_nonGPE & dist_miles normalized by tokens
c<-c[order(c$nongpe_places_total/c$Tokens),]
c$non_gpe_total_rank<-seq(1:nrow(c))

#dist_miles
c<-c[order(c$dist_miles/c$Tokens),]
c$dist_miles_rank<-seq(1:nrow(c))
#transform all 0 values into rank 1
c$dist_miles_rank[c$dist_miles == 0] <- 1
#reorder
c<-c[order(c$book_id),]
#reorder
c<-c[order(c$dist_miles_rank, -c$non_gpe_total_rank),]
#subset
d<-c[c$Genre %in% c("PW", "BS", "NYT", "MY"),]
d<-d[1:50,]
write.csv(d, file="TopAdultFiction_NonGPE.csv", row.names = F)

#Deixis (There+Here Frequency)
c$deixis_count_perplace<-str_count(c$nongpe_places, "'here'|'there'")/c$nongpe_places_total

#End Point Similarity
#calculates the distance between the starting location and the final location and calculates a z score
#for that distance (how many standard deviations above or below avg distance in the book is this distance)
#this illustrates the relationship between distance and narrative closure in the sense of circularity

##### Most frequent place calculations ######

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

#function to remove stopwords
remove_words <- function(text_vector, remove) {
  # Construct a single regex pattern that matches any word in the remove vector
  pattern <- paste0("\\b(", paste(remove, collapse="|"), ")\\b")
  
  # Replace matched words with empty string
  cleaned_text <- gsub(pattern, "", text_vector)
  
  # Remove leading and trailing whitespace
  return(trimws(cleaned_text))
}

#extract every place name and tabulate
fic<-c[c$Category == "FIC",]
list_of_vectors <- lapply(fic$gpe_sequences, splitElements2)
place.v<-unlist(list_of_vectors)
top.v<-names(sort(table(place.v), decreasing = T)[1:5])

#for the most common place extract the next place in sequence
#i.e. where do you go from New York?
find_next_place <- function(list_of_vectors, input_vector) {
  # Initialize an empty list to store the results
  output_list <- list()
  
  # Loop over each element in the input vector
  for (input_place in input_vector) {
    # Initialize a temporary vector to store the next places for the current input place
    temp_vector <- c()
    
    # Loop over each vector in the list
    for (place_vector in list_of_vectors) {
      # Find the indices of the input place in the current vector
      indices <- which(place_vector == input_place)
      
      # Loop through each index and get the next place if not the last element
      for (index in indices) {
        if (index < length(place_vector)) {
          next_place <- place_vector[index + 1]
          # Append the next place to the temporary vector
          temp_vector <- c(temp_vector, next_place)
        }
      }
    }
    
    # Store the temporary vector in the output list
    output_list[[input_place]] <- temp_vector
  }
  
  return(output_list)
}

# Function to get the top five items in descending order
get_top_five <- function(output_list, n) {
  # Initialize an empty list to store the results
  top_five_list <- list()
  
  # Loop over each element in the output list
  for (input_place in names(output_list)) {
    # Create a frequency table for the current vector
    freq_table <- table(output_list[[input_place]])
    
    # Sort the frequency table in descending order
    sorted_freq_table <- sort(freq_table, decreasing = TRUE)
    
    # Get the top five items
    top_five <- head(sorted_freq_table, n)
    
    # Store the top five items in the output list
    top_five_list[[input_place]] <- top_five
  }
  
  return(top_five_list)
}

top_five_to_df <- function(top_five_output) {
  # Initialize an empty list to store the results
  df_list <- list()
  
  # Loop over each element in the top_five_output list
  for (input_place in names(top_five_output)) {
    # Extract the names of the places (excluding the counts)
    place_names <- names(top_five_output[[input_place]])
    
    # Create a data frame row with the input place name and the place names
    df_row <- data.frame(input_place = input_place, matrix(place_names, nrow = 1))
    
    # Append the data frame row to the list
    df_list <- append(df_list, list(df_row))
  }
  
  # Combine all rows into a single data frame
  final_df <- bind_rows(df_list)
  
  return(final_df)
}
#example
list_of_vectors <- list(
  c("Paris", "London", "Berlin"),
  c("Rome", "Madrid", "Paris"),
  c("Berlin", "Paris", "London")
)
input_vector <- c("Paris", "Berlin")
output <- find_next_place(list_of_vectors, input_vector)
top_five_output <- get_top_five(output, 5)

#actual 
input_vector<-top.v
output <- find_next_place(list_of_vectors, input_vector)
top_five_output <- get_top_five(output, 10)
df<-top_five_to_df(top_five_output)
write.csv(df, file="MostCommonPlacesAndNextHop.csv", row.names = F)

#repeat for nonGPEs
library(tm)
stop<-stopwords("en")
stop<-unlist(strsplit(stop,"[[:punct:]]"))
stop<-unique(stop)

#extract vector of places
list_of_vectors <- lapply(fic$nongpe_places_cleaned, splitElements2)

#cleaning function
clean_places <- function(vector, stop_words) {
  # Convert to lower case
  vector <- tolower(vector)
  
  # Remove punctuation
  vector <- gsub("[[:punct:]]", "", vector)
  
  # Remove stopwords
  vector <- removeWords(vector, stop_words)
  
  # Remove white spaces
  vector <- gsub("\\s+", " ", vector)
  
  # Remove blanks
  vector <- vector[vector != ""]
  
  # Trim spaces from beginning and end of each element
  vector <- trimws(vector)
  
  # Remove duplicates in sequence
  #vector <- rle(vector)$values
  
  return(vector)
}

cleaned_list_of_vectors <- lapply(list_of_vectors, clean_places, stop_words = stop)

#tabulate
top.non<-names(sort(table(unlist(cleaned_list_of_vectors)), decreasing = T)[1:5])

#run function
library(dplyr)
input_vector<-top.non
output <- find_next_place(cleaned_list_of_vectors, input_vector)
top_five_output <- get_top_five(output, 10)
df<-top_five_to_df(top_five_output)
write.csv(df, file="MostCommonNONGPEsAndNextHop.csv", row.names = F)


#for the most common place extract the next place in sequence
#i.e. where do you go from New York?
top.plus1.non<-vector(mode="character", length=length(top.non))
for (i in 1:length(top.non)){
  next.v<-places[(which(places == top.non[i])+1)]
  top.plus1.non[i]<-paste(names(sort(table(next.v), decreasing = T)[1:5]), collapse = ",")
}

df2<-data.frame(top.non, top.plus1.non)
colnames(df2)<-c("Top Places", "Next Places")

df3<-cbind(df, df2)



