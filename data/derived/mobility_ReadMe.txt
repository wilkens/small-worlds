Mobility Read Me

Column Names

book_id : Filename
char_id : bookNLP character ID for main character
char_count : number of references to main character
inf_gender : inferred gender (see supplementary for method)
gpe_places : all GPE places that the main character is "in"
num_gpe_places : total *unique* gpe_places
nongpe_places : all non-GPE places (LOC) that the main character is "in"
num_nongpe_places : total *unique* nongpe places
all_places: combined gpe + non
num_all_places : total combined
gpe_sequences : order of GPEs in narrative with duplicates in sequence removed (i.e. India - India - Canada becomes India - Canada)
dist_miles : total geodesic distance between gpe_sequences (see supplementary for calculation)
char_rank : rank of character's total mentions. Always 1 here.
num_words : number of tokens considered. 320K = whole book
Category : Fiction or Non-fiction
Genre : one of 12 categories (see https://openhumanitiesdata.metajnl.com/articles/10.5334/johd.88)
Tokens : token count per book
num_gpe_places_norm : number GPE places per token
num_nongpe_places_norm : number nongpe places per token
num_gpe_places_norm_byCharacter : number GPE places per character reference
num_nongpe_places_norm_byCharacter : number nongpe places per character reference
char_count_norm : character references per token
nongpe_places_cleaned : removed "here" and "there" and "it"
nongpe_places_total : all nongpe places from cleaned
gpe_places_total : all gpe places
ttr_nongpe : type token ratio for nongpe places (unique / total)
ttr_gpe : type token ratio for gpe places (unique / total)
avg_Distance_GPE : dist_miles / gpe_places_total
avg_Distance_GPE_Tokens : dist_miles / Tokens
non_gpe_ratio : nongpe_places_total / gpe_places_total
deixis_count_perplace : frequency of 'here','there' / nongpe_places_total
semantic_dist_total : total cosine distance between all nongpe's in sequence (see supplementary)
semantic_dist_mean : total distance / number nongpe's in embedding model 
semantic_dist_mean_Tokens : total distance / Tokens

*** Semantic Distance Calculation ***

A. Cleaning
1. "here" / "there" removed
2. stopwords removed
3. lowercased
4. White spaced removed
5. Empty values removed
6. Sequential duplicates removed (room, room, kitchen becomes room, kitchen)
B. Semantic distance matrix
7. Uses Glove 6B word embeddings (Wikipedia + Gigaword 5, 400K vocabulary, 100 dimensions)
8. For 1-grams matches single word with embedding vector
9. For n-grams takes average vector of all words in embedding
10. Computes pairwise cosine distance between all nongpe places
11. Calculates total distance as sum of all cosine distances of nongpe sequence
12. Calculates avg distance as total distance / total nongpe places (for which there was an embedding value, i.e. nrow of distance matrix)




