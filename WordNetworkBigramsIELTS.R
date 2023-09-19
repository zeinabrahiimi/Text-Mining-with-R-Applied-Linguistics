# This code processes a dataset of IELTS essays to identify and visualize frequently 
# co-occurring bigrams (groups of two words) while excluding stop words and numeric entries.
# The explanation for each line of code is succinctly provided through comments.

# Installing and loading necessary packages. Since they were already installed, I only loaded them.
library(readr)
library(tidytext)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)

# Reading the dataset
#To perform this analysis, you may want to transform the text file into a CSV file.
text <- read_csv("ielts.csv")

# Tokenizing the text into bigrams
n_words <- text %>%
  dplyr::select(essay_text) %>%
  unnest_tokens(paired_words,
                essay_text,
                token = "ngrams",
                n = 2)

# Loading stop words
data("stop_words")

# Separating the bigrams into individual words
separated_words <- n_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

# Removing rows containing numbers or digits
filtered <- separated_words %>%
  filter(!grepl("[0-9]", word1)) %>%
  filter(!grepl("[0-9]", word2))

# Removing stop words
filtered <- filtered %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Creating a graph from the word co-occurrence data
bigram_graph <- filtered %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n > 1) # Filters out word combinations occurring only once and retains those occurring more than once.

# Selecting the top 200 most frequent word pairs (bigrams)
top_200_bigrams <- head(bigram_graph, 200)

# Creating a graph from the top 200 bigrams
word_graph <- top_200_bigrams %>%
  graph_from_data_frame(directed = TRUE)

# Setting the layout
layout <- layout_with_fr(word_graph)

# Plotting the word network with different arrow sizes
# Creating a graph visualization using ggraph
ggraph(word_graph, layout = layout) +
  # Adding edges between words with alpha (transparency) based on their frequency
  geom_edge_link(aes(edge_alpha = n), arrow = arrow(type = "closed", length = unit(0.15, "inches"))) +
  # Adding nodes (words) as points with a green color and size of 2
  geom_node_point(color = "green", size = 2) +
  # Adding labels to the nodes (words) with adjustments for vertical and horizontal alignment
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  # Adding titles and subtitles to the visualization
  labs(
    title = "Word Co-occurrence Network (Top 200 Bigrams)",
    subtitle = "Frequently co-occurring word pairs in the  IELTS Essays Dataset"
  ) +
  # Creating a theme with a white background
  theme_void()
