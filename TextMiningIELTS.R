# for text mining
install.packages('tm')
# for creating interactive word clouds
install.packages('wordcloud2')
# for reading data
install.packages("readr")
# for data manipulation
install.packages("dplyr")

# Loading the packages
library(wordcloud2)
library(tm)
library(readr)
library(dplyr)

# Creating a Corpus from text documents in a specified directory
docs <- Corpus(DirSource('E:/M/TextMiningR/IELTSfolder')) 
# Inspecting the content of the 'docs' Corpus
inspect(docs)

# Preprocessing the text documents in 'docs':
# Removing numbers, punctuation, extra whitespace, convert to lowercase,
# and removing common English stopwords and custom words
docs = docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(removeWords, c('that','and'))


# Defining a content transformer function to replace specific characters with spaces
toSpace <- content_transformer(function(x, pattern)  { return (gsub(pattern, " ",x))})
# Applying the 'toSpace' function to remove specific characters from text
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "'")
# Inspecting the preprocessed 'docs' Corpus
inspect(docs)


# Creating a Term-Document Matrix (TDM) from the preprocessed documents
tdm = TermDocumentMatrix(docs) %>%
  as.matrix()

# Calculating the frequency of each word
words = sort(rowSums(tdm), decreasing = TRUE)

# Creating a data frame with words and their frequencies
df = data.frame(word = names(words), freq = words)

# Filtering out short words and the word "don'"
df = df %>%
  filter(nchar(as.character(word)) > 2,
         word != "don'")

# Creating an interactive word cloud using 'wordcloud2'
wordcloud2(df)


# Customizing the word cloud by specifying colors and background color
pic.colors = c('#fa5d62', '#fdd124', '#2ebab1', '#86edfa', '#bc3a7e', '#dbcdd7')
pic.background = '#342554'

# Creating a customized word cloud with specified colors and background color
wordcloud2(df,
           color= rep_len(pic.colors, nrow(df)),
           backgroundColor = pic.background)


# Improving the word cloud by specifying font family, size, and rotation ratio
wordcloud2(df,
           color= rep_len(pic.colors, nrow(df)),
           backgroundColor = pic.background,
           fontFamily = 'DM Sans',
           size = 2.5,
           minSize = 5,
           rotateRatio = 0)

