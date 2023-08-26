install.packages('tm')
install.packages('wordcloud2')
install.packages("readr")
install.packages("dplyr")

library(wordcloud2)
library(tm)
library(readr)
library(dplyr)


docs <- Corpus(DirSource('E:/M/TextMiningR/IELTSfolder')) #the txt file is in the folder. SO you should open folder not 
# txt.
inspect(docs)

docs = docs %>%
  #tm_map(content_transformer(removeHTML)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, stopwords("SMART")) %>%
  tm_map(removeWords, c('that','and'))



#tm_map(stemDocument)

toSpace <- content_transformer(function(x, pattern)  { return (gsub(pattern, " ",x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "'")
inspect(docs)




tdm = TermDocumentMatrix(docs) %>%
  as.matrix()

words = sort(rowSums(tdm), decreasing = TRUE)

df = data.frame(word = names(words), freq = words)

#to avoid see stop words. it is a manual filter.
df = df %>%
  filter(nchar(as.character(word)) > 2,
         word != "don'")
#after this modification, you can see df by tapping in environment tab.

#Now, you can create an amazing wordcloud2 for most frequent words in your data frame:)))
wordcloud2(df)


#now, you can improve your wordcloud2 and make it better:)

#Tip1:  you can use it as "a comparison tool".so, u can execute all these codes
#for all your corpora :))

#Tip2: Change the Colors! go to https://imagecolorpicker.com/en and upload your favorite vibrant pic from 
#your PC to this website. then you can find your favorite colors codes and change your wordcloud2 colors!

pic.colors = c('#fa5d62', '#fdd124', '#2ebab1', '#86edfa', '#bc3a7e', '#dbcdd7')
pic.background = '#342554'


wordcloud2(df,
           color= rep_len(pic.colors, nrow(df)),
           backgroundColor = pic.background)



wordcloud2(df,
           color= rep_len(pic.colors, nrow(df)),
           backgroundColor = pic.background,
           fontFamily = 'DM Sans',
           size = 2.5,
           minSize = 5,
           rotateRatio = 0)



#Tip5: Masks[Documentation in Description] by custom image file like 'png' 
#masalan shekle wordcloud2 ro mese logoye Twitter dar biyari!
#or you can specify a shape for it like diamond or heart!
#or make a specific letter by Lettercloud command [LETTERCLOUDDF, "PEACE"] : masalan plot dar ghaleb kalameye
#peace" bashe ya dar ghalebe harfe "R"!


#Tip6: Stem text: baraye inke bejaye {teacher, teaching, teach, etc.} faghat yedoone teach dashte bashim.
#albate ba in ravash yeseri kalamat stemeshoon naghes mishe k inja mitooni ba tavajjoh be context i k toosh 
#hasti, explore koni che kalameyi boode!

#Tip7: Using N-grams instead of single words :)))


#Tip8: color and divide words by meaning :) 