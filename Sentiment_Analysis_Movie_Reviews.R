#########################################################################################
################################## Text Pre-Processing ##################################
#########################################################################################

# In the code below, we will go through the typical steps to pre-process/ clean text
# These are just guidelines. Depending on what text is used an input, there might be a need to
# make slight changes to these pre-processing functions
# These are the thechniques covered in this notebook:
# 1. Bring to lower case
# 2. Remove numbers
# 3. Remove stopwords 
# 4. Remove punctuation 
# 5. Remove/ change certain words
# 6. Remove white space
# 7. Lemmatization / Stemming
# Other techniques could be applied to text as well. For example, if you work with social media data,
# you might want to remove tags and URLs from text. 
# After cleaning the data, let's transform it to:
# DTM = Document Term Matrix
# TDM = Term Document Matrix
# Tf-Idf = Term Frequency-Inverse Document Frequency 


##############################
######## load packages #######
##############################
# if the packages below are not installed, then uncomment the install.packages() lines and run them
#install.packages("dplR")
#install.packages("tm")
#install.packages("textstem")
library(dplyr) # dplyr package is used for data manipulation; it uses pipes: %>%
library(tm) # contains the stopwords dictionary
library(textstem) # used for stemming and lemmatization

##############################
##### read the data in R #####
##############################
# it's good practice to set up the working directory
# all the files youo read in R or write from R will be in the working directory you set up
# if you copy the path of your file from the foler, change all the \ to /
#setwd("your folder path")
scripts <- read.csv("allreviews.csv")


##############################
### check the type of data ###
##############################
str(scripts)
head(scripts)
View(scripts)


# For each cleaning task, let's create a new column to see the before and after


###############################
##### bring to lower case #####
###############################
# Text normalization allows words like "Something" and "something" be treated in the same way.
# You would usually transform all the words to lower case. 
# However, there might be times you don't wish to do so. 
# Ex: "US" and "us" mean different things and should remain as they are.
scripts <- scripts %>% mutate(Reviews_lower = tolower(Reviews))

# mutate() function is from the dplyr package and it is used to create a new column
head(scripts)

###############################
####### remove numbers ########
###############################
# this function looks for any number in the text and then removes it
# if desired to replace the numbers with a space, add a space inside the quotes: ' ' instead of ''
# [[:digit:]] is a regex expresion. Read more here: https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
scripts <- scripts %>% mutate(Reviews_noNumbers = gsub('[[:digit:]]','',Reviews_lower)) # gsub functoin searches for any digit in the text and removes it; 
head(scripts)


###############################
###### remove stopwords #######
###############################
# Stop words are the words that appear the most in the English vocabulary ("a", "the", "for).
# These don't provide important meaning and are therefore removed.
# R already provides packages that contain a collection of stopwords
# English stopwords
stopwords('en')
# Check the structure of the stopwords dictionary
str(stopwords('en')) # it is a vector in character format
# Subset the stopwords
stopwords('en')[1:10]
# remove certain stopwords
stopwords('en')[!stopwords('en') %in% c('i')]  # notice that the first stopword, i, was removed
# stopwords('en') %in% c('i') ---> this gives back a vector with TRUE and FALSE
# ! ---> this gives negation
# Add new stopwords
c(stopwords('en'), "under")  # notice that the stopwords have a new element: under
# Remove the stopwords from text; If you wish to modify the stopwords dictionary by adding/ removing any, then use code from above
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
scripts <- scripts %>% mutate(Reviews_noStopWords = gsub(stopwords_regex,'',Reviews_noNumbers))
head(scripts)


###############################
##### remove punctuation ######
###############################
# Punctuation (.,!?:;), symbols (*^&) are removed, unless there is a reason to keep them
scripts <- scripts %>% mutate(Reviews_noPunctuation = gsub('[[:punct:]]','',Reviews_noStopWords))
head(scripts)


################################
# remove/ change certain words #
################################
# Replace words that have typos with the correct words. If synonyms are present, these can be replaced as well.
# Example of fixing a typo
#scripts <-scripts %>% mutate(Narrative_noTypos = gsub('thankssssssss','thanks',Narrative_noPunctuation))
#head(scripts)


################################
###### remove whitespace #######
################################
# Remove extra white space (this would include space, tab, vertical tab, newline, form feed, carriage return):
scripts <- scripts %>% mutate(Reviews_noSpaces = gsub('\\s+',' ',Reviews_noPunctuation))
head(scripts)



# Stemming and Lemmatization are techniques to reduce the number of terms based on grammatical inflections.
# Stemming removes the end of a words to bring the words to a common base form.
# Lemmatization removes a word's suffix to reduce the size of the vocabulary while bringing it to its root form.
# When doing text minening, you would use either stemming either lemmatization

################################
########### stemming ###########
################################
#scripts <-scripts %>% mutate(Narrative_Stem = stem_strings(Narrative_noSpaces))
#head(scripts)

################################
######## lemmatization #########
################################
scripts <-scripts %>% mutate(Reviews_Lemma = lemmatize_strings(Reviews_noSpaces))

head(scripts)


# We'll use lemmatization going forwards

# keep just the text column
my_text <- scripts %>% select(Reviews_Lemma)

write.csv(my_text, "reviews.csv")

################################
########## create DTM ##########
################################
# To create a DTM, we need to change the data frame to a corpus. First, we need to have a data frame whose column names are doc_id and text.
# doc_id represents the document/ line of text;
# text represents the content; this is what will be used to create the DTM
# https://www.rdocumentation.org/packages/tm/versions/0.7-6/topics/DataframeSource
my_corpus <- my_text
my_corpus <- my_corpus %>% rename(text = Reviews_Lemma)  %>% mutate(doc_id = rownames(my_text))
my_corpus <- Corpus(DataframeSource(my_corpus))  # transform the data frame into a corpus
str(my_corpus)
# check the first conversation
inspect(my_corpus[[1]])
# Transform the text to DTM
my_dtm <- as.matrix(DocumentTermMatrix(my_corpus))
str(my_dtm)


################################
########## create TDM ##########
################################
my_tdm <- as.matrix(TermDocumentMatrix(my_corpus))
str(my_tdm)


################################
######### create Tf-Idf ########
################################
my_tfidf <- as.matrix(DocumentTermMatrix(my_corpus, control = list(weighting = weightTfIdf)))
str(my_tfidf)

library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(reshape2)
library(tidytext)

# Read the text file from local machine , choose file interactively
script2 <- read.csv("reviews.csv")
script2 <- script2 %>% filter(Reviews_Lemma!= "")
(script2 <- script2 %>% select(Reviews_Lemma))

# Load the data as a corpus

TextDoc <- VCorpus(x = VectorSource(script2),
                   readerControl = list(reader=readPlain,
                                        language="en"))
TextDoc <- tm_map(TextDoc, removeWords, stopwords())
#TextDoc

#frequency

# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descending value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)
View(dtm_d)

# Plot the most frequent words
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col ="darkolivegreen", main ="Top 10 words in reviews",
        ylab = "Frequencies")

#generate word cloud
#set.seed(1234)
#wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
#max.words=50, random.order=FALSE, rot.per=0.40, 
#colors=brewer.pal(10, "Paired"))



#Most used words from dialogue #####

#Sentiment Analysis

#Tokenize Sentiment of Script

tokens <- scripts %>%  
  mutate(dialogue=as.character(scripts$Dialogue_Lemma)) %>%
  unnest_tokens(word, Dialogue)

tokens <- tokens %>% select(Speaker, word)

# Find Main Sentiment of Script
#install.packages("textdata")
library(textdata)
library(dplyr)
sentiments <- tokens %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) 

bing <- get_sentiments("bing") %>% 
  mutate(lexicon = "bing", 
         words_in_lexicon = n_distinct(word))    

nrc <- get_sentiments("nrc") %>% 
  mutate(lexicon = "nrc", 
         words_in_lexicon = n_distinct(word))

afinn <- get_sentiments("afinn") %>% 
  mutate(lexicon = "afinn", 
         words_in_lexicon = n_distinct(word))

#ggplot(my_text$Dialogue_Lemma=sentiments, aes(x=reorder(sentiment, -n, sum), y=n))
#geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
#labs(x="Sentiment", y="Frequency") +
#theme_bw()

#Most used words ######
tokens <- my_text %>%  
  mutate(dialogue=as.character(my_text$Dialogue_Lemma)) %>%
  unnest_tokens(word, Dialogue_Lemma)

tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors=c("#F8766D", "#00BFC4"), max.words=50)

# Sentiments and frequency associated with each word  
sentiments <- tokens %>% 
  inner_join(nrc, "word") %>%
  count(word, sentiment, sort=TRUE) 

# Frequency of each sentiment
ggplot(data=sentiments, aes(x=reorder(sentiment, -n, sum), y=n)) + 
  geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
  labs(x="Sentiment", y="Frequency") +
  theme_bw()

# Top 10 terms for each sentiment
sentiments %>%
  group_by(sentiment) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(y="Frequency", x="Terms") +
  coord_flip() +
  theme_bw() 

# Sentiment analysis for the Top 10 characters with more dialogues
top10 <- as.data.frame(sort(table(my_text$Speaker_lower), decreasing=TRUE))[1:10,]
top10
library(dplyr)
library(tidytext)

#tokens %>%
filter(Speaker %in% c("HARRY","HERMIONE","RON")) %>%
  inner_join(nrc, "word") %>%
  count(Speaker, sentiment, sort=TRUE) %>%
  ggplot(aes(x=sentiment, y=n)) +
  geom_col(aes(fill=sentiment), show.legend=FALSE) +
  facet_wrap(~Speaker, scales="free_x") +
  labs(x="Sentiment", y="Frequency") +
  coord_flip() +
  theme_bw() 



# Most frequent words for each character
tokens %>%
  count(my_text$Sentiment, word) %>%
  group_by(my_text$Speaker) %>% 
  arrange(desc(n)) %>%
  slice(1:10) %>%
  ungroup() %>%
  mutate(word2=factor(paste(word, my_text$Speaker, sep="__"), 
                      levels=rev(paste(word, my_text$Speaker, sep="__"))))%>%
  ggplot(aes(x=word2, y=n)) +
  geom_col(aes(fill=my_text$Speaker), show.legend=FALSE) +
  facet_wrap(~my_text$Speaker, scales="free_y") +
  labs(x="Sentiment", y="Frequency") +
  scale_x_discrete(labels=function(x) gsub("__.+$", "", x)) +
  coord_flip() +
  theme_bw()

#