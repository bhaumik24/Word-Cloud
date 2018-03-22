#Reading the twitter data

obama1 <- read.csv("obama1.csv",h=FALSE)
obama2 <- read.csv("obama2.csv",h=FALSE)
obama3 <- read.csv("obama3.csv",h=FALSE)
obama4 <- read.csv("obama4.csv",h=FALSE)

obama <- rbind(obama1,obama2,obama3,obama4)
str(obama)
names(obama)

#Converting to the dataframe

tweets <- data.frame(obama$V2)

#Renaming the column

names(tweets)<-"Tweet_Text"

str(tweets)
names(tweets)

#Data Pre-processing using tm package

library(tm)

#Building a text corpus
#source for the corpus

tweets.corpus <- Corpus(VectorSource(tweets$Tweet_Text)) 
summary(tweets.corpus)
head(tweets.corpus)

#inspecting element in corpus

inspect(tweets.corpus[1:10])

#Data Transformations

tweets.corpus <- tm_map(tweets.corpus,tolower) #converting to lower case
tweets.corpus <- tm_map(tweets.corpus,stripWhitespace) #removing extra white spaces
tweets.corpus <- tm_map(tweets.corpus,removePunctuation) #removing punctuation
tweets.corpus <- tm_map(tweets.corpus,removeNumbers) #removing numbers

my_stopwords<-c(stopwords('english'),'http*') #can add more words from standard list
tweets.corpus<- tm_map(tweets.corpus,removeWords, my_stopwords)


#Building Term Document Matrix

tweets.tdm <- TermDocumentMatrix(tweets.corpus)
tweets.tdm
dim(tweets.tdm) #Dimensions of term document matrix
inspect(tweets.tdm[1:10,1:10]) #inspecting the term document matrix

#Removing sparse terms
#words that occur infrequently
#this function call removes those terms which have
#at least 97% percent of sparse (i.e. terms occuring 0 time in a document)

tweets.imp <-removeSparseTerms(tweets.tdm,0.97)
tweets.imp
inspect(tweets.imp[1:10,1:10])


#Finding words and frequencies

temp <- inspect(tweets.imp)
wordFreq <- data.frame(apply(temp,1,sum))
wordFreq <- data.frame (ST = row.names(wordFreq), Freq = wordFreq[,1])
wordFreq <-wordFreq[order(wordFreq$Freq, decreasing = T),]
row.names(wordFreq)<-NULL


#Basic Analysis
#Finding the most frequent terms/words

findFreqTerms(tweets.tdm,5) #occuring minimum 5 times
findFreqTerms(tweets.tdm,7) #occuring minimum 7 times
findFreqTerms(tweets.tdm,10) #occuring minimum 10 times
findFreqTerms(tweets.tdm,20) #occuring minimum 20 times
findFreqTerms(tweets.tdm,30) #occuring minimum 30 times
findFreqTerms(tweets.tdm,70) #occuring minimum 70 times


#Finding association between terms/words

findAssocs(tweets.tdm,"republicans",0.2)
findAssocs(tweets.tdm,"climate",0.3)
findAssocs(tweets.tdm,"china",0.3)
findAssocs(tweets.tdm,"dreams",0.3)
findAssocs(tweets.tdm,"congress",0.3)
findAssocs(tweets.tdm,"myanmar",0.4)
findAssocs(tweets.tdm,"leader",0.3)
findAssocs(tweets.tdm,"brisbane",0.2)
findAssocs(tweets.tdm,"paradise",0.3)

#building a wordcloud
#RcolorBrewer

install.packages("wordcloud")
install.packages("RcolorBrewer")
brewer.pal #helps you identify the group of pallete colours

display.brewer.all()

display.brewer.pal(8,"Dark2")
display.brewer.pal(8,"Purples")
display.brewer.pal(10,"Oranges")

pal2 <-brewer.pal(8,"Dark2")

#plot your wordCloud

wordcloud(tweets.corpus, min.freq=40, max.words = 100, random.order=T, colors = pal2)

#to change the font

wordcloud(tweets.corpus, min.freq=40, max.words = 100, random.order=T, colors = pal2, vfont=c("script","plain"))
