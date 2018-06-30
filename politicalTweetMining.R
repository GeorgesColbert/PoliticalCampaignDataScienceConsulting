####### # Tweets Mining app was created 



#### Use the youtube video to follow https://www.youtube.com/watch?v=qWmMKmPVtgk

#Use the youtube video to follow part 2: https://www.youtube.com/watch?v=jk8e6RQGzqw


#### https://twitter.com/BitcoinTweets website url

###Consumer Key (API Key)	e8snPvrnYPRxLlD3q9cGgSB7v
#Consumer Secret (API Secret)	ZXeixJRMBKwOaWpjx0tdV4oj9oK8wEwgFe8Z7ykvEtJOOnS4Tx
#Access Level	Read and write (modify app permissions)
#Owner	gebanks90
#Owner ID	343070505

#Access Token	343070505-L45mKFbmUdivLwRCrBqVOZS0i53AEo50InU4ucuD
#Access Token Secret	li7JTCCX6VRRhwgzeRg5WFP3aetdPykDxDP5Ww9fgMR26
#Access Level	Read and write
#Owner	gebanks90
#Owner ID	343070505

api_key <- "e8snPvrnYPRxLlD3q9cGgSB7v"
api_secret <- "ZXeixJRMBKwOaWpjx0tdV4oj9oK8wEwgFe8Z7ykvEtJOOnS4Tx"
access_token <- "343070505-L45mKFbmUdivLwRCrBqVOZS0i53AEo50InU4ucuD"
access_token_secret <- 'li7JTCCX6VRRhwgzeRg5WFP3aetdPykDxDP5Ww9fgMR26'

# Install packages twitteR, tm, wordcloud


#load Library twitteR
library(twitteR)
library(wordcloud)
library(tm)
library(SnowballC)
library(readr)
library(rtweet)
library(dplyr)
library(ggplot2)  
library(reshape2)



setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)



#Getting Bitcoin Tweets

R.tweet <- searchTwitter("MAGA + Virginia", n = 1000, lang = 'en', since = '2018-06-03', until = '2018-06-10')

R.tweetdf <- twListToDF(R.tweet)
head(R.tweetdf)
str(R.tweetdf)

##### How many are retweets?

table(R.tweetdf$isRetweet)



#######
tweet.Corpus <- Corpus(VectorSource(R.tweetdf$text))
tweet.Corpus <- tm_map(tweet.Corpus, removeWords, stopwords())

remove_url <- function(x) gsub("http[^[:space:]]*","",x)
tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(remove_url))


##### remove anything other than english letters and space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(removeNumPunct))
tweet.Corpus <- tm_map(tweet.Corpus, removePunctuation)
tweet.Corpus<- tm_map(tweet.Corpus, content_transformer(tolower))
tweet.Corpus <- tm_map(tweet.Corpus, stripWhitespace)
tweet.Corpus <- tm_map(tweet.Corpus, stemDocument)   ### if stemDocument doesn't run, install the package 'SnowballC'
##
dtm <-  DocumentTermMatrix(tweet.Corpus)

#### Specific words you may want to remove #Virginia, Republican, GOP, Democrat, 2018, Primary
myStopWords <- "Corey Stewart"
myStopWords <- c("Corey Stewart",  "Virginia", "Republican", "GOP", "Democrat", 2018, "Primary", " Corey", "Stewart", "coreystewartva","stewart","corey") 
tweet.Corpus <-tm_map(tweet.Corpus, removeWords,myStopWords)

library(wordcloud)
wordcloud(tweet.Corpus, min.freq = 10, random.order = F)


######################


### import package "sentimentr"
library(sentimentr)

bit.tweetdf$text <- gsub("[^0-9A-Za-z///' ]", "",bit.tweetdf$text)
#### remove http link
bit.tweetdf$text <- gsub("http\\w+", "",bit.tweetdf$text)
#### remove rt
bit.tweetdf$text <- gsub("rt", "",bit.tweetdf$text)
### remove at
bit.tweetdf$text <- gsub("@\\w+", "",bit.tweetdf$text)

bit.tweetdf$text <- tolower(bit.tweetdf$text)


emo_R_tweets <- sentiment(R.tweetdf$text)
View(emo_R_tweets)


R.tweetdf$sentiment <- emo_R_tweets$sentiment  ## attaching the sentiment measure to every tweet
View(R.tweetdf)


### grabbing positive tweets, sample of 

positive_tweets <- head(unique(R.tweetdf[order(emo_R_tweets$sentiment, decreasing = T),c(1,17)]),25)
positive_tweets

#grabbing negative tweets
negative_tweets <- head(unique(R.tweetdf[order(emo_R_tweets$sentiment),c(1,17)]),25)
negative_tweets


# creating a table to create a wordcloud
write.table(positive_tweets$text, file = "/Users/georgesericcolbert/Desktop/Projects/Political Tweets/WordCloud/positive.txt",sep = "")
write.table(negative_tweets$text, file = "/Users/georgesericcolbert/Desktop/Projects/Political Tweets/WordCloud/negative.txt",sep = "")

#### 
library(tm)
tweet.Corpus.2 <- Corpus(DirSource(directory = "/Users/georgesericcolbert/Desktop/Projects/Political Tweets/WordCloud/WordCloud"))
summary(tweet.Corpus.2)

library(tm)

clean.tweet.Corpus.2 <- tm_map(tweet.Corpus.2, tolower)
clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, removeWords, stopwords())
clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, content_transformer(removeNumPunct))
clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, removePunctuation)
clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, stripWhitespace)
clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, stemDocument) 

###### TermDocumentMatrix and DocumentTermMatrix do the same thing
tc2_tdm <- TermDocumentMatrix(clean.tweet.Corpus.2)

tc2_matrix <- as.matrix(tc2_tdm)

colnames(tc2_matrix) <- c("negative Tweets", "Postive Tweets")
comparison.cloud(tc2_matrix, max.words = 50, random.order = FALSE)


######### 


library(data.table)
grep("bitch", bit.tweetdf$text)



 o <- bit.tweetdf[bit.tweetdf$text %like% "nick", ]
head(o)

 w <- slice(d,6)

 
 
 ###### 
 
 
 #### 
 
 ## most retweet Tweet talking about Corey Stewart
 
 ### Most Common Words used with Corey Stewart
 
 
 ##
 
 
 ################################### ################################### ################################### ###################################

 Stew_tweetdf_03_11 <- read_csv("Tim.tweetdf.03.11.csv")
 temp <- filter(Part3, screenName == 'CoreyStewartVA')
 write.csv(Part3, file =  "Tim.tweetdf.13.21.csv")
 
 
 
 
 t <- filter(VA.tweetdf, screenName == 'UK_Groypley')
 t <- t[order(t$created, decreasing=FALSE),]
 
 
 
 #WordCloud of tweets using the hastage #VASen
 
 
 VA.tweet.3 <- searchTwitter('#vasen', n = 30000, lang = 'en', since = '2018-06-12', until = '2018-06-21')
 

  Part1<-twListToDF(VA.tweet.3) 
  
  Part3 <- rbind(Part1,Part2)
  
  Part3 <- Part3[!duplicated(Part3),]
  
  VA.tweetdf <- twListToDF(VA.tweet.3)
 
 write.csv(VA.tweetdf, file =  "Nick.tweetdf.PrimDay.csv")
 
 VA.tweetdf <- read_csv("VA.tweetdf.07.17.csv")
 VA.tweetdf.2 <- read_csv("VA.tweetdf.17.23.csv")
 VA.tweetdf.3 <- read_csv("VA.tweetdf.23.30.csv")
 VA.tweetdf.4 <- read_csv("VA.tweetdf.30.05.csv")
 
 
 
 VA.tweetdf <- rbind(VA.tweetdf,VA.tweetdf.2,VA.tweetdf.3, VA.tweetdf.4)
 
 
 ##### How many are retweets?
 
 table(VA.tweetdf$isRetweet)

  
 #######
 tweet.Corpus <- Corpus(VectorSource(VA.tweetdf$text))
 tweet.Corpus <- tm_map(tweet.Corpus, removeWords, stopwords())
 
 remove_url <- function(x) gsub("http[^[:space:]]*","",x)
 tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(remove_url))
 
 
 ##### remove anything other than english letters and space
 
 removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
 tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(removeNumPunct))
 tweet.Corpus <- tm_map(tweet.Corpus, removePunctuation)
 tweet.Corpus<- tm_map(tweet.Corpus, content_transformer(tolower))
 tweet.Corpus <- tm_map(tweet.Corpus, stripWhitespace)
 
 myStopWords <- c("virginia", "2018","senate", "vasen", "nickforva")
 
 tweet.Corpus <-tm_map(tweet.Corpus, removeWords,myStopWords)
 
 
 Corpus.copy <- tweet.Corpus
 
 
 
 tweet.Corpus <- tm_map(tweet.Corpus, stemDocument)   ### if stemDocument doesn't run, install the package 'SnowballC'
 #tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(stemCompletion), dictionary = Corpus.copy)
 
 

 
 
 

 
 #### Specific words you may want to remove #Virginia, Republican, GOP, Democrat, 2018, Primary
 #myStopWords <- "Corey Stewart"
 #myStopWords <- c("Corey Stewart",  "Virginia", "Republican", "GOP", "Democrat", 2018, "Primary", " Corey", "Stewart", "coreystewartva","stewart","corey") 
 

 #tweet.Corpus <- tm_map(tweet.Corpus, PlainTextDocument)

 
 
 tdm <-  TermDocumentMatrix(tweet.Corpus)
 dtm <- DocumentTermMatrix(tweet.Corpus)
 
 
 #library(wordcloud)
 #wordcloud(tweet.Corpus, min.freq = 10, random.order = F)
 
 
 
 freq <- colSums(as.matrix(dtm)) 
 
 
 
 
 library(wordcloud)
 dark2 <- brewer.pal(6, "Dark2")
 wordcloud(names(freq), freq, min.freq = 200, rot.per=0.2, colors=dark2,random.order = F) 
 

 ### find tweets that use the word michael
 
 mike <- VA.tweetdf[grep("michael",VA.tweetdf$text),]
 
 #### filter out retweets to only get original tweets
 mike <- distinct(mike,text,.keep_all=TRUE)
 
 head(select(mike,text,screenName,retweetCount,created), n=7)
 
 
 
 
 ##### inspect frequent words
 
 (freq.terms <- findFreqTerms(tdm, lowfreq = 50))
 
 
 term.freq <-  rowSums(as.matrix(tdm))
 
 term.freq <- subset(term.freq, term.freq >=100)
 
  df2 <- data.frame(term=names(term.freq), freq = term.freq)
  library(ggplot2)
  
  ggplot(df2, aes(x=term, y = freq))+geom_bar(stat = "identity") + xlab("Terms")+ylab("Count")+ coord_flip()
  
  # Plot Word Frequencies 
  
  
  wf <- data.frame(word=names(freq), freq=freq) 
  wf <- wf[order(wf$freq, decreasing=TRUE),]
  head(wf, n = 10)  
  
  library(ggplot2)   
  
  plot.tweet <- ggplot(subset(wf, freq>75), aes(x = reorder(word,-freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  plot.tweet   
  
# which words are associated with "corey"
  
  findAssocs(tdm,"coreystewartva", 0.1)
  
  findAssocs(tdm,"nickforva", 0.1)
  
  findAssocs(tdm,"ewjacksonsr",0.1 )

  # Number of Tweets per Day
  
  time.freq <- as.data.frame(table(VA.tweetdf$Date))
  
  
  colnames(time.freq) <- c("Date","TweetsperDay")
  time.freq$Date <- as.Date(time.freq$Date)
  
  ggplot(data = time.freq,aes(Date,TweetsperDay))+geom_bar(stat="identity")+scale_x_date(labels = date_format("%Y-%m-%d"))
  
  
  
  ## Plot  Likes/Retweets per Day
  
  B<- filter(VA.tweetdf, screenName == 'CoreyStewartVA')
  
  B <- filter(B, isRetweet == 'FALSE')
  
   B<- select(B, retweetCount,favoriteCount, created)
   
 
  B$Date <- as.Date(B$created)
  
  b <-  B %>% group_by(Date) %>% summarise(totalRT=sum(retweetCount), totalFav=sum(favoriteCount))
  
  
  ggplot(b) +
    geom_bar(aes(x=Date, y=totalRT),stat="identity", fill="tan1", colour="sienna3")+ geom_bar(aes(x=Date, y=totalFav),stat="identity", fill="red", colour="sienna3")
  
  
  ggplot(b) + geom_bar(aes(x=Date, y=totalFav),stat="identity", fill="red", colour="sienna3")
  
  
  ## Plots Stacked
  
  df_melt = melt(b, id.vars = 'Date')
  
  ggplot(df_melt, aes(x = Date, y = value,colour=variable)) + geom_line()
    
  ggplot(df_melt, aes(x = Date, y = value,colour=variable)) + geom_bar(stat="identity")
  
  
  # Grouped
  ggplot(df_melt, aes(fill=variable, y=value, x=Date)) + 
    geom_bar(position="dodge", stat="identity")
  
  # Stacked
  ggplot(df_melt, aes(fill=variable, y=value, x=Date)) + 
    geom_bar( stat="identity")
  
  
  
  ###########  ########################## Sentiment Analysis ##############  ##########################
  
  
  
  
  ### import package "sentimentr"
  library(sentimentr)
  VA.tweetdf <- read_csv("/Users/georgesericcolbert/Tim.tweetdf.13.21.csv")
  VA.tweetdf <- filter(VA.tweetdf, screenName != 'MikeWebbNow')
  
  
  VA.tweetdf$text <- gsub("[^0-9A-Za-z///' ]", "",VA.tweetdf$text)
  #### remove http link
  VA.tweetdf$text <- gsub("http\\w+", "",VA.tweetdf$text)
  #### remove rt
  VA.tweetdf$text <- gsub("rt", "",VA.tweetdf$text)
  ### remove at
  VA.tweetdf$text <- gsub("@\\w+", "",VA.tweetdf$text)
  
  VA.tweetdf$text <- tolower(VA.tweetdf$text)
  
  
  emo_R_tweets <- sentiment(VA.tweetdf$text)
  #View(emo_R_tweets)
  
  VA.tweetdf$sentiment <- emo_R_tweets$sentiment  ## attaching the sentiment measure to every tweet
  #View(R.tweetdf)
  
  ############Top 10 Negative Tweets
  
  
  
  ### grabbing positive tweets, sample of 
  
  
  
  #positive_tweets <- head(unique(VA.tweetdf[order(VA.tweetdf$sentiment, decreasing = T),c(2,18)]),10)
  #positive_tweets
  
  #grabbing negative tweets
  #negative_tweets <- head(unique(VA.tweetdf[order(emo_R_tweets$sentiment),c(2,18)]),10)
  #negative_tweets
  
  I<- VA.tweetdf[order(VA.tweetdf$sentiment, decreasing = FALSE),]
  I<-   filter(I, isRetweet == 'FALSE')
  I <-  head(select(I,text,retweetCount,favoriteCount, created,screenName),10)
  
  as.data.frame(I)
  

  ###########Top 10 Positive Tweets
  
  
  ### grabbing positive tweets, sample of 
  
  
  
  #positive_tweets <- head(unique(VA.tweetdf[order(VA.tweetdf$sentiment, decreasing = T),c(2,18)]),10)
  #positive_tweets
  
  #grabbing negative tweets
  #negative_tweets <- head(unique(VA.tweetdf[order(emo_R_tweets$sentiment),c(2,18)]),10)
  #negative_tweets
  
  I<- VA.tweetdf[order(VA.tweetdf$sentiment, decreasing = TRUE),]
  I<-   filter(I, isRetweet == 'FALSE')
  I <-  head(select(I,text,retweetCount,favoriteCount, created,screenName),10)
  
  as.data.frame(I)
  
   
  ###### 
  
  b <- subset(B, sentiment < -1)
  
  ########################## WorldCloud of words used in top 1000 most negateive and most positive tweets
  
  
  
  
  
  positive_tweets <- head(unique(VA.tweetdf[order(VA.tweetdf$sentiment, decreasing = T),c(2,18)]),1000)
  #positive_tweets
  
  #grabbing negative tweets
  negative_tweets <- head(unique(VA.tweetdf[order(emo_R_tweets$sentiment),c(2,18)]),1000)
  
  #negative_tweets
  
  
  write.table(positive_tweets$text, file = "/Users/georgesericcolbert/Desktop/Projects/Political Tweets/WordCloud/positive.txt",sep = "")
  write.table(negative_tweets$text, file = "/Users/georgesericcolbert/Desktop/Projects/Political Tweets/WordCloud/negative.txt",sep = "")
  
  #### 
  library(tm)
  tweet.Corpus.2 <- Corpus(DirSource(directory = "/Users/georgesericcolbert/Desktop/Projects/Political Tweets/WordCloud"))
  summary(tweet.Corpus.2)
  
  library(tm)
  
  clean.tweet.Corpus.2 <- tm_map(tweet.Corpus.2, tolower)
  clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, removeWords, stopwords())
  clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, content_transformer(removeNumPunct))
  clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, removePunctuation)
  clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, stripWhitespace)
  clean.tweet.Corpus.2 <- tm_map(clean.tweet.Corpus.2, stemDocument) 
  
  ###### TermDocumentMatrix and DocumentTermMatrix do the same thing
  tc2_tdm <- TermDocumentMatrix(clean.tweet.Corpus.2)
  
  tc2_matrix <- as.matrix(tc2_tdm)
  
  colnames(tc2_matrix) <- c("negative Tweets", "Postive Tweets")
  comparison.cloud(tc2_matrix, max.words = 40, random.order = FALSE)
  
  
  
  
  ################################### ################################### ################################### ###################################
 
 
  Stew.tweet <- searchTwitter("coreystewartva", n = 2000, lang = 'en', since = '2018-05-27', until = '2018-06-04')
 
 
  Stew.tweetdf <- twListToDF(Stew.tweet)
 
  write.csv(Stew.tweetdf, file =  "Stew.tweetdf.28.03.csv")
  
  Stew.tweetdf <- read_csv("Stew.tweetdf.07.17.csv")
  
  Stew.tweetdf.2 <- read_csv("Stew.tweetdf.17.23.csv")
  
  Stew.tweetdf.3 <- read_csv("Stew.tweetdf.17.23.csv")
  
  Stew.tweetdf <- rbind(Stew.tweetdf,Stew.tweetdf.2, Stew.tweetdf.3)
  
  ##### How many are retweets?
  
  table(Stew.tweetdf$isRetweet)
  
  
  #######
  tweet.Corpus <- Corpus(VectorSource(Stew.tweetdf$text))
  tweet.Corpus <- tm_map(tweet.Corpus, removeWords, stopwords())
  
  remove_url <- function(x) gsub("http[^[:space:]]*","",x)
  tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(remove_url))
  
  
  ##### remove anything other than english letters and space
  
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
  tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(removeNumPunct))
  tweet.Corpus <- tm_map(tweet.Corpus, removePunctuation)
  tweet.Corpus<- tm_map(tweet.Corpus, content_transformer(tolower))
  tweet.Corpus <- tm_map(tweet.Corpus, stripWhitespace)
  
  myStopWords <- c("virginia", "republican", "gop", 2018, "primary", " corey", "stewart", "coreystewartva") 
  
  tweet.Corpus <-tm_map(tweet.Corpus, removeWords,myStopWords)
  
  tweet.Corpus <- tm_map(tweet.Corpus, stemDocument)   ### if stemDocument doesn't run, install the package 'SnowballC'
  ##

  
 
  
  dtm <- DocumentTermMatrix(tweet.Corpus)
  
  freq <- colSums(as.matrix(dtm)) 
  
  library(wordcloud)
  dark2 <- brewer.pal(6, "Dark2")
  wordcloud(names(freq), freq, min.freq = 20, rot.per=0.2, colors=dark2,random.order = F) 
  
  ### find  tweets that use most frequent words
  
  
  mike <- Stew.tweetdf[grep("ColMorrisDavi",Stew.tweetdf$text),]
  
  #### filter out retweets to only get original tweets
  mike <- distinct(mike,text,.keep_all=TRUE)
  mike <- mike[order(mike$retweetCount, decreasing=TRUE),]
  
  head(select(mike,text,screenName,retweetCount,created), n=7)
  
  #######
  
  mike <- Stew.tweetdf[grep("IngrahamAngl",Stew.tweetdf$text),]
  
  #### filter out retweets to only get original tweets
  mike <- distinct(mike,text,.keep_all=TRUE)
  mike <- mike[order(mike$retweetCount, decreasing=TRUE),]
  
  head(select(mike,text,screenName,retweetCount,created), n=7)
  
  
  ########
  
  mike <- Stew.tweetdf[grep("LouDobb",Stew.tweetdf$text),]
  
  #### filter out retweets to only get original tweets
  mike <- distinct(mike,text,.keep_all=TRUE)
  mike <- mike[order(mike$retweetCount, decreasing=TRUE),]
  
  head(select(mike,text,screenName,retweetCount,created), n=7)
  
  
  
  
  ####
  
  mike <- Stew.tweetdf[grep("DLoesch",Stew.tweetdf$text),]
  
  #### filter out retweets to only get original tweets
  mike <- distinct(mike,text,.keep_all=TRUE)
  mike <- mike[order(mike$retweetCount, decreasing=TRUE),]
  
  head(select(mike,text,screenName,retweetCount,created), n=7)
  
  
  #######
  
  mike <- Stew.tweetdf[grep("EWErickson",Stew.tweetdf$text),]
  
  #### filter out retweets to only get original tweets
  mike <- distinct(mike,text,.keep_all=TRUE)
  mike <- mike[order(mike$retweetCount, decreasing=TRUE),]
  
  head(select(mike,text,screenName,retweetCount,created), n=7)
  
  
  
  
  ##### inspect frequent words
  (freq.terms <- findFreqTerms(dtm, lowfreq = 50))
  

  
  # Plot Word Frequencies 
  
  wf <- data.frame(word=names(freq), freq=freq) 
  wf <- wf[order(wf$freq, decreasing=TRUE),]
  head(wf, n = 20) 
  
  library(ggplot2)   
  
  plot.tweet <- ggplot(subset(wf, freq>75), aes(x = reorder(word,-freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  plot.tweet   
  
  
  ## Find Associations
  
  
  findAssocs(dtm,"republican", 0.1)
  
  findAssocs(dtm,"coreystewartva", 0.1)
  
  
  
  ##### Most favorite tweets of the week talking about coreystewartva
  library(dplyr)
  Stew.tweetdf <- Stew.tweetdf[order(Stew.tweetdf$favoriteCount, decreasing=TRUE),]
  
  head(select(Stew.tweetdf,text,favoriteCount,created,screenName))
  head(Stew.tweetdf$text)
  
  
  ##### Most retweeted tweets of the week talking about coreystewartva
  Stew.tweetdf <- Stew.tweetdf[order(Stew.tweetdf$retweetCount, decreasing=TRUE),]
  
  head(select(Stew.tweetdf,text,retweetCount,created,screenName))
  head(Stew.tweetdf$text)
  
  
  
  ### other than from coreystewart's account, most favorited 
  temp <- filter(Stew.tweetdf, screenName != 'CoreyStewartVA')
  temp <- temp[order(temp$favoriteCount, decreasing=TRUE),]
  head(select(temp,text,favoriteCount,created,screenName))
  
  ### other than from coreystewart's account, most retweeted
  temp <- filter(Stew.tweetdf, screenName != 'CoreyStewartVA')
  temp <- temp[order(temp$retweetCount, decreasing=TRUE),]
  head(select(temp,text,retweetCount,created,screenName))
  
  
  
  ###### Accounts that have the highest amounts of Retweets
  
  
  ### accounts that have the most total Retweets mentioning Corey Stewart
   x <- aggregate(temp$retweetCount, by=list(screenName=temp$screenName), FUN=sum)
  x <- x[order(x$x, decreasing=TRUE),]
  head(x)
  
  ####### example of tweets from said accounts
  ## HDowning113
  temp <- filter(Stew.tweetdf, screenName == 'HDowning113')
  temp <- temp[order(temp$retweetCount, decreasing=TRUE),]
  head(select(temp,text,screenName,retweetCount))
  
  ## TheGunGuy85
  temp <- filter(Stew.tweetdf, screenName == 'TheGunGuy85')
  temp <- temp[order(temp$retweetCount, decreasing=TRUE),]
  head(select(temp,text,screenName,retweetCount))
  
  
  ### Lynnlittle08
  temp <- filter(Stew.tweetdf, screenName == 'Lynnlittle08')
  temp <- temp[order(temp$retweetCount, decreasing=TRUE),]
  head(select(temp,text,screenName,retweetCount))
  
  
  ##Nov2018election
  
  temp <- filter(Stew.tweetdf, screenName == 'Nov2018election')
  temp <- temp[order(temp$retweetCount, decreasing=TRUE),]
  head(select(temp,text,screenName,retweetCount))
  
  
  ######### 
  
  library(ggplot2)
  library(scales)
  
  
  #Plotting how often coreystewartva is tweeted per day
  Stew.tweetdf$Date <- as.Date(Stew.tweetdf$created)
  
  time.freq <- as.data.frame(table(Stew.tweetdf$Date))
  colnames(time.freq) <- c("Date","TweetsperDay")
  time.freq$Date <- as.Date(time.freq$Date)
  
  ggplot(data = time.freq,aes(Date,TweetsperDay))+geom_bar(stat="identity")+scale_x_date(labels = date_format("%Y-%m-%d"))
  
  
  
  
  
  
  ####################################################### ################################### ############################
  
  
  nick.tweet <- searchTwitter("nickforva", n = 7000, lang = 'en', since = '2018-06-04', until = '2018-06-08')
  
  
  nick.tweetdf <- twListToDF(nick.tweet)
  
  nick.tweetdf.2$X1 <- NULL
  nick.tweetdf.2$X1_1 <- NULL
  
  write.csv(nick.tweetdf, file =  "nick.tweetdf.04.07.csv")
  

  
  nick.tweetdf <- read_csv("nick.tweetdf.26.05.csv")
  
  nick.tweetdf.2 <- read_csv("nick.tweetdf.26.05.csv")
  
  
  
  nick.tweetdf <- rbind(nick.tweetdf,nick.tweetdf.2)
  
  
  ##### How many are retweets?
  
  table(nick.tweetdf$isRetweet)
  
  
  #######
  tweet.Corpus <- Corpus(VectorSource(nick.tweetdf$text))
  tweet.Corpus <- tm_map(tweet.Corpus, removeWords, stopwords())
  
  remove_url <- function(x) gsub("http[^[:space:]]*","",x)
  tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(remove_url))
  
  
  ##### remove anything other than english letters and space
  
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
  tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(removeNumPunct))
  tweet.Corpus <- tm_map(tweet.Corpus, removePunctuation)
  tweet.Corpus<- tm_map(tweet.Corpus, content_transformer(tolower))
  tweet.Corpus <- tm_map(tweet.Corpus, stripWhitespace)
  
  myStopWords <- c("virginia", "republican", "gop", 2018, "Primary", "nick", "freitas", "nickforva") 
  
  tweet.Corpus <-tm_map(tweet.Corpus, removeWords,myStopWords)
  
  
  tweet.Corpus <- tm_map(tweet.Corpus, stemDocument)   ### if stemDocument doesn't run, install the package 'SnowballC'
  ##
  
  
  #Corpus.copy <- tweet.Corpus
  
  #tweet.Corpus.X <- tweet.Corpus
  
  #tweet.Corpus.X <- tm_map(tweet.Corpus.X, stemDocument)   ### if stemDocument doesn't run, install the package 'SnowballC'
  #tweet.Corpus.X <- tm_map(tweet.Corpus.X, content_transformer(stemCompletion), dictionary = Corpus.copy)
  
  
  #tweet.Corpus.X <- tm_map(tweet.Corpus.X, PlainTextDocument)
  
  
  ####create DocumentTermMatrix
  
  
  
  tweet.dtm <- DocumentTermMatrix(tweet.Corpus)
  
  
  
  
  tweet.matrix <- as.matrix(tweet.dtm)
  
  
  freq <- colSums(as.matrix(tweet.dtm)) 
  
  
  ##### worldcloud 
  
  library(wordcloud)
  dark2 <- brewer.pal(6, "Dark2")
  wordcloud(names(freq), freq, min.freq = 25, rot.per=0.2, colors=dark2,random.order = F) 
  
  
  
  
  
  
  #### most frequently used words
  
  
  
  # Plot Word Frequencies 
  
  wf <- data.frame(word=names(freq), freq=freq) 
  wf <- wf[order(wf$freq, decreasing=TRUE),]
  head(wf, n = 20) 
  library(ggplot2)   
  
  plot.tweet <- ggplot(subset(wf, freq>200), aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  plot.tweet   

  ##### Most favorite tweets of the week talking about nickforVA
  library(dplyr)
  nick.tweetdf <- nick.tweetdf[order(nick.tweetdf$favoriteCount, decreasing=TRUE),]
  
  head(select(nick.tweetdf,text,favoriteCount,created,screenName))
  #head(nick.tweetdf$text)
  
  ##### Most retweeted tweets of the week talking about nickforva
  nick.tweetdf <- nick.tweetdf[order(nick.tweetdf$retweetCount, decreasing=TRUE),]
  
  head(select(nick.tweetdf,text,retweetCount,created,screenName))
  #head(nick.tweetdf$text)
  
  ### other than from coreystewart's account, most favorited 
  temp <- filter(nick.tweetdf, screenName != 'NickForVA')
  temp <- temp[order(temp$favoriteCount, decreasing=TRUE),]
  head(select(temp,text,favoriteCount,created,screenName))
  
  ### other than from coreystewart's account, most retweeted
  temp <- filter(nick.tweetdf, screenName != 'NickForVA')
  temp <- temp[order(temp$retweetCount, decreasing=TRUE),]
  head(select(temp,text,retweetCount,created,screenName))
  
  
  ### accounts that have the most total Retweets mentioning Corey Stewart
  x <- aggregate(temp$retweetCount, by=list(screenName=temp$screenName), FUN=sum)
  x <- x[order(x$x, decreasing=TRUE),]
  head(x)
  
  
  library(ggplot2)
  library(scales)
  
  
  #Plotting how often nickforva is tweeted per day
  nick.tweetdf$Date <- as.Date(nick.tweetdf$created)
  
  time.freq <- as.data.frame(table(nick.tweetdf$Date))
  colnames(time.freq) <- c("Date","TweetsperDay")
  time.freq$Date <- as.Date(time.freq$Date)
  
  ggplot(data = time.freq,aes(Date,TweetsperDay))+geom_bar(stat="identity")+scale_x_date(labels = date_format("%Y-%m-%d"))
  
  
  ####################################################### ################################### ############################
  
  ewjack.tweet <- searchTwitter("ewjacksonsr", n = 3000, lang = 'en', since = '2018-06-03', until = '2018-06-10')
  
  ewjack.tweetdf <- twListToDF(ewjack.tweet)
  
  
  ##### How many are retweets?
  
  table(ewjack.tweetdf$isRetweet)
  
  
  #######
  tweet.Corpus <- Corpus(VectorSource(ewjack.tweetdf$text))
  tweet.Corpus <- tm_map(tweet.Corpus, removeWords, stopwords())
  
  remove_url <- function(x) gsub("http[^[:space:]]*","",x)
  tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(remove_url))
  
  
  ##### remove anything other than english letters and space
  
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
  tweet.Corpus <- tm_map(tweet.Corpus, content_transformer(removeNumPunct))
  tweet.Corpus <- tm_map(tweet.Corpus, removePunctuation)
  tweet.Corpus<- tm_map(tweet.Corpus, content_transformer(tolower))
  tweet.Corpus <- tm_map(tweet.Corpus, stripWhitespace)
  
  myStopWords <- c("virginia", "republican", "gop", 2018, "primary", "ew", "jackson", "ewjacksonsr") 
  
  tweet.Corpus <-tm_map(tweet.Corpus, removeWords,myStopWords)
  
  
  tweet.Corpus <- tm_map(tweet.Corpus, stemDocument)   ### if stemDocument doesn't run, install the package 'SnowballC'
  ##
  
  
  #Corpus.copy <- tweet.Corpus
  
  #tweet.Corpus.X <- tweet.Corpus
  
  #tweet.Corpus.X <- tm_map(tweet.Corpus.X, stemDocument)   ### if stemDocument doesn't run, install the package 'SnowballC'
  #tweet.Corpus.X <- tm_map(tweet.Corpus.X, content_transformer(stemCompletion), dictionary = Corpus.copy)
  
  
  #tweet.Corpus.X <- tm_map(tweet.Corpus.X, PlainTextDocument)
  
  
  ####create DocumentTermMatrix
  
  
  
  tweet.dtm <- DocumentTermMatrix(tweet.Corpus)
  
  
  
  
  tweet.matrix <- as.matrix(tweet.dtm)
  
  
  freq <- colSums(as.matrix(tweet.dtm)) 
  
  
  ##### worldcloud 
  
  library(wordcloud)
  dark2 <- brewer.pal(6, "Dark2")
  wordcloud(names(freq), freq, min.freq = 10, rot.per=0.2, colors=dark2,random.order = F) 
  
  
  
  
  
  
  #### most frequently used words
  
  
  
  wf <- data.frame(word=names(freq), freq=freq)   
  head(wf)  
  
  library(ggplot2)   
  
  plot.tweet <- ggplot(subset(wf, freq>10), aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  plot.tweet   
  
  ##### Most favorite tweets of the week talking about coreystewartva
  library(dplyr)
  ewjack.tweetdf <- ewjack.tweetdf[order(ewjack.tweetdf$favoriteCount, decreasing=TRUE),]
  
  head(select(ewjack.tweetdf,text,favoriteCount,created,screenName))
  head(ewjack.tweetdf$text)
  
  ##### Most retweeted tweets of the week talking about nickforva
  ewjack.tweetdf <- ewjack.tweetdf[order(ewjack.tweetdf$retweetCount, decreasing=TRUE),]
  
  head(select(ewjack.tweetdf,text,retweetCount,created,screenName))
  head(ewjack.tweetdf$text)
  
  ### other than from coreystewart's account, most favorited 
  temp <- filter(ewjack.tweetdf, screenName != '')
  temp <- temp[order(temp$favoriteCount, decreasing=TRUE),]
  head(select(temp,text,favoriteCount,created,screenName))
  
  ### other than from coreystewart's account, most retweeted
  temp <- filter(nick.tweetdf, screenName != 'NickForVA')
  temp <- temp[order(temp$retweetCount, decreasing=TRUE),]
  head(select(temp,text,retweetCount,created,screenName))
  
  
  ### accounts that have the most total Retweets mentioning Corey Stewart
  x <- aggregate(temp$retweetCount, by=list(screenName=temp$screenName), FUN=sum)
  x <- x[order(x$x, decreasing=TRUE),]
  head(x)
  
  
  library(ggplot2)
  library(scales)
  
  
  #Plotting how often nickforva is tweeted per day
  ewjack.tweetdf$Date <- as.Date(ewjack.tweetdf$created)
  
  time.freq <- as.data.frame(table(ewjack.tweetdf$Date))
  colnames(time.freq) <- c("Date","TweetsperDay")
  time.freq$Date <- as.Date(time.freq$Date)
  
  ggplot(data = time.freq,aes(Date,TweetsperDay))+geom_bar(stat="identity")+scale_x_date(labels = date_format("%Y-%m-%d"))
  
  
  
  
 
  
  
  
  