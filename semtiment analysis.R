# Load data
data <- read.csv2("testdata.manual.2009.06.14.csv", sep = ",", quote = '"')

names(data) <- c("fromid", "toid", "date", "userfrom", "userto", "msg")

# Ectract text msg and write to file
text <- data[, "msg"]
write.csv(text, file = "temp/data.txt")

install.packages("tm")
install.packages("wordcloud")
library("tm")
library("wordcloud")
# Corpus need a dir not a vector apparently :O :/ wtf
lords <- Corpus(DirSource("temp/"))

# Add filters
lords <- tm_map(lords, stripWhitespace)
lords <- tm_map(lords, content_transformer(tolower))
lords <- tm_map(lords, removeWords, stopwords("english"))
lords <- tm_map(lords, stemDocument)

# Plot wordcloud
wordcloud(lords, scale=c(5,0.5), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

################
# Sentiment analysis
################

library(RCurl)
library(RJSONIO)
library(rjson)
library(stringr)
library(tm)
library(wordcloud)
install.packages('devtools')
require('devtools')
install_github('mananshah99/sentR')
require('sentR')
# Max 1000 requests per day
# So please only do 1 runthrough of the getSentiment loop as it takes 497 requests
# The key is associated to me, but i left it here so you could try to execute it.
# You might also want to execute after the 27th as i did it on this date
apikey = "4a572cc48b46ae91fd40eaa31b878101"

data <- read.csv2("testdata.manual.2009.06.14.csv", sep = ",", quote = '"')

names(data) <- c("fromid", "toid", "date", "userfrom", "userto", "msg")

# Ectract text msg and write to file
tweet_txt <- data[, "msg"]


getSentiment <- function (text, key){
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  
  data <- getURL(paste("http://api.datumbox.com/1.0/TwitterSentimentAnalysis.json?api_key=", key, "&text=",text, sep=""))
  
  js <- RJSONIO::fromJSON(data, asText=TRUE);
  
  # get mood probability
  sentiment = js$output$result
  
  return(list(sentiment=sentiment))
}

clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

# clean text
tweet_clean = clean.text(tweet_txt)
tweet_num = length(tweet_clean)
# data frame (text, sentiment)
tweet_df = data.frame(text=tweet_clean, sentiment=rep("", tweet_num),stringsAsFactors=FALSE)

# apply function getSentiment
sentiment = rep(0, tweet_num)
for (i in 1:tweet_num)
{
  tmp = getSentiment(tweet_clean[i], apikey)
  
  tweet_df$sentiment[i] = tmp$sentiment
}

# delete rows with no sentiment
tweet_df <- tweet_df[tweet_df$sentiment!="",]


#separate text by sentiment
sents = levels(factor(tweet_df$sentiment))

# get the labels and percents
labels <-  lapply(sents, function(x) paste(x,format(round((length((tweet_df[tweet_df$sentiment ==x,])$text)/length(tweet_df$sentiment)*100),2),nsmall=2),"%"))

nemo = length(sents)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = tweet_df[tweet_df$sentiment == sents[i],]$text
  
  emo.docs[i] = paste(tmp,collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = labels

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
