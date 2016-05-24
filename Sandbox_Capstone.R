
library(RCurl)
data_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists( "SwiftKey_data.zip")){
  download.file(data_url, "SwiftKey_data.zip", method = "libcurl")

  # extract zip files
  unzip("SwiftKey_data.zip")
}


blogs<-readLines("~/final/en_US/en_US.blogs.txt", skipNul = TRUE, warn= FALSE)
news<-readLines("~/final/en_US/en_US.news.txt", skipNul = TRUE, warn=FALSE)
tweets<-readLines("~/final/en_US/en_US.twitter.txt", skipNul = TRUE, warn=FALSE)
  
 blogssize<-file.info("~/final/en_US/en_US.blogs.txt")$size/ 1024 ^ 2
 newssize<-file.info("~/final/en_US/en_US.news.txt")$size/ 1024 ^ 2
 tweetssize<-file.info("~/final/en_US/en_US.twitter.txt")$size/ 1024 ^ 2
 
 size<-c(blogssize,newssize,tweetssize)
 dfsize<-data.frame(size)
 row.names(dfsize) <- c("blogs size", "news size", "tweets size")
 dfsize
 
 blogssub<-blogs[1:1000]
 newssub<-news[1:1000]
 tweetssub<- tweets[1:1000]
 corpussubset<-c(blogssub,newssub,tweetssub)
 cpsub<-corpussubset
 
 suppressWarnings(require("RWeka"))
 suppressWarnings(require("tm"))
 
 #clean up sample corpus
 cpsubclean<-Corpus(VectorSource(cpsub))
 cpsubclean<-tm_map(cpsubclean, tolower)
 cpsubclean<-tm_map(cpsubclean, stripWhitespace)
 cpsubclean<-tm_map(cpsubclean, removePunctuation)
 cpsubclean<-tm_map(cpsubclean, removeNumbers)
 cpsubclean<-tm_map(cpsubclean, PlainTextDocument)
 
 #make ngrams 
 unigram<- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
 bigram<- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
 trigram<- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
 
 options(mc.cores=1) #hangs if you dont include this option on Mac OS
 
 tdmuni<- TermDocumentMatrix(cpsubclean, control=list(tokenize=unigram))
 tdmbi<- TermDocumentMatrix(cpsubclean, control=list(tokenize=bigram))
 tdmtri<- TermDocumentMatrix(cpsubclean, control=list(tokenize=trigram))
 
 findFreqTerms(tdmuni, lowfreq=100)
 findFreqTerms(tdmbi, lowfreq=50)
 findFreqTerms(tdmtri, lowfreq=10)
 
 