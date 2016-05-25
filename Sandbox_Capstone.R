
require("RWeka")
require("tm")
install.packages("e1071", dependencies = TRUE, repos='http://cran.rstudio.com/')


#read in combined subset sheet 
cpsub<-read.csv("corpussubset.csv", stringsAsFactors = FALSE)


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

unisum<-rowSums(as.matrix(tdmuni))
bisum<-rowSums(as.matrix(tdmbi))
trisum<-rowSums(as.matrix(tdmtri))

#sort by decreasing
unifreq<-sort(unisum, decreasing=TRUE)
bifreq<-sort(bisum, decreasing=TRUE)
trifreq<-sort(trisum, decreasing=TRUE)

head(unifreq)
head(bifreq)
head(trifreq)


names(trifreq)

uni_unique<- unique(tdmuni$dimnames$Terms)



