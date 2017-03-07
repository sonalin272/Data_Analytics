library(RCurl)
library(XML)
library(tm)
library(magrittr)
library(ggplot2)
#Store all file paths in the list
urls <- c("E:/Sonali_MS/ITMD529/data_mining/cocacola/Cocacola_2016.htm", 
          "E:/Sonali_MS/ITMD529/data_mining/cocacola/Cocacola_2015.html",
          "E:/Sonali_MS/ITMD529/data_mining/cocacola/Cocacola_2014.html",
          "E:/Sonali_MS/ITMD529/data_mining/cocacola/Cocacola_2013.html",
          "E:/Sonali_MS/ITMD529/data_mining/cocacola/Cocacola_2012.html",
          "E:/Sonali_MS/ITMD529/data_mining/cocacola/Cocacola_2011.html")

plainTxt <- list()

#Remove all xml tags and parse html
for(i in urls){
html.files <- readLines(urls[i])
html = htmlTreeParse(html.files, useInternal=TRUE)
plainTxt[i] <- toString(xpathApply(html, "//body//text()
                                   [not(ancestor::script)][not(ancestor::style)]", xmlValue))
}

#Copy file into temp file
cat(plainTxt[[1]], file="E:/Sonali_MS/ITMD529/data_mining/cocacola/temp.txt", sep="n", append = FALSE)
cocacola.files <-readLines("E:/Sonali_MS/ITMD529/data_mining/cocacola/temp.txt")

#Get the start and end of QA sections for all files and crop it
#----------------------------------------------------------
qa_sentence_start <- "Q&A WITH OUR CHAIRMAN" 
grep(qa_sentence_start, cocacola.files, ignore.case = TRUE)
qa_sentence_end <- "ANNUAL MEETING OF SHAREOWNERS" 
grep(qa_sentence_end, cocacola.files, ignore.case = TRUE)
QA.Section_2016 <- cocacola.files[404:602]

qa_sentence_start <- "Q&A FROM OUR CHAIRMAN" 
grep(qa_sentence_start, cocacola.files, ignore.case = TRUE)
qa_sentence_end <- "VOTING INFORMATION"
grep(qa_sentence_end, cocacola.files, ignore.case = TRUE)
QA.Section_2015 <- cocacola.files[471:653]

qa_sentence_start <- "Q&A WITH OUR CHAIRMAN" 
grep(qa_sentence_start, cocacola.files, ignore.case = TRUE)
qa_sentence_end <- "ANNUAL MEETING OF SHAREOWNERS" 
grep(qa_sentence_end, cocacola.files, ignore.case = TRUE)
QA.Section_2014 <- cocacola.files[366:602]

#----------------------------------------------------
#Store data into text files
cat(QA.Section_2016, file="E:/Sonali_MS/ITMD529/data_mining/cocacola//qa_section/qa_2016.txt", sep="n", append = FALSE)
cat(QA.Section_2015, file="E:/Sonali_MS/ITMD529/data_mining/cocacola//qa_section/qa_2015.txt", sep="n", append = FALSE)
cat(QA.Section_2014, file="E:/Sonali_MS/ITMD529/data_mining/cocacola//qa_section/qa_2014.txt", sep="n", append = FALSE)

#Create corpus of files
cname <- "E:/Sonali_MS/ITMD529/data_mining/cocacola/qa_section"
qa_sections<-Corpus(DirSource(cname))
summary(qa_sections)

#View doc using function
viewDocs <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs(qa_sections, 3)

#To lower case:
qa_sections <- tm_map(qa_sections, content_transformer(tolower))

#Remove Numbers:
qa_sections <- tm_map(qa_sections, removeNumbers)

#Remove Stop Words:
qa_sections <- tm_map(qa_sections, removeWords, stopwords("english"))
viewDocs(qa_sections, 1)

#Check the stopwords
length(stopwords("english")) 
stopwords("en")

#Replacing certain expressions with spaces:
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))
qa_sections <- tm_map(qa_sections, toSpace, "/|<|>|”|=|@|\\|:|;|-|--\"")
viewDocs(qa_sections, 1)

#Remove Punctuations:
qa_sections <- tm_map(qa_sections, removePunctuation)
#Punctuation characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
#Strip white spaces:
qa_sections <- tm_map(qa_sections, stripWhitespace)
viewDocs(qa_sections, 1)

#Remove non-required words
qa_sections <- tm_map(qa_sections, removeWords, c("n", "â€", "a", "i", "â", "nn","nj","k","the","address","variety","questions",
                                                  "can", "also", "e", "mail", "via","indicate","named","page","going","section", "currently","place","degree","said","way","every","many","various",                                              "with","following","either","using","will","ivs","comments","commonly","across","includes",
                                                  "if", "may", "help", "us", "will","well","date","provided","many","around","begins",
                                                  "please", "unless", "visit","need","close","also","preferences","persons","thinking",
                                                  "nnn", "address","pm","eastern","time","see","plus","still","ahead","topics","account",
                                                  "break","always","active","actions","achieve","adopt","added","actionsnwe","accessnthere","accessnas","little",
                                                  "lot","made","make","making","managed","makes","looking","look","line","like","light","momentum","much",
                                                  "received","recently","restore","including","back","contents","addition","additional","combined",
                                                  "plain","english","read","year","value","one","companyâ€™s","serve","believe","ensure"))

#Create term doc matrix
qa_dtm <- DocumentTermMatrix(qa_sections)
qa_dtm
inspect(qa_dtm[1:3, 650:690])

#Store sum of columns
freq <- colSums((as.matrix(qa_dtm)))
length(freq)

#Sort the frequency table
ord <- order(freq)
#Check the lower and higher freqency words
freq[head(ord)]
freq[tail(ord,25)]

#Remove sparse terms
#Keep value as0.6 so if term appears in 2/3 of docs then it will keep the terms
dtms <- removeSparseTerms(qa_dtm, 0.6)
dim(dtms)
inspect(dtms)
freq <-colSums(as.matrix(dtms))
#Sort the frequency table
ord <- order(freq)
#Again heck the lower and higher freqency words after sparse term removal
freq[head(ord)]
freq[tail(ord,25)]

#Check higher and lower frequency terms after sparse term removal
#set the threshold looking at term matrix
findFreqTerms(dtms, lowfreq=7)
findFreqTerms(dtms, highfreq=2)

#find association with cola word
assoc_word<-findAssocs(dtms, "diversity", corlimit = .9)
associated.terms <- data.frame(word=assoc_word)
class(associated.terms)
associated.terms.matrix<-data.frame(word=rownames(associated.terms),value=associated.terms)

#Sort matrix of terms
freq <- sort(colSums(as.matrix(dtms)), decreasing = TRUE)
#Get the high frequent words
head(freq, 30)
wf <- data.frame(word=names(freq), freq=freq)
head(wf)

#Plot the graph of words
install.packages("ggplot2")
subset(wf, freq<6) %>% ggplot(aes(word, freq))
subset(wf, freq>6)
#Plot the graph of words
install.packages("dplyr")
library(dplyr)
subset(wf, freq>6) %>% ggplot(aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

# Generating a Word Cloud
install.packages("wordcloud")
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=7,random.color=TRUE,colors=rainbow(7),random.order = FALSE)

