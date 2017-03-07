library(RCurl)
library(XML)
library(tm)
library(magrittr)

urls <- c("E:/Sonali_MS/ITMD529/data_mining/Merck/Cocacola_2016.htm", 
          "E:/Sonali_MS/ITMD529/data_mining/Merck/Cocacola_2015.html",
          "E:/Sonali_MS/ITMD529/data_mining/Merck/Cocacola_2014.html")

 QA.Section <- plainTxt <- list()

  html.files <- readLines(urls[3])
  html = htmlTreeParse(html.files, useInternal=TRUE)
  plainTxt[3] <- toString(xpathApply(html, "//body//text()
                                  [not(ancestor::script)][not(ancestor::style)]", xmlValue))

cat(plainTxt[[1]], file="E:/Sonali_MS/ITMD529/data_mining/Merck/temp.txt", sep="n", append = TRUE)
cocacola.files <-readLines("E:/Sonali_MS/ITMD529/data_mining/Merck/temp.txt")

qa_sentence_start <- "ANSWERS ABOUT THE ANNUAL MEETING AND VOTING" 
grep(qa_sentence_start, cocacola.files, ignore.case = TRUE)

qa_sentence_end <- "DIRECTOR NOMINATIONS FOR THE 2017 ANNUAL MEETING OF SHAREHOLDERS" 
qa_sentence_end <- "REQUIREMENTS FOR SUBMITTING PROXY"
grep(qa_sentence_end, merck.files, ignore.case = TRUE)

QA.Section_2016 <- merck.files[9505:20293]
QA.Section_2015 <- merck.files[29959:30236]
cat(QA.Section_2016, file="E:/Sonali_MS/ITMD529/data_mining/Merck//qa_section/qa_2016.txt", sep="n", append = TRUE)
cat(QA.Section_2015, file="E:/Sonali_MS/ITMD529/data_mining/Merck//qa_section/qa_2015.txt", sep="n", append = TRUE)

cname <- "E:/Sonali_MS/ITMD529/data_mining/Merck//qa_section"
qa_sections<-Corpus(DirSource(cname))
summary(qa_sections)

viewDocs <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs(qa_sections, 1)

#To lower case:
qa_sections <- tm_map(qa_sections, content_transformer(tolower))

#Remove Numbers:
  qa_sections <- tm_map(qa_sections, removeNumbers)
#Remove Stop Words:
  qa_sections <- tm_map(qa_sections, removeWords, stopwords("english"))
viewDocs(qa_sections, 1)

length(stopwords("english")) 
stopwords("en")

#Replacing certain expressions with spaces:
 toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ",x))
 qa_sections <- tm_map(qa_sections, toSpace, "/|<|>|"|=|@|\\|:|;|-|,.?--()\"")
 viewDocs(qa_sections, 1)
 
 #Remove Punctuations:
qa_sections <- tm_map(qa_sections, removePunctuation)
 #Punctuation characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
 #Strip white spaces:
 qa_sections <- tm_map(qa_sections, stripWhitespace)
  viewDocs(qa_sections, 1)
  
  qa_sections <- tm_map(qa_sections, removeWords, c("n", "â???", "a", "i", "â", "nn","nj","k",
                                                    "can", "also", "e", "mail", "via","indicate","named","pages",
                                                    "with","following","either","using","will","ivs",
                                                    "if", "may", "help", "us", "will","well","date","provided",
                                                    "please", "unless", "visit","need","close","also","preferences","persons",
                                                    "nnn", "address","pm","eastern","time","see","plus",
                                                    "break","always",""))
  
  