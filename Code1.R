#Onetime setup
install.packages("devtools")
devtools::install_github("crubba/htmltable")
library(htmltab)
library(XML)

#Yahoo
url <- "E:/Sonali_MS/ITMD529/data_mining/yahoo.html"
tables = readHTMLTable(url)
length(tables)
i<-395
for(i in length(tables):1){
 p<-tables[[i]]
 if(sum(grepl("Principal", p[,1],ignore.case = TRUE) && grepl("Position", p[,1],ignore.case = TRUE))>0 && !is.null(p))
    {
    proxy.2015 <- htmltab(doc = url, which = i)
    break
    }
}

proxy.2015 <- htmltab(doc = url, which = "//*[text()[contains(.,'Principal')] and text()[contains(.,'Position')]]/ancestor::table",header = 2)
compensation.table.data <- proxy.2015[2:4, c("V1", "V4","V8","V16","V20")]
compensation.table.data <-rename(compensation.table.data, c("V1"="Name and Principal Position", "V4"="Year", "V8"="Salary", "V16"="Stock_Awards", "V20"="Option_Awards"))
compensation.table.data

#MASCO CORPORATION
url <- "E:/Sonali_MS/ITMD529/data_mining/masco.txt"
compensation.2015 <- htmltab(doc = url, which = "//*[text()[contains(.,'Principal')] and text()[contains(.,'Position')]]/ancestor::table")
compensation.table.2015 <- compensation.2015[3:4, c("V1", "V4","V8","V12","V16")]
compensation.table.2015 <-rename(compensation.table.2015, c("V1"="Name and Principal Position", "V4"="Year", "V8"="Salary", "V12"="Stock_Awards", "V16"="Option_Awards"))
compensation.table.2015

#Pentair
url <- "E:/Sonali_MS/ITMD529/data_mining/Pentair.txt"
proxy.2015 <- htmltab(doc = url, which = "//*[text()[contains(.,'Principal')] and text()[contains(.,'Position')]]/ancestor::table",header = 4,encoding = "UTF-8")
compensation.table.data <- proxy.2015[3:5, c("V1", "V2","V3","V6","V8")]
compensation.table.data <-rename(compensation.table.data, c("V1"="Name and Principal Position", "V2"="Year", "V3"="Salary", "V6"="Stock_Awards", "V8"="Option_Awards"))
compensation.table.data

#Lifelock
url <- "E:/Sonali_MS/ITMD529/data_mining/lifelock.txt"
proxy.2015 <- htmltab(doc = url, which = "//*[text()[contains(.,'Principal')] and text()[contains(.,'Position')]]/ancestor::table")
compensation.table.data <- proxy.2015[5:7, c("V1", "V4","V8","V16","V20")]
compensation.table.data <-rename(compensation.table.data, c("V1"="Name and Principal Position", "V4"="Year", "V8"="Salary", "V16"="Stock_Awards", "V20"="Option_Awards"))
compensation.table.data