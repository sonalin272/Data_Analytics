library("tm")
library(htmltab)
library(XML)
library(xlsx) #load the package

#Create final table
final.summary.table <- data.frame(Company_Name=character(),
                                  Name_and_Principal_Position=character(),
                                  Year=character(),
                                  Salary=character(),
                                  Stock_Awards=character(),
                                  Option_Awards=character(),
                                  stringsAsFactors=FALSE)

#Set directory and save all files in this dir using standard naming convension:: company name followed by filing year e.g Morgan_2015.htm
cname <- "E:/Sonali_MS/ITMD529/data_mining/dataset"
length(dir(cname))
dir(cname)
proxies <- Corpus(DirSource(cname))

#Fetch all files from corpus
#Notes: 12:2007 2003 file is not working as parsing is not proper
#8:2003 to 1998 files not working as xpath is not able to retrive table so has to give table no. manually in htmltab for these files

#for loop in descending order so that recent filing will get executed first
file_index<-1
for(file_index in length(dir(cname)):1){
  url <- paste(cname,meta(proxies[[file_index]],"id"),sep = "/")
  comp_name<-substr(meta(proxies[[file_index]],"id"),1,nchar(meta(proxies[[file_index]],"id"))-9)
  tables = readHTMLTable(url)
  length(tables)
  for(i in length(tables):1){
    p<-tables[[i]]
      if(sum(grepl("Principal", p[,1],ignore.case = TRUE))>0 && sum(grepl("Position", p[,1],ignore.case = TRUE))>0 && !is.null(p))
      {
        proxy.table <- htmltab(doc = url, which = i)
        flag<-1
        break
      }
  }
  
  #proxy.table <- htmltab(doc = url, which = "//*[text()[contains(.,'Principal')] 
   #                   and text()[contains(.,'Name')]]/ancestor::table")
  
  #Remove garbage values
  garbage_value<-c("Â—","Â—Â—","Â Â Â Â","Â Â Â","Â—Â Â","ÂÂ","Â","$","â€")
  for(i in 1:length(proxy.table)){
    for(x in garbage_value){
      proxy.table[,i]<-gsub(x,"",proxy.table[,i])   
      proxy.table[,i][proxy.table[,i] == ""]<-NA
    }
  }
  
  #Remove unwanted columns and rename headers
  header_arr <- c("Name", "Year", "Salary", "Stock","Option")
  names(header_arr) <- c("Name_and_Principal_Position", "Year", "Salary", "Stock_Awards","Option_Awards")
  x<-""
  j<-1
  i<-1
  flag<-0
  
  while(i != (length(proxy.table)+1)){
    for(x in header_arr){
      if(grepl(x,proxy.table[i],ignore.case = TRUE)|(grepl(x,names(proxy.table[i]),ignore.case = TRUE)))
      {  colnames(proxy.table)[i]<-names(header_arr[j])
      flag=1
      break
      }
      j<-j+1 
    }
    if(flag == 0)
      proxy.table[,i]<-NULL  
    else
      i<-i+1
    j<-1
    flag<-0
  }
  
  #Remove extra year column if any 
  #Year column is never null so if year values doesn't appear in it then delete that column
  #as it's a garbage column
  year<-as.character(c(1994:2015))
  if(sum(proxy.table[1:nrow(proxy.table),2] %in% year)==0)
    proxy.table[,2]<-NULL 
  
  #remove null rows
  proxy.table <- proxy.table[rowSums(is.na(proxy.table)) != ncol(proxy.table),]
  proxy.table <-proxy.table[proxy.table$Year %in% year,]

  #To select CEO rows
  value<-""
  
  for(i in 1:nrow(proxy.table)){
    if(proxy.table$Year[i] %in% year)
    {  first_occurance<-i
    value=proxy.table$Year[i]
    break
    }
  }
  second_occurance<-first_occurance+1
  i<-i+1
  while(i != nrow(proxy.table)){
    if(proxy.table$Year[i] == value | is.na(proxy.table$Year[i]))
    {  second_occurance<-i
    break
    }
    i<-i+1 
  }
  
  proxy.table <-proxy.table[first_occurance:(second_occurance-1),]
  #compensation.table.data <-proxy.2015[!duplicated(proxy.2015["Year"]),]
  
  #Remove null columns and add required columns back as it should match final table format
  proxy.table <- proxy.table[,colSums(is.na(proxy.table)) != nrow(proxy.table)] 
  for(x in header_arr){
    if(sum(grepl(x,colnames(proxy.table),ignore.case = TRUE)) == 0){
      if(x == "Stock")
        proxy.table[,"Stock_Awards"]<-""
      if(x == "Option")
        proxy.table[,"Option_Awards"]<-""
    }
  }
  #Merge tables 
  for(i in 1:nrow(proxy.table)){
    if(!as.integer(proxy.table$Year[i]) %in% final.summary.table$Year)
    { final.summary.table[sapply(final.summary.table, is.factor)] <- lapply(final.summary.table[sapply(final.summary.table, is.factor)], as.character)
    final.summary.table <- rbind(final.summary.table,c(Company_Name=comp_name,proxy.table[i,]))
    }
  }
}

write.xlsx(x = final.summary.table, file = "E:/Sonali_MS/ITMD529/data_mining/dataset/Text_Mining1.xlsx",
           sheetName = "Morgan_Stanley", row.names = FALSE)
