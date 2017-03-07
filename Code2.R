library("tm")
library(htmltab)
#Create final table
final.summary.table <- data.frame(Company_Name=character(),
                                  Name_and_Principal_Position=character(),
                                  Year=character(),
                                  Salary=character(),
                                  Stock_Awards=character(),
                                  Option_Awards=character(),
                                  stringsAsFactors=FALSE)
								  
#Yahoo
url <- "E:/Sonali_MS/ITMD529/data_mining/dataset/Morgan_Stanley_2015.htm"
#Extract summary table using keyword search
proxy.table <- htmltab(doc = url, which = "//*[text()[contains(.,'Principal')] 
                       and text()[contains(.,'Name')]]/ancestor::table")
					   
#Remove garbage columns
  i<-2
  while(i != (length(proxy.table)+1)){
    proxy.table[,i] <- replace(as.character(proxy.table[,i]), grep("Â", substr(proxy.table[,i], 1,nchar("Â")) ,fixed=TRUE) , NA)
    proxy.table[,i] <- replace(as.character(proxy.table[,i]), grep("â€", substr(proxy.table[,i], 1,nchar("â€")) ,fixed=TRUE) , NA)
    proxy.table[,i][proxy.table[,i] == "$"]<-NA
    i<-i+1
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
  
  #To select CEO rows
  i<-1
  value<-""
  
  while(i != nrow(proxy.table)){
    if(proxy.table$Year[i] %in% year)
    {  first_occurance<-i
    value=proxy.table$Year[i]
    break
    }
    i<-i+1 
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
  
  #Remove null columns
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
  i<-1
  while(i != (nrow(proxy.table)+1)){
    if(!as.integer(proxy.table$Year[i]) %in% final.summary.table$Year)
    { final.summary.table[sapply(final.summary.table, is.factor)] <- lapply(final.summary.table[sapply(final.summary.table, is.factor)], as.character)
    final.summary.table <- rbind(final.summary.table,c(Company_Name="Ford",proxy.table[i,]))
    }
    i<-i+1 
  }