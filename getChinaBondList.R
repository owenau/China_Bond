library(magrittr)
library(rvest)
library(plyr)


#generate URL from inputs
getURL <- function(page = 1, bondType = "", couponType = "", issueYear = "", rtngShrt= ""){
  
  url <- paste("http://www.chinamoney.com.cn/fe/chinamoney/searchBondInfoForward.action?rtngShrt=",rtngShrt,
               "&pagingPage_il_=", page,"&issueEnty=&couponType=",couponType,"&enty=&issueYear=",issueYear,"&bondType=",
               bondType,"&bondName=&bondCode=&",sep = "")
  return(url)
  
}

getChinaBondList <- function(bondType = "", couponType = "", issueYear = "", rtngShrt= ""){

  #get the first page of the results
  webpage1 <- read_html(getURL(1, bondType, couponType, issueYear, rtngShrt))
  
  #get bond list from page 1
  mainTable <- listFromPage(webpage1)
  
  #get footer in the first page
  footer <- webpage1 %>% html_nodes(".market-note-1") %>% html_text
  
  
  #get total number of pages from the footer
  totalPage <- regmatches(footer,regexpr("1/[0-9]+",footer))
  totalPage <- substr(totalPage,3,nchar(totalPage))
  
  
  if (totalPage >=2){
    
    for (page in 2:totalPage){
      
      mainTable <- getURL(page, bondType, couponType, issueYear, rtngShrt) %>%
                    read_html %>%
                    listFromPage %>%
                    rbind(mainTable, .)
      
    }
    
  }
  
  colnames(mainTable) <- c('Name', 'Code', 'Issuer', 'BondType', 'IssueDate', 'LatestRating', 'Link')
  
  return(mainTable)
  
}

getChinaBondTC <- function(bondURL){
  
  #extract bond T&C and put into a matrix
  bondTC <- bondURL %>% 
            read_html %>%
            html_nodes(".bdr-rgt1+ .row2 , .bdr-rgt1") %>% 
            html_text %>% 
            gsub("\\s","",.) %>%  #remove white space from the table
            matrix(nrow = 2)
  
  output <- bondTC[2,]
  dim(output) <- c(1,length(output))
  colnames(output) <- bondTC[1,]
  
  return(output)
}

combinedOutput <- function(bondType = "", couponType = "", issueYear = "", rtngShrt= ""){
  
  bondTable <- getChinaBondList(bondType, couponType, issueYear, rtngShrt)
  
  bondTC <- lapply(bondTable[,"Link"],getChinaBondTC)
  bondTC <- do.call(rbind.fill.matrix,bondTC)
  
  output <- cbind(bondTable,bondTC)
  return(rbind(colnames(output),output))
  
}


#================================================================================================================
#get bond list from webpage, from the HTML elements.
listFromPage <- function(webpage){
  
  #extract list of bonds (a vector) and reshape it to a matrix
  bondList <- webpage %>%
              html_nodes(".dreport-row2 , .dreport-row1") %>%
              html_text %>%
              matrix(ncol = 6, byrow = TRUE)
  
  #extract bond URLs
  bondLinks <-  webpage %>% html_nodes("#searchBondInfoForward+ table td:nth-child(1) a") %>% html_attr("href")
  bondLinks <-  bondLinks[1:(length(bondLinks)-2)] %>% paste0("http://www.chinamoney.com.cn", .)
  
  return(cbind(bondList,bondLinks))
}