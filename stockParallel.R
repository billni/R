gc()
starttime <- Sys.time()

###########################################
stocklist <- read.table("stocklist.txt", colClasses=c("character"), col.names=c("code"))

checkmarket <- function(stock) {
  ifelse(substr(stock, 1, 1)==6, "sh", "sz")
}
stocklist$market <- apply(stocklist, 2, checkmarket)
stocklist$market <- paste(stocklist$code, stocklist$market, sep=".")


getstock <- function(stocklist, i) {  
  library(RCurl)
  library(XML)  
  d <- debugGatherer()
  html.url <- paste("http://stockdata.stock.hexun.com/zlkp/s", stocklist[i, "code"], ".shtml", sep="")
  html.page <- htmlTreeParse(getURL(html.url, .opts = list(debugfunction=d$update,verbose = TRUE)), useInternalNode=T)
  html.content.1 <- getNodeSet(doc=html.page, path = "//div[@class='s_box']//p[@class='text_01']")
  content <- getChildrenStrings(html.content.1[[1]])
  text <- enc2utf8(content[2])
  textint <- utf8ToInt(text)
  text <- intToUtf8(textint[which(textint<60 & textint>45 | textint==38170 | textint==21348)])
  #yyyy
  start <- regexpr("20" , text)
  end <- unlist(gregexpr(intToUtf8(38170) , text))
  stocklist[i, "main.cost.year"] <- as.numeric(substr(text, start, end-1))
  #month day
  start <- regexpr(intToUtf8(38170) , text)
  end <- start+4
  stocklist[i, "main.cost.month.day"] <- as.numeric(substr(text, start+1, end))
  # main cost
  start <- regexpr(intToUtf8(21348) , text)
  end <- nchar(text)
  stocklist[i, "main.cost.value"]<- as.numeric(substr(text, start+1, end))
  ######################
  real.stock.url <- paste("http://bdcjhq.hexun.com/quote?s2=", stocklist[i, "market"], sep="")  
  real.stock.page <- htmlTreeParse(getURL(real.stock.url, .opts = list(debugfunction=d$update, verbose = TRUE)), useInternalNode=T, encoding="GBK")
  real.stock.content <- getNodeSet(doc=real.stock.page, path = "//script")
  content <- getChildrenStrings(real.stock.content[[2]])
  text <- enc2utf8(content)
  start <- regexpr("pc", text)
  if (start != -1) {
    end <-  regexpr("time", text)
    text <- substring(text, start, end-2)
    text <- gsub("\"", "", text)
    text <- gsub("na", "name", text)
    text <- gsub("pc", "yesterday", text)
    text <- gsub("op", "open", text)
    text <- gsub("vo", "volumn", text)
    text <- gsub("tu", "turn.volumn", text)
    text <- gsub("hi", "high", text)
    text <- gsub("lo", "low", text)
    text <- gsub("la", "lastest.value", text)
    text <- gsub("type", "type", text)
    text <- gsub("time", "time", text)
    text <- gsub("sy", "pe", text)
    text <- gsub("lt", "circulating.shares", text)
    text <- gsub("sz", "market.value", text)
    text <- gsub("hs", "turnover.rate", text)
    splitedtext <- strsplit(text, ",")
    ltext <- lapply(splitedtext, "strsplit", ":")
    stocklist[i, "yesterday"]<- ltext[[1]][[1]][2]
    stocklist[i, "open"] <- ltext[[1]][[2]][2]
    stocklist[i, "volumn"] <- ltext[[1]][[3]][2]
    stocklist[i,"turn.volumn"] <- ltext[[1]][[4]][2]
    stocklist[i, "high"] <- ltext[[1]][[5]][2]
    stocklist[i, "low"] <- ltext[[1]][[6]][2]
    stocklist[i, "lastest.value"] <- ltext[[1]][[7]][2]
    stocklist[i, "type"] <- ltext[[1]][[8]][2]
    print(stocklist[i,])
  } else {
    gc()
  }
}


###########################################
library(foreach)
library(doParallel)
n <- nrow(stocklist)

cl <- makeCluster(40)
registerDoParallel(cl)
result <- foreach(j=1:n, .combine="rbind", .errorhandling="remove") %dopar% getstock(stocklist, j)
stopCluster(cl)
filename <- paste("stocklist_", Sys.Date(), ".csv", sep="")
write.csv(result, filename)
print(Sys.time()-starttime)


#############################################
recommend <- result[which(as.numeric(result$main.cost.value)-as.numeric(result$lastest.value)>0 & result$lastest.value>0),c("code", "main.cost.value","lastest.value")]
##############################################
library(rjson)
library(RCurl)
library(XML)  
gc()
starttime <- Sys.time()

d <- debugGatherer()
stocks <- paste(recommend[1:nrow(recommend), "code"], collapse = ",")
stockurl <- paste("http://data.10jqka.com.cn/ajax/stockpick.php?code=", stocks, sep="")  
jsonData <- fromJSON(getURL(stockurl, .opts = list(debugfunction=d$update, verbose = TRUE)))
jsonData
###############################
