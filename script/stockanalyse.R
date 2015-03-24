gc()
starttime <- Sys.time()
Sys.setenv(http_proxy="http://niyong:111111yN@isa06:8008/")
library(rjson)
library(RCurl)
library(XML)

###########################################
stocklist <- read.table("stocklist.txt", colClasses=c("character"), col.names=c("code"))
d <- debugGatherer()
checkmarket <- function(stock) { 
  ifelse(substr(stock, 1, 1)==6, "sh", "sz")
}
stocklist$market <- apply(stocklist, 2, checkmarket)
stocklist$market <- paste(stocklist$code, stocklist$market, sep=".")

n <- nrow(stocklist)
for(i in 1:100) {
  html.url <- paste("http://stockdata.stock.hexun.com/zlkp/s", stocklist[i, "code"], ".shtml", sep="")
  html.page <- htmlTreeParse(getURL(html.url, .opts = list(debugfunction=d$update,verbose = TRUE)), useInternalNode=T)
  html.content.1 <- getNodeSet(doc=html.page, path = "//div[@class='s_box']//p[@class='text_01']")
  content <- getChildrenStrings(html.content.1[[1]])  
  text <- enc2utf8(content[2])
  
  textint <- utf8ToInt(text)
  text <- intToUtf8(textint[which(textint<60 & textint>45 | textint==38170 | textint==21348)])
  
  #yyyy
  start <- regexpr("20" , text)
  ends <- unlist(gregexpr(intToUtf8(38170) , text))
  
  stocklist[i, "maincost.year"] <- as.numeric(substr(text, start, ends-1))
  
  #month day
  start <- regexpr(intToUtf8(38170) , text)
  ends <- unlist(gregexpr(intToUtf8(21348) , text))
  stocklist[i, "maincost.month.day"] <- as.numeric(substr(text, start+1, ends-3))
  
  # main cost
  start <- regexpr(intToUtf8(21348) , text)
  ends <- nchar(text)
  stocklist[i, "main.cost.value"]<- as.numeric(substr(text, start+1, ends))
  
  ######################
  real.stock.url <- paste("http://bdcjhq.hexun.com/quote?s2=", stocklist[i, "market"], sep="")
  #real.stock.url <- "http://bdcjhq.hexun.com/quote?s2=000001.sh"
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

if (FALSE) {}
library(foreach)
# 启用parallel作为foreach并行计算的后???
library(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
# 并行计算方式
foreach(j=1:n) %dopar% getstock(j)
stopCluster(cl)
}
write.table(stocklist, "stocklist.csv")

#############################################
#test
if (FALSE) {  
  library(RCurl)
  library(XML)  
  
  d <- debugGatherer()
  html.url <- paste("http://stockdata.stock.hexun.com/zlkp/s", stocklist[1, "code"], ".shtml", sep="")
  html.page <- htmlTreeParse(getURL(html.url, .opts = list(debugfunction=d$update,verbose = TRUE)), useInternalNode=T)
  html.content.1 <- getNodeSet(doc=html.page, path = "//div[@class='s_box']//p[@class='text_01']")
  content <- getChildrenStrings(html.content.1[[1]])
  text <- enc2utf8(content[2])
  textint <- utf8ToInt(text)
  text <- intToUtf8(textint[which(textint<60 & textint>45 | textint==38170 | textint==21348)])
  #yyyy
  start <- regexpr("20" , text)
  end <- unlist(gregexpr(intToUtf8(38170) , text))  
  maincost.year <- as.numeric(substr(text, start, end-1))
  
  #month day
  start <- regexpr(intToUtf8(38170) , text)
  end <- start+4
  maincost.month.day <- as.numeric(substr(text, start+1, end))
  
  # main cost
  start <- regexpr(intToUtf8(21348) , text)
  end <- nchar(text)
  main.cost.value<- as.numeric(substr(text, start+1, end))
}


#############get real stock information#################3

real.stock.url <- "http://bdcjhq.hexun.com/quote?s2=00638.sz"
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
  text <- gsub("tu", "turn_volumn", text)
  text <- gsub("hi", "high", text)
  text <- gsub("lo", "low", text)
  text <- gsub("la", "lastest_value", text)
  text <- gsub("type", "type", text)
  text <- gsub("time", "time", text)
  text <- gsub("sy", "pe", text)
  text <- gsub("lt", "circulating_shares", text)
  text <- gsub("sz", "market_value", text)
  text <- gsub("hs", "turnover_rate", text)
  
  splitedtext <- strsplit(text, ",")
  ltext <- lapply(splitedtext, "strsplit", ":")
  yesterday<- ltext[[1]][[1]][2]
  open <- ltext[[1]][[2]][2]
  volumn <- ltext[[1]][[3]][2]
  turn_volumn <- ltext[[1]][[4]][2]
  high <- ltext[[1]][[5]][2]
  low <- ltext[[1]][[6]][2]
  lastest_value <- ltext[[1]][[7]][2]
  type <- ltext[[1]][[8]][2]
}


#####to unicode###
a <- utf8ToInt(text)
intToUtf8(a[which(a<256)])


###############################3
jsonall <- list()
for (i in 0:24) {
  stocks <- paste(stocklist[(i*100) : (i*100+99),], collapse = ",")
  stockurl <- paste("http://data.10jqka.com.cn/ajax/stockpick.php?code=", stocks, sep="")  
  jsonData <- fromJSON(getURL(stockurl, .opts = list(debugfunction=d$update,verbose = TRUE)))  
  jsonall <- c(jsonall, jsonData)
}
for (i in 1:length(jsonall)) {
  print(jsonall[[i]][[1]]$title)
}


#############################################################
path <- "stocklist"
files <- dir(path, full.names=T)
data.stock <- data.frame()
for(file.name in files) {
  data.tmp <- read.table(file.name, header=T, blank.lines.skip = TRUE, stringsAsFactors=F)
  data.stock <- rbind(data.stock, data.tmp)
}


#############################################################

