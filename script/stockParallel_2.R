
############################年报公布时间########################################

# Mass Tradeoff 大宗交易
# http://stockdata.stock.hexun.com/dzjy/index.aspx
# http://stockdata.stock.hexun.com/dzjy/search.aspx

# 年报公布时间
# http://datainfo.hexun.com/wholemarket/html/cbcx.aspx

# 行业概念
# http://vol.stock.hexun.com/Data/Industry/RankDetail.ashx?period=1&&date=2015-03-19&groupby=0&addby=3&plate=1&count=10
gc()
starttime <- Sys.time()
Sys.setenv(http_proxy="http://niyong:111111yN@isa06:8008/")
library(rjson)
library(RCurl)
library(XML)  
html.url <- "http://vol.stock.hexun.com/Data/Industry/RankDetail.ashx?period=1&&date=2015-03-20&groupby=0&addby=3&plate=1&count=1"
html.page <- getURL(html.url, .encoding="gb2312", .opts = list(debugfunction=d$update,verbose = TRUE))
html.page.json <- substr(html.page, start=2, stop=nchar(html.page)-1)

data.name.start <- gregexpr("data0:", html.page.json)
data.name.start <- gregexpr("StockNameLink:", html.page.json)
data.stock.volumn <- gregexpr("data9:", html.page.json)

l<-gregexpr("data(\\d*):([\\d*]|[\\w*])", html.page.json)
l.stop <- lapply(l, "+", attr(l[[1]], "match.length"))
for (i in 1:9) {
  print(substr(html.page.json, start=l[[1]][i], stop=l.stop[[1]][i]))
}

fromJSON(json.text)
##############################################################

