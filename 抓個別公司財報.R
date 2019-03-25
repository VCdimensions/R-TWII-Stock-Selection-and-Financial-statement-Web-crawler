############抓每日股價前兩百名
rm(list = ls())
gc()
library(RCurl)
library(XML)
 
#########################找出股價前兩百名的股票
url <- "http://www.wantgoo.com/Stock/TWStock/Stat?type=%E6%88%90%E4%BA%A4%E5%83%B9"
html <- getURL(url,.encoding = 'utf-8')
html <- htmlParse(html,encoding="utf-8")

StkNum <- xpathApply(html,"/html//div[@id='wrap']//div[@class='mainCont']//table[@class='tb rw5n tbhl']//tr/td[@class='lt']",xmlValue)
StkNum <- unlist(StkNum) #股價前兩百名的股票代號

StkName <- xpathApply(html,"/html/body/div[@id='wrap']//table[@class='tb rw5n tbhl']/tbody/tr/td[@class='cn']",xmlValue)
StkName <- unlist(StkName) #股價前兩百名的股票名稱

Stk <- data.frame(StkNum = StkNum, StkName = StkName)


##########################經營績效
library(RCurl)
library(XML)

rm(list=ls())
stkno<-9941 #

url<-sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCD/ZCD.DJHTM?A=%s",stkno)
html <- getURL(url, .encoding = "big5")
html <- iconv(html,'big5','utf8')
html <- htmlParse(html,encoding = "utf-8")

data <- xpathSApply(html,"//table[@id='oMainTable']//td[@class='t3n1']",xmlValue)
data2 <- xpathSApply(html,"//table[@id='oMainTable']//tr/td[@class='t3n0']",xmlValue)
data <- sub(",","",data)
data <- matrix(as.numeric(data),ncol = 7,byrow = T)
data <- data.frame(data2,data,stringsAsFactors = F)
colnames(data)=c("季別","加權平均股本","營業收入","稅前淨利","稅後淨利","每股營收(元)",
                 "稅前每股盈餘(元)","稅後每股盈餘(元)")

#######################獲利能力
rm(list=ls())
stkno<- 2542 #

url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCE/ZCE.DJHTM?A=%s",stkno)
html <- getURL(url, .encoding = "big5")
html <- iconv(html,'big5','utf8')
html <- htmlParse(html,encoding = "utf-8")

data <- xpathSApply(html,"//table[@id='oMainTable']//td[@class]",xmlValue)
data<- data[13:length(data)]
data <- matrix(data,ncol = 10,byrow = T)

time <- data[,1]
data<- data[,2:length(data[1,])]
data<- gsub(",","",data) 
data<- sub("%","",data)
data<- as.numeric(data)
data<- matrix(data,ncol = 9,byrow =F)
data[,4]<- data[,4]/100
data[,6]<- data[,6]/100
data<- data.frame(time,data)
colnames(data)<- c("季別","營業收入","營業成本","營業毛利","毛利率%","營業利益",
                   "營益率%","業外收支","稅前淨利","稅後淨利")

###################營收盈餘
library(RCurl)
library(XML)

rm(list=ls())
stkno<- 9941 #

url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCH/ZCH.DJHTM?A=%s",stkno)
html <- getURL(url, .encoding = "big5")
html <- iconv(html,'big5','utf8')
html <- htmlParse(html,encoding = "utf-8")

data <- xpathSApply(html,"//table[@id='oMainTable']//tr/td",xmlValue)
data <- data[13:length(data)]
data <- gsub("," , "",data)
data <- matrix(data,ncol = 7,byrow = T)
time <- data[,1]
data <- data[,2:length(data[1,])] 
data <- sub("%","",data)
data <- matrix(as.numeric(data),ncol=6,byrow = F)
data[,2] <- data[,2]/100
data[,4] <- data[,4]/100
data[,6] <- data[,6]/100
data <- data.frame(time,data)
colnames(data)=c("年/月","合併營收","月增率%","去年同期","年增率%","累計營收","累計營收年增率%")

###################現金流量表
library(RCurl)
library(XML)

rm(list=ls())
stkno<- 2330 #

url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZC3/ZC3.DJHTM?A=%s",stkno)
html <- getURL(url, .encoding = "big5")
html <- iconv(html,'big5','utf8')
html <- htmlParse(html,encoding = "utf-8")

data <- xpathSApply(html,"//table[@class='t01']//tr/td",xmlValue)
time <- data[3:10]
data <- data[-c(1:10)]
name <- data[c(seq(1,length(data),9))]
name <- name[-c(length(name))]
data <- data[-c(seq(1,length(data),9))]
data <- gsub("N/A","0",data)
data <- gsub(",","",data)
data <- matrix(as.numeric(data),ncol = 8,byrow = T)
rownames(data)=c(name)
colnames(data)=c(time)
