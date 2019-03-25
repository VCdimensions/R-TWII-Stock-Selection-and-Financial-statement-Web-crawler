rm(list = ls())
library(RCurl)
library(XML)
######################################
stkno <- c(  "3008", "3691", "6409", "1476", "6414", "8044" ,"3611" ,"3552" ,"2474", "8299",
             "3558" ,"3152" ,"1707" ,"6504" ,"1264" ,"9910", "2228" ,"4426" ,"4107" ,"1536",
             "4965", "8086", "8016" ,"5388" ,"2317" ,"4979" ,"9941")

#######################################
PEgbpercent <- 0.2  
PEgood <-1+PEgbpercent
PEnormal <- 1
PEbed <- 1-PEgbpercent
########################################
REVgbpercent <- 0.1  
REVgood <-1+REVgbpercent
REVnormal <- 1
REVbed <- 1-PEgbpercent
################################
################################
################################

pricePREdict <- NULL
for(i in 1:length(stkno)){
  stk <- stkno[i]
  
  url <- paste("https://djinfo.cathaysec.com.tw/Z/ZC/ZCX/ZCXNEWCATHAYSEC.DJHTM?A=",stk,sep="")
  html <- getURL(url,.encoding = 'big5')
  html <- iconv(html,'big5','utf-8')
  html <- htmlParse(html,encoding='utf-8')
  ####################
  data <- xpathApply(html,"//td[@class='t0']/table[@class='t01'][1]//tr[5]/td[@class='t3n1'][1]",xmlValue)
  PE <- as.numeric(data)
  ######################
  data <- xpathApply(html,"//td[@class='t0']/table[@class='t01'][1]//tr[4]/td[@class='t3n1'][1]",xmlValue) 
  nowPE <- as.numeric(data)
  #################
  data <- xpathApply(html,"//table[@class='t01'][1]//tr[2]/td[@class='t3n1'][4]",xmlValue) 
  data <- gsub(",","",data)
  nowprice <- as.numeric(data)
  
  ##############################
  url<-sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCD/ZCD.DJHTM?A=%s",stk)
  html <- getURL(url, .encoding = "big5")
  html <- iconv(html,'big5','utf8')
  html <- htmlParse(html,encoding = "utf-8")
  
  data <- xpathSApply(html,"//table[@id='oMainTable']//tr/td",xmlValue)
  data <- data[10:length(data)]
  data <- matrix(data,ncol = 8,byrow = T)
  
  time <- data[,1]
  data<- data[,2:length(data[1,])]
  data<- gsub(",","",data) 
  data<- as.numeric(data)
  data<- matrix(data,ncol = 7,byrow =F)
  dat<- data.frame(time,data)
  
  colnames(dat)=c("季別","加權平均股本","營業收入","稅前淨利","稅後淨利","每股營收(元)",
                  "稅前每股盈餘(元)","稅後每股盈餘(元)")
  ###########
  EPS14 <- sum(dat$'稅後每股盈餘(元)'[4:7]) 
  
  ####################################
  url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCE/ZCE.DJHTM?A=%s",stk)
  html <- getURL(url, .encoding = "big5")
  html <- iconv(html,'big5','utf8')
  html <- htmlParse(html,encoding = "utf-8")
  
  data <- xpathSApply(html,"//table[@id='oMainTable']//td[@class]",xmlValue)
  data <- data[13:length(data)]
  data <- matrix(data,ncol = 10,byrow = T)
  
  time <- data[,1]
  data<- data[,2:length(data[1,])]
  data<- gsub(",","",data) 
  data<- sub("%","",data)
  data<- as.numeric(data)
  data<- matrix(data,ncol = 9,byrow =F)
  data[,4]<- data[,4]/100
  data[,6]<- data[,6]/100
  dat1<- data.frame(time,data)
  colnames(dat1)<- c("季別","營業收入","營業成本","營業毛利","毛利率%","營業利益",
                     "營益率%","業外收支","稅前淨利","稅後淨利")
  
  ################################
  url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCH/ZCH.DJHTM?A=%s",stk)
  html <- getURL(url,.encoding = "big5")
  html <- iconv(html,"big5","utf8")
  html <- htmlParse(html,encoding = "utf-8")
  
  data <- xpathSApply(html,"//table//tr[1]//table[@id='oMainTable']//tr/td",xmlValue)
  data <- data[13:length(data)]
  data <- matrix(data,ncol = 7,byrow = T)
  
  time <- data[,1]
  data<- data[,2:length(data[1,])]
  data<- gsub(",","",data) 
  data<- sub("%","",data)
  data<- as.numeric(data)
  data<- matrix(data,ncol = 6,byrow =F)
  data[,2]<- data[,2]/100
  data[,4]<- data[,4]/100
  data[,6]<- data[,6]/100
  dat2<- data.frame(time,data)
  colnames(dat2)<- c("年/月","合併營收","月增率%","去年同期","年增率%","累計營收",
                     "累計年增率%")
  #############
  revenuerate11m <- mean(dat2$'年增率%'[1:11]) 
  
  ###############################
  EPS15Q3good <- sum(dat$'稅後每股盈餘(元)'[1:3]) + dat$'稅後每股盈餘(元)'[4]*REVgood
  EPS15Q3normal <- sum(dat$'稅後每股盈餘(元)'[1:3]) + dat$'稅後每股盈餘(元)'[4]*REVnormal
  EPS15Q3bed <- sum(dat$'稅後每股盈餘(元)'[1:3]) + dat$'稅後每股盈餘(元)'[4]*REVbed
  
  #############
  goodPE <- PE*PEgood
  normalPE <- PE*PEnormal
  bedPE <- PE*PEbed
  
  ##############
  goodnowPE <- nowPE*PEgood
  normalnowPE <- nowPE*PEnormal
  bednowPE <- nowPE*PEbed
  
  #################
  goodprice <- goodPE*EPS15Q3good
  normalprice <- normalPE*EPS15Q3normal
  bedprice <- bedPE*EPS15Q3bed
  GNBprice <- cbind(goodprice,normalprice,bedprice)
  
  nowgoodprice <- goodnowPE*EPS15Q3good
  nownormalprice <- normalnowPE*EPS15Q3normal
  nowbedprice <- bednowPE*EPS15Q3bed
  nowGNBprice <- cbind(nowgoodprice,nownormalprice,nowbedprice)
  
  stock <- stkno[i]
  stockprice <- cbind(stock,nowprice,GNBprice,nowGNBprice)
  
  pricePREdict <- rbind(pricePREdict,stockprice)
  
}

colnames(pricePREdict)=c("stock","nowprice","goodprice","normalprice","bedprice",
                         "nowgoodprice","nownormalprice","nowbedprice")




