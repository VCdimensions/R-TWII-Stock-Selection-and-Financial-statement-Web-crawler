############抓每日股價前兩百名
rm(list = ls())
library(RCurl)
library(XML)
library(quantmod)
library(TTR)


#########################找出股價前兩百名的股票
url <- "http://www.wantgoo.com/Stock/TWStock/Stat?type=%E6%88%90%E4%BA%A4%E5%83%B9"
html <- getURL(url,.encoding = 'utf-8')
html <- htmlParse(html,encoding="utf-8")

StkNum <- xpathApply(html,"/html//div[@id='wrap']//div[@class='mainCont']//table[@class='tb rw5n tbhl']//tr/td[@class='lt']",xmlValue)
StkNum <- unlist(StkNum) #股價前兩百名的股票代號

StkName <- xpathApply(html,"/html/body/div[@id='wrap']//table[@class='tb rw5n tbhl']/tbody/tr/td[@class='cn']",xmlValue)
StkName <- unlist(StkName) #股價前兩百名的股票名稱

Stock <- data.frame(StkNum = StkNum, StkName = StkName)

##########################剔除F-"開頭的公司和公司資料為null的
SelectedStkNum <- as.character(Stock[!grepl('KY',StkName) & nchar(StkNum) == 4,1])

#########################判斷矩陣
condition <- matrix(0,ncol=length(SelectedStkNum),nrow = 7)
colnames(condition)<- as.character(SelectedStkNum)
rownames(condition)<- paste("condition",1:7)

################################################
####獲利能力-剔除上市不到兩年且四年中有其中一年虧損的公司 且營益率波動不大
####近一年營收年增率都為正數  且近兩年淨現金流量為正
################################################
for(i in 1:length(SelectedStkNum)){
  
  stk<-SelectedStkNum[i]
  
  url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCE/ZCE.DJHTM?A=%s",stk)
  html <- getURL(url, .encoding = "big5")
  html <- iconv(html,'big5','utf8')
  html <- htmlParse(html,encoding = "utf-8")
  
  data <- xpathSApply(html,"//table[@id='oMainTable']//td[@class]",xmlValue)
  data<- as.vector(data[13:length(data)])
  if(is.null(data)) next
  data <- matrix(data,ncol = 10,byrow = T)
  
  data <- na.omit(data)
  if(any(data=="") ) next
  
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
  
  earning <- data$'稅後淨利'
  earning1 <- sum(earning[1:4])
  earning2 <- sum(earning[5:8])
  earning3 <- sum(earning[9:12])
  earning4 <- sum(earning[13:16])
  cond1 <- cbind(earning1,earning2,earning3,earning4) 
  cond1_1 <- ((!is.na(cond1[1]))&(!is.na(cond1[2])))==T ###將小於兩年的公司剔除 TRUE時為大於兩年
  cond1[is.na(cond1)] <- 0 #將NA換為零
  cond1_2 <- sum(cond1<0)==0 #四年淨利都要為正 TRUE 為四年都正公司
  
  if(cond1_1&&cond1_2){condition[1,i]<-1}else{condition[1,i]<-0}  #上市超過兩年且四年沒有虧損
 
  
   
  dat <- data
  dat <- data$"營益率%"
  dat <- abs(dat)
  change <- rev(ROC(rev(dat)))[which(!is.na(rev(ROC(rev(dat)))[1:8]))] #近八季營益率變動率
  cond2 <- sum(abs(change) < 0.25)>=5 #季和季變動率超過25% 不超過三季
  if(cond2==T){condition[2,i] <- 1}else{condition[2,i] <- 0}
  
  
  
  url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZCH/ZCH.DJHTM?A=%s",stk)
  html <- getURL(url, .encoding = "big5")
  html <- iconv(html,'big5','utf8')
  html <- htmlParse(html,encoding = "utf-8")
  
  data <- xpathSApply(html,"//table[@id='oMainTable']//tr/td",xmlValue)
  if(is.null(data)) next
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
 
  data <- na.omit(data)
  if(any(data=="") ) next
  
  dat <- data$"年增率%"
  cond3 <- sum(dat[1:12][!is.na(dat[1:12])] >=0)    #近12個月營收年增率大於零月數
  if(cond3==12){condition[3,i]=1}else{condition[3,i]=0}
  
  
  
  url <- sprintf("https://djinfo.cathaysec.com.tw/Z/ZC/ZC3/ZC3.DJHTM?A=%s",stk)
  html <- getURL(url, .encoding = "big5")
  html <- iconv(html,'big5','utf8')
  html <- htmlParse(html,encoding = "utf-8")
  
  data <- xpathSApply(html,"//table[@class='t01']//tr/td",xmlValue)
  if(is.null(data)) next
  time <- data[3:10]
  data <- data[-c(1:10)]
  name <- data[c(seq(1,length(data),9))]
  name <- name[-c(length(name))]
  data <- data[-c(seq(1,length(data),9))]
  data <- gsub("N/A","0",data)
  data <- gsub(",","",data)

  data <- na.omit(data)
  if(any(data=="") ) next
  
    if(length(data)<312){condition[4,i]=0}else{             #小於兩年公司 cond4值接等於零
    data <- matrix(as.numeric(data),ncol = 8,byrow = T)
    rownames(data)=c(name)
    colnames(data)=c(time)
  
    begin <- data[36,][length(data[36,])] #期初現金流量
    end <- data[37,][1] #期末現金流量
    cond4 <- (end-begin)>=0   ##近兩年淨現金流量為正
    if(cond4){condition[4,i]=1}else{condition[4,i]=0}
  }
print(stk)
}

#####################################
######所有條件都符合的股票代碼
#####################################
condition <- as.data.frame(condition)
colnames(condition)[which(apply(condition,2,sum)==4)]  #滿足四個條件

names <- colnames(condition)[which(apply(condition,2,sum)==4)]

