library(tidyverse)
var.yrs <- 10
biz.days <- 252
opt_steps <- 100
hist.len <- var.yrs * biz.days
EUROSUFFIX<-c('.VI', '.BR', '.TL', '.HE', '.NX', '.PA', '.BE', '.BM', '.DU', '.F', '.HM', '.HA', '.MU', '.SG', '.DE', '.AT', '.IR', '.TI', '.MI', '.RG', '.VS', '.AS', '.LS', '.MC')
USDSUFFIX<-c('.CBT', '.CME', '.NYB', '.CMX', '.NYM', '.USA')
SUP_Currency<-c('USD','EUR')
CurrencyInfo <- read.csv(file = 'addInfo.csv',sep = ';')
SAMPLE_ticker <- read.csv(file = 'collections.csv',sep = ';')
SAMPLE_ticker_by_collection <- SAMPLE_ticker %>% select(collection) %>%  group_by(collection) %>% summarise()
str_serv <- "http://127.0.0.1:8000"