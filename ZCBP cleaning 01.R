###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
detach()
#zbp01detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2001/zbp01detail.txt')
#zip_2010_0 <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
#save(zip_2010_0, file = 'R code/zip_2010_0.Rda')
#save(zbp01detail_o, file = 'R code/zbp01detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp01detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names01<- c('restaurant01', 'cafeterias01', 'dessert01', 'fastfood01', 'bakery01', 'bars01', 'conv01', 'gasconv01', 'warehouse01', 'grocery01', 'liquor01')
naics01 <-data.frame(naics, naics_names01)

#convert factor into string
naics01<- data.frame(lapply(naics01, as.character), stringsAsFactors = FALSE)

#merge zbp data with naics data to subset data
#merge zbp data with naics data to subset data
zip <- data.frame(zbp01detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp01detail<- inner_join(zbp01detail_o, naics01, by = "naics")
zbp01detail<- full_join(zip, zbp01detail, by = 'zip')
zbp01detail<-zbp01detail[,-2]
zbp01detail[is.na(zbp01detail)]<-0
#save(zbp01detail, file = 'R code/zbp01detail.Rda')

#load data filtered by naics codes
#load('R code/zbp01detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp01detail<- inner_join(zbp01detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp01detailwide<-data.table::dcast(data.table(zbp01detail), zip~ naics_names01, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp01detail[!duplicated(zbp01detail$zip),c(1,14,15)]

#create wide data
zbp01detailwide<- inner_join(zbp01detailwide, state, by = "zip")
zbp01detailwide[is.na(zbp01detailwide)]<-0
save(zbp01detailwide, file = "R code/zbp01detailwide.Rda")

#creating outcome variables
load("R code/zbp01detailwide.Rda")
zbp01detailwide<-mutate(zbp01detailwide, food01 = est_bakery01 + est_cafeterias01 + est_dessert01 + est_fastfood01 + est_restaurant01)
zbp01detailwide<-mutate(zbp01detailwide, alcohol01 = est_bars01 + est_liquor01)
zbp01detailwide<-mutate(zbp01detailwide, freshfood01 = (est_grocery01 - (n1_4_grocery01+n5_9_grocery01 + n10_19_grocery01))+est_warehouse01)
zbp01detailwide<-mutate(zbp01detailwide, unfreshfood01 = n1_4_grocery01+n5_9_grocery01 + n10_19_grocery01+ est_conv01+est_gasconv01)

zbp01detailwide<-zbp01detailwide[,c(1,122:127)]

save(zbp01detailwide, file = 'R code/zbp01detailwide.Rda')
saveRDS(zbp01detailwide, 'R code/zbp01detailwide.rds')




