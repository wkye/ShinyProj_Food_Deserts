###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp10detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2010/zbp10detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp10detail_o, file = 'R code/zbp10detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp10detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names10<- c('restaurant10', 'cafeterias10', 'dessert10', 'fastfood10', 'bakery10', 'bars10', 'conv10', 'gasconv10', 'warehouse10', 'grocery10', 'liquor10')
naics10 <-data.frame(naics, naics_names10)

#convert factor into string
naics10<- data.frame(lapply(naics10, as.character), stringsAsFactors = FALSE)
names(zbp10detail_o)<-tolower(names(zbp10detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp10detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp10detail<- inner_join(zbp10detail_o, naics10, by = "naics")
zbp10detail<- full_join(zip, zbp10detail, by = 'zip')
zbp10detail<-zbp10detail[,-2]
zbp10detail[is.na(zbp10detail)]<-0

#load data filtered by naics codes
#load('R code/zbp10detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp10detail<- inner_join(zbp10detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp10detailwide<-data.table::dcast(data.table(zbp10detail), zip~ naics_names10, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp10detail[!duplicated(zbp10detail$zip),c(1,14,15)]

#create wide data
zbp10detailwide<- inner_join(zbp10detailwide, state, by = "zip")
zbp10detailwide[is.na(zbp10detailwide)]<-0
#save(zbp10detailwide, file = "R code/zbp10detailwide.Rda")

#creating outcome variables
#load("R code/zbp10detailwide.Rda")
zbp10detailwide<-mutate(zbp10detailwide, food10 = est_bakery10 + est_cafeterias10 + est_dessert10 + est_fastfood10 + est_restaurant10)
zbp10detailwide<-mutate(zbp10detailwide, alcohol10 = est_bars10 + est_liquor10)
zbp10detailwide<-mutate(zbp10detailwide, freshfood10 = (est_grocery10 - (n1_4_grocery10+n5_9_grocery10 + n10_19_grocery10))+est_warehouse10)
zbp10detailwide<-mutate(zbp10detailwide, unfreshfood10 = n1_4_grocery10+n5_9_grocery10 + n10_19_grocery10+ est_conv10+est_gasconv10)

zbp10detailwide<-zbp10detailwide[,c(1,122:127)]

save(zbp10detailwide, file = 'R code/zbp10detailwide.Rda')
saveRDS(zbp10detailwide, 'R code/zbp10detailwide.rds')
