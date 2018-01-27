###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp11detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2011/zbp11detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp11detail_o, file = 'R code/zbp11detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp11detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names11<- c('restaurant11', 'cafeterias11', 'dessert11', 'fastfood11', 'bakery11', 'bars11', 'conv11', 'gasconv11', 'warehouse11', 'grocery11', 'liquor11')
naics11 <-data.frame(naics, naics_names11)

#convert factor into string
naics11<- data.frame(lapply(naics11, as.character), stringsAsFactors = FALSE)
names(zbp11detail_o)<-tolower(names(zbp11detail_o))

#merge zbp data with naics data to subset data
#merge zbp data with naics data to subset data
zip <- data.frame(zbp11detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp11detail<- inner_join(zbp11detail_o, naics11, by = "naics")
zbp11detail<- full_join(zip, zbp11detail, by = 'zip')
zbp11detail<-zbp11detail[,-2]
zbp11detail[is.na(zbp11detail)]<-0
#load data filtered by naics codes
#load('R code/zbp11detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o# %>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp11detail<- inner_join(zbp11detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp11detailwide<-data.table::dcast(data.table(zbp11detail), zip~ naics_names11, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp11detail[!duplicated(zbp11detail$zip),c(1,14,15)]

#create wide data
zbp11detailwide<- inner_join(zbp11detailwide, state, by = "zip")
zbp11detailwide[is.na(zbp11detailwide)]<-0
#save(zbp11detailwide, file = "R code/zbp11detailwide.Rda")

#creating outcome variables
#load("R code/zbp11detailwide.Rda")
zbp11detailwide<-mutate(zbp11detailwide, food11 = est_bakery11 + est_cafeterias11 + est_dessert11 + est_fastfood11 + est_restaurant11)
zbp11detailwide<-mutate(zbp11detailwide, alcohol11 = est_bars11 + est_liquor11)
zbp11detailwide<-mutate(zbp11detailwide, freshfood11 = (est_grocery11 - (n1_4_grocery11+n5_9_grocery11 + n10_19_grocery11))+est_warehouse11)
zbp11detailwide<-mutate(zbp11detailwide, unfreshfood11 = n1_4_grocery11+n5_9_grocery11 + n10_19_grocery11+ est_conv11+est_gasconv11)

zbp11detailwide<-zbp11detailwide[,c(1,122:127)]

save(zbp11detailwide, file = 'R code/zbp11detailwide.Rda')
saveRDS(zbp11detailwide, 'R code/zbp11detailwide.rds')

