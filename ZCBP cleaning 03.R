###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
#zbp03detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2003/zbp03detail.txt')
#zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
#save(zip_2010_o, file = 'R code/zip_2010.Rda')
#save(zbp03detail_o, file = 'R code/zbp03detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp03detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names03<- c('restaurant03', 'cafeterias03', 'dessert03', 'fastfood03', 'bakery03', 'bars03', 'conv03', 'gasconv03', 'warehouse03', 'grocery03', 'liquor03')
naics03 <-data.frame(naics, naics_names03)

#convert factor into string
naics03<- data.frame(lapply(naics03, as.character), stringsAsFactors = FALSE)
names(zbp03detail_o)<-tolower(names(zbp03detail_o))

#merge zbp data with naics data to subset data
#merge zbp data with naics data to subset data
zip <- data.frame(zbp03detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp03detail<- inner_join(zbp03detail_o, naics03, by = "naics")
zbp03detail<- full_join(zip, zbp03detail, by = 'zip')
zbp03detail<-zbp03detail[,-2]
zbp03detail[is.na(zbp03detail)]<-0
#save(zbp03detail, file = 'R code/zbp03detail.Rda')

#load data filtered by naics codes
#load('R code/zbp03detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o# %>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp03detail<- inner_join(zbp03detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp03detailwide<-data.table::dcast(data.table(zbp03detail), zip~ naics_names03, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp03detail[!duplicated(zbp03detail$zip),c(1,14,15)]

#create wide data
zbp03detailwide<- inner_join(zbp03detailwide, state, by = "zip")
zbp03detailwide[is.na(zbp03detailwide)]<-0
#save(zbp03detailwide, file = "R code/zbp03detailwide.Rda")

#creating outcome variables
#load("R code/zbp03detailwide.Rda")
zbp03detailwide<-mutate(zbp03detailwide, food03 = est_bakery03 + est_cafeterias03 + est_dessert03 + est_fastfood03 + est_restaurant03)
zbp03detailwide<-mutate(zbp03detailwide, alcohol03 = est_bars03 + est_liquor03)
zbp03detailwide<-mutate(zbp03detailwide, freshfood03 = (est_grocery03 - (n1_4_grocery03+n5_9_grocery03 + n10_19_grocery03))+est_warehouse03)
zbp03detailwide<-mutate(zbp03detailwide, unfreshfood03 = n1_4_grocery03+n5_9_grocery03 + n10_19_grocery03+ est_conv03+est_gasconv03)

zbp03detailwide<-zbp03detailwide[,c(1,122:127)]

save(zbp03detailwide, file = 'R code/zbp03detailwide.Rda')
saveRDS(zbp03detailwide, 'R code/zbp03detailwide.rds')
