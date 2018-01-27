###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
#zbp00detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2000/zbp00detail.txt')
#zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
#save(zip_2010_o, file = 'R code/zip_2010_o.Rda')
#save(zbp00detail_o, file = 'R code/zbp00detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp00detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names00<- c('restaurant00', 'cafeterias00', 'dessert00', 'fastfood00', 'bakery00', 'bars00', 'conv00', 'gasconv00', 'warehouse00', 'grocery00', 'liquor00')
naics00 <-data.frame(naics, naics_names00)

#convert factor into string
naics00<- data.frame(lapply(naics00, as.character), stringsAsFactors = FALSE)

#merge zbp data with naics data to subset data
zip <- data.frame(zbp00detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp00detail<- inner_join(zbp00detail_o, naics00, by = "naics")
zbp00detail<- full_join(zip, zbp00detail, by = 'zip')
zbp00detail<-zbp00detail[,-2]
zbp00detail[is.na(zbp00detail)]<-0
#save(zbp00detail, file = 'R code/zbp00detail.Rda')

#load data filtered by naics codes
#load('R code/zbp00detail_o.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#filter data based on a list of states
zip_2010<- zip_2010_o
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp00detail<- inner_join(zbp00detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp00detailwide<-data.table::dcast(data.table(zbp00detail), zip~ naics_names00, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp00detail[!duplicated(zbp00detail$zip),c(1,14,15)]

#create wide data
zbp00detailwide<- inner_join(zbp00detailwide, state, by = "zip")
zbp00detailwide[is.na(zbp00detailwide)]<-0
save(zbp00detailwide, file = "R code/zbp00detailwide.Rda")

#creating outcome variables
load("R code/zbp00detailwide.Rda")
zbp00detailwide<-mutate(zbp00detailwide, food00 = est_bakery00 + est_cafeterias00 + est_dessert00 + est_fastfood00 + est_restaurant00)
zbp00detailwide<-mutate(zbp00detailwide, alcohol00 = est_bars00 + est_liquor00)
zbp00detailwide<-mutate(zbp00detailwide, freshfood00 = (est_grocery00 - (n1_4_grocery00+n5_9_grocery00 + n10_19_grocery00))+est_warehouse00)
zbp00detailwide<-mutate(zbp00detailwide, unfreshfood00 = n1_4_grocery00+n5_9_grocery00 + n10_19_grocery00+ est_conv00+est_gasconv00)

zbp00detailwide<-zbp00detailwide[,c(1,122:127)]

save(zbp00detailwide, file = 'R code/zbp00detailwide.Rda')
saveRDS(zbp00detailwide, 'R code/zbp00detailwide.rds')









