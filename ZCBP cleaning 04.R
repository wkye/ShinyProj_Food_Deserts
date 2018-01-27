###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
#zbp04detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2004/zbp04detail.txt')
#zip_2010_o <- read_csv("~/Box ync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
#save(zip_2010_o, file = 'R code/zip_2010.Rda')
#save(zbp04detail_o, file = 'R code/zbp04detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp04detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names04<- c('restaurant04', 'cafeterias04', 'dessert04', 'fastfood04', 'bakery04', 'bars04', 'conv04', 'gasconv04', 'warehouse04', 'grocery04', 'liquor04')
naics04 <-data.frame(naics, naics_names04)

#convert factor into string
naics04<- data.frame(lapply(naics04, as.character), stringsAsFactors = FALSE)
names(zbp04detail_o)<-tolower(names(zbp04detail_o))

#merge zbp data with naics data to subset data
#merge zbp data with naics data to subset data
zip <- data.frame(zbp04detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp04detail<- inner_join(zbp04detail_o, naics04, by = "naics")
zbp04detail<- full_join(zip, zbp04detail, by = 'zip')
zbp04detail<-zbp04detail[,-2]
zbp04detail[is.na(zbp04detail)]<-0

#load data filtered by naics codes
#load('R code/zbp04detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp04detail<- inner_join(zbp04detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp04detailwide<-data.table::dcast(data.table(zbp04detail), zip~ naics_names04, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp04detail[!duplicated(zbp04detail$zip),c(1,14,15)]

#create wide data
zbp04detailwide<- inner_join(zbp04detailwide, state, by = "zip")
zbp04detailwide[is.na(zbp04detailwide)]<-0
#save(zbp04detailwide, file = "R code/zbp04detailwide.Rda")

#creating outcome variables
#load("R code/zbp04detailwide.Rda")
zbp04detailwide<-mutate(zbp04detailwide, food04 = est_bakery04 + est_cafeterias04 + est_dessert04 + est_fastfood04 + est_restaurant04)
zbp04detailwide<-mutate(zbp04detailwide, alcohol04 = est_bars04 + est_liquor04)
zbp04detailwide<-mutate(zbp04detailwide, freshfood04 = (est_grocery04 - (n1_4_grocery04+n5_9_grocery04 + n10_19_grocery04))+est_warehouse04)
zbp04detailwide<-mutate(zbp04detailwide, unfreshfood04 = n1_4_grocery04+n5_9_grocery04 + n10_19_grocery04+ est_conv04+est_gasconv04)

zbp04detailwide<-zbp04detailwide[,c(1,122:127)]

save(zbp04detailwide, file = 'R code/zbp04detailwide.Rda')
saveRDS(zbp04detailwide, 'R code/zbp04detailwide.rds')
