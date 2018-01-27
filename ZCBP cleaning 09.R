###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp09detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2009/zbp09detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp09detail_o, file = 'R code/zbp09detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp09detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names09<- c('restaurant09', 'cafeterias09', 'dessert09', 'fastfood09', 'bakery09', 'bars09', 'conv09', 'gasconv09', 'warehouse09', 'grocery09', 'liquor09')
naics09 <-data.frame(naics, naics_names09)

#convert factor into string
naics09<- data.frame(lapply(naics09, as.character), stringsAsFactors = FALSE)
names(zbp09detail_o)<-tolower(names(zbp09detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp09detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp09detail<- inner_join(zbp09detail_o, naics09, by = "naics")
zbp09detail<- full_join(zip, zbp09detail, by = 'zip')
zbp09detail<-zbp09detail[,-2]
zbp09detail[is.na(zbp09detail)]<-0

#load data filtered by naics codes
#load('R code/zbp09detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o# %>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp09detail<- inner_join(zbp09detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp09detailwide<-data.table::dcast(data.table(zbp09detail), zip~ naics_names09, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp09detail[!duplicated(zbp09detail$zip),c(1,14,15)]

#create wide data
zbp09detailwide<- inner_join(zbp09detailwide, state, by = "zip")
zbp09detailwide[is.na(zbp09detailwide)]<-0
#save(zbp09detailwide, file = "R code/zbp09detailwide.Rda")

#creating outcome variables
#load("R code/zbp09detailwide.Rda")
zbp09detailwide<-mutate(zbp09detailwide, food09 = est_bakery09 + est_cafeterias09 + est_dessert09 + est_fastfood09 + est_restaurant09)
zbp09detailwide<-mutate(zbp09detailwide, alcohol09 = est_bars09 + est_liquor09)
zbp09detailwide<-mutate(zbp09detailwide, freshfood09 = (est_grocery09 - (n1_4_grocery09+n5_9_grocery09 + n10_19_grocery09))+est_warehouse09)
zbp09detailwide<-mutate(zbp09detailwide, unfreshfood09 = n1_4_grocery09+n5_9_grocery09 + n10_19_grocery09+ est_conv09+est_gasconv09)

zbp09detailwide<-zbp09detailwide[,c(1,122:127)]

save(zbp09detailwide, file = 'R code/zbp09detailwide.Rda')
saveRDS(zbp09detailwide, 'R code/zbp09detailwide.rds')

