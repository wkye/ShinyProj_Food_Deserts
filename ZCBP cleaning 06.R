###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp06detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2006/zbp06detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp06detail_o, file = 'R code/zbp06detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp06detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names06<- c('restaurant06', 'cafeterias06', 'dessert06', 'fastfood06', 'bakery06', 'bars06', 'conv06', 'gasconv06', 'warehouse06', 'grocery06', 'liquor06')
naics06 <-data.frame(naics, naics_names06)

#convert factor into string
naics06<- data.frame(lapply(naics06, as.character), stringsAsFactors = FALSE)
names(zbp06detail_o)<-tolower(names(zbp06detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp06detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp06detail<- inner_join(zbp06detail_o, naics06, by = "naics")
zbp06detail<- full_join(zip, zbp06detail, by = 'zip')
zbp06detail<-zbp06detail[,-2]
zbp06detail[is.na(zbp06detail)]<-0
#save(zbp06detail, file = 'R code/zbp06detail.Rda')

#load data filtered by naics codes
#load('R code/zbp06detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o# %>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp06detail<- inner_join(zbp06detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp06detailwide<-data.table::dcast(data.table(zbp06detail), zip~ naics_names06, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp06detail[!duplicated(zbp06detail$zip),c(1,14,15)]

#create wide data
zbp06detailwide<- inner_join(zbp06detailwide, state, by = "zip")
zbp06detailwide[is.na(zbp06detailwide)]<-0
#save(zbp06detailwide, file = "R code/zbp06detailwide.Rda")

#creating outcome variables
#load("R code/zbp06detailwide.Rda")
zbp06detailwide<-mutate(zbp06detailwide, food06 = est_bakery06 + est_cafeterias06 + est_dessert06 + est_fastfood06 + est_restaurant06)
zbp06detailwide<-mutate(zbp06detailwide, alcohol06 = est_bars06 + est_liquor06)
zbp06detailwide<-mutate(zbp06detailwide, freshfood06 = (est_grocery06 - (n1_4_grocery06+n5_9_grocery06 + n10_19_grocery06))+est_warehouse06)
zbp06detailwide<-mutate(zbp06detailwide, unfreshfood06 = n1_4_grocery06+n5_9_grocery06 + n10_19_grocery06+ est_conv06+est_gasconv06)

zbp06detailwide<-zbp06detailwide[,c(1,122:127)]

save(zbp06detailwide, file = 'R code/zbp06detailwide.Rda')
saveRDS(zbp06detailwide, 'R code/zbp06detailwide.rds')
