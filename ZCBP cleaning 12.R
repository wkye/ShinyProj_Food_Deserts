###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp12detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2012/zbp12detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp12detail_o, file = 'R code/zbp12detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp12detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722511', '722514', '722515', '722513', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names12<- c('restaurant12', 'cafeterias12', 'dessert12', 'fastfood12', 'bakery12', 'bars12', 'conv12', 'gasconv12', 'warehouse12', 'grocery12', 'liquor12')
naics12 <-data.frame(naics, naics_names12)

#convert factor into string
naics12<- data.frame(lapply(naics12, as.character), stringsAsFactors = FALSE)
names(zbp12detail_o)<-tolower(names(zbp12detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp12detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp12detail<- inner_join(zbp12detail_o, naics12, by = "naics")
zbp12detail<- full_join(zip, zbp12detail, by = 'zip')
zbp12detail<-zbp12detail[,-2]
zbp12detail[is.na(zbp12detail)]<-0
#save(zbp12detail, file = 'R code/zbp12detail.Rda')

#load data filtered by naics codes
#load('R code/zbp12detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp12detail<- inner_join(zbp12detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp12detailwide<-data.table::dcast(data.table(zbp12detail), zip~ naics_names12, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp12detail[!duplicated(zbp12detail$zip),c(1,14,15)]

#create wide data
zbp12detailwide<- inner_join(zbp12detailwide, state, by = "zip")
zbp12detailwide[is.na(zbp12detailwide)]<-0
#save(zbp12detailwide, file = "R code/zbp12detailwide.Rda")

#creating outcome variables
#load("R code/zbp12detailwide.Rda")
zbp12detailwide<-mutate(zbp12detailwide, food12 = est_bakery12 + est_cafeterias12 + est_dessert12 + est_fastfood12 + est_restaurant12)
zbp12detailwide<-mutate(zbp12detailwide, alcohol12 = est_bars12 + est_liquor12)
zbp12detailwide<-mutate(zbp12detailwide, freshfood12 = (est_grocery12 - (n1_4_grocery12+n5_9_grocery12 + n10_19_grocery12))+est_warehouse12)
zbp12detailwide<-mutate(zbp12detailwide, unfreshfood12 = n1_4_grocery12+n5_9_grocery12 + n10_19_grocery12+ est_conv12+est_gasconv12)

zbp12detailwide<-zbp12detailwide[,c(1,122:127)]

save(zbp12detailwide, file = 'R code/zbp12detailwide.Rda')
saveRDS(zbp12detailwide, 'R code/zbp12detailwide.rds')

