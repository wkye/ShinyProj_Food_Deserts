rm(list = ls())
library(data.table)
load ('R code/zbptotal.rda')
zip<-vector('character')
zip<-zbptotal$zip
zip<-zip[!duplicated(zip)]
#zip_2010_o <- read_csv("data/2010/zip 2010 Long.csv")
#test<-zip_2010_o[1:10,1:100]
#save(zip_2010_o, file = 'R code/zip_2010_o')
load('R code/zip_2010_o')
names_variables<- zip_2010_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2010_o)<-zip_2010_o[1,]

variables_lst<-c('Geo_ZCTA5', 'SE_T001_001', 'SE_T002_002','SE_T008_002','SE_T008_003','SE_T008_004','SE_T008_005','SE_T008_006','SE_T008_007','SE_T008_008','SE_T008_009','SE_T008_010','SE_T008_011','SE_T008_012','SE_T008_013', 'SE_T055_003','SE_T055_004','SE_T055_005','SE_T055_006','SE_T055_007','SE_T055_008','SE_T055_009','SE_T055_010')

df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip', 'pop_10', 'popden_10', 'age5_10', 'age5_9_10', 'age10_14_10', 'age_15_17_10', 'age18_24_10', 'age25_34_10', 'age_35_44_10', 'age45_54_10', 'age55_64_10', 'age65_74_10', 'age_75_84_10','age85_10', 'race_white_10', 'race_black_10', 'race_amerind_10', 'race_asian_10', 'race_islander_10', 'race_other_10', 'race_two_10', 'race_hispanic_10')

zip_2010<-filter(zip_2010_o, Geo_ZCTA5 %in% zip)
zip_2010<-select(zip_2010, Geo_ZCTA5, SE_T001_001, SE_T002_002,SE_T008_002,SE_T008_003,SE_T008_004,SE_T008_005,SE_T008_006,SE_T008_007,SE_T008_008,SE_T008_009,SE_T008_010,SE_T008_011,SE_T008_012,SE_T008_013, SE_T055_003,SE_T055_004,SE_T055_005,SE_T055_006,SE_T055_007,SE_T055_008,SE_T055_009,SE_T055_010)


names(zip_2010)<-new_names

#ACS
#zip_2010_acs_o <- read_csv("data/2010/zip 2010 ACS.csv")
#save(zip_2010_acs_o, file = 'R code/zip 2010 acs.rda')
load('R code/zip 2010 acs.rda')
names_variables<- zip_2010_acs_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2010_acs_o)<-zip_2010_acs_o[1,]

variables_lst<-c('Geo_ZCTA5','SE_T033_001', 'SE_T033_002','SE_T056_001', 'SE_T056_002', 'SE_T056_003','SE_T056_004','SE_T056_005','SE_T056_006','SE_T056_007','SE_T056_008','SE_T056_009','SE_T056_010','SE_T056_011','SE_T056_012','SE_T056_013','SE_T056_014', 'SE_T056_015','SE_T056_016','SE_T056_017', 'SE_T057_001', 'SE_T115_001', 'SE_T115_002')
df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip','employed_pop_10', 'employed_10' , 'households_10', 'inc_10_10', 'inc_15_10', 'inc_20_10', 'inc_25_10', 'inc_30_10', 'inc_35_10', 'inc_40_10', 'inc_45_10','inc_50_10', 'inc60_10', 'inc75_10', 'inc100_10', 'inc125_10', 'inc150_10', 'inc200_10','inc_201_10', 'inc_med_10', 'pov_d_10', 'below_pov_10')

zip_2010_acs<-filter(zip_2010_acs_o, Geo_ZCTA5 %in% zip)
zip_2010_acs<-select(zip_2010_acs, Geo_ZCTA5, SE_T033_001, SE_T033_002, SE_T057_001,SE_T056_001, SE_T056_002, SE_T056_003,SE_T056_004,SE_T056_005,SE_T056_006,SE_T056_007,SE_T056_008,SE_T056_009,SE_T056_010,SE_T056_011,SE_T056_012,SE_T056_013,SE_T056_014, SE_T056_015,SE_T056_016,SE_T056_017, SE_T115_001, SE_T115_002)


names(zip_2010_acs)<-new_names

zip_2010<-inner_join(zip_2010, zip_2010_acs, by = 'zip')

x<-data.frame(lapply(zip_2010[2:45], function(x) as.numeric(as.character(x))))
zip_2010_wk<-cbind(zip_2010[,1], x)

zip_2010_wk<- mutate(zip_2010_wk, children_10=age5_10+age5_9_10 + age10_14_10+ age18_24_10, youngadult_10= age25_34_10, middleage_10= age_35_44_10+age45_54_10, elderly_10= age55_64_10+age65_74_10+age_75_84_10 +age85_10, race_other_10 = race_amerind_10 +race_islander_10+race_other_10 +race_two_10)

zip_2010_wk<-select(zip_2010_wk, zip, pop_10, popden_10,race_white_10, race_black_10, race_asian_10, race_other_10,race_hispanic_10, employed_pop_10, employed_10, pov_d_10,inc_med_10, below_pov_10, children_10, youngadult_10, middleage_10)
save(zip_2010_wk, file = 'R code/zip_2010.rda')

saveRDS(zip_2010_wk, 'R code/zip_2010.rds')







