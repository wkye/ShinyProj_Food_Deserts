#ACS
rm(list = ls())
load ('R code/zbptotal.rda')
zip<-vector('character')
zip<-zbptotal$zip
zip<-zip[!duplicated(zip)]


#zip_2009_acs_o <- read_csv("data/2009/zip 2009 ACS.csv")
#save(zip_2009_acs_o, file = 'R code/zip 2009 acs.rda')
load('R code/zip 2009 acs.rda')
names_variables<- zip_2009_acs_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2009_acs_o)<-zip_2009_acs_o[1,]

variables_lst<-c('Geo_ZCTA5','SE_T001_001','SE_T002_002', 'SE_T008_002','SE_T008_003','SE_T008_004','SE_T008_005','SE_T008_006','SE_T008_007','SE_T008_008','SE_T008_009','SE_T008_010','SE_T008_011','SE_T008_012','SE_T008_013', 'SE_T008_014', 'SE_T008_015', 'SE_T008_016','SE_T008_017','SE_T008_018','SE_T008_019','SE_T008_020','SE_T008_021','SE_T008_022','SE_T008_023','SE_T008_024','SE_T014_003','SE_T014_004','SE_T014_005','SE_T014_006','SE_T014_007','SE_T014_008','SE_T014_009','SE_T014_010','SE_T033_001', 'SE_T033_002', 'SE_T056_001', 'SE_T056_002', 'SE_T056_003','SE_T056_004','SE_T056_005','SE_T056_006','SE_T056_007','SE_T056_008','SE_T056_009','SE_T056_010','SE_T056_011','SE_T056_012','SE_T056_013','SE_T056_014', 'SE_T056_015','SE_T056_016','SE_T056_017', 'SE_T057_001', 'SE_T115_001', 'SE_T115_002')        
df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip','pop_09','popden_09','age5_09', 'age5_9_09', 'age10_14_09', 'age_15_17_09','age18_19_09','age20_09','age_21_09','age22_24_09', 'age25_29_09','age30_34_0','age35_39_09','age40_44','age45_49_09', 'age50_54_09', 'age55_59_09','age60_61_09','age62_64','age65_66_09','age67_69_09','age70_74_09',  'age75_74_09', 'age80_84_09', 'age85_09','race_white_09', 'race_black_09', 'race_amerind_09', 'race_asian_09', 'race_islander_09', 'race_other_09', 'race_two_09', 'race_hispanic_09', 'employed_pop_09', 'employed_09','households_09','inc_10_09', 
             'inc_15_09', 'inc_20_09', 'inc_25_09', 'inc_30_09', 'inc_35_09', 'inc_40_09', 'inc_45_09','inc_50_09', 'inc60_09', 'inc75_09', 'inc100_09', 'inc125_09', 'inc150_09', 'inc200_09','inc_201_09', 'inc_med_09', 'pov_d_09', "below_pov_09")

zip_2009_acs<-filter(zip_2009_acs_o, Geo_ZCTA5 %in% zip)
zip_2009_acs<-select(zip_2009_acs, Geo_ZCTA5,SE_T001_001,SE_T002_002, SE_T008_002,SE_T008_003,SE_T008_004,SE_T008_005,SE_T008_006,SE_T008_007,SE_T008_008,SE_T008_009,SE_T008_010,SE_T008_011,SE_T008_012,SE_T008_013, SE_T008_014, SE_T008_015, SE_T008_016,SE_T008_017,SE_T008_018,SE_T008_019,SE_T008_020,SE_T008_021,SE_T008_022,SE_T008_023,SE_T008_024,SE_T014_003,SE_T014_004,SE_T014_005,SE_T014_006,SE_T014_007,SE_T014_008,SE_T014_009,SE_T014_010,SE_T033_001, SE_T033_002, SE_T056_001, SE_T056_002, SE_T056_003,SE_T056_004,SE_T056_005,SE_T056_006,SE_T056_007,SE_T056_008,SE_T056_009,SE_T056_010,SE_T056_011,SE_T056_012,SE_T056_013,SE_T056_014, SE_T056_015,SE_T056_016,SE_T056_017, SE_T057_001, SE_T115_001, SE_T115_002)

names(zip_2009_acs)<-new_names

x<-data.frame(lapply(zip_2009_acs[2:56], function(x) as.numeric(as.character(x))))
zip_2009_acs_wk<-cbind(zip_2009_acs[,1], x)

zip_2009_acs_wk<- mutate(zip_2009_acs_wk, children_09=age5_09+age5_9_09 + age10_14_09+ age_15_17_09, youngadult_09= age18_19_09 +age20_09 + age_21_09 +age22_24_09+age25_29_09 +age30_34_0, middleage_09= age35_39_09+age40_44+age45_49_09+age50_54_09, elderly_09= age55_59_09+age60_61_09+age62_64 +age65_66_09+age67_69_09+age70_74_09+age75_74_09+age80_84_09+age85_09, race_other_09 = race_amerind_09 +race_islander_09+race_other_09 +race_two_09)

zip_2009_acs_wk<-select(zip_2009_acs_wk, zip, pop_09, popden_09,race_white_09, race_black_09, race_asian_09, race_other_09,race_hispanic_09, employed_pop_09, employed_09, pov_d_09,inc_med_09, below_pov_09, children_09, youngadult_09, middleage_09)
save(zip_2009_acs_wk, file = 'R code/zip_2009.rda')
saveRDS(zip_2009_acs_wk, 'R code/zip_2009.rds')

