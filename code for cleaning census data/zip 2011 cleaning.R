#ACS
rm(list = ls())
load ('R code/zbptotal.rda')
zip<-vector('character')
zip<-zbptotal$zip
zip<-zip[!duplicated(zip)]


#zip_2011_acs_o <- read_csv("data/2011/zip 2011 ACS.csv")
#save(zip_2011_acs_o, file = 'R code/zip 2011 acs.rda')
load('R code/zip 2011 acs.rda')
names_variables<- zip_2011_acs_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2011_acs_o)<-zip_2011_acs_o[1,]

variables_lst<-c('Geo_ZCTA5','SE_T001_001','SE_T002_002', 'SE_T008_002','SE_T008_003','SE_T008_004','SE_T008_005','SE_T008_006','SE_T008_007','SE_T008_008','SE_T008_009','SE_T008_010','SE_T008_011','SE_T008_012','SE_T008_013', 'SE_T008_014', 'SE_T008_015', 'SE_T008_016','SE_T008_017','SE_T008_018','SE_T008_019','SE_T008_020','SE_T008_021','SE_T008_022','SE_T008_023','SE_T008_024','SE_T014_003','SE_T014_004','SE_T014_005','SE_T014_006','SE_T014_007','SE_T014_008','SE_T014_009','SE_T014_010','SE_T033_001', 'SE_T033_002', 'SE_T056_001', 'SE_T056_002', 'SE_T056_003','SE_T056_004','SE_T056_005','SE_T056_006','SE_T056_007','SE_T056_008','SE_T056_009','SE_T056_010','SE_T056_011','SE_T056_012','SE_T056_013','SE_T056_014', 'SE_T056_015','SE_T056_016','SE_T056_017', 'SE_T057_001','SE_T115_001','SE_T115_002')
df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip','pop_11','popden_11','age5_11', 'age5_9_11', 'age10_14_11', 'age_15_17_11','age18_19_11','age20_11','age_21_11','age22_24_11', 'age25_29_11','age30_34_0','age35_39_11','age40_44','age45_49_11', 'age50_54_11', 'age55_59_11','age60_61_11','age62_64','age65_66_11','age67_69_11','age70_74_11',  'age75_74_11', 'age80_84_11', 'age85_11','race_white_11', 'race_black_11', 'race_amerind_11', 'race_asian_11', 'race_islander_11', 'race_other_11', 'race_two_11', 'race_hispanic_11', 'employed_pop_11', 'employed_11','households_11','inc_10_11', 
             'inc_15_11', 'inc_20_11', 'inc_25_11', 'inc_30_11', 'inc_35_11', 'inc_40_11', 'inc_45_11','inc_50_11', 'inc60_11', 'inc75_11', 'inc100_11', 'inc125_11', 'inc150_11', 'inc200_11','inc_201_11', 'inc_med_11', 'pov_d_11', 'below_pov_11')

zip_2011_acs<-filter(zip_2011_acs_o, Geo_ZCTA5 %in% zip)
zip_2011_acs<-select(zip_2011_acs, Geo_ZCTA5,SE_T001_001,SE_T002_002, SE_T008_002,SE_T008_003,SE_T008_004,SE_T008_005,SE_T008_006,SE_T008_007,SE_T008_008,SE_T008_009,SE_T008_010,SE_T008_011,SE_T008_012,SE_T008_013, SE_T008_014, SE_T008_015, SE_T008_016,SE_T008_017,SE_T008_018,SE_T008_019,SE_T008_020,SE_T008_021,SE_T008_022,SE_T008_023,SE_T008_024,SE_T014_003,SE_T014_004,SE_T014_005,SE_T014_006,SE_T014_007,SE_T014_008,SE_T014_009,SE_T014_010,SE_T033_001, SE_T033_002, SE_T056_001, SE_T056_002, SE_T056_003,SE_T056_004,SE_T056_005,SE_T056_006,SE_T056_007,SE_T056_008,SE_T056_009,SE_T056_010,SE_T056_011,SE_T056_012,SE_T056_013,SE_T056_014, SE_T056_015,SE_T056_016,SE_T056_017, SE_T057_001, SE_T115_001,SE_T115_002)

names(zip_2011_acs)<-new_names

x<-data.frame(lapply(zip_2011_acs[2:56], function(x) as.numeric(as.character(x))))
zip_2011_acs_wk<-cbind(zip_2011_acs[,1], x)

zip_2011_acs_wk<- mutate(zip_2011_acs_wk, children_11=age5_11+age5_9_11 + age10_14_11+ age_15_17_11, youngadult_11= age18_19_11 +age20_11 + age_21_11 +age22_24_11+age25_29_11 +age30_34_0, middleage_11= age35_39_11+age40_44+age45_49_11+age50_54_11, elderly_11= age55_59_11+age60_61_11+age62_64 +age65_66_11+age67_69_11+age70_74_11+age75_74_11+age80_84_11+age85_11, race_other_11 = race_amerind_11 +race_islander_11+race_other_11 +race_two_11)

zip_2011_acs_wk<-select(zip_2011_acs_wk, zip, pop_11, popden_11,race_white_11, race_black_11, race_asian_11, race_other_11,race_hispanic_11, employed_pop_11, employed_11, pov_d_11,inc_med_11, below_pov_11, children_11, youngadult_11, middleage_11)
save(zip_2011_acs_wk, file = 'R code/zip_2011.rda')

saveRDS(zip_2011_acs_wk, 'R code/zip_2011.rds')




