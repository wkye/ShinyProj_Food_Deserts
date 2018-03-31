#ACS
rm(list = ls())
load ('R code/zbptotal.rda')
zip<-vector('character')
zip<-zbptotal$zip
zip<-zip[!duplicated(zip)]


#zip_2013_acs_o <- read_csv("data/2013/zip 2013 acs.csv")
#save(zip_2013_acs_o, file = 'R code/zip 2013 acs.rda')
load('R code/zip 2013 acs.rda')
names_variables<- zip_2013_acs_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2013_acs_o)<-zip_2013_acs_o[1,]

variables_lst<-c('Geo_ZCTA5','SE_T001_001','SE_T002_002', 'SE_T008_002','SE_T008_003','SE_T008_004','SE_T008_005','SE_T008_006','SE_T008_007','SE_T008_008','SE_T008_009','SE_T008_010','SE_T008_011','SE_T008_012','SE_T008_013', 'SE_T008_014', 'SE_T008_015', 'SE_T008_016','SE_T008_017','SE_T008_018','SE_T008_019','SE_T008_020','SE_T008_021','SE_T008_022','SE_T008_023','SE_T008_024','SE_T014_003','SE_T014_004','SE_T014_005','SE_T014_006','SE_T014_007','SE_T014_008','SE_T014_009','SE_T014_010','SE_T033_001', 'SE_T033_002', 'SE_T056_001', 'SE_T056_002', 'SE_T056_003','SE_T056_004','SE_T056_005','SE_T056_006','SE_T056_007','SE_T056_008','SE_T056_009','SE_T056_010','SE_T056_011','SE_T056_012','SE_T056_013','SE_T056_014', 'SE_T056_015','SE_T056_016','SE_T056_017', 'SE_T057_001', 'SE_T115_001', 'SE_T115_002')
df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip','pop_13','popden_13','age5_13', 'age5_9_13', 'age10_14_13', 'age_15_17_13','age18_19_13','age20_13','age_21_13','age22_24_13', 'age25_29_13','age30_34_0','age35_39_13','age40_44','age45_49_13', 'age50_54_13', 'age55_59_13','age60_61_13','age62_64','age65_66_13','age67_69_13','age70_74_13',  'age75_74_13', 'age80_84_13', 'age85_13','race_white_13', 'race_black_13', 'race_amerind_13', 'race_asian_13', 'race_islander_13', 'race_other_13', 'race_two_13', 'race_hispanic_13', 'employed_pop_13', 'employed_13','households_13','inc_10_13', 
             'inc_15_13', 'inc_20_13', 'inc_25_13', 'inc_30_13', 'inc_35_13', 'inc_40_13', 'inc_45_13','inc_50_13', 'inc60_13', 'inc75_13', 'inc100_13', 'inc125_13', 'inc150_13', 'inc200_13','inc_201_13', 'inc_med_13', 'pov_d_13','below_pov_13' )

zip_2013_acs<-filter(zip_2013_acs_o, Geo_ZCTA5 %in% zip)
zip_2013_acs<-select(zip_2013_acs, Geo_ZCTA5,SE_T001_001,SE_T002_002, SE_T008_002,SE_T008_003,SE_T008_004,SE_T008_005,SE_T008_006,SE_T008_007,SE_T008_008,SE_T008_009,SE_T008_010,SE_T008_011,SE_T008_012,SE_T008_013, SE_T008_014, SE_T008_015, SE_T008_016,SE_T008_017,SE_T008_018,SE_T008_019,SE_T008_020,SE_T008_021,SE_T008_022,SE_T008_023,SE_T008_024,SE_T014_003,SE_T014_004,SE_T014_005,SE_T014_006,SE_T014_007,SE_T014_008,SE_T014_009,SE_T014_010,SE_T033_001, SE_T033_002, SE_T056_001, SE_T056_002, SE_T056_003,SE_T056_004,SE_T056_005,SE_T056_006,SE_T056_007,SE_T056_008,SE_T056_009,SE_T056_010,SE_T056_011,SE_T056_012,SE_T056_013,SE_T056_014, SE_T056_015,SE_T056_016,SE_T056_017, SE_T057_001, SE_T115_001, SE_T115_002)

names(zip_2013_acs)<-new_names

x<-data.frame(lapply(zip_2013_acs[2:56], function(x) as.numeric(as.character(x))))
zip_2013_acs_wk<-cbind(zip_2013_acs[,1], x)

zip_2013_acs_wk<- mutate(zip_2013_acs_wk, children_13=age5_13+age5_9_13 + age10_14_13+ age_15_17_13, youngadult_13= age18_19_13 +age20_13 + age_21_13 +age22_24_13+age25_29_13 +age30_34_0, middleage_13= age35_39_13+age40_44+age45_49_13+age50_54_13, elderly_13= age55_59_13+age60_61_13+age62_64 +age65_66_13+age67_69_13+age70_74_13+age75_74_13+age80_84_13+age85_13, race_other_13 = race_amerind_13 +race_islander_13+race_other_13 +race_two_13)

zip_2013_acs_wk<-select(zip_2013_acs_wk, zip, pop_13, popden_13,race_white_13, race_black_13, race_asian_13, race_other_13,race_hispanic_13, employed_pop_13, employed_13, pov_d_13,inc_med_13, below_pov_13, children_13, youngadult_13, middleage_13)
save(zip_2013_acs_wk, file = 'R code/zip_2013.rda')
saveRDS(zip_2013_acs_wk, 'R code/zip_2013.rds')




