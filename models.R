rm(list = ls())

getwd()
setwd('/Users/williamkye/Box Sync/nyc data science academy/project_1/')
load('R data/final data.rda')
str(final_data)

#install.packages('semPlot')
library(lavaan)
#library(semPlot)
library(dplyr)

final_data = mutate(final_data, black = ifelse(per_race_black_00 >= 70, 1, 0))
final_data = mutate(final_data, white = ifelse(per_race_white_00 >= 70, 1, 0))
final_data = mutate(final_data, hispanic = ifelse(per_race_hispanic_00 >= 70, 1, 0))
final_data = mutate(final_data, integrated = ifelse(black !=1 & white !=1 & hispanic !=1, 1, 0))
final_data = mutate(final_data, pov00 = below_pov_00/pov_d_00)
final_data = mutate(final_data, pov09 = below_pov_09/pov_d_09)
final_data = mutate(final_data, pov10 = below_pov_10/pov_d_10)
final_data = mutate(final_data, pov11 = below_pov_11/pov_d_11)
final_data = mutate(final_data, pov12 = below_pov_12/pov_d_12)
final_data = mutate(final_data, pov13 = below_pov_13/pov_d_13)
final_data = mutate(final_data, pov14 = below_pov_14/pov_d_14)







final_data = mutate(final_data, logfastfood00  = log(fastfood00+1))
final_data = mutate(final_data, logfastfood09  = log(fastfood09+1))
final_data = mutate(final_data, logfastfood10  = log(fastfood10+1))
final_data = mutate(final_data, logfastfood11  = log(fastfood11+1))
final_data = mutate(final_data, logfastfood12  = log(fastfood12+1))
final_data = mutate(final_data, logfastfood13  = log(fastfood13+1))
final_data = mutate(final_data, logfastfood14  = log(fastfood15+1))

final_data = mutate(final_data, logfreshfood00  = log(freshfood00+1))
final_data = mutate(final_data, logfreshfood09  = log(freshfood09+1))
final_data = mutate(final_data, logfreshfood10  = log(freshfood10+1))
final_data = mutate(final_data, logfreshfood11  = log(freshfood11+1))
final_data = mutate(final_data, logfreshfood12  = log(freshfood12+1))
final_data = mutate(final_data, logfreshfood13  = log(freshfood13+1))
final_data = mutate(final_data, logfreshfood14  = log(freshfood15+1))

final_data = mutate(final_data, logunfreshgroceries00  = log(unfreshgroceries00+1))
final_data = mutate(final_data, logunfreshgroceries09  = log(unfreshgroceries09+1))
final_data = mutate(final_data, logunfreshgroceries10  = log(unfreshgroceries10+1))
final_data = mutate(final_data, logunfreshgroceries11  = log(unfreshgroceries11+1))
final_data = mutate(final_data, logunfreshgroceries12  = log(unfreshgroceries12+1))
final_data = mutate(final_data, logunfreshgroceries13  = log(unfreshgroceries13+1))
final_data = mutate(final_data, logunfreshgroceries14  = log(unfreshgroceries15+1))

final_data = mutate(final_data, logalcohol00  = log(alcohol00+1))
final_data = mutate(final_data, logalcohol09  = log(alcohol09+1))
final_data = mutate(final_data, logalcohol10  = log(alcohol10+1))
final_data = mutate(final_data, logalcohol11  = log(alcohol11+1))
final_data = mutate(final_data, logalcohol12  = log(alcohol12+1))
final_data = mutate(final_data, logalcohol13  = log(alcohol13+1))
final_data = mutate(final_data, logalcohol14  = log(alcohol15+1))



#summary(final_data[150:160])
names(final_data)
ggplot(final_data, aes(logfastfood00))+geom_histogram()
names(final_data)
#Models --- fastfood
final_data$pov

m.logfastfood<- 'i=~ 1*logfastfood00 + 1*logfastfood09+ 1*logfastfood10+ 1*logfastfood11+ 1*logfastfood12 + 1*logfastfood13+ 1*logfastfood14
s=~ 0*logfastfood00 + 9*logfastfood09+ 10*logfastfood10+ 11*logfastfood11+ 12*logfastfood12 + 13*logfastfood13+ 14*logfastfood14


# time invariant regression
i ~ black + hispanic + integrated 

s ~ black + hispanic + integrated 

# time varying covariates

logfastfood00 ~ inc_med_00 + pop_00 + pov00
logfastfood09 ~ inc_med_09 + pop_09 + pov09 +  per_race_black_09 + per_race_hispanic_09 + per_race_asian_09 + per_race_other_09
logfastfood10 ~ inc_med_10 + pop_10 + pov10 +  per_race_black_10 + per_race_hispanic_10 + per_race_asian_10 + per_race_other_10
logfastfood11 ~ inc_med_11 + pop_11 + pov11 + per_race_black_11 + per_race_hispanic_11 + per_race_asian_11 + per_race_other_11
logfastfood12 ~ inc_med_12 + pop_12 + pov12 +  per_race_black_12 + per_race_hispanic_12 + per_race_asian_12 + per_race_other_12
logfastfood13 ~ inc_med_13 + pop_13 + pov13 + per_race_black_13 + per_race_hispanic_13 + per_race_asian_13 + per_race_other_13
logfastfood14 ~ inc_med_14 + pop_14 + pov14 + per_race_black_14 + per_race_hispanic_14 + per_race_asian_14 + per_race_other_14
'


logfastfood<- growth(m.logfastfood, data = final_data)
summary(logfastfood)

#Models --- freshfood

m.logfreshfood<- 'i=~ 1*logfreshfood00 + 1*logfreshfood09+ 1*logfreshfood10+ 1*logfreshfood11+ 1*logfreshfood12 + 1*logfreshfood13+ 1*logfreshfood14
s=~ 0*logfreshfood00 + 9*logfreshfood09+ 10*logfreshfood10+ 11*logfreshfood11+ 12*logfreshfood12 + 13*logfreshfood13+ 14*logfreshfood14


# time invariant regression
i ~ black + hispanic + integrated 

s ~ black + hispanic + integrated 

# time varying covariates

logfreshfood00 ~ inc_med_00 + pop_00 + pov00
logfreshfood09 ~ inc_med_09 + pop_09 + pov09 +  per_race_black_09 + per_race_hispanic_09 + per_race_asian_09 + per_race_other_09
logfreshfood10 ~ inc_med_10 + pop_10 + pov10 +  per_race_black_10 + per_race_hispanic_10 + per_race_asian_10 + per_race_other_10
logfreshfood11 ~ inc_med_11 + pop_11 + pov11 + per_race_black_11 + per_race_hispanic_11 + per_race_asian_11 + per_race_other_11
logfreshfood12 ~ inc_med_12 + pop_12 + pov12 +  per_race_black_12 + per_race_hispanic_12 + per_race_asian_12 + per_race_other_12
logfreshfood13 ~ inc_med_13 + pop_13 + pov13 + per_race_black_13 + per_race_hispanic_13 + per_race_asian_13 + per_race_other_13
logfreshfood14 ~ inc_med_14 + pop_14 + pov14 + per_race_black_14 + per_race_hispanic_14 + per_race_asian_14 + per_race_other_14
'


logfreshfood<- growth(m.logfreshfood, data = final_data)
summary(logfreshfood)

#Models --- unfreshgroceries

m.logunfreshgroceries<- 'i=~ 1*logunfreshgroceries00 + 1*logunfreshgroceries09+ 1*logunfreshgroceries10+ 1*logunfreshgroceries11+ 1*logunfreshgroceries12 + 1*logunfreshgroceries13+ 1*logunfreshgroceries14
s=~ 0*logunfreshgroceries00 + 9*logunfreshgroceries09+ 10*logunfreshgroceries10+ 11*logunfreshgroceries11+ 12*logunfreshgroceries12 + 13*logunfreshgroceries13+ 14*logunfreshgroceries14


# time invariant regression
i ~ black + hispanic + integrated 

s ~ black + hispanic + integrated 

# time varying covariates

logunfreshgroceries00 ~ inc_med_00 + pop_00 + pov00
logunfreshgroceries09 ~ inc_med_09 + pop_09 + pov09 +  per_race_black_09 + per_race_hispanic_09 + per_race_asian_09 + per_race_other_09
logunfreshgroceries10 ~ inc_med_10 + pop_10 + pov10 +  per_race_black_10 + per_race_hispanic_10 + per_race_asian_10 + per_race_other_10
logunfreshgroceries11 ~ inc_med_11 + pop_11 + pov11 + per_race_black_11 + per_race_hispanic_11 + per_race_asian_11 + per_race_other_11
logunfreshgroceries12 ~ inc_med_12 + pop_12 + pov12 +  per_race_black_12 + per_race_hispanic_12 + per_race_asian_12 + per_race_other_12
logunfreshgroceries13 ~ inc_med_13 + pop_13 + pov13 + per_race_black_13 + per_race_hispanic_13 + per_race_asian_13 + per_race_other_13
logunfreshgroceries14 ~ inc_med_14 + pop_14 + pov14 + per_race_black_14 + per_race_hispanic_14 + per_race_asian_14 + per_race_other_14
'


logunfreshgroceries<- growth(m.logunfreshgroceries, data = final_data)
summary(logunfreshgroceries)

#Models --- alcohol

m.logalcohol<- 'i=~ 1*logalcohol00 + 1*logalcohol09+ 1*logalcohol10+ 1*logalcohol11+ 1*logalcohol12 + 1*logalcohol13+ 1*logalcohol14
s=~ 0*logalcohol00 + 9*logalcohol09+ 10*logalcohol10+ 11*logalcohol11+ 12*logalcohol12 + 13*logalcohol13+ 14*logalcohol14


# time invariant regression
i ~ black + hispanic + integrated 

s ~ black + hispanic + integrated 

# time varying covariates

logalcohol00 ~ inc_med_00 + pop_00 + pov00
logalcohol09 ~ inc_med_09 + pop_09 + pov09 +  per_race_black_09 + per_race_hispanic_09 + per_race_asian_09 + per_race_other_09
logalcohol10 ~ inc_med_10 + pop_10 + pov10 +  per_race_black_10 + per_race_hispanic_10 + per_race_asian_10 + per_race_other_10
logalcohol11 ~ inc_med_11 + pop_11 + pov11 + per_race_black_11 + per_race_hispanic_11 + per_race_asian_11 + per_race_other_11
logalcohol12 ~ inc_med_12 + pop_12 + pov12 +  per_race_black_12 + per_race_hispanic_12 + per_race_asian_12 + per_race_other_12
logalcohol13 ~ inc_med_13 + pop_13 + pov13 + per_race_black_13 + per_race_hispanic_13 + per_race_asian_13 + per_race_other_13
logalcohol14 ~ inc_med_14 + pop_14 + pov14 + per_race_black_14 + per_race_hispanic_14 + per_race_asian_14 + per_race_other_14
'


logalcohol<- growth(m.logalcohol, data = final_data)
summary(logalcohol)

summary(logfreshfood)
