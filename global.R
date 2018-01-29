library(data.table)
library(rgdal)
library(broom)
library(dplyr)
library(ggplot2)
library(leaflet)
library(maps)
library(RColorBrewer)
library(rmapshaper)


load('shape1.rda')
load('bc_fastfood.rda')
load('margins_fastfood.rda')
load('bc_freshfood.rda')

load('bc_unfreshgroceries.rda')

load('bc_alcohol.rda')
load('scat.rda')
load('scat data.rda')
load('margins_freshfood.rda')
load('margins_unfreshgroceries.rda')
load('margins_alcohol.rda')


# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/shape1.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/bc_fastfood.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/margins_fastfood.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/bc_freshfood.rda')
# 
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/bc_unfreshgroceries.rda')
# 
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/bc_alcohol.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/scat.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/scat data.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/margins_freshfood.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/margins_unfreshgroceries.rda')
# load('/Users/williamkye/Box Sync/nyc data science academy/project 1/shiny/margins_alcohol.rda')