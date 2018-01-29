library(DT)
library(shiny)
library(shinydashboard)
library(data.table)
library(plotly)
library(leaflet)
library(shiny)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet.extras)
library(R.utils)


shinyUI(dashboardPage( skin = 'black',
  dashboardHeader(title = tags$b(h3('William Kye'))),
  dashboardSidebar(
    
    sidebarUserPanel(tags$h5('William Kye'),
                     image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
    sidebarMenu(id ='sideBarMenu',
                menuItem("Map", tabName = "map", icon = icon('map')),
                menuItem('Descriptive Statistics', tabName = 'scatter', icon = icon('book')),
                menuItem('Regression', tabName = 'regression', icon = icon('book')),
                menuItem('Predicted Values', tabName ='pred', icon = icon('book'))),
    conditionalPanel("input.sideBarMenu == 'map'",
                     selectizeInput(
                       inputId = 'city',
                       label = 'city',
                       choices = unique(shape1$new_names)),
                     selectizeInput
                     (inputId ="food", 
                      label = "category",
                      choices = c('fastfood', 'alcohol', 'freshfood', 'unfreshgroceries')),
                     sliderInput(
                       inputId = 'time',
                       label = 'year',
                       min = 00,
                       max = 14,
                       value = 00,
                       step = 1,
                       animate = animationOptions(interval=2000,loop=TRUE)
                     )
                     ),
    conditionalPanel("input.sideBarMenu == 'scatter'",
                     selectizeInput
                     (inputId ="scatter_outcome", 
                       label = "category",
                       choices = c('fastfood', 'alcohol', 'freshfood', 'unfreshgroceries')),
                     selectizeInput(
                       inputId = 'collapse',
                       label = 'collapse into groups',
                       choices = c('original scatterplot', 'collapse into groups', 'show boxplot'),
                       selected = 'original scatterplot'
                       
                     )
    ),
    conditionalPanel("input.sideBarMenu == 'regression'",
                     selectizeInput(
                       inputId = 'outcome',
                       label = 'Outcome variable',
                       choices = c('fastfood', 'freshfood', 'unfreshgroceries', 'alcohol')
                     )
                   ),
    conditionalPanel("input.sideBarMenu == 'pred'",
                     sliderInput(
                       inputId = 'percent_white',
                       label = 'percent white',
                       min = 00,
                       max = 100,
                       value =60,
                       step = 1,
                       animate = animationOptions(interval=200,loop=TRUE)
                     ),
                     sliderInput(
                       inputId = 'percent_black',
                       label = 'percent black',
                       min = 00,
                       max = 100,
                       value =13,
                       step = 1,
                       animate = animationOptions(interval=200,loop=TRUE)
                     ),
                     sliderInput(
                       inputId = 'percent_asian',
                       label = 'percent asian',
                       min = 00,
                       max = 100,
                       value =7,
                       step = 1,
                       animate = animationOptions(interval=200,loop=TRUE)
                     ),
                     sliderInput(
                       inputId = 'percent_hispanic',
                       label = 'percent hispanic',
                       min = 00,
                       max = 100,
                       value =17,
                       step = 1,
                       animate = animationOptions(interval=200,loop=TRUE)
                     )
    )
    
  ),
  
  dashboardBody(
    # tags$head(tags$link(rel = "stylesheet", type = 'text/css', href = "custom.css")),
    tags$head(tags$style(HTML('
        .skin-black .main-header .logo {
                           background: black;
                            color:white;
                            font-size: 10pt;
                            }
        .skin-black .main-header .logo:hover {
                            background: rgb(85,0,0);
                            }
        .skin-black .left-side, .skin-black .main-sidebar, .skin-black .wrapper{
                             background: -webkit-linear-gradient( black , white);
                             }
        .skin-black .main-header .navbar{
                             background: -webkit-linear-gradient(left, black, white);
                            }
        .skin-black .user-panel>.info{
                      display:none;
                             }
            .user-panel>.image>img{
                        max-width: 125px;
                      height: 125px;
                      border-radius:20%;
                       margin-left: 40px;
                              }
                            '))),
    
    tabItems(
      tabItem(
        tabName = 'map',fluidRow(
        box(
          leafletOutput('map', height = 1000),
          width = '100%', height = '100%'
        )),
        h4(tags$b('- The map above consists of 15 years of data from the census and the zip code business patterns data set, which records the total number of businesses by category in a zip code. The interactive choropleth map allows users to explore what areas in a city have access to 
               healthy and unhealthy food resources while the tabs provide info on the socioeconomic and demographic characteristics of these neighborhoods')),
        br(),
        (' -Use the city toggle to select a metropolitan area, and the category toggle to pick an outcome. Then use the year slider the examine how these neighborhoods change over time. There is a play button below the slider!')
        ),
      tabItem(
        tabName = 'scatter',
        fluidRow(box(
          plotlyOutput('scatter'),
          width = '100%', height = '100%'
        )
      ),h4(tags$b('- The above plots transfrom a scatterplot of an outcome variable by percent white into a box plot examining differences in "healthy" food between predominantly black, predominantly white, predominantly hispanic, and heterogeneous neighborhoods')),
      br(),
      br(),
      h4(tags$b('- The boxplots reveal several interesting patterns regarding the relationship between neighborhood racial composition and access to healthy and unhealthy food resources')),
      '-Predominantly black neighborhoods have less access to fresh food compared to other neighborhoods, but at the same time, they also have less access to food options in general, including unfresh food sources, fast food restaurants, and liquor stores',
      br(),
      '- Heterogeneous neighborhoods (areas that are not 70% or more one race) and white neighborhoods have the most outliers, suggesting that there are qualitative differences within these groups that are not entirely captured by just examining neighborhood racial composition',
      br(),
      '- In contrast to the processes taking places in predominantly black neighborhoods, hispanic communities seem to have an excess of food resources, both healthy and unhealthy'
      
      
      ),
      tabItem(
        tabName = 'scatter',
        box(
          plotlyOutput('boxplot', height = 1000),
          width = '100%', height = '100%'
        )
      ),
      tabItem(
        tabName = 'regression',
        fluidRow(
          plotlyOutput('predicted_values'),
          width = '50%', height = '60%'

        ),
        br(),
        h4(tags$b('The barcharts above display the coefficients from a negative binomial regression where I controll for poverty, log of median income, and population is the exposure variable')),
        '- coefficients are in log of rates, and the reference category is percent white. So effects above zero suggest a increasing effect on the outcome variabeles (for example, fastfood restaurants), relative to whites, while below zero demonstrat the opposite, decreasing effects'
      ),
      tabItem(
        tabName = 'pred',
        fluidRow(
        box(
            fluidRow(column(6,plotlyOutput('bar_chart_freshfood')),
            column(6,plotlyOutput('bar_chart_fastfood'))),
            width = '20%', height = '50%'
        ),
        box(
           fluidRow(column(6,plotlyOutput('bar_chart_alcohol')),
                    column(6,plotlyOutput('bar_chart_unfreshgroceries'))),
           width = '20%', height = '50%'
         )

      ),
      h4(tags$b('- For insight into the relative effects of the coefficients from the regression, the bar charts above are the predicted values for neighborhoods at their relative means. Use the slider on the right to examine how manipulating the composition of a neighborhood (while keeping the other values at their relative mean) affects the predicted number of healthy and unhealthy food sources ')),
      h4(tags$b('- For example, the default parameters on the sliders are the mean values for each neighborhood in the data set. So if the user increases, say, percent white, only the green bar will change, reflecting the relative effect of changin the percentage of whites in a neighbohrood'))
      )
      )
    )
    
    
  )
)
