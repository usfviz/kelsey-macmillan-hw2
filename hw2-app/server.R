library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Data import
  pop <- read.csv('population.csv', stringsAsFactors = FALSE) %>%
    gather('Year','Population',5:61) %>%
    rename(Country=Country.Name) %>%
    rename(Country_Code=Country.Code) %>%
    select(one_of(c('Country','Country_Code','Year','Population')))
  
  fert <- read.csv('fertility.csv', skip = 3, stringsAsFactors = FALSE) %>%
    gather('Year','Fertility_Rate',5:61) %>%
    rename(Country=Country.Name) %>%
    rename(Country_Code=Country.Code) %>%
    select(one_of(c('Country','Country_Code','Year','Fertility_Rate')))
  
  life <-read.csv('lifeexpectancy.csv', skip=3, stringsAsFactors = FALSE) %>%
    gather('Year','Life_Expectancy',5:61) %>%
    rename(Country=Country.Name) %>%
    rename(Country_Code=Country.Code) %>%
    select(one_of(c('Country','Country_Code','Year','Life_Expectancy')))
  
  region <- read.csv('country_meta.csv', stringsAsFactors = FALSE) %>%
    rename(Country_Code=Country.Code) %>%
    select(one_of(c('Country_Code','Region')))
  
  df <- pop %>%
    left_join(fert, by = c('Country','Country_Code','Year')) %>%
    left_join(life, by = c('Country','Country_Code','Year')) %>%
    left_join(region, by='Country_Code') %>%
    mutate(Year = gsub('X([0-9]{4})', '\\1', Year)) %>%
    mutate(Region = gsub('(.*)&(.*)', '\\1 and \\2', Region)) %>%
    filter(as.character(Region) != '') %>%
    mutate(id = row_number()) %>%
    mutate(Region = factor(Region))
  
  # Add color and alpha information
  color_df = data.frame(Region=c("Latin America  and  Caribbean",
                                  "South Asia",
                                  "Sub-Saharan Africa",
                                  "Europe  and  Central Asia",
                                  "Middle East  and  North Africa",
                                  "East Asia  and  Pacific",
                                  "North America"),
                        Color=c('#1b9e77','#d95f02','#7570b3','#e7298a',
                                '#66a61e','#e6ab02','#a6761d'))
  
  df <- df %>%
    left_join(color_df, by="Region") %>%
    mutate(Alpha=0.15)
  
  # Generate plot
  v <- reactive({
    
    # Interactive inputs
    input_year <- input$year
    input_regions <- input$region

    tooltip_fun <- function(x) {
      if(is.null(x)) return(NULL)
      paste('Country: ', df$Country[df$id==x$id], 
            '<br>Population:', df$Population[df$id==x$id])
    }
    
    mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
      condition <- eval(substitute(condition), .data, envir)
      .data[condition, ] <- .data[condition, ] %>% mutate(...)
      .data
    }

    df %>%
      filter(Year == input_year) %>%
      mutate_cond(Region %in% input_regions, Alpha=0.9) %>%
      ggvis(x=~Life_Expectancy, y=~Fertility_Rate, fill:=~Color, 
            size=~Population^0.75, key:=~id, opacity:=~Alpha) %>%
      scale_numeric("size", range = c(10^1.2,10^3.4)) %>%
      scale_numeric("x", domain = c(25,90)) %>%
      scale_numeric("y", domain = c(0,9)) %>%
      layer_points() %>%
      hide_legend("fill") %>%
      hide_legend("size") %>%
      add_axis('x', title='Life Expectancy') %>%
      add_axis('y', title='Fertility Rate') %>%
      add_tooltip(tooltip_fun, "hover") %>%
      add_tooltip(tooltip_fun, "click") %>%
      set_options(height = 480, width = 680)
  })
  
  v %>% bind_shiny("plot")
  
})