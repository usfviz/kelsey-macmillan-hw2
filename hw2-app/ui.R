library(shiny)
library(ggvis)
library(dplyr)
library(tidyr)

generate_label <- function(color_string, label_string){
  html_string = paste0(c("<svg height='18'width='25'style='display:inline-block; vertical-align: middle' xmlns='http://www.w3.org/2000/svg'>
                         <circle cx='9' cy='9' r='9' fill-opacity='0.9' fill='",
                         color_string,
                         "'/></svg>",
                         label_string),
                       collapse = '')
  print(html_string)
  return(html_string)
}

shinyUI(fluidPage(
  
  # Sidebar with a slider input for the number of bins
  fluidRow(
    column(width=3,
           checkboxGroupInput("region",
                              "Region",
                              choiceNames = list(HTML(generate_label('#1b9e77',"Latin America  and  Caribbean")),
                                                 HTML(generate_label('#d95f02',"South Asia")),
                                                 HTML(generate_label('#7570b3',"Sub-Saharan Africa")),
                                                 HTML(generate_label('#e7298a',"Europe  and  Central Asia")),
                                                 HTML(generate_label('#66a61e',"Middle East  and  North Africa")),
                                                 HTML(generate_label('#e6ab02',"East Asia  and  Pacific")),
                                                 HTML(generate_label('#a6761d',"North America"))),
                              choiceValues = list("Latin America  and  Caribbean",
                                          "South Asia",
                                          "Sub-Saharan Africa",
                                          "Europe  and  Central Asia",
                                          "Middle East  and  North Africa",
                                          "East Asia  and  Pacific",
                                          "North America"),
                              selected = c("Latin America  and  Caribbean",
                                           "South Asia",
                                           "Sub-Saharan Africa",
                                           "Europe  and  Central Asia",
                                           "Middle East  and  North Africa",
                                           "East Asia  and  Pacific",
                                           "North America"))),
    column(width=9,
    # Show a plot of the generated distribution
      ggvisOutput("plot"),
      sliderInput("year", "Year", 1960, 2014, value=1960, step=1, 
                  sep='', width='680px',
                  animate=animationOptions(interval=300))
    )
  )
))