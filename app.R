# TOP ----
library(shiny)
#library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
#library(shinyFiles)
library(shinyWidgets)
library(DT)
#library(uuid)
#library(shinyalert)
#library(tidyverse)
#library(readxl)

source("R/global.R")




ui <- 
  navbarPage(
    # add title and logos inside a div
    title = "Naturtyper etter Miljødirektoratets Instruks",
    
  
    # '-------------       
    # **TAB 1 ----
    tabPanel("Tab1",
             sidebarLayout(
               sidebarPanel(width = 3),
             mainPanel(width = 9,
                       
                       plotOutput('placeholder')
                       )
             )),
   

             
    # '-------------       
    # **TAB 2 ----
    tabPanel("Tab2",
             plotOutput('placeholder2')),


    # '-------------             
    # **TAB More ----
    navbarMenu("More",
               
               # Instructions----
               tabPanel("Instructions",
                        p("This app was developed by Anders L. Kolstad ", tags$a(href="https://github.com/anders-kolstad/", target='_blank', "")),
                        
                        
                        # Contact ----
                        
                        tabPanel("Contact",
                                 
                                 p("...",style = "width: 500px;")
                        )
               )
    )
    )





# SERVER ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤----------------------------------------------------------------------
# '-------------       



server <- function(input, output, session) ({
  
  output$placeholder <- renderPlot({
    dat <- cars
    plot(dat$speed, dat$dist)
  })
  
  output$placeholder2 <- renderPlot({
    dat <- cars
    plot(dat$speed, dat$dist)
  })
  
  })


shinyApp(ui = ui, server = server)

