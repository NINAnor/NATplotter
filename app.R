# TOP ----
library(shiny)
#library(shinydashboard)
library(ggplot2)
library(dashboardthemes)
#library(shinyFiles)
#library(shinyWidgets)
library(DT)
#library(uuid)
#library(shinyalert)
#library(tidyverse)
#library(readxl)

# source("R/global.R")


# Set path to data
dir <- substr(getwd(), 1,2)
path <- ifelse(dir == "C:", 
               "P:/41001581_egenutvikling_anders_kolstad/data/",
               "/data/Egenutvikling/41001581_egenutvikling_anders_kolstad/data/")


# Define data object anme
naturtyper <- NULL


# function to read data with progress bar
readData <- function(session, naturtyper) {
  
  progress <- Progress$new(session)
  
  progress$set(value = 0, message = 'Loading...')
  
  naturtyper <<- readRDS(paste0(path, "naturtyper.rds"))

  progress$set(value = 0.25, message = 'Loading...')
  
  progress$close()
}



ui <- 
  navbarPage(
    # add title and logos inside a div
    title = "Naturtyper etter Miljødirektoratets Instruks",
    
  
    # '-------------       
    # **TAB 1 ----
    tabPanel("Oversikt",
             sidebarLayout(
               sidebarPanel(width = 3),
             mainPanel(width = 9,
                       tabsetPanel(
                         tabPanel("Figur", plotOutput('years')),
                         tabPanel("Tabell", DTOutput('years_tbl'))
                       )
                       
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
  
  if(is.null(naturtyper)){
    readData(session, naturtyper)
  }
  
    
    
  output$years <- renderPlot({
    temp <- naturtyper %>%
      group_by(kartleggingsar) %>%
      summarise(count = n())
    
    ggplot(temp, aes(x = kartleggingsar, y = count))+
      geom_bar(stat="identity",
               fill = "grey80",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = 12)+
      labs(x = "År", y = "Antall lokaliteter")
  })
  
  output$years_tbl <- renderDT({
    naturtyper %>%
      group_by('Kartleggingsår' = kartleggingsar) %>%
      summarise(Antall_lokaliteter = n())
  })
  
  
  
  output$placeholder2 <- renderPlot({
    dat <- cars
    plot(dat$speed, dat$dist)
  })
  
  })


shinyApp(ui = ui, server = server)

