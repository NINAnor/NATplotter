# TOP ----
library(shiny)
#library(shinydashboard)
#library(dashboardthemes)
library(tidyverse)
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
               sidebarPanel(width = 3,
                            radioButtons('y-axis-oversikt',
                                         'Hva vil du ha på y-aksen?',
                                         choices = c("Antall_lokaliteter", "Areal_km2"),
                                         selected = "Antall_lokaliteter"
                                         ),
                            radioButtons('x-axis-oversikt',
                                         'Hva vil du ha på x-aksen?',
                                         choices = c("kartleggingsar", 
                                                     "tilstand", 
                                                     "naturmangfold", 
                                                     "lokalitetskvalitet", 
                                                     "mosaikk", 
                                                     "usikkerhet", 
                                                     "hovedøkosystem", 
                                                     "oppdragstaker", 
                                                     "objtype",
                                                     "uk_naertruet",
                                                     "uk_sentralokosystemfunksjon",
                                                     "uk_spesieltdarligkartlagt",
                                                     "uk_truet"),
                                         selected = "kartleggingsar"
                            )),
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
  
  myX <- reactive(rlang::sym(input$x-axis-oversikt))
  myy <- reactive(rlang::sym(input$y-axis-oversikt))
  
  
  summary1 <- reactive({return(tbl_df(naturtyper) %>%
    group_by(myX_var = !!myX()) %>%
    summarise(Antall_lokaliteter = n(),
              Areal_km2 = round(sum(km2), 0)))})
  
  output$years <- renderPlot({
    ggplot(summary1(), aes(x = myX_var, y = !!myy()))+
      geom_bar(stat="identity",
               fill = "grey80",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = 12)+
      xlab(input$x-axis-oversikt)
  })
  
  output$years_tbl <- renderDT({
    summary1()
  })
  
  
  
  output$placeholder2 <- renderPlot({
    dat <- cars
    plot(dat$speed, dat$dist)
  })
  
  })


shinyApp(ui = ui, server = server)

