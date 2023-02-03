# TOP ----
library(shiny)
#library(shinydashboard)
#library(dashboardthemes)
library(tidyverse)
library(shinyWidgets)
library(DT)



# Define data object anme
naturtyper <- NULL


# function to read data with progress bar
readData <- function(session, naturtyper) {
  
  progress <- Progress$new(session)
  
  progress$set(value = 0, message = 'Loading...')
  
  naturtyper <<- readRDS("shinyData/naturtyper.rds")

  progress$set(value = 0.25, message = 'Loading...')
  
  progress$close()
}


varList <- c("kartleggingsår", 
             "tilstand", 
             "naturmangfold", 
             "lokalitetskvalitet", 
             "mosaikk", 
             "usikkerhet", 
             "hovedøkosystem", 
             "oppdragstaker",
             "fylke",
             "uk_nærtruet",
             "uk_sentraløkosystemfunksjon",
             "uk_spesieltdårligkartlagt",
             "uk_truet")

varList_special <- c("hovedøkosystem","oppdragstaker", "fylke")

ui <- 
  navbarPage(
    # add title and logos inside a div
    title = "Naturtyper etter Miljødirektoratets Instruks",
    
  
    # '-------------       
    # **TAB 1 ----
    tabPanel("Oversikt",
             sidebarLayout(
               sidebarPanel(width = 3,
                            pickerInput('y_axis_oversikt',
                                         'Hva vil du ha på y-aksen?',
                                         choices = c("Antall_lokaliteter", "Areal_km2"),
                                         selected = "Antall_lokaliteter"
                                         ),
                            pickerInput('x_axis_oversikt',
                                         'Hva vil du ha på x-aksen?',
                                         choices = varList,
                                        selected = "kartleggingsår")
                            ),
             mainPanel(width = 9,
                       tabsetPanel(
                         
                         tabPanel("Figur", 
                                  plotOutput('years')
                                  ),
                         tabPanel("Tabell", 
                                  DTOutput('years_tbl')
                                  )
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
  
  
 
  summary1 <- reactive({naturtyper %>%
                          group_by(myVar = !! rlang::sym(input$x_axis_oversikt)) %>%
                          summarise(Antall_lokaliteter = n(),
                                    Areal_km2 = round(sum(km2), 0)) })
  
  
  
  output$years <- renderPlot({
    
    dat_plot <- summary1()
    if(input$x_axis_oversikt %in% varList_special) dat_plot <- dat_plot %>% mutate(myVar = fct_reorder(factor(myVar), !! rlang::sym(input$y_axis_oversikt)))

    gg_out <- ggplot(dat_plot, aes_string(x = "myVar", y = input$y_axis_oversikt))+
      geom_bar(stat="identity",
               fill = "grey80",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = 12)+
      xlab(input$x_axis_oversikt)
    
    if(input$x_axis_oversikt %in% varList_special) gg_out <- gg_out + coord_flip()
    
    return(gg_out)
  })
  
  
   output$years_tbl <- renderDT({
    summary1() %>%
       rename(!!input$x_axis_oversikt := myVar)
       })

  
  output$placeholder2 <- renderPlot({
    dat <- cars
    plot(dat$speed, dat$dist)
  })
  
  })


shinyApp(ui = ui, server = server)

