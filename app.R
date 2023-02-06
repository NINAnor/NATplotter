# TOP ----
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)

# Define data object anme
naturtyper <- NULL

# function to read data with progress bar
readData <- function(session, naturtyper) {
  progress <- Progress$new(session)
  progress$set(value = 0, message = 'Loading data...')
  naturtyper <<- readRDS("shinyData/naturtyper.rds")
  ntyper <<- unique(naturtyper$naturtype)
  progress$set(value = 0.25, message = 'Melting data...')
  # Melted data set
  naturtyper_long <- tidyr::separate_rows(naturtyper, ninbeskrivelsesvariabler, sep=",") %>%
    separate(col = ninbeskrivelsesvariabler,
             into = c("NiN_variable_code", "NiN_variable_value"),
             sep = "_",
             remove=F) %>%
    mutate(NiN_variable_value = as.numeric(NiN_variable_value)) %>%
    filter(!str_detect(NiN_variable_code, "LKM"))
  
  
  progress$set(value = 0.5, message = 'Melting data...')
  progress$close()
}


varList <- c("kartleggingsår", 
             "tilstand", 
             "naturmangfold", 
             "lokalitetskvalitet", 
             "hovedøkosystem", 
             "oppdragstaker",
             "naturtype",
             "fylke",
             "region",
             "kommuner",
             "mosaikk", 
             "usikkerhet", 
             "uk_nærtruet",
             "uk_sentraløkosystemfunksjon",
             "uk_spesieltdårligkartlagt",
             "uk_truet")

varList_special <- c("hovedøkosystem","oppdragstaker", "fylke", "kommuner", "naturtype", "region")
varList_special_trunkert <- c("oppdragstaker", "kommuner", "naturtype")

antall <- length(ntyper)
antall_lok <- nrow((naturtyper))




ui <- 
  navbarPage(
    title = "Naturtyper etter Miljødirektoratets Instruks",
    
    # '-------------       
    # **TAB 1 ----
    tabPanel("Oversikt",
             sidebarLayout(
               sidebarPanel(width = 3,
                            pickerInput('x_axis_oversikt',
                                         'Hva vil du ha på x-aksen?',
                                         choices = varList,
                                        selected = "kartleggingsår"),
                            radioGroupButtons(
                              inputId = "sortBy",
                              label = "(der relevant) Sorter y-axen etter:",
                              choices = c("Antall_lokaliteter", "Areal_km2")
                            )),
             mainPanel(width = 9,
                       tabsetPanel(
                         
                         tabPanel("Figur", 
                                  column(6,
                                    plotOutput('years_count'),
                                    textOutput('warning1')),
                                  column(6,
                                    plotOutput('years_area')),
                                  ),
                         tabPanel("Tabell", 
                                  DTOutput('years_tbl')
                                  )
                       )
                       
                       )
             )),
   
             
    # '-------------       
    # **TAB 2 ----
    tabPanel("Naturtyper",
             sidebarLayout(
               sidebarPanel(width = 3,
                            pickerInput('naturtype',
                                        "Velg naturtype",
                                        choices = ntyper,
                                        options = list(
                                          `live-search` = TRUE))
               ),
               mainPanel(width = 9,
                         column(6, 
                            plotOutput('ntyp_years_count')
                                ),
                         column(6,
                            plotOutput('ntyp_years_area')
                                )
                         )
               )
             ),


    # '-------------             
    # **TAB More ----
    navbarMenu("Mer",
               
               # Instructions----
               tabPanel("Contakt",
                        p("Denne appen er laget av  ", tags$a(href="https://github.com/anders-kolstad/", target='_blank', "Anders L. Kolstad."))),
               tabPanel('Informasjon',
                        p("See ", tags$a(href="https://github.com/NINAnor/naturtypedata/blob/main/dataRAW.R", target='_blank', "her"), " for detaljer om hvordan datasettet er tilrettelagt.
                          Datasettet består av", antall, "naturtyper som er kartlagt etter Miljødirekatoratets instruks senest i 2021. Dette utgjør", antall_lok, "lokaliteter. Kartleggingsinstruksen inneholder 111 natutyper i 2021 (og 2022). Det er antatt at de resterende typene so ikke finnes i dette datasettet gjelder typer som  kartlegges med fjernmåling, eller som ikke er påmøtt i felt enda. Dette bør undersøkes og evt bekreftes." )
                        )
               )
                        
               
    )
    





# SERVER ¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤----------------------------------------------------------------------
# '-------------       



server <- function(input, output, session) ({
  
  if(is.null(naturtyper)){
    readData(session, naturtyper)
  }
  
  
  # OVERSIKT TAB
  summary1 <- reactive({naturtyper %>%
                          group_by(myVar = !! rlang::sym(input$x_axis_oversikt)) %>%
                          summarise(Antall_lokaliteter = n(),
                                    Areal_km2 = round(sum(km2), 0)) })
  
  output$years_count <- renderPlot({
    dat_plot <- summary1()
    if(input$x_axis_oversikt %in% varList_special) dat_plot <- dat_plot %>% mutate(myVar = fct_reorder(factor(myVar), !! rlang::sym(input$sortBy)))
    if(input$x_axis_oversikt %in% varList_special_trunkert) dat_plot <- dat_plot %>% arrange(desc(Antall_lokaliteter)) %>% slice_head(n = 15)
    
    gg_out <- ggplot(dat_plot, aes_string(x = "myVar", y = "Antall_lokaliteter"))+
      geom_bar(stat="identity",
               fill = "#FFCC99",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = 12)+
      xlab(input$x_axis_oversikt)
    if(input$x_axis_oversikt %in% varList_special) gg_out <- gg_out + coord_flip()
    return(gg_out)
  })
  
  output$years_area <- renderPlot({
    dat_plot <- summary1()
    if(input$x_axis_oversikt %in% varList_special) dat_plot <- dat_plot %>% mutate(myVar = fct_reorder(factor(myVar), !! rlang::sym(input$sortBy)))
    if(input$x_axis_oversikt %in% varList_special_trunkert) dat_plot <- dat_plot %>% arrange(desc(Antall_lokaliteter)) %>% slice_head(n = 15)
    gg_out <- ggplot(dat_plot, aes_string(x = "myVar", y = "Areal_km2"))+
      geom_bar(stat="identity",
               fill = "#FF9933",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = 12)+
      xlab(input$x_axis_oversikt)
    if(input$x_axis_oversikt %in% varList_special) gg_out <- gg_out + coord_flip()
    return(gg_out)
  })
  
  output$warning1 <- renderText(if(input$x_axis_oversikt %in% varList_special_trunkert) "Kun de 15 vanligste grupperingene er vist (målt i antall lokaliteter)")
  
  output$years_tbl <- renderDT({
    summary1() %>%
       rename(!!input$x_axis_oversikt := myVar)
       })
  
  
  # NATURTYPE TAB
  ntyp_selected <- reactive({
    naturtyper %>%
      filter(naturtype == input$naturtype) %>%
      group_by(kartleggingsår) %>%
      summarise(Antall_lokaliteter = n(),
                Areal_km2 = round(sum(km2), 0))
  })
  
  
 # ntyp_vars <- 

  output$ntyp_years_count <- renderPlot({
    gg_out <- ggplot(ntyp_selected(), aes(x = kartleggingsår, y = Antall_lokaliteter))+
      geom_bar(stat="identity",
               fill = "#FFCC99",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = 12)
    return(gg_out)
  })
  
  output$ntyp_years_area <- renderPlot({
    gg_out <- ggplot(ntyp_selected(), aes(x = kartleggingsår, y = Areal_km2))+
      geom_bar(stat="identity",
               fill = "#FF9933",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = 12)
    return(gg_out)
  })
  
 #output$ntyp_vars <- renderPlot({
 #  naturtyper_long %>% 
 #    filter(naturtype == input$naturtype) %>%
 #    group_by(NiN_variable_code) %>%
 #    summarise(Antall_lokaliteter = n(),
 #              Areal_km2 = round(sum(km2), 0)) %>%
 #  
 #  gg_out <- ggplot(ntyp_selected(), aes(x = kartleggingsår, y = Areal_km2))+
 #    geom_bar(stat="identity",
 #             fill = "#FF9933",
 #             colour = "grey20",
 #             linewidth=1.5)+
 #    theme_bw(base_size = 12)
 #  return(gg_out)
 #})
  
  })


shinyApp(ui = ui, server = server)

