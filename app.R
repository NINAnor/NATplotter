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
  progress$set(value = 0, message = 'Loading raw data...')
  naturtyper <<- readRDS("shinyData/naturtyper.rds")
  ntyper <<- unique(naturtyper$naturtype)
  progress$set(value = 0.25, message = 'Loading melted data...')
  naturtyper_long <<- readRDS("shinyData/naturtyper_long.rds")
  progress$set(value = 0.5, message = 'Preparing data...')
  naturtyper_long <<- naturtyper_long %>%
    mutate("måned" = substr(kartleggingsdato, 5, 6))
  naturtyper <<- naturtyper %>%
    mutate("måned" = substr(kartleggingsdato, 5, 6))
  progress$close()
}


varList <- c("kartleggingsår",
             "måned",
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

varList2 <- c("kartleggingsår",
              "måned",
             "tilstand", 
             "naturmangfold", 
             "lokalitetskvalitet", 
             "oppdragstaker",
             "fylke",
             "region",
             "kommuner",
             "mosaikk")




varList_special <- c("hovedøkosystem","oppdragstaker", "fylke", "kommuner", "naturtype", "region")
varList_special_trunkert <- c("oppdragstaker", "kommuner", "naturtype")



myBase_size <- 20


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
                             )
                            ),
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
    tabPanel("Naturtyper - enkel",
             sidebarLayout(
               sidebarPanel(width = 3,
                            uiOutput('pickNaturtype'),
               ),
               mainPanel(width = 9,
                         tabsetPanel(
                           tabPanel("Oversikt",
                              fluidRow(p(strong("Variasjon over år")),),
                              fluidRow(
                                column(6, 
                                       plotOutput('ntyp_years_count')
                                       ),
                                column(6,
                                       plotOutput('ntyp_years_area')
                                       )
                              ),
                              fluidRow(p(strong("Variasjon i tilstand")),),
                              fluidRow(
                                column(6, 
                                       plotOutput('ntyp_tilstand_count')
                                ),
                                column(6,
                                       plotOutput('ntyp_tilstand_area')
                                )
                              ),
                              fluidRow(p(strong("Variasjon i naturmangfold")),),
                              fluidRow(
                                column(6, 
                                       plotOutput('ntyp_natur_count')
                                ),
                                column(6,
                                       plotOutput('ntyp_natur_area')
                                )
                              ),
                              fluidRow(p(strong("Variasjon i lokalitetskvalitet")),),
                              fluidRow(
                                column(6, 
                                       plotOutput('ntyp_kvalitet_count')
                                ),
                                column(6,
                                       plotOutput('ntyp_kvalitet_area')
                                )
                              ),
                              fluidRow(p(strong("Variasjon i øvrige NiN-variabler")),),
                              fluidRow(
                                plotOutput('ntyp_vars',
                                           height = "800px")
                              )
                           ),
                           tabPanel("Tabell",
                                    DTOutput('ntyp_tabell')
                                    )
                           )
                         )
               )
             ),
    tabPanel("Naturtyper - detaljert",
             sidebarLayout(
               sidebarPanel(width=3,
                            uiOutput('pickNaturtype2'),
                            pickerInput('variable1',
                                        "NiN-variabel",
                                        choices = NULL,
                                        options = list(
                                          `live-search` = TRUE)),
                            pickerInput('myFacet',
                                        "Velg evt. gruppering",
                                        choices = c("Ingen", varList2),
                                        options = list(
                                          `live-search` = TRUE)),
                            numericInput("ncols", "Antall kolonner", value = 2, min = 1, max = 4),
                            radioGroupButtons(
                              inputId = "yaxis",
                              label = "Hva skal y-aksen vise?",
                              choices = c("Antall_lokaliteter", "Areal_km2")
                            )
                            ),
               mainPanel(width = 9,
                         fluidRow(p("Her kan du gjøre et enda mer detaljert utvalg for å lage akkurat den figuren du ønsker"),),
                         tabsetPanel(
                           tabPanel("Figur",
                                    plotOutput('ntyp_utvalg',
                                               height = "800px"),
                                    textOutput('warning2')),
                           tabPanel("Tabell",
                                    DTOutput('ntyp_utvalg_table'))
                           )
                         )
             )),


    # '-------------             
    # **TAB More ----
    navbarMenu("Mer",
               
               # Instructions----
               tabPanel("Contakt",
                        p("Denne appen er laget av  ", tags$a(href="https://github.com/anders-kolstad/", target='_blank', "Anders L. Kolstad."))),
               tabPanel('Informasjon',
                        uiOutput('info')
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
      theme_bw(base_size = myBase_size)+
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
      theme_bw(base_size = myBase_size)+
      xlab(input$x_axis_oversikt)
    if(input$x_axis_oversikt %in% varList_special) {
      gg_out <- gg_out + 
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())+
      coord_flip() 
    }
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
  
  ntyp_selected_tilstand <- reactive({
    naturtyper %>%
      filter(naturtype == input$naturtype) %>%
      group_by(tilstand) %>%
      summarise(Antall_lokaliteter = n(),
                Areal_km2 = round(sum(km2), 0))
  })
  
  ntyp_selected_natur <- reactive({
    naturtyper %>%
      filter(naturtype == input$naturtype) %>%
      group_by(naturmangfold) %>%
      summarise(Antall_lokaliteter = n(),
                Areal_km2 = round(sum(km2), 0))
  })
  
  ntyp_selected_kvalitet <- reactive({
    naturtyper %>%
      filter(naturtype == input$naturtype) %>%
      group_by(lokalitetskvalitet) %>%
      summarise(Antall_lokaliteter = n(),
                Areal_km2 = round(sum(km2), 0))
  })
  
  
  output$ntyp_years_count <- renderPlot({
    gg_out <- ggplot(ntyp_selected(), aes(x = kartleggingsår, y = Antall_lokaliteter))+
      geom_bar(stat="identity",
               fill = "#FFCC99",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_years_area <- renderPlot({
    gg_out <- ggplot(ntyp_selected(), aes(x = kartleggingsår, y = Areal_km2))+
      geom_bar(stat="identity",
               fill = "#FF9933",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_tilstand_count <- renderPlot({
    gg_out <- ggplot(ntyp_selected_tilstand(), aes(x = tilstand, y = Antall_lokaliteter))+
      geom_bar(stat="identity",
               fill = "#FFCC99",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_tilstand_area <- renderPlot({
    gg_out <- ggplot(ntyp_selected_tilstand(), aes(x = tilstand, y = Areal_km2))+
      geom_bar(stat="identity",
               fill = "#FF9933",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_natur_count <- renderPlot({
    gg_out <- ggplot(ntyp_selected_natur(), aes(x = naturmangfold, y = Antall_lokaliteter))+
      geom_bar(stat="identity",
               fill = "#FFCC99",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_natur_area <- renderPlot({
    gg_out <- ggplot(ntyp_selected_natur(), aes(x = naturmangfold, y = Areal_km2))+
      geom_bar(stat="identity",
               fill = "#FF9933",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_kvalitet_count <- renderPlot({
    gg_out <- ggplot(ntyp_selected_kvalitet(), aes(x = lokalitetskvalitet, y = Antall_lokaliteter))+
      geom_bar(stat="identity",
               fill = "#FFCC99",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_kvalitet_area <- renderPlot({
    gg_out <- ggplot(ntyp_selected_kvalitet(), aes(x = lokalitetskvalitet, y = Areal_km2))+
      geom_bar(stat="identity",
               fill = "#FF9933",
               colour = "grey20",
               linewidth=1.5)+
      theme_bw(base_size = myBase_size)
    return(gg_out)
  })
  
  output$ntyp_tabell <- renderDT({
    ntyp_selected() 
  })
  
 output$ntyp_vars <- renderPlot({
   naturtyper_long %>% 
     filter(naturtype == input$naturtype) %>%
     group_by(NiN_variable_code, NiN_variable_value) %>%
     summarise(Antall_lokaliteter = n(),
               Areal_km2 = round(sum(km2), 0)) %>%
     ggplot(aes(x = NiN_variable_value, y = Antall_lokaliteter))+
     geom_bar(stat="identity",
              fill = "#FFCC99",
              colour = "grey20",
              linewidth=1.5)+
     theme_bw(base_size = myBase_size)+
     facet_wrap(.~NiN_variable_code,
                scales = "free",
                ncol = 3)
 })
 
 
 naturtyper_long_selected2 <- reactive({
   naturtyper_long %>% 
     filter(naturtype == input$naturtype2)
 })

 # A reactive list of possible variables to look at for each naturtype
 observeEvent(input$naturtype2, {
   updatePickerInput(session = session, inputId = "variable1",
                     choices = list("Generelle variabler" = varList2,
                                    "NiN-variabler" = unique(naturtyper_long_selected2()$NiN_variable_code))) 
 })
 
 
 # Prep data for plotting
 naturtyper_selected_var <- reactive({
   
   temp <- naturtyper_long_selected2() %>%
     { if( !input$variable1 %in% varList2) {
       filter(., NiN_variable_code == input$variable1) %>%
         pivot_wider(., id_cols = identifikasjon_lokalid,
                     names_from = NiN_variable_code,
                     values_from = NiN_variable_value) %>%
         full_join(select(naturtyper, identifikasjon_lokalid, km2),
                   by = "identifikasjon_lokalid") %>%
         select(identifikasjon_lokalid, km2, 2)
     } else { select(., identifikasjon_lokalid, km2, input$variable1) %>%
         distinct(., identifikasjon_lokalid, .keep_all = T)}}
   
    if(input$myFacet != "Ingen") {
     temp2 <- naturtyper_long_selected2() %>%
       { if( !input$myFacet %in% varList2) {
         filter(., NiN_variable_code == input$myFacet) %>%
           pivot_wider(., id_cols = identifikasjon_lokalid,
                       names_from = NiN_variable_code,
                       values_from = NiN_variable_value)
       } else { select(., identifikasjon_lokalid, !! rlang::sym(input$myFacet)) %>%
           distinct(., identifikasjon_lokalid, .keep_all = T) }} %>%
       full_join(temp, by = "identifikasjon_lokalid") %>%
       rename("second_variable" = 2,
              "first_variable" = 4)
    } else {temp2 <- temp %>%
    rename("first_variable" = 3)}
     
   temp_out <- temp2 %>%
     group_by(first_variable) %>%
     {if (input$myFacet != "Ingen") group_by(., second_variable, .add=T) else . } %>%
     summarise(Antall_lokaliteter = n(),
               Areal_km2 = round(sum(km2), 0))
   
   if(input$variable1 %in% varList_special) 
     temp_out <- temp_out %>% 
        mutate(first_variable =
                 fct_reorder(factor(first_variable), !! rlang::sym(input$yaxis)))
   
   if(input$variable1 %in% varList_special_trunkert) 
     temp_out <- temp_out %>%
      arrange(desc(!! rlang::sym(input$yaxis))) %>% 
     slice_head(n = 15)
   
   return(temp_out)
    
 })
   

output$ntyp_utvalg <- renderPlot({
  out <- naturtyper_selected_var() %>%
    ggplot(aes_string(x = "first_variable", y = input$yaxis))+
    geom_bar(stat="identity",
             fill = if_else(input$yaxis == "Antall_lokaliteter", "#FFCC99", "#FF9933"),
             colour = "grey20",
             linewidth=1.5)+
    theme_bw(base_size = myBase_size)+
    xlab(input$variable1)
  if(input$myFacet != "Ingen") {
    out <- out +
      facet_wrap(.~second_variable,
                 ncol = input$ncols)
  }
  if(input$variable1 %in% varList_special) out <- out + coord_flip()
  
  return(out)
})
  

output$warning2 <- renderText(if(input$variable1 %in% varList_special_trunkert) "Kun de 15 vanligste grupperingene er vist (målt i antall lokaliteter)")


output$ntyp_utvalg_table <- renderDT({
  naturtyper_selected_var()
  })

output$info <- renderUI({
  antall <- length(ntyper)
  antall_lok <- nrow(naturtyper)
  tagList(
  p("See ", tags$a(href="https://github.com/NINAnor/naturtypedata/blob/main/dataRAW.R", target='_blank', "her"), " for detaljer om hvordan datasettet er tilrettelagt. Datasettet består av", antall, "naturtyper som er kartlagt etter Miljødirekatoratets instruks senest i 2021. Dette utgjør", antall_lok, "lokaliteter. Kartleggingsinstruksen inneholder 111 natutyper i 2021 (og 2022). Det er antatt at de resterende typene so ikke finnes i dette datasettet gjelder typer som  kartlegges med fjernmåling, eller som ikke er påmøtt i felt enda. Dette bør undersøkes og evt bekreftes." )
  )
})

output$pickNaturtype <- renderUI({
  tagList(
    pickerInput('naturtype',
                "Velg naturtype",
                choices = ntyper,
                options = list(
                  `live-search` = TRUE))
  )
})

output$pickNaturtype2 <- renderUI({
  tagList(
    pickerInput('naturtype2',
                "Velg naturtype",
                choices = ntyper,
                options = list(
                  `live-search` = TRUE))
    )
  })
})



shinyApp(ui = ui, server = server)

