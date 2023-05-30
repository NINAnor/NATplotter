# TOP ----
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(data.table)

# Define data object anme
naturtyper <- NULL

# function to read data with progress bar
readData <- function(session, naturtyper) {
  progress <- Progress$new(session)
  
  progress$set(value = 0, message = 'Loading raw data...')
  naturtyper <<- readRDS("shinyData/naturtyper.rds")

  progress$set(value = 0.25, message = 'Loading melted data...')
  naturtyper_long <<- readRDS("shinyData/naturtyper_long.rds")
  
  progress$set(value = 0.5, message = 'Preparing data...')
  
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
             "kriterium_nærTruet",
             "kriterium_sentralØkosystemFunksjon",
             "kriterium_spesieltDårligKartlagt",
             "kriterium_truet")

varList2 <- c("kartleggingsår",
              "måned",
             "tilstand", 
             "naturmangfold", 
             "lokalitetskvalitet", 
             "oppdragstaker",
             "fylke",
             "region",
             "kommuner",
             "mosaikk",
             "km2")




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
                              label = "Der relevant, sorter x-axen etter:",
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
                       ),
                       
                       linebreaks(20),
                       hr(),
                       p("Av: ", tags$a(href="https://github.com/anders-kolstad/", target='_blank', "Anders L. Kolstad")),
                       img(src='NINA_logo_sort_txt_norsk_under.png', align = "right", height=180,width=250)
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
                           ),
                         linebreaks(20),
                         hr(),
                         p("Av: ", tags$a(href="https://github.com/anders-kolstad/", target='_blank', "Anders L. Kolstad")),
                         img(src='NINA_logo_sort_txt_norsk_under.png', align = "right", height=180,width=250)
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
                            ),
                            numericRangeInput(
                              inputId = "years_subset",
                              label = "Filtrer etter kartleggingsår:", 
                              min = 2018,
                              max = 2022,
                              value = c(2018, 2022)
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
                           ),
                         linebreaks(20),
                         hr(),
                         p("Av: ", tags$a(href="https://github.com/anders-kolstad/", target='_blank', "Anders L. Kolstad")),
                         img(src='NINA_logo_sort_txt_norsk_under.png', align = "right", height=180,width=250)
                         )
             )),
    
    # '-------------             
    # **TAB RÅDATA ----   
    tabPanel("Rådata",
             sidebarLayout(
               sidebarPanel(width=3,
                            h5("Her er noen vanlige filtre. Du kan også filtrere hver enkelt kolonne til høyre."),
                            numericRangeInput(
                              inputId = "years_subset_raw",
                              label = "Filtrer etter kartleggingsår:", 
                              min = 2018,
                              max = 2022,
                              value = c(2018, 2022)
                            ),
                            uiOutput('pickNaturtype_raw'),
                            uiOutput('fylke_raw'),
                            uiOutput('kommune_raw')
                            ),
               mainPanel(width=9,
                         DTOutput('raw'))
             )),


    # '-------------             
    # **TAB More ----
    navbarMenu("Mer",
               
               # Instructions----
               tabPanel("Contakt",
                        p("Denne appen er laget av  ", tags$a(href="https://github.com/anders-kolstad/", target='_blank', "Anders L. Kolstad"), "ved NINA, Trondheim")),
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
  summary1 <- reactive({
    naturtyper %>%
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
         pivot_wider(., id_cols = identifikasjon_lokalId,
                     names_from = NiN_variable_code,
                     values_from = NiN_variable_value) %>%
         full_join(select(naturtyper, 
                          identifikasjon_lokalId, 
                          km2, 
                          kartleggingsår),
                   by = "identifikasjon_lokalId") %>%
         select(identifikasjon_lokalId, km2, 2, kartleggingsår)
     } else { select(., identifikasjon_lokalId, km2, 
                     input$variable1, kartleggingsår) %>%
         distinct(., identifikasjon_lokalId, .keep_all = T)}}
   
    if(input$myFacet != "Ingen") {
     temp2 <- naturtyper_long_selected2() %>%
       { if( !input$myFacet %in% varList2) {
         filter(., NiN_variable_code == input$myFacet) %>%
           pivot_wider(., id_cols = identifikasjon_lokalId,
                       names_from = NiN_variable_code,
                       values_from = NiN_variable_value)
       } else { select(., identifikasjon_lokalId, !! rlang::sym(input$myFacet)) %>%
           distinct(., identifikasjon_lokalId, .keep_all = T) }} %>%
       full_join(temp, by = "identifikasjon_lokalId") %>%
       rename(any_of(c('kartleggingsår' = 'kartleggingsår.x'))) %>%
       mutate(year_num = as.numeric(kartleggingsår)) %>%
       filter(year_num %between% input$years_subset) %>%
       rename("second_variable" = 2,
              "first_variable" = 4)
    } else {temp2 <- temp %>%
      mutate(year_num = as.numeric(kartleggingsår)) %>%
      filter(year_num %between% input$years_subset) %>%
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
  if(input$variable1=="km2"){
    if(input$myFacet != "Ingen") {
    naturtyper_long |> 
      filter(naturtype == input$naturtype2) |>
      group_by(identifikasjon_lokalId, .data[[input$myFacet]]) |>
      summarise(Areal_km2 = round(sum(km2), 0)) |>  
      ggplot(aes(Areal_km2))+
      geom_histogram(
        fill = "#FFCC99",
        colour = "grey20",
        linewidth=1.5)+
      theme_bw(base_size = myBase_size)+
        facet_wrap(~.data[[input$myFacet]])
    
    }else{
      naturtyper_long |> 
        filter(naturtype == input$naturtype2) |>
        group_by(identifikasjon_lokalId) |>
        summarise(Areal_km2 = round(sum(km2), 0)) |>  
        ggplot(aes(Areal_km2))+
        geom_histogram(
          fill = "#FFCC99",
          colour = "grey20",
          linewidth=1.5)+
        theme_bw(base_size = myBase_size)
      
    }
    
  }else{
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
  }
  }) 

output$warning2 <- renderText(if(input$variable1 %in% varList_special_trunkert) "Kun de 15 vanligste grupperingene er vist (målt i antall lokaliteter)")


output$ntyp_utvalg_table <- renderDT({
  if(input$variable1=="km2"){
    if(input$myFacet != "Ingen") {
      naturtyper_long |> 
        filter(naturtype == input$naturtype2) |>
        group_by(identifikasjon_lokalId, .data[[input$myFacet]]) |>
        summarise(Areal_km2 = round(sum(km2), 0)) 
      }else{
        naturtyper_long |> 
          filter(naturtype == input$naturtype2) |>
          group_by(identifikasjon_lokalId) |>
          summarise(Areal_km2 = round(sum(km2), 0))
      }
    }else{
  
  naturtyper_selected_var()
    }})

output$info <- renderUI({
  antall <- length(unique(naturtyper$naturtype))
  antall_lok <- nrow(naturtyper)
  tagList(
    p("Denne appen har som hensikt å gjøre det lettere å undersøke datasettet Naturtyper etter Miljødirektoratets Instruks, spesielt med tanke på fordelingen av feltregistrerte variabler og aggregterte tilstand- eller kvalitetsvariabler på tvers av romlig og tidsmessig variasjon."),
  p("See ", tags$a(href="https://github.com/NINAnor/naturtypedata/blob/main/dataRAW.R", target='_blank', "her"), " for detaljer om hvordan datasettet er tilrettelagt. Datasettet består av", antall, "naturtyper som er kartlagt etter Miljødirekatoratets instruks senest i 2021. Dette utgjør", antall_lok, "lokaliteter. Kartleggingsinstruksen inneholder 111 natutyper i 2022. Det er antatt at de resterende typene som ikke finnes i dette datasettet gjelder typer som  kartlegges med fjernmåling, eller som ikke er påmøtt i felt enda. Dette bør undersøkes og evt bekreftes." )
  )
})

output$pickNaturtype <- renderUI({
  tagList(
    pickerInput('naturtype',
                "Velg naturtype",
                choices = sort(unique(naturtyper$naturtype)),
                options = list(
                  `live-search` = TRUE))
  )
})

output$pickNaturtype2 <- renderUI({
  tagList(
    pickerInput('naturtype2',
                "Velg naturtype",
                choices = sort(unique(naturtyper$naturtype)),
                options = list(
                  `live-search` = TRUE
                  ))
    )
  })

#### RÅDATA
output$pickNaturtype_raw <- renderUI({
  tagList(
    pickerInput('naturtype_raw',
                "Velg naturtype",
                choices = sort(unique(naturtyper$naturtype)),
                selected = unique(naturtyper$naturtype),
                multiple = T,
                options = list(
                  `live-search` = TRUE,
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Ingen...",
                  `select-all-text` = "Alle",
                  `none-selected-text` = "..."))
  )
})

output$fylke_raw <- renderUI({
  tagList(
    pickerInput('fylke_raw',
                "Velg fylke",
                choices = sort(unique(naturtyper$fylke)),
                multiple = T,
                selected = unique(naturtyper$fylke),
                options = list(
                  `live-search` = TRUE,
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Ingen...",
                  `select-all-text` = "Alle",
                  `none-selected-text` = "..."))
  )
})

output$kommune_raw <- renderUI({
  tagList(
    pickerInput('kommune_raw',
                "Velg kommune",
                choices = sort(unique(naturtyper$kommuner)),
                multiple = T,
                selected = unique(naturtyper$kommuner),
                options = list(
                  `live-search` = TRUE,
                  `actions-box` = TRUE,
                  `deselect-all-text` = "Ingen...",
                  `select-all-text` = "Alle",
                  `none-selected-text` = "..."))
  )
})

raw_filtered <- reactive({
  naturtyper %>%
    filter(naturtype %in% input$naturtype_raw) %>%
    mutate(year_num = as.numeric(kartleggingsår)) %>%
    filter(year_num %between% input$years_subset_raw,
           fylke %in% input$fylke_raw,
           kommuner %in% input$kommune_raw)
})

output$raw <- renderDT({
  DT::datatable(raw_filtered(),
                options = list(
                  pageLength = 15,  ## number of rows to output for each page
                  scrollX = TRUE,   ## enable scrolling on X axis
                  scrollY = TRUE,   ## enable scrolling on Y axis
                  autoWidth = TRUE ## use smart column width handling
                  ),
                filter = 'top'
  )
})

})



shinyApp(ui = ui, server = server)

