library(shiny)
library(shinydashboard)
library(data.table)
library(shinyFiles)
library(highcharter)
library(shinycssloaders)
library(colourpicker)
library(httr)
library(purrr)
library(dplyr)
library(jsonlite)

get_data <- function() {
  map_reg <-
    read.csv(file = "/home/biagio/R/COVID-19-master/dati-regioni/dpc-covid19-ita-regioni-latest.csv", stringsAsFactors = FALSE)
  map_reg <- map_reg[-3,]
  map_reg <-
    map_reg[order(map_reg$codice_regione, decreasing = FALSE), ]
  rownames(map_reg) <- 1:nrow(map_reg)
  
  dati_mappa <-
    map_reg[c(3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)]
  
  names(dati_mappa)[names(dati_mappa) == "codice_regione"] <- "id"
  names(dati_mappa)[names(dati_mappa) == "denominazione_regione"] <-
    "name"
  
  return(dati_mappa)
  
}


get_ita <- function() {
  ita <- read_json("/home/biagio/R/data_plot/json_prov.json")
  return(ita)
  
}

get_map <- function() {
  map <-
    "https://raw.githubusercontent.com/stefanocudini/leaflet-geojson-selector/master/examples/italy-regions.json" %>%
    GET() %>%
    content() %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  
  return(map)
  
}


ui <- dashboardPage(
  #HEADER
  dashboardHeader(title =
                    'COVID-19 - DVS',
                  
                  tags$li(div(class = "visitors-1","Data ultimo aggiornamento:"),
                          class = "dropdown"),
                  tags$li(div(class = "visitors-1",tags$b(textOutput("date_end"))),
                          class = "dropdown"),
                  tags$li(div(class = "visitors",HTML("<i class='fas fa-eye'></i>")),
                          class = "dropdown"),
                  tags$li(div(class = "visitors",tags$b(textOutput("counter"))),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.devstatistics.com/', target="_blank",
                            img(
                              src = "logo_icon.png",
                              widht = "20px",
                              height = "20px"
                            )),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.facebook.com/devstatistics/', target="_blank",
                            img(
                              src = "fb.png",
                              widht = "20px",
                              height = "20px"
                            )),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.instagram.com/devstatistics/?hl=it/', target="_blank",
                            img(
                              src = "ig.png",
                              widht = "20px",
                              height = "20px"
                            )),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.linkedin.com/in/dev-statistics-8284481a7', target="_blank",
                            img(
                              src = "lin.png",
                              widht = "25px",
                              height = "25px"
                            )),
                          class = "dropdown"),
                  tags$li(a(href = 'https://twitter.com/devstatistics', target="_blank",
                            img(
                              src = "twi.png",
                              widht = "25px",
                              height = "25px"
                            )),
                          class = "dropdown")
   

  ), 
  #SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Introduzione",
        tabName = "Introduzione", 
        icon = icon("home")),
      menuItem(
        "Data", 
        tabName = "Data", 
        icon = icon("database")),
      menuItem(
        "Charts", 
        tabName = "Charts", 
        icon = icon("chart-line")),
      menuItem(
        "Maps",
        tabName = "Maps",
        icon = icon("map-marked-alt")
      ),
      menuItem(
        "Summary",
        tabName = "Summary",
        icon = icon("tachometer-alt")
      ),
      menuItem("Information", tabName = "Information", icon = icon("info"))
      
    )
  ),
  
  #BODY
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    
    #---------------------------------- INTRODUZIONE UI ----------------------------------#
    
    tabItems(
      tabItem(
        tabName = "Introduzione",
        
        div (class = "intro_logo_counter",
          class = "text-center intro_logo",
          img(
            src = "logo.png",
            widht = "140px",
            height = "70px"
          ),
        ),
        
        div(
          class = "text-justify row",
          name = "intro1",
          
          h1("Dashboard Covid-19", class = "text-center"),
          br(),
          p(class="text-padding-fix",
            "Benvenuto nella Dashboard Covid-19 sviluppata dal team di DevStatistics. Lo scopo del nostro lavoro è quello di
                    creare uno strumento per la visualizzazione dei dati relativi alla epidemia attualmente in corso, che sia fruibile
                    e gratuito per tutti gli utenti."
          ),
          p(class="text-padding-fix",
            "La dashboard è composta da 5 sezioni principali: Data, Charts, Summary, Maps e Information."
          ),
          
          div(class="list-intro",
        
          tags$li(
            class = "intro",
            "Dati: Visualizza e scarica i dataset utilizzati per creare la dashboard o l'intera",
            a(
              href = "https://github.com/pcm-dpc/COVID-19",
              target = "_blank",
              "Repository Git Hub della Protezione Civile"
            ),
            "della protezione civile"
          ),
          tags$li(
            class = "intro",
            "Charts: Crea, personalizza e scarica i grafici dei dati relativi al Coronavirus"
          ),
          tags$li(
            class = "intro",
            "Maps: Visualizza, personalizza e scarica i tuoi grafici su mappa"
          ),
          tags$li(
            class = "intro",
            "Summary: Visualizza i dati di maggior interesse selezionati dal nostro team"
          ),
          tags$li(
            class = "intro",
            "Information: Trova informazioni e link utili per rimanere aggiornare sulle ultime news in tema Coronavirus"
          ),
        ),
          br(),
          
          hr(),
          br(),
        
        ),
        
        
        fluidRow(
          class = "container-fluid",
          
          box(
            class = 'col-sm-12',
            title = "Casi pazienti ospedalizzati",
            width = 6,
            solidHeader = FALSE,
            collapsible = TRUE,
            status = "success",
            withSpinner(
              highchartOutput("bar_intro"),
              type = getOption("spinner.type", default = 1),
              color = getOption("spinner.color", default = "#0275D8"),
              color.background = getOption("spinner.color.background", "#ff7e7e")
            ),
          ),
          
          box(
            class = 'col-sm-12',
            title = "Percentuale di Positivi, Deceduti e Guariti sul Totale dei casi",
            width = 6,
            solidHeader = FALSE,
            collapsible = TRUE,
            status = "success",
            withSpinner(
              highchartOutput("pie_intro"),
              type = getOption("spinner.type", default = 1),
              color = getOption("spinner.color", default = "#0275D8"),
              color.background = getOption("spinner.color.background", "#ff7e7e")
            ),
          ),
        ),
      ),
      
      
      #---------------------------------- DATA UI ----------------------------------#
      
      
      
      tabItem(
        tabName = "Data",
        class = 'row',
        h1("Dati messi a disposizione della protezione civile", class = "text-center"),
        br(),
        p(class="text-padding-fix",
          "In questa sezione della dashboard sono messi a disposizione i dati relativi al COVID-19 forniti dalla protezione c
                      ivile e disponibili sulla repository di",
          a(href = "https://github.com/", target = "_blank", "GitHub"),
          "aggiornata ogni giorno.",
          "La Repository è raggiungibile da qui:",
          a(
            href = "https://github.com/pcm-dpc/COVID-19",
            target = "_blank",
            "Repository Git Hub Protezione Civile"
          ),
          class = "text-justify"
        ),
        p(class="text-padding-fix",
          "Navigando la Dashboard è comunque possibile visionare e scaricare i dati che
                      il nostro Team ha ritenuto più utili al fine di informarsi
                      sull'attuale stato dell'emergenza COVID-19.",
          class = "text-justify"
        ),
        br(),
        
        
        fluidRow(class = "data-box",
          box(
            title = "Visualizza dati",
            width = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success",
            
            selectInput(
              "data_type",
              p("Livello di dettaglio dati"),
              c(
                "Casi a livello Nazionale" = "dati-andamento-nazionale",
                "Casi a livello Regionale" = "dati-regioni",
                "Casi a livello Provinciale" = "dati-province"
              )
            ),
            
            selectInput(
              inputId = 'file',
              label = p("Seleziona il file da visualizzare"),
              choices = list.files(
                path = "/home/biagio/R/COVID-19-master/",
                full.names = FALSE,
                recursive = FALSE
              )
            ),
            div(
              class = "text-center",
              actionButton(
                inputId = "action",
                style = "font-size: 18px;",
                label = "View",
                icon = icon("eye")
              )
            ),
          ),
          
          box(
            title = "Scarica Dati",
            widht = 6,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success",
            p(
              tags$b(
                "Per scaricare il file che hai selezionato premi il tasto",
                tags$u("Download File")
              )
            ),
            div(
              class = "text-center",
              downloadButton("downloadData",
                             style = "font-size: 18px;",
                             "Download Files")
            ),
            hr(),
            p(
              tags$b(
                "Per scaricare l'intera repository della protezione civile premi il pulsante",
                tags$u("Download Repository")
              )
            ),
            
            div(
              class = "text-center",
              actionButton(
                "downloadRepo",
                "Download Repository",
                style = "font-size: 18px;",
                onclick = "location.href='https://codeload.github.com/pcm-dpc/COVID-19/zip/master'",
                icon = icon("github")
              )
            ),
          ),
        ),
        br(),
        DT::dataTableOutput("data_vis"),
        
        uiOutput("pdfview"),
        br(),
        br(),
        
      ),
      #fine tag item data
      
      #---------------------------------- CHARTS UI ----------------------------------#
      
      tabItem(
        tabName = "Charts",
        
        br(),
        
        fluidRow(
          class = 'container-fluid',

          box(
            title = "Dati",
            width = 5,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success",
            
            selectInput(
              "data_type1",
              p("Livello di dettaglio dati"),
              c(
                "Nazionale" = "dati-nazionali",
                "Regionale" = "dati-regioni",
                "Provinciale" = "dati-province"
              ),
              selected = "dati"
            ),
            
            selectInput(
              "data_graph",
              p("Dataset"),
              c(
                "Casi Positivi, Guariti e Deceduti" = "andamento_nazionale",
                "Pazienti Ospedalizzati" = "pazienti_ospedalizzati",
                "Tamponi e Nuovi Positivi" = "tamponi_e_positivi",
                "Nuovi casi al netto di Guariti e Deceduti" = "casi_netti"
              ),
              selected = "andamento_nazionale"
            ),
            
            conditionalPanel(
              condition = 'input.data_type1 == "dati-regioni"',
              selectInput(
                "choose_region",
                "Regioni",
                choices = list.files(
                  path = "/home/biagio/R/data_plot/regioni/",
                  full.names = FALSE,
                  recursive = FALSE
                ),
              )
            ),
            
            conditionalPanel(
              condition = 'input.data_type1 == "dati-province"',
              selectInput(
                "choose_province",
                "Province",
                choices = list.files(
                  path = "/home/biagio/R/data_plot/province/",
                  full.names = FALSE,
                  recursive = FALSE
                ),
              )
            ),
            
            conditionalPanel(
              condition = 'input.data_graph != "tot_casi"',
              selectInput(
                "graph_time",
                p("Tipologia dato"),
                c(
                  "Giornaliero" = "_giorn",
                  "Media mobile 7 giorni" = "_mm",
                  "Cumulativo" = "_cumulativo"
                ),
                selected = "_giorn"
              ),
            ),
            
            div(class = "description",
                
            p(tags$b("Descrizione dei dati")),
            textOutput("description"),
            
            ),
            
          ),
          box(
            title = "Pannello di personalizzazione",
            width = 7,
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "success",
            
            
            selectInput(
              "graph_type",
              p("Tipo di grafico"),
              c(
                "Grafico a linee" = "line",
                "Grafico ad area" = "area",
                "Scatter Plot" = "scatter",
                "Bar Plot" = "stacked_bar"
              ),
              selected = "line"
            ),
            
            selectInput(
              "graph_theme",
              p("Tema del grafico"),
              c(
                "Google" = "google",
                "Economist" = "economist",
                "Fivethirtyeight" = "538",
                "Financial Times" = "financial_times",
                "Dotabuff" = "dotabuff",
                "Flat" = "flat",
                "Simple" = "simple",
                "Elementary" = "elementary",
                "Monokai" = "monokai"
              ),
              selected = "google"
            ),
            

                p(tags$b("Colori del grafico")),
                fluidRow( class = "row-color",
                  conditionalPanel(
                    condition = 'input.data_graph != "tot_casi"',
                  column(2,
                         colourInput("col1", "", "#00cdab"),
                  ),
                  column(2,
                         colourInput("col2", "", "#a05dcf"),
                  ),
                  column(2,
                         conditionalPanel(condition = 'input.data_graph != "casi_netti" && input.data_graph != "tamponi_e_positivi"',
                                          colourInput("col3", "", "#294187"),),
                  ),
                ),
                  column(6,
                         div(class = "option_graph",
                         checkboxInput(label = "Punti su linea", "point_status", value = FALSE),
                         checkboxInput(label = "Legenda", "legend", value = TRUE),
                  ),
                  ),                       
                
                ),

            
            
            conditionalPanel(
              condition = 'input.data_graph != "tot_casi"',
            div(class="range",  
              dateRangeInput('range',
                             label = p(tags$b("Intervallo di tempo")),
                             start = "2020-02-25", end = Sys.Date(),
                             min = "2020-02-25", max = Sys.Date(),
                             separator = " to ", format = "yyyy-mm-dd",
                             language = 'it',
              )
            ),
            ),
            
            div(
              class = "text-center",
              actionButton(
                inputId = "plot",
                style = "font-size: 18px;",
                label = "View",
                icon = icon("chart-bar")
              )
            ),
          ),
        ),
        
        hr(),
        
        fluidRow(
          class = "container-fluid-fixed",
          
          box(
            # class = 'col-sm-12',
            title = textOutput('plot_title'),
            width = 12,
            height = "600px",
            solidHeader = FALSE,
            collapsible = TRUE,
            status = "success",
            withSpinner(
              highchartOutput("plot1", width = "100%", height = "500px"),
              type = getOption("spinner.type", default = 1),
              color = getOption("spinner.color", default = "#0275D8"),
              color.background = getOption("spinner.color.background", "#ff7e7e")
            )
          ),
        
        ),
        
      ),
      #fine tag item charts
      
      tabItem(
        tabName = "Maps",
        
        box(
          title = "Dati",
          width = 3,
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "success",
          
          
          selectInput(
            "map_data",
            p("Regioni/Province"),
            c("Regioni" = "region_map",
              "Province" = "province_map"),
            selected = "province_map"
          ),
          
          selectInput(
            "map_type",
            p("Seleziona il dataset"),
            c("Casi Totali" = "casi_totali"),
            selected = "casi_totali"
          ),
          
          selectInput(
            "map_set_type",
            p("Seleziona il tipo di mappa"),
            c("Gradient Map" = "gradient_map",
              "Bubble Map" = "bubble_map"),
            selected = "gradient_map"
          ),
          
          fluidRow( class = "row-color",
                             colourInput("col_map", p("Seleziona il colore"), "#6E00CF"),
          ),
          
          div(
            class = "text-center",
            actionButton(
              inputId = "map_button",
              style = "font-size: 18px;",
              label = "View Map",
              icon = icon("chart-bar")
            )
          ),
          
        ),
        box(
          title = "Mappa",
          width = 9,
          heigth = "800px",
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "success",
          withSpinner(
            highchartOutput("map",  width = "100%", height = "800px"),
            type = getOption("spinner.type", default = 1),
            color = getOption("spinner.color", default = "#0275D8"),
            color.background = getOption("spinner.color.background", "#ff7e7e")
          )
        )
      ),
      

      tabItem(
        tabName = "Summary",
        
        fluidRow(
          div(
            class = 'col-sm-12 button-stats',
            
            valueBoxOutput("box_positivi_2", width = 3),
            valueBoxOutput("box_deceduti_2", width = 3),
            valueBoxOutput("box_guariti_2", width = 3),
            valueBoxOutput("box_casi_totali_2", width = 3),
            
          ),
        ),
        
        fluidRow(
          box(
            class = "summary",
            status = "primary",
            width = 4,
            title = "",
            withSpinner(
              highchartOutput("s_plot1"),
              type = getOption("spinner.type", default = 1),
              color = getOption("spinner.color", default = "#0275D8"),
              color.background = getOption("spinner.color.background", "#ff7e7e")
            )
          ),
          box(
            width = 8,
            title = "",
            status = "primary",
            withSpinner(
              highchartOutput("s_plot2"),
              type = getOption("spinner.type", default = 1),
              color = getOption("spinner.color", default = "#0275D8"),
              color.background = getOption("spinner.color.background", "#ff7e7e")
            )
          )
        ),
        
        fluidRow(class = "container-fluid-fixed",
          box(
          width = 12,
          title = "",
          status = "primary",
          withSpinner(
            highchartOutput("s_plot4"),
            type = getOption("spinner.type", default = 1),
            color = getOption("spinner.color", default = "#0275D8"),
            color.background = getOption("spinner.color.background", "#ff7e7e")
          )
        ), ),
        
        fluidRow(
          box(
            title = "",
            heigth = "500px",
            status = "primary",
            withSpinner(
              highchartOutput("s_plot5"),
              type = getOption("spinner.type", default = 1),
              color = getOption("spinner.color", default = "#0275D8"),
              color.background = getOption("spinner.color.background", "#ff7e7e")
            )
          ),
          
          box(
            title = "",
            heigth = "500px",
            status = "primary",
            withSpinner(
              highchartOutput("s_map", width = "100%"),
              type = getOption("spinner.type", default = 1),
              color = getOption("spinner.color", default = "#0275D8"),
              color.background = getOption("spinner.color.background", "#ff7e7e")
            )
          )
        ),
        
        fluidRow(class = "container-fluid-fixed",
          box(
            title = "Dati elaborati:",
            status = "primary",
            width = 12,
            DT::dataTableOutput("data_vis2")
          )
          
        ),
        
      )
      
    )#fine tag items sidebar
  )
)



server <- function(input, output, session) {
  
  
  #------------------------------------- TEST SERVER AREA ----------------------------------#

  observe({
  
  if(input$data_type1 == "dati-nazionali"){
    detail_lev <- "nazionale."
  }
  if(input$data_type1 == "dati-regioni"){
    detail_lev <- "regionale."
  }
  
    if(input$data_graph == "andamento_nazionale" || input$data_graph == "andamento_regionale"){
      data_ref <- "ai casi positivi, guariti e deceduti"
    }
    
    if(input$data_graph == "pazienti_ospedalizzati"){
      data_ref <- "ai pazienti ospedalizzati"
    }
    
    if(input$data_graph == "tamponi_e_positivi"){
      data_ref <- "ai tamponi effettuati ed al numero di positivi rilevati"
    }
    
    if(input$data_graph == "casi_netti"){
      data_ref <- "ai nuovi casi positivi al netto di guariti e deceduti"
    }
    
    if(input$data_graph == "tot_casi"){
      data_ref <- "al totale dei casi"
    }
    
    if(input$graph_time == "_giorn"){
      d_type <- "la variazione giornaliera"
    }
    
    if(input$graph_time == "_mm"){
      d_type <- "la media mobile per 7 giorni"
    }
    
    if(input$graph_time == "_cumulativo"){
      d_type <- "il totale dei casi rilevati"
    }
    
    if(input$data_type1 == "dati-province"){
      detail_lev <- "provinciale."
      d_type <- "il totale dei casi rilevati"
    }
      
      
  start <- input$range[1]
  end <- input$range[2]
    
  output$description <-
    renderText({
      paste("I dati selezionati sono relativi",data_ref,
            "con livello di dettaglio", detail_lev, 
            "I dati rappresentano", d_type, 
            "e si riferiscono al periodo che va dal", start,
            "fino al", end,
            
            sep=" ")
    })
  
  })
  
  
  #---------------------------------- INTRODUZIONE SERVER ----------------------------------#
  
  output$counter <- renderText({
    if (!file.exists("/home/biagio/ShinyAppTest/dashboard/counter.RData")) {
      counter <- 0
      
    } else{
      load(file = "/home/biagio/ShinyAppTest/dashboard/counter.RData")
      counter <- counter + 1
      save(counter, file = "/home/biagio/ShinyAppTest/dashboard/counter.RData")
      counter
    }
  })
  
  bar_plot_intro <<-
    read.csv(text = readLines("/home/biagio/R/data_plot/intro_plot.csv"),
             header = TRUE)
  output$bar_intro <- renderHighchart({

    color1 <- "#00c0ef"
    color2 <- "#675ad0"
    color3 <- "#11ce3a"
    highchart() %>%
      hc_xAxis(categories = sum$data,
               title = list(text = "Serie Storica")) %>%
      hc_title(text = "Casi pazienti ospedalizzati") %>%
      hc_subtitle(text = "Giornaliero",
                  style = list(color = "#94908F", useHTML = TRUE)) %>%
      hc_add_series(
        sum$totale_ospedalizzati_giorn,
        type = "area",
        name = "Var giornaliera del numero totale di ospedalizzati",
        color = color1,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        sum$ricoverati_con_sintomi_giorn,
        type = "area",
        name = "Var giornaliera del numero di ricoverati con sintomi",
        color = color2,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        sum$terapia_intensiva_giorn,
        type = "area",
        name = "Var giornaliera del numero di ricoverati in terapia intensiva",
        color = color3,
        showInLegend = TRUE
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(title = list(text = "Numero Casi"),
               plotLines = list(list(
                 value = 0,
                 color = 'black',
                 width = 2
               ))) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
  })
  
  #PLOTTO GRAFICI INTRO
  output$pie_intro <- renderHighchart({
    cols_pie <- c("#00cdab", "#a05dcf", "#294187")
    
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 45,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(
        depth = 50,
        allowPointSelect = TRUE,
        cursor = 'pointer',
        dataLabels = list(enabled = TRUE,
                          format = '{point.name}')
      )) %>%
      hc_colors(cols_pie) %>%
      hc_title(text = "Percentuale di Positivi, Deceduti e Guariti sul Totale dei casi") %>%
      hc_add_series_labels_values(
        labels = bar_plot_intro$data_name,
        values = bar_plot_intro$perc,
        name = "%",
        showInLegend = TRUE
      ) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(title = list(text = "Numero Casi")) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
  })
  
  #---------------------------------- DATA SERVER ----------------------------------#
  
  #RENDO DINAMICA LA CREAZIONE DEL SECONDO SELECT INPUT FILE
  observe({
    path_file <-
      paste("/home/biagio/R/COVID-19-master/", input$data_type, sep = "")
    updateSelectInput(session,
                      "file",
                      choices = list.files(
                        path = path_file,
                        full.names = FALSE,
                        recursive = FALSE
                      ))
    
  })
  
  #CREO DATASET PER FILE DOWNLOAD
  datasetInput <- reactive({
    path <-
      paste("/home/biagio/R/COVID-19-master/",
            input$data_type,
            "/",
            input$file,
            sep = "")
    read.csv(text = readLines(path), header = TRUE)
  })
  
  #STAMPO IL DATASET IN BASE ALLA SCELTA DELL'UTENTE
  observeEvent(input$action, {
    path <-
      paste("/home/biagio/R/COVID-19-master/",
            input$data_type,
            "/",
            input$file,
            sep = "")
    x <- read.csv(text = readLines(path), header = TRUE)
    
    output$data_vis <- DT::renderDataTable(
      x,
      style = 'bootstrap',
      class = 'cell-border stripe',
      options = list(
        ordering = F,
        processing = TRUE,
        scrollX = TRUE,
        columnDefs = list(list(
          className = 'dt-center', targets = "_all"
        ))
      )
    )
  })
  
  #PULSANTE DI DOWNLOAD FILE
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$data_type, "-", "Covid19-DevStats-pcm", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  
  #---------------------------------- CHARTS SERVER ----------------------------------#
  
  path_test <<- paste("/home/biagio/R/data_plot/info_box.csv")
  test <<- read.csv(text = readLines(path_test), header = TRUE)
  
  #UPDATE SELECT INPUT BOX IN CHARTS
  
  #UPDATE DATA GRAPH
  observe({
    if (input$data_type1 == "dati-nazionali") {
      updateSelectInput(
        session,
        "data_graph",
        choices = c(
          "Casi a livello nazionale" = "andamento_nazionale",
          "Pazienti Ospedalizzati" = "pazienti_ospedalizzati",
          "Tamponi e Nuovi Positivi" = "tamponi_e_positivi",
          "Nuovi casi al netto di Guariti e Deceduti" = "casi_netti"
        )
      )
      
    }
    
    if (input$data_type1 == "dati-regioni") {
      updateSelectInput(
        session,
        "data_graph",
        choices = c(
          "Casi a livello regionale" = "andamento_regionale",
          "Pazienti Ospedalizzati" = "pazienti_ospedalizzati",
          "Tamponi e Nuovi Positivi" = "tamponi_e_positivi",
          "Nuovi casi al netto di Guariti e Deceduti" = "casi_netti"
        )
      )
      
    }
    
    if (input$data_type1 == "dati-province") {
      updateSelectInput(
        session,
        "data_graph",
        choices = c("Totale casi per provincia" = "tot_casi")
      )
      
    }
    
  })
  
  
  #UPDATE GRAPH TIME
  observe({
    if (input$data_graph == "andamento_nazionale" ||
        input$data_graph == "pazienti_ospedalizzati" ||
        input$data_graph == "tamponi_e_positivi") {
      updateSelectInput(
        session,
        "graph_time",
        choices = c(
          "Giornaliero" = "_giorn",
          "Media mobile 7 giorni" = "_mm",
          "Cumulativo" = "_cumulativo"
        )
      )
      
    }
    if (input$data_graph == "casi_netti") {
      updateSelectInput(
        session,
        "graph_time",
        choices = c(
          "Giornaliero" = "_giorn",
          "Media mobile 7 giorni" = "_mm"
        )
      )
      
    }
    
    
    path_y <<- paste("/home/biagio/R/data_plot/dati_nazionali.csv")
    y <<- read.csv(text = readLines(path_y), header = TRUE)
    
    
    #UPDATE MAP SELECT BOX
    if (input$map_data == "province_map") {
      updateSelectInput(
        session,
        "map_type",
        choices = c("Casi Totali" = "casi_totali"),
        selected = "casi_totali"
      )
      
    }
    
    if (input$map_data == "region_map") {
      updateSelectInput(
        session,
        "map_type",
        choices =  c(
          "Ricoverati con sintomi" = "ricoverati_con_sintomi",
          "Terapia Intensiva" = "terapia_intensiva",
          "Totale ospedalizzati" = "totale_ospedalizzati",
          "Isolamento domiciliare" = "isolamento_domiciliare",
          "Totale Positivi" = "totale_positivi",
          "Variazione totale Positivi" = "variazione_totale_positivi",
          "Nuovi Positivi" = "nuovi_positivi",
          "Dimessi Guariti" = "dimessi_guariti",
          "Deceduti" = "deceduti",
          "Totale Casi" = "totale_casi",
          "Tamponi" = "tamponi",
          "Casi Testati" = "casi_testati"
        ),
        selected = "ricoverati_con_sintomi"
      )
      
      
    }
    
    
    
    #INPUT DATARANGE
    rows <- nrow(y)
    date_start<-as.character(y[1,"data"])
    date_end<-as.character(y[rows,"data"])
    
    updateDateRangeInput(session, "range",
                         label = "Intervallo di tempo",
                         start = date_start,
                         end = date_end,
                         min = date_start,
                         max = date_end
    )
    
    output$date_end <- renderText({
      
      date_end<-as.character(y[rows,"data"])
      
    })
    
    
  })
  
  
  # PRE PLOT CHARTS SECTION
  output$plot1 <- renderHighchart({
    path_y <<- paste("/home/biagio/R/data_plot/dati_nazionali.csv")
    y <<- read.csv(text = readLines(path_y), header = TRUE)
    
    n_max <- nrow(y)
    theme <- hc_theme_smpl()
    
    path_x <<- paste("/home/biagio/R/data_plot/dati_nazionali.csv")
    pre_plot <<- read.csv(text = readLines(path_x), header = TRUE)
    
    highchart() %>%
      hc_xAxis(categories = pre_plot$data[1:n_max],
               title = list(text = "Serie Storica")) %>%
      hc_title(text = "Casi Positivi, Guariti e Deceduti Italia") %>%
      hc_subtitle(text = "Giornaliero",
                  style = list(color = "#94908F", useHTML = TRUE)) %>%
      hc_add_series(
        pre_plot$nuovi_positivi[1:n_max],
        type = 'line',
        name = "Var giornaliera del numero di Positivi",
        color = input$col1,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        pre_plot$dimessi_giorn[1:n_max],
        type = 'line',
        name = "Var giornaliera del numero di Dimessi Guariti",
        color = input$col2,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        pre_plot$deceduti_giorn[1:n_max],
        type = 'line',
        name = "Var giornaliera del numero di Decedutii",
        color = input$col3,
        showInLegend = TRUE
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
      hc_yAxis(title = list(text = "Numero Casi")) %>%
      hc_add_theme(theme) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
  })
  
  
  observeEvent(input$plot, {
    if (input$data_type1 == "dati-nazionali") {
      path_x <<- paste("/home/biagio/R/data_plot/dati_nazionali.csv")
    }
    if (input$data_type1 == "dati-regioni") {
      path_x <<-
        paste("/home/biagio/R/data_plot/regioni/",
              input$choose_region,
              sep = "")
    }
    if (input$data_type1 == "dati-province") {
      path_x <<-
        paste("/home/biagio/R/data_plot/province/",
              input$choose_province,
              sep = "")
    }
    
    x <<- read.csv(text = readLines(path_x), header = TRUE)
    
    if (input$graph_theme == "google") {
      theme <- hc_theme_google()
    }
    if (input$graph_theme == "economist") {
      theme <- hc_theme_economist()
    }
    if (input$graph_theme == "538") {
      theme <- hc_theme_538()
    }
    if (input$graph_theme == "financial_times") {
      theme <- hc_theme_ft()
    }
    if (input$graph_theme == "dotabuff") {
      theme <- hc_theme_db()
    }
    if (input$graph_theme == "flat") {
      theme <- hc_theme_flat()
    }
    if (input$graph_theme == "simple") {
      theme <- hc_theme_smpl()
    }
    if (input$graph_theme == "elementary") {
      theme <- hc_theme_elementary()
    }
    if (input$graph_theme == "monokai") {
      theme <- hc_theme_monokai()
    }
    
    if (input$graph_type == "line") {
      type <- "line"
    }
    if (input$graph_type == "area") {
      type <- "area"
    }
    if (input$graph_type == "scatter") {
      type <- "scatter"
    }
    if (input$graph_type == "stacked_bar") {
      type <- "column"
    }
    
    if (input$point_status == FALSE) {
      status <- FALSE
    }
    if (input$point_status == TRUE ||
        input$graph_type == "scatter") {
      status <- TRUE
    }
    
    if (input$legend == TRUE) {
      legend <- TRUE
    }
    if (input$legend == FALSE) {
      legend <- FALSE
    }
    
    if (input$data_type1 == "dati-nazionali") {
      dyn_title <- "Italia"
    }
    if (input$data_type1 == "dati-regioni") {
      dyn_title <- input$choose_region
      dyn_title <- substr(dyn_title, 1, nchar(dyn_title) - 4)
    }
    if (input$data_type1 == "dati-province") {
      dyn_title <- input$choose_province
      dyn_title <- substr(dyn_title, 1, nchar(dyn_title) - 14)
    }
    
    
    min <- which(grepl(input$range[1], y$data))
    max <- which(grepl(input$range[2], y$data))
    
    if (input$graph_time == "_giorn") {
      output$plot_title <- renderText({
        "Casi Positivi, Guariti e Deceduti per giorno"
      })
      output$plot1 <- renderHighchart({
        highchart() %>%
          hc_xAxis(categories = x$data[min:max],
                   title = list(text = "Serie Storica")) %>%
          hc_title(text = paste("Casi Positivi, Guariti e Deceduti", dyn_title, sep = " ")) %>%
          hc_subtitle(text = "Giornaliero",
                      style = list(color = "#94908F", useHTML = TRUE)) %>%
          hc_add_series(
            x$nuovi_positivi[min:max],
            type = type,
            name = "Var giornaliera del numero di Positivi",
            color = input$col1,
            showInLegend = legend
          ) %>%
          hc_add_series(
            x$dimessi_giorn[min:max],
            type = type,
            name = "Var giornaliera del numero di Dimessi Guariti",
            color = input$col2,
            showInLegend = legend
          ) %>%
          hc_add_series(
            x$deceduti_giorn[min:max],
            type = type,
            name = "Var giornaliera del numero di Decedutii",
            color = input$col3,
            showInLegend = legend
          ) %>%
          hc_plotOptions(series = list(marker = list(enabled = status))) %>%
          hc_yAxis(title = list(text = "Numero Casi")) %>%
          hc_add_theme(theme) %>%
          hc_exporting(
            enabled = TRUE,
            filename = "COVID_PLOT_DevStatistics",
            buttons = list(contextButton = list(
              menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
            ))
          ) %>%
          hc_credits(enabled = TRUE,
                     text = "DevStatistics",
                     href = "https://www.devstatistics.com/")
        
      })
    }
    
    if (input$graph_time == "_mm") {
      output$plot_title <-
        renderText({
          "Casi Positivi, Guariti e Deceduti - media mobile 7 giorni"
        })
      output$plot1 <- renderHighchart({
        highchart() %>%
          hc_xAxis(categories = x$data[min:max],
                   title = list(text = "Serie Storica")) %>%
          hc_title(text = paste("Casi Positivi, Guariti e Deceduti", dyn_title, sep = " ")) %>%
          hc_subtitle(text = "Media mobile (7giorni)",
                      style = list(color = "#94908F", useHTML = TRUE)) %>%
          hc_add_series(
            x$nuovi_positivi_mm[min:max],
            type = type,
            name = "Media mobile (7 giorni) Nuovi Positivi",
            color = input$col1,
            showInLegend = legend
          ) %>%
          hc_add_series(
            x$dimessi_giorn_mm[min:max],
            type = type,
            name = "Media mobile (7 giorni) Dimessi Guariti",
            color = input$col2,
            showInLegend = legend
          ) %>%
          hc_add_series(
            x$deceduti_giorn_mm[min:max],
            type = type,
            name = "Media mobile (7 giorni) Deceduti Giornalieri",
            color = input$col3,
            showInLegend = legend
          ) %>%
          hc_plotOptions(series = list(marker = list(enabled = status))) %>%
          hc_add_theme(theme) %>%
          hc_yAxis(title = list(text = "Numero Casi")) %>%
          hc_exporting(
            enabled = TRUE,
            filename = "COVID_PLOT_DevStatistics",
            buttons = list(contextButton = list(
              menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
            ))
          ) %>%
          hc_credits(enabled = TRUE,
                     text = "DevStatistics",
                     href = "https://www.devstatistics.com/")
        
      })
    }
    
    if (input$graph_time == "_cumulativo") {
      output$plot_title <- renderText({
        "Casi Positivi, Guariti e Deceduti - Cumulativo"
      })
      output$plot1 <- renderHighchart({
        highchart() %>%
          hc_xAxis(categories = x$data[min:max],
                   title = list(text = "Serie Storica")) %>%
          hc_title(text = paste("Casi Positivi, Guariti e Deceduti", dyn_title, sep = " ")) %>%
          hc_subtitle(text = "Sul totale",
                      style = list(color = "#94908F", useHTML = TRUE)) %>%
          hc_add_series(
            x$totale_positivi[min:max],
            type = type,
            name = "Totale Positivi",
            color = input$col1,
            showInLegend = legend
          ) %>%
          hc_add_series(
            x$dimessi_guariti[min:max],
            type = type,
            name = "Totale Dimessi",
            color = input$col2,
            showInLegend = legend
          ) %>%
          hc_add_series(
            x$deceduti[min:max],
            type = type,
            name = "Totale Guariti",
            color = input$col3,
            showInLegend = legend
          )  %>%
          hc_plotOptions(series = list(marker = list(enabled = status))) %>%
          hc_add_theme(theme) %>%
          hc_yAxis(title = list(text = "Numero Casi")) %>%
          hc_exporting(
            enabled = TRUE,
            filename = "COVID_PLOT_DevStatistics",
            buttons = list(contextButton = list(
              menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
            ))
          ) %>%
          hc_credits(enabled = TRUE,
                     text = "DevStatistics",
                     href = "https://www.devstatistics.com/")
        
      })
    }
    # }
    
    #GRAFICI OSPEDALIZZATI
    if (input$data_graph == "pazienti_ospedalizzati") {
      if (input$graph_time == "_giorn") {
        output$plot_title <-
          renderText({
            "Pazienti ospedalizzati per giorno"
          })
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste("Pazienti Ospedalizzati -", dyn_title, sep =
                                    " ")) %>%
            hc_subtitle(text = "Giornaliero",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$totale_ospedalizzati_giorn[min:max],
              type = type,
              name = "Var giornaliera del numero totale di ospedalizzati",
              color = input$col1,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$ricoverati_con_sintomi_giorn[min:max],
              type = type,
              name = "Var giornaliera del numero di ricoverati con sintomi",
              color = input$col2,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$terapia_intensiva_giorn[min:max],
              type = type,
              name = "Var giornaliera del numero di ricoverati in terapia intensiva",
              color = input$col3,
              showInLegend = legend
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi"),
                     plotLines = list(list(
                       value = 0,
                       color = 'black',
                       width = 2
                     ))) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
          
        })
      }
      
      if (input$graph_time == "_mm") {
        output$plot_title <-
          renderText({
            "Pazienti ospedalizzati per media mobile (7 giorni)"
          })
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste("Pazienti Ospedalizzati -", dyn_title, sep =
                                    " ")) %>%
            hc_subtitle(text = "Media mobile (7 giorni)",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$totale_ospedalizzati_mm[min:max],
              type = type,
              name = "Media mobile (7 giorni) del totale di ospedalizzati",
              color = input$col1,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$ricoverati_con_sintomi_mm[min:max],
              type = type,
              name = "Media mobile (7 giorni) dei ricoverati con sintomi",
              color = input$col2,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$terapia_intensiva_mm[min:max],
              type = type,
              name = "Media mobile (7 giorni) dei ricoverati in terapia intensiva",
              color = input$col3,
              showInLegend = legend
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi"),
                     plotLines = list(list(
                       value = 0,
                       color = 'black',
                       width = 2
                     ))) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
          
        })
      }
      
      if (input$graph_time == "_cumulativo") {
        output$plot_title <-
          renderText({
            "Pazienti ospedalizzati sul totale"
          })
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste("Pazienti Ospedalizzati -", dyn_title, sep =
                                    " ")) %>%
            hc_subtitle(text = "Sul totale",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$totale_ospedalizzati[min:max],
              type = type,
              name = "Totale di ospedalizzati",
              color = input$col1,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$ricoverati_con_sintomi[min:max],
              type = type,
              name = "Ricoverati con sintomi",
              color = input$col2,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$terapia_intensiva[min:max],
              type = type,
              name = "Ricoverati in terapia intensiva",
              color = input$col3,
              showInLegend = legend
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi")) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
        })
      }
    }
    
    #GRAFICI TAMPONI E NUOVI POSIVITI
    if (input$data_graph == "tamponi_e_positivi") {
      output$plot_title <-
        renderText({
          "Tamponi e numero di positivi per giorno"
        })
      if (input$graph_time == "_giorn") {
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste("Tamponi e Positivi -", dyn_title, sep =
                                    " ")) %>%
            hc_subtitle(text = "Giornaliero",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$tamponi_giorn[min:max],
              type = type,
              name = "Var giornaliera del numero di Tamponi effettuati",
              color = input$col1,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$nuovi_positivi[min:max],
              type = type,
              name = "Var giornaliera del numero di positivi",
              color = input$col2,
              showInLegend = legend
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi")) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
        })
      }
      
      if (input$graph_time == "_mm") {
        output$plot_title <-
          renderText({
            "Tamponi e numero di positivi per media mobile (7 giorni)"
          })
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste("Tamponi e Positivi -", dyn_title, sep =
                                    " ")) %>%
            hc_subtitle(text = "Media mobile (7 giorni)",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$tamponi_mm[min:max],
              type = type,
              name = "Media mobile (7 giorni) dei Tamponi effettuati",
              color = input$col1,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$nuovi_positivi_mm[min:max],
              type = type,
              name = "Media mobile (7 giorni) dei Nuovi Positivi",
              color = input$col2,
              showInLegend = legend
            )  %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi")) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
        })
      }
      
      if (input$graph_time == "_cumulativo") {
        output$plot_title <-
          renderText({
            "Tamponi e numero di positivi sul totale"
          })
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste("Tamponi e Positivi -", dyn_title, sep =
                                    " ")) %>%
            hc_subtitle(text = "Sul totale",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$tamponi[min:max],
              type = type,
              name = "Totale Tamponi effettuati",
              color = input$col1,
              showInLegend = legend
            ) %>%
            hc_add_series(
              x$totale_positivi[min:max],
              type = type,
              name = "Totale Tamponi effettuati",
              color = input$col2,
              showInLegend = legend
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi")) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
        })
      }
    }
    
    #GRAFICI CASI NETTI
    if (input$data_graph == "casi_netti") {
      output$plot_title <-
        renderText({
          "Nuovi posiviti al netto di guariti e deceduti per giorno"
        })
      if (input$graph_time == "_giorn") {
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste(
              "Casi Positivi al netto di Deceduti e Guariti -",
              dyn_title,
              sep = " "
            )) %>%
            hc_subtitle(text = "Giornaliero",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$casi_netti[min:max],
              type = type,
              name = "Casi positivi al netto di deceduti e guariti",
              color = input$col1,
              showInLegend = legend
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi"),
                     plotLines = list(list(
                       value = 0,
                       color = 'black',
                       width = 2
                     ))) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
        })
      }
      
      if (input$graph_time == "_mm") {
        output$plot_title <-
          renderText({
            "Nuovi posiviti al netto di guariti e deceduti per media mobile (7 giorni)"
          })
        output$plot1 <- renderHighchart({
          highchart() %>%
            hc_xAxis(categories = x$data[min:max],
                     title = list(text = "Serie Storica")) %>%
            hc_title(text = paste(
              "Casi Positivi al netto di Deceduti e Guariti -",
              dyn_title,
              sep = " "
            )) %>%
            hc_subtitle(text = "Media mobile (7 giorni)",
                        style = list(color = "#94908F", useHTML = TRUE)) %>%
            hc_add_series(
              x$casi_netti_mm[min:max],
              type = type,
              name = "Media mobile (7 giorni) dei casi positivi al netto di deceduti e guariti",
              color = input$col2,
              showInLegend = legend
            ) %>%
            hc_plotOptions(series = list(marker = list(enabled = status))) %>%
            hc_add_theme(theme) %>%
            hc_yAxis(title = list(text = "Numero Casi"),
                     plotLines = list(list(
                       value = 0,
                       color = 'black',
                       width = 2
                     ))) %>%
            hc_exporting(
              enabled = TRUE,
              filename = "COVID_PLOT_DevStatistics",
              buttons = list(contextButton = list(
                menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
              ))
            ) %>%
            hc_credits(enabled = TRUE,
                       text = "DevStatistics",
                       href = "https://www.devstatistics.com/")
        })
      }
      
    }
    
    #GRAFICI CASI TOTALI PROVINCIA
    if (input$data_graph == "tot_casi") {
      output$plot_title <- renderText({
        "Totale casi per provincia"
      })
      
      data <- unique(x$data)
      output$plot1 <- renderHighchart({
        hchart(x,
               type,
               hcaes (y = totale_casi, group = "denominazione_provincia")) %>%
          hc_xAxis(categories = data,
                   title = list(text = "Serie Storica")) %>%
          hc_title(text = paste("Totale casi per provincia-", dyn_title, sep =
                                  " ")) %>%
          hc_plotOptions(series = list(marker = list(enabled = status))) %>%
          hc_add_theme(theme) %>%
          hc_yAxis(title = list(text = "Numero Casi")) %>%
          hc_exporting(
            enabled = TRUE,
            filename = "COVID_PLOT_DevStatistics",
            buttons = list(contextButton = list(
              menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
            ))
          ) %>%
          hc_credits(enabled = TRUE,
                     text = "DevStatistics",
                     href = "https://www.devstatistics.com/")
      })
    }
    
    
  })
  
  #---------------------------------- MAPS SERVER ----------------------------------#
  
  
  #PRE PLOT MAP
  output$map <- renderHighchart({
    path_y <<- paste("/home/biagio/R/data_plot/mappa_prov.csv")
    dfita2 <<- read.csv(text = readLines(path_y), header = TRUE)
    ita <- get_ita()
    
    colfunc <- colorRampPalette(c(input$col_map, "white"))
    colors_map <- colfunc(10)
    color_end <- colors_map[2]
    color_start <- colors_map[8]
    
    highchart(type = "map") %>%
      hc_title(text = "Mappa dei casi totali suddivisa per provincia") %>%
      hc_add_series_map(
        map = ita,
        df = dfita2,
        joinBy = "hasc",
        value = "totale_casi",
        name = "Totale casi",
        borderColor = "black",
        borderWidth = 0.2
      ) %>%
      hc_colorAxis(minColor = color_start, maxColor = color_end) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
  })
  
  observeEvent(input$map_button, {
    if (input$map_data == "region_map") {
      if (input$map_type == "ricoverati_con_sintomi") {
        value <- input$map_type
        name <- "Ricoverati con sintomi"
      }
      if (input$map_type == "terapia_intensiva") {
        value <- input$map_type
        name <- "Terapia intensiva"
      }
      if (input$map_type == "totale_ospedalizzati") {
        value <- input$map_type
        name <- "Totale ospedalizzati"
      }
      if (input$map_type == "isolamento_domiciliare") {
        value <- input$map_type
        name <- "Isolamento domiciliare"
      }
      if (input$map_type == "totale_positivi") {
        value <- input$map_type
        name <- "Totale Positivi"
      }
      if (input$map_type == "variazione_totale_positivi") {
        value <- input$map_type
        name <- "Variazione totale positivi"
      }
      if (input$map_type == "nuovi_positivi") {
        value <- input$map_type
        name <- "Nuovi Positivi"
      }
      if (input$map_type == "dimessi_guariti") {
        value <- input$map_type
        name <- "Dimessi Guariti"
      }
      if (input$map_type == "deceduti") {
        value <- input$map_type
        name <- "Deceduti"
      }
      if (input$map_type == "totale_casi") {
        value <- input$map_type
        name <- "Totale Casi"
      }
      if (input$map_type == "tamponi") {
        value <- input$map_type
        name <- "Tamponi"
      }
      if (input$map_type == "casi_testati") {
        value <- input$map_type
        name <- "Casi testati"
      }
      
      output$map <- renderHighchart({
        
        map <- get_map()
        dati_mappa <- get_data()
        
        colfunc <- colorRampPalette(c(input$col_map, "white"))
        colors_map <- colfunc(10)
        color_end <- colors_map[2]
        color_start <- colors_map[8]
        
        highchart(type = "map") %>%
          hc_add_series_map(
            map = map,
            df = dati_mappa,
            joinBy = "id",
            value = value,
            name = name,
            borderColor = "black",
            borderWidth = 0.2
          ) %>%
          hc_title(text = paste("Mappa", name, "suddivisa per regione", sep = " ")) %>%
          hc_colorAxis(minColor = color_start, maxColor = color_end) %>%
          hc_exporting(
            enabled = TRUE,
            filename = "COVID_PLOT_DevStatistics",
            buttons = list(contextButton = list(
              menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
            ))
          ) %>%
          hc_mapNavigation(enabled = TRUE) %>%
          hc_credits(enabled = TRUE,
                     text = "DevStatistics",
                     href = "https://www.devstatistics.com/")
      })
      
    }
    
    if (input$map_data == "province_map" && input$map_set_type == "bubble_map") {
      output$map <- renderHighchart({
        
        col_map <- input$col_map
        
        path_y <<- paste("/home/biagio/R/COVID-19-master/dati-province/dpc-covid19-ita-province-latest.csv")
        txt <<- read.csv(text = readLines(path_y), header = TRUE)
        test <- txt[,c("denominazione_provincia","lat","long","totale_casi")]
        
        names(test)[names(test) == "denominazione_provincia"] <- "name"
        names(test)[names(test) == "long"] <- "lon"
        names(test)[names(test) == "totale_casi"] <- "z"
        
        test<-test[!(test$name=="In fase di definizione/aggiornamento"),]
      
        hcmap("countries/it/it-all.js", showInLegend = FALSE) %>% 
          hc_title(text = "Mappa dei casi totali suddivisa per provincia") %>%
          hc_add_series(data = test, type = "mapbubble", name="Totale casi", maxSize = '5%', color = col_map) %>% 
          hc_mapNavigation(enabled = TRUE)%>%
          hc_exporting(
            enabled = TRUE,
            filename = "COVID_PLOT_DevStatistics",
            buttons = list(contextButton = list(
              menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
            ))
          ) %>%
          hc_credits(enabled = TRUE,
                     text = "DevStatistics",
                     href = "https://www.devstatistics.com/")
        
      })
    }
    
    if (input$map_data == "province_map" && input$map_set_type == "gradient_map") {
      output$map <- renderHighchart({
        path_y <<- paste("/home/biagio/R/data_plot/mappa_prov.csv")
        dfita2 <<- read.csv(text = readLines(path_y), header = TRUE)
        ita <- get_ita()

        highchart(type = "map") %>%
          hc_title(text = "Mappa dei casi totali suddivisa per provincia") %>%
          hc_add_series_map(
            map = ita,
            df = dfita2,
            joinBy = "hasc",
            value = "totale_casi",
            name = "Totale casi",
            borderColor = "black",
            borderWidth = 0.2
          ) %>%
          hc_exporting(
            enabled = TRUE,
            filename = "COVID_PLOT_DevStatistics",
            buttons = list(contextButton = list(
              menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
            ))
          ) %>%
          hc_mapNavigation(enabled = TRUE) %>%
          hc_credits(enabled = TRUE,
                     text = "DevStatistics",
                     href = "https://www.devstatistics.com/")
        
        
      })
    }
    
  })
  
  
  #---------------------------------- SUMMARY SERVER ----------------------------------#
  
  #VALUE BOX
  output$box_guariti_2 <- renderValueBox({
    valueBox("Guariti",
             test$guariti,
             icon = icon("plus-square"),
             color = "green")
  })
  
  output$box_positivi_2 <- renderValueBox({
    valueBox("Positivi",
             test$positivi,
             icon = icon("procedures"),
             color = "yellow")
  })
  
  output$box_deceduti_2 <- renderValueBox({
    valueBox("Deceduti",
             test$deceduti,
             icon = icon("heartbeat"),
             color = "red")
  })
  
  output$box_casi_totali_2 <- renderValueBox({
    valueBox("Casi Totali",
             test$totale_casi,
             icon = icon("user-plus"))
  })
  
  #SUMMARY PLOT1
  output$s_plot1 <- renderHighchart({
    cols_pie <- c("#00cdab", "#a05dcf", "#294187")
    
    highchart() %>%
      hc_chart(type = "pie",
               options3d = list(
                 enabled = TRUE,
                 alpha = 45,
                 beta = 0
               )) %>%
      hc_plotOptions(pie = list(
        depth = 50,
        allowPointSelect = TRUE,
        cursor = 'pointer',
        dataLabels = list(enabled = TRUE,
                          format = '{point.name}')
      )) %>%
      hc_colors(cols_pie) %>%
      hc_title(text = "Percentuale di Positivi, Deceduti e Guariti sul Totale dei casi") %>%
      hc_add_series_labels_values(
        labels = bar_plot_intro$data_name,
        values = bar_plot_intro$perc,
        name = "%",
        showInLegend = TRUE
      ) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(title = list(text = "Numero Casi")) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
  })
  
  
  path_sum <<- paste("/home/biagio/R/data_plot/dati_nazionali.csv")
  sum <- read.csv(text = readLines(path_sum), header = TRUE)
  sum <- sum[-1]
  
  #SUMMARY PLOT2
  output$s_plot2 <- renderHighchart({
    
    color1 <- "#3c5a99"
      color2 <- "#7e03c3"
        color3 <- "#23a1f2"
      
    highchart() %>%
      hc_xAxis(categories = sum$data,
               title = list(tesumt = "Serie Storica")) %>%
      hc_title(text = "Casi Positivi, Guariti e Deceduti - giornaliero") %>%
      hc_subtitle(tesumt = "Giornaliero",
                  style = list(color = "#94908F", useHTML = TRUE)) %>%
      hc_add_series(
        sum$nuovi_positivi,
        type = 'area',
        name = "Var giornaliera del numero di Positivi",
        color = color1,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        sum$dimessi_giorn,
        type = 'area',
        name = "Var giornaliera del numero di Dimessi Guariti",
        color = color2,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        sum$deceduti_giorn,
        type = 'area',
        name = "Var giornaliera del numero di Decedutii",
        color = color3,
        showInLegend = TRUE
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
      hc_yAxis(title = list(tesumt = "Numero Casi")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contesumtButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
    
  })
  
  #SUMMARY PLOT3
  output$s_map <- renderHighchart({
    
    col_map <- input$col_map
    
    path_y <<- paste("/home/biagio/R/COVID-19-master/dati-province/dpc-covid19-ita-province-latest.csv")
    txt <<- read.csv(text = readLines(path_y), header = TRUE)
    test <- txt[,c("denominazione_provincia","lat","long","totale_casi")]
    
    names(test)[names(test) == "denominazione_provincia"] <- "name"
    names(test)[names(test) == "long"] <- "lon"
    names(test)[names(test) == "totale_casi"] <- "z"
    
    test<-test[!(test$name=="In fase di definizione/aggiornamento"),]
    
    hcmap("countries/it/it-all.js", showInLegend = FALSE) %>% 
      hc_title(text = "Mappa dei casi totali suddivisa per provincia") %>%
      hc_add_series(data = test, type = "mapbubble", name="Totale casi", maxSize = '8%', color = "#6E00CF") %>% 
      hc_mapNavigation(enabled = TRUE)%>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
  })
  
  #SUMMARY PLOT4
  output$s_plot4 <- renderHighchart({
    
    color1 <- "#00c0ef"
      color2 <- "#675ad0"
        color3 <- "#11ce3a"
    
    highchart() %>%
      hc_xAxis(categories = sum$data,
               title = list(text = "Serie Storica")) %>%
      hc_title(text = "Casi pazienti ospedalizzati") %>%
      hc_subtitle(text = "Giornaliero",
                  style = list(color = "#94908F", useHTML = TRUE)) %>%
      hc_add_series(
        sum$totale_ospedalizzati_giorn,
        type = "line",
        name = "Var giornaliera del numero totale di ospedalizzati",
        color = color1,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        sum$ricoverati_con_sintomi_giorn,
        type = "line",
        name = "Var giornaliera del numero di ricoverati con sintomi",
        color = color2,
        showInLegend = TRUE
      ) %>%
      hc_add_series(
        sum$terapia_intensiva_giorn,
        type = "line",
        name = "Var giornaliera del numero di ricoverati in terapia intensiva",
        color = color3,
        showInLegend = TRUE
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(title = list(text = "Numero Casi"),
               plotLines = list(list(
                 value = 0,
                 color = 'black',
                 width = 2
               ))) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
    
  })
  
  #SUMMARY PLOT4
  output$s_plot5 <- renderHighchart({
    
    color1 <- "#21769a"
    
    highchart() %>%
      hc_xAxis(categories = sum$data,
               title = list(text = "Serie Storica")) %>%
      hc_title(text = "Nuovi casi al netto di guariti e deceduti (media mobile 7 giorni)") %>%
      hc_subtitle(text = "Giornaliero",
                  style = list(color = "#94908F", useHTML = TRUE)) %>%
      hc_add_series(
        sum$casi_netti_mm,
        type = "column",
        name = "Casi positivi al netto di deceduti e guariti",
        color = color1,
        showInLegend = TRUE
      ) %>%
      hc_plotOptions(series = list(marker = list(enabled = FALSE)),
                     lineWidth = 0.5) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_yAxis(title = list(text = "Numero Casi"),
               plotLines = list(list(
                 value = 0,
                 color = 'black',
                 width = 2
               ))) %>%
      hc_exporting(
        enabled = TRUE,
        filename = "COVID_PLOT_DevStatistics",
        buttons = list(contextButton = list(
          menuItems = list('downloadPNG', 'downloadSVG', 'separator', 'label')
        ))
      ) %>%
      hc_credits(enabled = TRUE,
                 text = "DevStatistics",
                 href = "https://www.devstatistics.com/")
  })
  
  #SUMMARY 6
  
  output$data_vis2 <- DT::renderDataTable(
    sum,
    style = 'bootstrap',
    class = 'cell-border stripe',
    options = list(
      ordering = F,
      processing = TRUE,
      scrollX = TRUE,
      columnDefs = list(list(
        className = 'dt-center', targets = "_all"
      ))
    )
  )
  
  
  
}

shinyApp(ui, server)
# Run the application
shinyApp(ui = ui, server = server)