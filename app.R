# Zwiększenie maksymalnego rozmiaru pliku do 30 MB
options(shiny.maxRequestSize = 30*1024^2)

# Ładowanie pakietów
library(shiny)
library(DT)
library(ggplot2)

# Definiowanie UI
ui <- fluidPage(
  titlePanel("Prototyp aplikacji Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Wybierz plik CSV", accept = ".csv"),
      hr(),
      actionButton("explore", "Eksploracja danych"),
      hr(),
      uiOutput("colnamesUI"),
      actionButton("renameCols", "Zmień nazwy kolumn"),
      hr(),
      DTOutput("varTypeTable"),
      selectInput("selectedVar", "Wybierz zmienną do zmiany typu", choices = NULL),
      selectInput("newVarType", "Wybierz nowy typ", choices = c("numeric", "character", "factor")),
      actionButton("changeVarType", "Zmień typ zmiennej"),
      actionButton("model", "Przetwarzanie przez model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Ramka danych", DTOutput("dataTable")),
        tabPanel("Statystyki",
                 fluidRow(
                   column(12, DTOutput("summ_table"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("Podsumowanie zmiennych numerycznych"), DTOutput("numSummaryTable"))
                 ),
                 hr(),
                 fluidRow(
                   column(12, h4("Podsumowanie zmiennych typu character"), DTOutput("charSummaryTable"))
                 )),
        tabPanel("Wykresy", plotOutput("plot"))
      )
    )
  )
)

# Definiowanie serwera
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  modifiedData <- reactiveValues(df = NULL)
  
  observeEvent(input$file, {
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    data(df)
    modifiedData$df <- df
    
    # Aktualizacja listy zmiennych do wyboru
    updateSelectInput(session, "selectedVar", choices = colnames(df))
  })
  
  output$colnamesUI <- renderUI({
    req(data())
    fluidRow(
      lapply(seq_along(colnames(data())), function(i) {
        textInput(paste0("colname_", i), label = paste("Nazwa kolumny", i), value = colnames(data())[i])
      })
    )
  })
  
  observeEvent(input$renameCols, {
    req(data())
    new_colnames <- sapply(seq_along(colnames(data())), function(i) {
      isolate(input[[paste0("colname_", i)]])
    })
    df <- data()
    colnames(df) <- new_colnames
    data(df)  # Aktualizacja reactiveVal po zmianie nazw kolumn
    modifiedData$df <- df
  })
  
  output$dataTable <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
  })
  
  output$varTypeTable <- renderDT({
    req(modifiedData$df)
    df <- modifiedData$df
    var_types <- sapply(df, class)
    
    var_type_df <- data.frame(
      Zmienna = names(var_types),
      Typ = unname(var_types),
      stringsAsFactors = FALSE
    )
    
    datatable(var_type_df, options = list(dom = 't', autoWidth = TRUE))
  })
  
  observeEvent(input$changeVarType, {
    req(modifiedData$df, input$selectedVar, input$newVarType)
    
    var_name <- input$selectedVar
    new_type <- input$newVarType
    
    df <- modifiedData$df
    
    # Konwersja zmiennej na wybrany typ
    if (new_type == "numeric") {
      df[[var_name]] <- as.numeric(df[[var_name]])
    } else if (new_type == "character") {
      df[[var_name]] <- as.character(df[[var_name]])
    } else if (new_type == "factor") {
      df[[var_name]] <- as.factor(df[[var_name]])
    }
    
    modifiedData$df <- df
    data(df)  # Aktualizacja reactiveVal po zmianie typu zmiennej
    
    # Aktualizacja tabeli typów zmiennych
    output$varTypeTable <- renderDT({
      var_types <- sapply(df, class)
      
      var_type_df <- data.frame(
        Zmienna = names(var_types),
        Typ = unname(var_types),
        stringsAsFactors = FALSE
      )
      
      datatable(var_type_df, options = list(dom = 't', autoWidth = TRUE))
    })
  })
  
  output$summ_table <- renderDT({
    req(modifiedData$df)
    
    summary_df <- data.frame(
      Wartość = c(ncol(modifiedData$df), nrow(modifiedData$df), sum(is.na(modifiedData$df))),
      row.names = c('Liczba zmiennych', 'Liczba obserwacji', 'Liczba braków danych')
    )
    
    datatable(summary_df, options = list(dom = 't', autoWidth = TRUE))
  })
  
  output$numSummaryTable <- renderDT({
    req(modifiedData$df)
    df <- modifiedData$df
    
    # Filtracja zmiennych numerycznych
    num_cols <- sapply(df, is.numeric)
    num_summary <- df[, num_cols, drop = FALSE]
    
    # Tworzenie tabeli podsumowań dla zmiennych numerycznych
    summaries <- lapply(seq_along(num_summary), function(i) {
      col <- num_summary[[i]]
      data.frame(
        Nazwa = names(num_summary)[i],
        Min = min(col, na.rm = TRUE),
        Max = max(col, na.rm = TRUE),
        Mean = mean(col, na.rm = TRUE),
        Median = median(col, na.rm = TRUE),
        SD = sd(col, na.rm = TRUE)
      )
    })
    
    datatable(do.call(rbind, summaries), options = list(dom = 't', autoWidth = TRUE))
  })
  
  output$charSummaryTable <- renderDT({
    req(modifiedData$df)
    df <- modifiedData$df
    
    # Filtracja zmiennych typu character
    char_cols <- sapply(df, is.character)
    char_summary <- df[, char_cols, drop = FALSE]
    
    # Tworzenie tabeli podsumowań dla zmiennych typu character
    summaries <- lapply(seq_along(char_summary), function(i) {
      col <- char_summary[[i]]
      data.frame(
        Nazwa = names(char_summary)[i],
        Unique = length(unique(col)),
        Mode = names(sort(table(col), decreasing = TRUE))[1],
        NAs = sum(is.na(col))
      )
    })
    
    datatable(do.call(rbind, summaries), options = list(dom = 't', autoWidth = TRUE))
  })
  
  output$plot <- renderPlot({
    req(data())
    ggplot(data(), aes(x = data()[,1], y = data()[,2])) + 
      geom_point() +
      labs(x = colnames(data())[1], y = colnames(data())[2])
  })
  
  observeEvent(input$model, {
    showModal(modalDialog(
      title = "Model",
      "Tutaj będzie przetwarzanie przez model.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)
