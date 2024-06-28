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
      actionButton("model", "Przetwarzanie przez model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Ramka danych", 
                 fluidRow(
                   column(6, DTOutput("dataTable")),
                   column(6, verbatimTextOutput("summary"))
                 )),
        tabPanel("Wykresy", plotOutput("plot"))
      )
    )
  )
)

# Definiowanie serwera
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file, {
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    data(df)
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
  })
  
  output$dataTable <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
  })
  
  output$summary <- renderPrint({
    req(data())
    df <- data()
    
    # Liczenie braków danych dla każdej kolumny
    na_count <- sapply(df, function(col) sum(is.na(col)))
    na_summary <- data.frame(Kolumna = names(na_count), Braki = na_count)
    
    # Tworzenie listy podsumowań dla każdej kolumny
    summaries <- lapply(seq_along(df), function(i) {
      col <- df[[i]]
      type <- if (is.numeric(col)) "Numeryczny" else if (is.character(col)) "Tekstowy" else "Inny"
      list(
        Nazwa = names(df)[i],
        Typ = type,
        Podsumowanie = summary(col),
        Braki = na_count[i]
      )
    })
    
    list(Braki = na_summary, Podsumowanie = summaries)
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
