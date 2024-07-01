options(shiny.maxRequestSize = 30*1024^2)


library(shiny)
library(DT)
library(tm)  # Pakiet do pracy z tekstem, w tym stopwords
library(shinyjs)  # Do dynamicznego panelu
library(shinyBS)  # Do wysuwającego się panelu
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(text2vec)


# Definiowanie UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Prototyp aplikacji Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      style = "overflow-y: auto; max-height: 90vh; position:relative;",
      fileInput("file", "Wybierz plik CSV", accept = ".csv"),
      hr(),
      uiOutput("colnamesUI"),
      actionButton("renameCols", "Zmień nazwy kolumn"),
      hr(),
      DTOutput("varTypeTable"),
      selectInput("selectedVar", "Wybierz zmienną do zmiany typu", choices = NULL),
      selectInput("newVarType", "Wybierz nowy typ", choices = c("numeric", "character", "factor")),
      actionButton("changeVarType", "Zmień typ zmiennej"),
      hr(),
      selectInput("cleanTextVar", "Wybierz zmienną tekstową do wyczyszczenia", choices = NULL),
      actionButton("cleanText", "Wyczyść tekst"),
      hr(),
      selectInput("missingDataAction", "Wybierz akcję dla braków danych", choices = c("Usuń rekordy", "Testowa akcja")),
      actionButton("handleMissingData", "Zastosuj akcję")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("Ramka danych", DTOutput("dataTable")),
        tabPanel("Statystyki",
                 actionButton("refreshStats", "Odśwież"),
                 hr(),
                 uiOutput("variableCheckboxes"),
                 actionButton("generateStats", "Generuj statystyki"),
                 hr(),
                 DTOutput("statsTable"),
                 hr(),
                 
                 uiOutput("ngramAnalysisUI"),
                 DTOutput("ngramTable"),
                 hr(),
                 
                 
                 selectInput("wordFreqVar", "Wybierz zmienną tekstową do analizy częstości słów", choices = NULL),
                 selectInput("sentimentVar", "Wybierz zmienną sentymentu", choices = NULL),
                 uiOutput("sentimentValuesUI"),
                 actionButton("analyzeWords", "Analizuj słowa"),
                 DTOutput("wordFreqTable"),
                 hr()
        ),
        tabPanel("Wykresy", pageWithSidebar(
          headerPanel('Wybierz wykres do narysowania'),
          sidebarPanel(
            selectInput('plotType', 'Typ wykresu', choices = c("Histogram", "Testowa akcja")),
            conditionalPanel(
              condition = "input.plotType == 'Histogram'",
              selectInput('x', 'Wybierz zmienną na osi X (dla histogramu)', choices = NULL),
              selectInput('group', 'Wybierz zmienną grupującą (dla histogramu)', choices = c("None")),
              uiOutput("valueSelectUI")
            ),
            actionButton("drawPlot", "Narysuj wykres")
          ),
          mainPanel(
            plotOutput('plot')
          )
        ))
        
      )
    )
  )
)
# Definiowanie serwera
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  modifiedData <- reactiveValues(df = NULL)
  colnamesInput <- reactiveVal(NULL)  # Dodatkowy reactiveVal na nazwy kolumn
  
  observeEvent(input$file, {
    # Usunięcie poprzedniego pliku i aktualizacja interfejsu
    data(NULL)
    modifiedData$df <- NULL
    colnamesInput(NULL)
    updateSelectInput(session, "selectedVar", choices = NULL)
    updateSelectInput(session, "wordFreqVar", choices = NULL)
    updateSelectInput(session, "sentimentVar", choices = NULL)
    updateSelectInput(session, "cleanTextVar", choices = NULL)
    
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    data(df)
    modifiedData$df <- df
    colnamesInput(colnames(df))  # Zapisanie nazw kolumn do reactiveVal
    updateSelectInput(session, "selectedVar", choices = colnames(df))
    updateSelectInput(session, "wordFreqVar", choices = colnames(df))
    updateSelectInput(session, "sentimentVar", choices = colnames(df))
    updateSelectInput(session, "cleanTextVar", choices = colnames(df))
  })
  
  output$colnamesUI <- renderUI({
    req(colnamesInput())  # Użycie colnamesInput do renderowania UI
    fluidRow(
      lapply(seq_along(colnamesInput()), function(i) {
        textInput(paste0("colname_", i), label = paste("Nazwa kolumny", i), value = colnamesInput()[i])
      })
    )
  })
  
  observeEvent(input$renameCols, {
    req(data(), colnamesInput())
    new_colnames <- sapply(seq_along(colnamesInput()), function(i) {
      isolate(input[[paste0("colname_", i)]])
    })
    df <- data()
    colnames(df) <- new_colnames
    data(df)  # Aktualizacja reactiveVal po zmianie nazw kolumn
    modifiedData$df <- df
    colnamesInput(colnames(df))  # Aktualizacja colnamesInput po zmianie nazw kolumn
    updateSelectInput(session, "selectedVar", choices = colnames(df))
    updateSelectInput(session, "wordFreqVar", choices = colnames(df))
    updateSelectInput(session, "sentimentVar", choices = colnames(df))
    updateSelectInput(session, "cleanTextVar", choices = colnames(df))
    updateSelectInput(session, 'dataTable', choices = colnames(df))
    updateSelectInput(session, 'x',choices = colnames(df))
    updateSelectInput(session, 'group',choices = colnames(df))
    
    
    output$dataTable <- renderDT({
      datatable(df, options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
    })
    updateSummaries(df)
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
  
  # Funkcja do aktualizacji podsumowań i tabel
  # Funkcja do aktualizacji podsumowań i tabel
  updateSummaries <- function(df) {
    output$summ_table <- renderDT({
      summary_df <- data.frame(
        Wartość = c(ncol(df), nrow(df), sum(is.na(df))),
        row.names = c('Liczba kolumn', 'Liczba wierszy', 'Liczba braków danych')
      )
      datatable(summary_df, options = list(dom = 't', autoWidth = TRUE))
    })
    
    output$numSummaryTable <- renderDT({
      num_cols <- sapply(df, is.numeric)
      num_summary <- df[, num_cols, drop = FALSE]
      
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
      char_cols <- sapply(df, is.character)
      char_summary <- df[, char_cols, drop = FALSE]
      
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
  }
  
  
  observeEvent(input$refreshStats, {
    req(modifiedData$df)
    df <- modifiedData$df
    
    updateSummaries(df)  # Aktualizacja podsumowań i tabel
  })
  
  observeEvent(input$sentimentVar, {
    req(modifiedData$df)
    df <- modifiedData$df
    sentiment_var <- input$sentimentVar
    
    sentiment_values <- unique(df[[sentiment_var]])
    output$sentimentValuesUI <- renderUI({
      selectInput("selectedSentimentValue", "Wybierz wartość sentymentu", choices = c(sentiment_values))
    })
  })
  
  observeEvent(input$analyzeWords, {
    req(modifiedData$df, input$wordFreqVar, input$sentimentVar, input$selectedSentimentValue)
    
    df <- modifiedData$df
    word_var <- input$wordFreqVar
    sentiment_var <- input$sentimentVar
    selected_sentiment <- input$selectedSentimentValue
    
    # Sprawdzenie czy wybrana zmienna jest typu character
    if (!is.character(df[[word_var]])) {
      showModal(modalDialog(
        title = "Błąd",
        "Wybrana zmienna do analizy słów musi być typu character.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Filtrowanie danych według wybranej wartości sentymentu
    filtered_df <- df[df[[sentiment_var]] == selected_sentiment, ]
    
    # Tworzenie wektora słów z tekstu
    words <- unlist(strsplit(filtered_df[[word_var]], "\\W"))
    words <- tolower(words)
    words <- words[words != ""]
    
    # Usuwanie stopwords
    words <- words[!words %in% stopwords("en")]
    
    # Tworzenie tabeli częstości słów
    word_freq_df <- data.frame(table(words))
    colnames(word_freq_df) <- c("Word", "Frequency")
    word_freq_df <- word_freq_df[order(-word_freq_df$Frequency), ]
    
    output$wordFreqTable <- renderDT({
      datatable(word_freq_df, options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
  
  observeEvent(input$cleanText, {
    req(modifiedData$df, input$cleanTextVar)
    
    df <- modifiedData$df
    text_var <- input$cleanTextVar
    
    # Sprawdzenie, czy wybrana zmienna jest typu character
    if (!is.character(df[[text_var]])) {
      showModal(modalDialog(
        title = "Błąd",
        "Wybrana zmienna do wyczyszczenia tekstu musi być typu character.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # Inicjowanie postępu
    withProgress(message = 'Czyszczenie tekstu', value = 0, {
      total_steps <- nrow(df)
      dots <- 0
      
      for (i in 1:total_steps) {
        text <- df[[text_var]][i]
        text <- tolower(text)
        text <- gsub("[[:punct:]]", "", text)
        text <- gsub("[[:digit:]]", "", text)
        text <- removeWords(text, stopwords("en"))
        text <- stripWhitespace(text)
        
        # Zamiana pustych ciągów znaków na NA
        if (str_length(text) == 0) {
          text <- NA
        }
        
        df[[text_var]][i] <- text
        
        # Aktualizacja postępu co sekundę
        incProgress(1 / total_steps)
        if (i %% (total_steps / 10) == 0) {
          dots <- (dots %% 3) + 1
          message <- paste('Czyszczenie tekstu', strrep('.', dots))
          setProgress(value = i / total_steps, message = message)
          Sys.sleep(0.05)
        }
      }
      
      modifiedData$df <- df
    })
    
    # Aktualizacja DataTable i podsumowań
    output$dataTable <- renderDT({
      datatable(df, options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
    })
    
    updateSummaries(df)  # Aktualizacja podsumowań i tabel
    
    # Wyświetlenie modalnego okna potwierdzenia
    showModal(modalDialog(
      title = "Tekst wyczyszczony",
      "Tekst został pomyślnie wyczyszczony.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$handleMissingData, {
    req(modifiedData$df, input$missingDataAction)
    
    df <- modifiedData$df
    
    if (input$missingDataAction == "Usuń rekordy") {
      for(i in length(colnames(df))){
        df <- replace_empty_text(df,colnames(df[i]))
      }
      df <- na.omit(df)
      
      } else if (input$missingDataAction == "Testowa akcja") {
      # Tylko komunikat, że testowa akcja została wywołana
      showModal(modalDialog(
        title = "Testowa akcja",
        "To jest tylko testowa akcja, brak rzeczywistych zmian w danych.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    modifiedData$df <- df
    
    # Aktualizacja DataTable i podsumowań
    output$dataTable <- renderDT({
      datatable(df, options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
    })
    
    updateSummaries(df)  # Aktualizacja podsumowań i tabel
    
    showModal(modalDialog(
      title = "Akcja zakończona",
      "Wybrana akcja dla braków danych została pomyślnie zakończona.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  output$variableCheckboxes <- renderUI({
    req(data())
    df <- data()
    checkboxGroupInput("variableSelection", "Wybierz zmienne do generowania statystyk:",
                       choices = names(df), selected = NULL)
  })
  
  observeEvent(input$generateStats, {
    req(data(), input$variableSelection)
    df <- data()
    selected_vars <- input$variableSelection
    
    stats_list <- lapply(selected_vars, function(var) {
      if (is.numeric(df[[var]])) {
        stats <- c(mean = mean(df[[var]], na.rm = TRUE),
                   median = median(df[[var]], na.rm = TRUE),
                   variance = var(df[[var]], na.rm = TRUE),
                   sd = sd(df[[var]], na.rm = TRUE))
      } else if (is.character(df[[var]])) {
        stats <- c(liczba_rekordow = length(df[[var]]),
                   liczba_unikalnych = length(unique(df[[var]])),
                   )
      } else {
        stats <- NULL
      }
      return(stats)
    })
    
    # Prepare data frame for rendering
    stats_df <- data.frame(Zmienna = selected_vars,
                           Statystyka = unlist(stats_list),
                           stringsAsFactors = FALSE)
    
    output$statsTable <- renderDT({
      datatable(stats_df, options = list(pageLength = 10, autoWidth = TRUE))
    })
  })
  
  # N-GRAMS
  
  output$ngramAnalysisUI <- renderUI({
    req(modifiedData$df)
    
    tagList(
      selectInput("ngramVar", "Wybierz zmienną tekstową do analizy n-gramów:", choices = colnames(modifiedData$df)),
      numericInput("ngramSize", "Wybierz rozmiar n-gramu:", value = 2, min = 2, max = 3),
      actionButton("analyzeNgrams", "Analizuj n-gramy")
    )
  })
  
  observeEvent(input$analyzeNgrams, {
    req(modifiedData$df, input$ngramVar, input$ngramSize)
    
    
    df <- modifiedData$df
    ngram_var <- input$ngramVar
    ngram_size <- input$ngramSize
    
    # Sprawdzenie, czy wybrana zmienna jest typu character
    if (!is.character(df[[ngram_var]])) {
      showModal(modalDialog(
        title = "Błąd",
        "Wybrana zmienna do wyczyszczenia tekstu musi być typu character.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    it <- itoken(df[[ngram_var]], tokenizer = word_tokenizer, progressbar = FALSE)
    v <- create_vocabulary(it, ngram = c(ngram_size, ngram_size))
    
    ngram_freq_df <- data.frame(
      Ngram = v$vocab$terms,
      Frequency = v$vocab$terms_counts,
      stringsAsFactors = FALSE
    )
    
    
    output$ngramTable <- renderDT({
      datatable(ngram_freq_df, options = list(pageLength = 10, autoWidth = TRUE))
    })
    
      showModal(modalDialog(
      title = "Akcja zakończona",
      "Znaleziono n-gramy.",
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
  
  
  ######## WYKRESY #############
  
  observeEvent(input$file, {
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
    data(df)
    modifiedData$df <- df
    colnamesInput(colnames(df))  # Zapisanie nazw kolumn do reactiveVal
    updateSelectInput(session, "selectedVar", choices = colnames(df))
    updateSelectInput(session, "wordFreqVar", choices = colnames(df))
    updateSelectInput(session, "sentimentVar", choices = colnames(df))
    updateSelectInput(session, "cleanTextVar", choices = colnames(df))
    updateSelectInput(session, "x", choices = colnames(df))
    updateSelectInput(session, "group", choices = c("None", colnames(df)))
  })
  
  observeEvent(input$x, {
    req(modifiedData$df)
    df <- modifiedData$df
    x_var <- input$x
    unique_values <- unique(df[[x_var]])
    output$valueSelectUI <- renderUI({
      selectInput("xValue", "Wybierz wartość z wybranej zmiennej (opcjonalnie)", choices = c("None", unique_values))
    })
  })
  
  observeEvent(input$drawPlot, {
    req(modifiedData$df, input$plotType)
    df <- modifiedData$df
    
    output$plot <- renderPlot({
      if (input$plotType == "Histogram") {
        req(input$x)
        
        # Filtrowanie danych na podstawie wybranej wartości zmiennej x (jeśli wybrano)
        if (!is.null(input$xValue) && input$xValue != "None") {
          df <- df[df[[input$x]] == input$xValue, ]
        }
        
        if (input$group == "None") {
          ggplot(df, aes_string(x = input$x)) +
            geom_histogram(binwidth = 1) +
            labs(title = paste("Histogram zmiennej", input$x))
        } else {
          ggplot(df, aes_string(x = input$x, fill = input$group)) +
            geom_bar() +
            labs(title = paste("Histogram zmiennej", input$x, "z podziałem na", input$group))+
            facet_wrap(facets = input$group)
        }
      } else if (input$plotType == "Testowa akcja") {
        # Tutaj możesz dodać dowolną testową akcję
        plot(1, 1, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
      }
    })
  })
  
  
  
}
  
  


# Uruchomienie aplikacji
shinyApp(ui, server)
