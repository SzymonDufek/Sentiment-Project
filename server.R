#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(tm)  # Pakiet do pracy z tekstem, w tym stopwords
library(shinyjs)  # Do dynamicznego panelu
library(shinyBS)  # Do wysuwającego się panelu
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggwordcloud)
library(tokenizers)
library(text2vec)
library(wordcloud)
library(textrecipes)

library(tidymodels)
library(tidytext)

library(rpart.plot)
library(rpart)
library(shinycssloaders)

library(vip)

options(shiny.maxRequestSize = 30 * 1024^2)

tidymodels_prefer()
conflicted::conflicts_prefer(shiny::observe)
# Define server logic required to draw a histogram
function(input, output, session) {

  colnamesInput <- reactiveVal(NULL)
  model_trained <- reactiveVal(FALSE)

  data <- reactive({
    req(input$upload)
    
    possible_seps <- list("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")
    
    selected_sep <- if (!is.null(input$sep) && input$sep %in% unlist(possible_seps)) input$sep else NULL
    
    if (!is.null(selected_sep)) {
      try({
        df <- read.csv(input$upload$datapath, header = input$header,encoding = 'latin1', sep = selected_sep, dec = if(selected_sep == ";") "," else ".")
        updateSelectInput(session, "variables_vis", choices = names(df))
        updateSelectInput(session, "sep", selected = possible_seps[[sep_name]])
        updateSelectInput(session, "sentenceLengthVar", choices = names(df))
        updateSelectInput(session, "sentimentVar", choices = names(df))
        return(df)
      }, silent = TRUE)
    }
    
    for (sep_name in names(possible_seps)) {
      sep <<- possible_seps[[sep_name]]
      try({
        df <- read.csv(input$upload$datapath, header = input$header,encoding = 'latin1', sep = sep, dec = if(sep == ";") "," else ".")
        updateSelectInput(session, "variables_vis", choices = names(df))
        updateSelectInput(session, "sep", selected = possible_seps[[sep_name]])
        updateSelectInput(session, "sentenceLengthVar", choices = names(df))
        updateSelectInput(session, "sentimentVar", choices = names(df))
        showNotification("Separator zmieniony automatycznie ze względu na błąd odczytu danych.", type = "message")
        
        return(df)
      }, silent = TRUE)
    }
    
    colnamesInput(colnames(modifiedData$df))
    showNotification("Error reading CSV file with all possible separators.", type = "error")
  })
  
  modifiedData <- reactiveValues(df = NULL)
  
  observe({
   # if (is.null(modifiedData$df)) {
      modifiedData$df <- data()
      #colnamesInput(colnames(modifiedData$df))
    #}
  })
  
  observe({
    updateSelectInput(session, "selectedVar", choices = colnames(modifiedData$df))
    updateSelectInput(session, "cleanTextVar", choices = colnames(modifiedData$df))
    #updateSelectInput(session, "missingDataAction", choices = colnames(modifiedData$df))
    updateSelectInput(session, "wordFreqVar", choices = colnames(modifiedData$df))
    updateSelectInput(session, "sentimentVar", choices = colnames(modifiedData$df))
    updateSelectInput(session, "sentenceLengthVar", choices = colnames(modifiedData$df))
    updateSelectInput(session, "variable", choices = colnames(modifiedData$df))
    
  })
  
  
  observe({
  output$dataTable <- renderDT(
    datatable(modifiedData$df, options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
  )
  })
  
  
  observe(
    output$varTypeTable <- renderDT({
      req(modifiedData$df)
      var_types <- sapply(modifiedData$df, class)
      
      var_type_df <- data.frame(
        Zmienna = names(var_types),
        Typ = unname(var_types),
        stringsAsFactors = FALSE
      )
    
    datatable(var_type_df, options = list(dom = 't', autoWidth = TRUE))
  })  
  )



  
  observeEvent(input$changeVarType, {
    req(modifiedData$df, input$selectedVar, input$newVarType)
    
    var_name <- input$selectedVar
    new_type <- input$newVarType
    
    
    # Konwersja zmiennej na wybrany typ
    if (new_type == "numeric") {
      modifiedData$df[[var_name]] <<- as.numeric(modifiedData$df[[var_name]])
    } else if (new_type == "character") {
      modifiedData$df[[var_name]] <<- as.character(modifiedData$df[[var_name]])
    } else if (new_type == "factor") {
      modifiedData$df[[var_name]] <<- as.factor(modifiedData$df[[var_name]])
    }
    
    # Aktualizacja reactiveVal po zmianie typu zmiennej
    updateSelectInput(session, "selectedVar", choices = colnames(modifiedData$df))
    
    output$varTypeTable <- renderDT({
      var_types <- sapply(modifiedData$df, class)
        
        var_type_df <- data.frame(
          Zmienna = names(var_types),
          Typ = unname(var_types),
          stringsAsFactors = FALSE
        )
      datatable(var_type_df, options = list(dom = 't', autoWidth = TRUE))
    })
    
  })
  
  
  observeEvent(input$cleanText, {
    req(input$cleanTextVar)
    showPageSpinner(caption = "Czyszczenie tekstu. Proszę czekać", color = '#f3969a')
    df <- modifiedData$df
    text_var <- input$cleanTextVar
    
    if (!is.character(df[[text_var]])) {
      showModal(modalDialog(
        title = "Błąd",
        "Wybrana zmienna do wyczyszczenia tekstu musi być typu character.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    
      total_steps <- nrow(df)
      dots <- 0
      
      for (i in 1:total_steps) {
        text <- df[[text_var]][i]
        text <- tolower(text)
        text <- gsub("[[:punct:]]", "", text)
        text <- gsub("[[:digit:]]", "", text)
        text <- gsub("[^\x01-\x7F]", "", text)
        text <- removeWords(text, stopwords("en"))
        text <- stripWhitespace(text)
        text <- stemDocument(text)
        
        if (str_length(text) == 0) {
          text <- NA
        }
        
        df[[text_var]][i] <- text
      }
      df <- na.omit(df)
      modifiedData$df <- df  
  
    hidePageSpinner()
    
    output$dataTable <- renderDT({
      datatable(df, colnames = colnamesInput(), options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
    })
    
    showModal(modalDialog(
      title = "Tekst wyczyszczony",
      "Tekst został pomyślnie wyczyszczony.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
    
   
  
  ####################
  
  replace_empty_text <- function(data, column) {
    data[[column]][nchar(str_squish(data[[column]])) == 0] <- NA
    return(data)
  }
  
  ####################
  
  
  
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
    
    modifiedData$df <<- df
    
    # Aktualizacja DataTable i podsumowań
    output$dataTable <- renderDT({
      datatable(df,colnames = colnamesInput(), options = list(pageLength = 10, autoWidth = TRUE, responsive = TRUE))
    })
    
    #updateSummaries(df)  # Aktualizacja podsumowań i tabel
    
    showModal(modalDialog(
      title = "Akcja zakończona",
      "Wybrana akcja dla braków danych została pomyślnie zakończona.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  ################# N-GRAMY
  
  output$ngramAnalysisUI <- renderUI({
    req(modifiedData$df)
    
    tagList(
      selectInput("ngramVar", tags$b("Wybierz zmienną tekstową do analizy n-gramów:"), choices = colnames(modifiedData$df)),
      numericInput("ngramSize", "Wybierz rozmiar n-gramu:", value = 2, min = 2, max = 3),
      actionButton("analyzeNgrams", "Analizuj n-gramy",class = "btn-centered",width = '100%')
    )
  })
  
  observeEvent(input$analyzeNgrams, {
    req(modifiedData$df, input$ngramVar, input$ngramSize)
    
    
    df <- na.omit(modifiedData$df)
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
    v <- create_vocabulary(it, ngram = c(ngram_size, ngram_size), sep_ngram = ' ')
    
    ngram_freq_df <- data.frame(
      Ngram = v$term,
      Frequency = v$term_count,
      stringsAsFactors = FALSE
    ) %>% 
      top_n(n = 10)
    
    
    output$ngramsTable <- renderDT({
      datatable(ngram_freq_df, options = list(pageLength = 10, autoWidth = TRUE))
    })
    
  })
  
  ########### CZESTOSC SLOW
  
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
  
  
  
  observeEvent(input$SLPbutton,{
    req(modifiedData$df, input$sentenceLengthVar)
    
    if (!is.character(modifiedData$df[[input$sentenceLengthVar]])) {
      showModal(modalDialog(
        title = "Błąd",
        "Wybrana zmienna do analizy słów musi być typu character.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    
    output$sentenceLengthPlot <- renderPlot({
      
      
      
      text_data <- modifiedData$df[[input$sentenceLengthVar]]
      
      # Tokenize into sentences
      sentences <- unlist(tokenize_sentences(text_data))
      
      # Calculate sentence lengths
      sentence_lengths <- sapply(sentences, function(sentence) {
        nchar(sentence)
      })
      
      # Plot histogram and optionally the density plot
      p <- ggplot(data.frame(length = sentence_lengths), aes(x = length)) +
        geom_histogram(aes(y = ..density..), bins = input$barsNum, fill = "blue", alpha = 0.5) +
        theme_minimal() +
        labs(title = "Histogram of Sentence Lengths", x = "Sentence Length", y = "Density")
      
      if (input$showDensity) {
        p <- p + geom_density(color = "red", size = 1)
      }
      
      print(p)
    })
    
  })
  
  
  #############    WORDCLOUD
  
 
  
  observe({
    updateSelectInput(session, "groupingVariable", choices = names(modifiedData$df))
  })
  
  observeEvent(input$drawWordcloud, {
    output$plot <- renderPlot({
      req(modifiedData$df, input$variable)
      
      text <- modifiedData$df[[input$variable]]
      selected_var <- input$variable
      
      # Create a corpus and clean the text
      corpus <- Corpus(VectorSource(text))
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords("en"))
      
      # Generate a term-document matrix
      tdm <- TermDocumentMatrix(corpus)
      m <- as.matrix(tdm)
      word_freqs <- sort(rowSums(m), decreasing = TRUE)
      df_word_freqs <- data.frame(word = names(word_freqs), freq = word_freqs)
      
      # Check if a grouping variable is selected
      if (!is.null(input$groupingVariable) && input$groupingVariable != "") {
        grouping_var <- modifiedData$df[[input$groupingVariable]]
        # Create a color vector based on the selected grouping variable
        group_factor <- as.factor(grouping_var)
        pal <- rainbow(length(levels(group_factor)))
        word_colors <- pal[as.numeric(group_factor)]
        names(word_colors) <- levels(group_factor)
        # Match word colors based on the group variable
        word_colors <- word_colors[match(df_word_freqs$word, text)]
      } else {
        word_colors <- "black"
      }
      
      # Generate the word cloud with optional grouping-based coloring
      try(wordcloud(words = df_word_freqs$word, freq = df_word_freqs$freq, max.words = 100, random.order = FALSE, colors = word_colors),silent = T)
    })
  })
  

  
  
  
  ################ MODELS
  
  
  output$var_select <- renderUI({
    req(modifiedData$df)
    vars <- names(modifiedData$df)
    tagList(
      selectInput("text_var", "Wybierz zmienną tekstową:", choices = vars),
      selectInput("target_var", "Wybierz zmienną objaśnianą (sentyment):", choices = vars)
    )
  })
  
  output$exclude_vars <- renderUI({
    req(modifiedData$df)
    vars <- modifiedData$df %>% 
      select(!c(input$target_var,input$text_var)) %>% 
      names()
    
    checkboxGroupInput("vars_to_exclude", "Wybierz zmienne do wykluczenia:", choices = vars)
  })
  
  # Trening modelu na podstawie wyboru użytkownika
  model <- eventReactive(input$train, {
    req(modifiedData$df, input$text_var, input$target_var, input$model_type)
    
    target <- input$target_var
    text <- input$text_var
    
    
    showPageSpinner(caption = 'Budowanie modelu. Proszę czekać.',color = "#f3969a")
    
    # Wybór zmiennych
    df <- modifiedData$df %>%
      select(-all_of(input$vars_to_exclude))  # Usuwanie zbędnych zmiennych
    
    # Podział na treningowe i testowe
    split <- initial_split(df, strata = target)
    train_data <- training(split)
    test_data <- testing(split)
    
    # Recipe
    rec <- recipe(as.formula(paste(target, "~ .")), data = train_data) %>% 
      step_tokenize(text) %>% 
    # step_stopwords(all_predictors()) %>%
      step_tokenfilter(text, max_tokens = input$max_tokens) %>%
      step_tfidf(text) %>%
      step_normalize(all_predictors())
    
    # Model na podstawie wyboru użytkownika
     if (input$model_type == "Random Forest") {
      model_spec <- rand_forest(trees = tune()) %>%
        set_mode("classification") %>%
        set_engine("ranger",importance = "impurity")
      
    } 
    
    # Workflow
    wf <- workflow() %>%
      add_recipe(rec) %>%
      add_model(model_spec)
    
    folds <- bootstraps(train_data,strata = !!sym(input$target_var),times = 10)
    
    grid <- grid_regular(trees(), levels =5)
    
    tuned <- tune_grid(
      wf,
      resamples = folds,
      grid = grid,
      metrics = metric_set(roc_auc, ppv, npv)
    )
    
    best_auc <- tuned %>% 
      select_best(metric = 'roc_auc')
    
    final <- finalize_workflow(wf,best_auc)
    
    sentiment_final <- last_fit(final,split)
    
    
    
    hidePageSpinner()
    showPageSpinner(caption = "Tworzenie wyników. Proszę czekać.",color = "#f3969a")
    
    
    output$metricsDT <- renderDT(
      sentiment_final %>% 
        collect_metrics() %>%
        
       # round(.estimate,digits = 2) %>% 
        datatable()
    )

    output$confMatrix <- renderPlot({
      cm <- sentiment_final %>%
        collect_predictions() %>%
        conf_mat(target, .pred_class)
      
      autoplot(cm,type = 'heatmap')
        
        })
    
    output$varImportance <- renderPlot({
      final %>% 
        fit(train_data) %>%
        extract_fit_parsnip() %>%
        vip::vi(lambda = best_auc$trees) %>%
        top_n(10, wt = abs(Importance)) %>%
        ungroup() %>%
        mutate(
          Importance = abs(Importance),
          Variable = str_remove(Variable, paste0("tfidf_",sym(text),"_")),
          Variable = forcats::fct_reorder(Variable, Importance)
        ) %>%
        ggplot(aes(x = Importance, y = Variable, fill = input$target_var)) +
        geom_col(show.legend = FALSE) +
        labs(y = NULL)
        
    })
      hidePageSpinner()
    model_trained(TRUE)
    output$model_output_ui <- renderUI({
      layout_columns(
        card(card_header('Metryki'),
             card_body(DTOutput('metricsDT'))
             ),
        card(card_header('Macierz trafności'),
             card_body(plotOutput('confMatrix'))
        ),
        card(card_header('Ważność słów do predykcji sentymentu'),
             card_body(plotOutput('varImportance'))
        ),col_widths = c(12,6,6)
          
      )
        
      })
    
    
    
  })
  
  # Wyświetlanie wyników modelu
  output$model_output <- renderPrint({
    req(model())
    model()
  })
  
  
}








