library(shiny)
library(bslib)
library(DT)
library(plotly)

thematic::thematic_shiny()

#===============================================================================
# Page 1
#===============================================================================

# Tab 1

tab1 <- fillPage(
  DTOutput('dataTable')  
  
)

# Tab 2

tab2 <- fillPage(
  layout_sidebar(
    sidebar = sidebar(
      actionButton("refreshStats", "Odśwież"),
      hr(),
      uiOutput("variableCheckboxes"),
      actionButton("generateStats", "Generuj statystyki"),
      hr(),
      DTOutput("statsTable"),
      hr(),
      uiOutput("ngramAnalysisUI"),
      hr(),
      selectInput("wordFreqVar", "Wybierz zmienną tekstową do analizy częstości słów", choices = NULL),
      selectInput("sentimentVar", "Wybierz zmienną sentymentu", choices = NULL),
      uiOutput("sentimentValuesUI"),
      actionButton("analyzeWords", "Analizuj słowa"),
      hr(),
      selectInput("sentenceLengthVar", "Select text variable for sentence length analysis", choices = NULL),
      checkboxInput("showDensity", "Show Density Plot", value = TRUE),
      actionButton("SLPbutton",'Długość słów')
      
        ),
    layout_columns(
      card(card_header('N-grams'),
           DTOutput("ngramsTable")
      ),
      card(card_header('Częstość słów'),
           DTOutput('wordFreqTable')
      ),
      card(card_header('Length of sentences'),
           plotOutput('sentenceLengthPlot')),
      col_widths =c(6,6,12) 
      
    )
  ),
  
)



# Tab 3

tab3 <- fillPage(
  layout_sidebar(
    sidebar = sidebar(
      selectInput('plotType', 'Typ wykresu', choices = c("Histogram", "Wordcloud", "Macierz korelacji")),
      conditionalPanel(
        condition = "input.plotType == 'Histogram'",
        selectInput('x', 'Wybierz zmienną na osi X (dla histogramu)', choices = NULL),
        selectInput('group', 'Wybierz zmienną grupującą (dla histogramu)', choices = c("None")),
        uiOutput("valueSelectUI"),
        hr(),
        actionButton("drawPlot", "Narysuj wykres")
      ),
      conditionalPanel(
        condition = "input.plotType == 'Wordcloud'",
        selectInput('variable', 'Wybierz zmienną (dla wordcloud)', choices = NULL),
        selectInput("groupingVariable", "Select grouping variable for word cloud coloring", choices = NULL, selected = NULL),
        actionButton("drawWordcloud", "Narysuj wordcloud")
      ),
      
    ),
    layout_columns(
      card(card_header(
        plotOutput("plot")
          ),full_screen = TRUE)
    )
  )
)

# Exploration Page

exploration <- page_fillable(
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Data Exploration</span>"),
    nav_panel("Ramka danych", tab1), # data browse and imputation
    nav_panel("Statystyki", tab2), # data description, variable statistics
    nav_panel("Wizualizacja", tab3) # distributions, boxplots, dependecies 
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

#===============================================================================
# Page 2
#===============================================================================


tab_m1 <- fluidPage(
  tags$style(HTML("
    .container-fluid {
      max-width: 100%;
    #model_building_title {
      margin-left: 20px;
      margin-top: 10px;
    }
  ")),
  layout_sidebar(
    sidebar = sidebar(
      titlePanel("Model target"),
      uiOutput("target_var_ui"),
      titlePanel("Model selection"),
      selectInput("model_select","Model",
                  choices = c("Decision Tree","XGBoost","Random Forest","SVM")),
      titlePanel("Dataset split"),
      sliderInput("split","Train size", min = 0.1, max = 0.95, step = 0.01, value = 0.8),
    ),
    div(id = "model_building_title", titlePanel(HTML("Model building summary"))),
    uiOutput("v_box"),
    uiOutput("model_select"),
    uiOutput("train_size"),
    uiOutput("type"),
    uiOutput("pre_steps"),
    actionButton("train_model", "Train Model"),
    htmlOutput("build_result")
  )
)

tab_m2 <- fluidPage(
  titlePanel("Modelowanie Sentymetu z Użyciem Drzewa Decyzyjnego"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("var_select"),
      uiOutput("exclude_vars"),
      sliderInput("split","", min = 0.1, max = 0.95, step = 0.01, value = 0.8),
      sliderInput("max_tokens","Maksymalna liczba tokenów", min = 100, max = 1000, step = 50, value = 600),
      selectInput("model_type", "Wybierz model:",
                  choices = c("Klasyczne drzewo decyzyjne", "Random Forest", "XGBoost")),
      actionButton("train", "Trenuj Model")
    ),
    
    mainPanel(
      verbatimTextOutput("model_output"),
      DTOutput('metricsDT'),
      plotOutput('confMatrix'),
      plotOutput("varImportance")
      
    )
  )
)

page2 <- page_fillable(
  navset_card_underline(
    title = HTML("<span style='font-size:100%; font-weight:bold;'>Model Building</span>"),
    nav_panel("Building", tab_m1),
    nav_panel("Results", tab_m2)
  ),
  theme = bs_theme(
    preset = "pulse"
  )
)

#===============================================================================
# Sidebar
#===============================================================================

sidebar_main <- sidebar(
  titlePanel(HTML("Upload file")),
  fileInput("upload", "Wybierz plik CSV", accept = ".csv"),
  #checkboxInput("header", "Header", TRUE),
  selectInput("sep", "Choose the separator:",
              choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
  checkboxInput("header", "Header", TRUE),
  titlePanel((HTML("Summary"))),
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
)

#===============================================================================
# Combine
#===============================================================================

page_navbar(
  title = HTML("Sentiment Project"),
  theme = bs_theme(
    preset = "pulse",
  ),
  underline = TRUE,
  sidebar = sidebar_main,
  padding = 0,
  nav_panel(title = HTML("<span style='font-size:100%; font-weight:bold;'>DATA EXPLORATION</span>"), exploration),
  nav_panel(title = HTML("<span style='font-size:100%; font-weight:bold;'>MODELLING & RESULTS</span>"), page2)
)