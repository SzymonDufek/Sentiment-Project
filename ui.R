library(shiny)
library(bslib)
library(DT)
library(plotly)
library(shinyWidgets)
thematic::thematic_shiny()

#===============================================================================
# Page 1
#===============================================================================

# Tab 1

tab1 <- fillPage(
  
    tags$style(HTML("
      # dataTable_div{
      overflox-x: auto;
      
      }
    ")),
  
  
  # HTML5 Section with a DataTable output
  
    div(id = 'dataTable_div',DTOutput('dataTable'))
)

# Tab 2

tab2 <- fillPage(
  layout_sidebar(
    sidebar = sidebar(
      open = 'open',
      uiOutput("ngramAnalysisUI"),
      hr(),
      selectInput("wordFreqVar", "Wybierz zmienną tekstową do analizy częstości słów", choices = NULL),
      selectInput("sentimentVar", "Wybierz zmienną sentymentu", choices = NULL),
      uiOutput("sentimentValuesUI"),
      actionButton("analyzeWords", "Analizuj słowa",class = "btn-centered"),
      hr(),
      selectInput("sentenceLengthVar", "Select text variable for sentence length analysis", choices = NULL),
      checkboxInput("showDensity", "Show Density Plot", value = TRUE),
      actionButton("SLPbutton",'Długość słów',class = "btn-centered")
      
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
      selectInput('plotType', 'Typ wykresu', choices = c("Wordcloud")),
      conditionalPanel(
        condition = "input.plotType == 'Wordcloud'",
        selectInput('variable', 'Wybierz zmienną (dla wordcloud)', choices = NULL),
        selectInput("groupingVariable", "Select grouping variable for word cloud coloring", choices = NULL, selected = NULL),
        hr(),
        actionButton("drawWordcloud", "Narysuj wordcloud",class = "btn-centered",width = '100%')
      ),
      
    ),
    layout_columns(
      card(card_body(
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
  titlePanel("Modelowanie Sentymetu z Użyciem Drzewa Decyzyjnego"),
  
  layout_sidebar(
    sidebar = sidebar(
      open = T,
      width = 350,
      uiOutput("var_select"),
      uiOutput("exclude_vars"),
      sliderInput("split","", min = 0.1, max = 0.95, step = 0.01, value = 0.8),
      sliderInput("max_tokens","Maksymalna liczba tokenów", min = 100, max = 1000, step = 50, value = 600),
      selectInput("model_type", "Wybierz model:",
                  choices = c("Random Forest")),
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
  ),
  theme = bs_theme(
    preset = "minty"
  )
)

#===============================================================================
# Sidebar
#===============================================================================

sidebar_main <- sidebar(
  width = 300,
  # HTML5 Header
  tags$header(
    style = "display: flex; justify-content: center; align-items: center;",
    titlePanel(HTML("Upload file"))
  ),
  
  # HTML5 Section for file upload
  tags$section(
    fileInput("upload", "Wybierz plik CSV", accept = ".csv"),
    selectInput("sep", "Choose the separator:",
                choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t")),
    checkboxInput("header", "Header", TRUE)
  ),
  
  # HTML5 Section for summary and variable actions
  tags$section(
    style = "display: flex; justify-content: center; align-items: center;",
    titlePanel(HTML("Summary")),
  ),
  tags$section(
    hr(),
    DTOutput("varTypeTable"),
    selectInput("selectedVar", "Wybierz zmienną do zmiany typu", choices = NULL),
    selectInput("newVarType", "Wybierz nowy typ", choices = c("numeric", "character", "factor")),
    
    # Centering button using a div and CSS flexbox
    tags$div(
      style = "display: flex; justify-content: center;",
      actionButton("changeVarType", "Zmień typ zmiennej")
    ),
    
    hr(),
    selectInput("cleanTextVar", "Wybierz zmienną tekstową do wyczyszczenia", choices = NULL),
    
    # Centering button using a div and CSS flexbox
    tags$div(
      style = "display: flex; justify-content: center;",
      actionButton("cleanText", "Wyczyść tekst")
    )
  ),
  
  # HTML5 Section for handling missing data
  tags$section(
    hr(),
    selectInput("missingDataAction", "Wybierz akcję dla braków danych", choices = c("Usuń rekordy", "Testowa akcja")),
    
    # Centering button using a div and CSS flexbox
    tags$div(
      style = "display: flex; justify-content: center;",
      actionButton("handleMissingData", "Zastosuj akcję")
    )
  ),
  
  # HTML5 Footer
  tags$footer(
    "Autor: Szymon Dufek"
  )
)

#===============================================================================
# Combine
#===============================================================================

page_navbar(
  title = HTML("Sentiment Project"),
  theme = bs_theme(
    preset = "minty",
  ),
  underline = TRUE,
  sidebar = sidebar_main,
  padding = 0,
  nav_panel(title = HTML("<span style='font-size:100%; font-weight:bold;'>DATA EXPLORATION</span>"), exploration),
  nav_panel(title = HTML("<span style='font-size:100%; font-weight:bold;'>MODELLING & RESULTS</span>"), page2)
)