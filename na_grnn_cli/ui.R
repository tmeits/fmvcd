# https://www.showmeshiny.com/
# https://shiny.rstudio.com/gallery/file-upload.html
# https://shiny.rstudio.com/gallery/file-download.html
# https://fonts.google.com/
# 18.4.17
# https://daattali.com/shiny/shinyjs-basic/
# Introducing shinyjs: perform common JavaScript operations in Shiny apps using plain R code
# https://shiny.rstudio.com/articles/tag-glossary.html
# install.packages("shinythemes")
# install.packages('shinythemes', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages("shinyjs")
# 

library(shiny)
library(shinythemes)
library(shinyjs)
require(lubridate)

shinyUI(
  tagList(
    navbarPage(
      "VS-NNET",
      tabPanel("NA.GRNN.CLI",
      # theme = "cerulean",  # <--- To use a theme, uncomment this "shinythemes",
      fluidPage( theme = shinytheme("darkly"), #darkly
      useShinyjs(),
      shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  #tags$head(
  #  tags$style(HTML("
  #                  @import url('//fonts.googleapis.com/css?family=Anton:400,700');
  #                  ")),
  #  # http://stackoverflow.com/questions/32256366/change-the-background-color-of-the-downloadbutton
  #  tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")
  #  ),
  #headerPanel("Imputing missing data with neuralnet GRNN R"),
  #headerPanel(h1("Imputing missing data with neuralnet GRNN R", 
  #               style = "font-family: 'Anton';
  #      font-weight: 500; line-height: 1.1; 
  #      color: #4d3a7d;")),
    sidebarLayout(
      sidebarPanel(
        fileInput('filecli', 'Choose upload .CLI File', # object in which NAs are to be replaced
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv', '.cli', '.CLI')),
        tags$div("Here is a sample",
               a("data file,",
                 href = "https://gist.github.com/anonymous/282df1e0d1c1c6ce9c02e1de347b2128"),
               "describing weather observations"),
        checkboxInput('vscli', 'VS-Oscilloscope Pascal format .CLI', TRUE), 
        checkboxInput('vsforcli', 'VS-Fortran 5 Classic format .CLI', FALSE),
        checkboxInput('vschinacli', 'VS-Fortran 5 China format .CLI', FALSE),
        tags$hr(),
        column(12, checkboxInput('header', 'Header', FALSE)),
        column(6, 
             
             radioButtons('sep', 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'), ';')),
        column(6,
             radioButtons('quote', 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"')),
        tags$hr(),
        selectInput("dataset", "Choose a .CLI dataset:", 
                  choices = c("krest1977", "krest1967", "krest1969"), selected = "krest1967"),
      # R shiny; how to use multiple inputs from selectInput to pass onto 'select' option in dplyr?
      # http://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
      # selectInput("Columns","Columns", names(mtcars), multiple = TRUE),
        tags$hr(),
      # *********************************************************************
        checkboxInput('manualSigma', 'Manual modification of Sigma', TRUE),
      
        #actionButton("button", "Toggle numericInput"),
        #actionButton("button2", "Toggle numericInput2"),
      #numericInput('countTest', 'The size of the test sample', 69,
      #             min = 21, max = 191),
      #sliderInput("numTestGRNN", label = h3("Number?"), min = 31, 
      #            max = 171, value = 65),
        selectInput("replaceNA", "Replace NA by:", 
                  choices = c("GRNN", "NNET", "validANN", "NeuralNet","Spline"), 
                  selected = "GRNN"),
        tags$hr(),
      # https://gist.github.com/aagarw30/9c60b87e839db05b8dcc
        downloadButton('downloadData', 'Download results', class = "butt"),
        tags$hr(),
        tags$div("Created by Iljin Victor, 2017", style = "color:green")#,
      #tags$h1('LORA')
    ),
    ##########################################
    mainPanel(
      tabsetPanel( # http://rstudio.github.io/shiny/tutorial/#tabsets
        tabPanel("Overview", 
                 #plotOutput('contentsPlot'),
                 plotOutput('plotPrecTemp'),
                 hr(),
                 #plotOutput('contentsPlotPrec'),
                 verbatimTextOutput("summaryCli"), hr()),
        tabPanel("Table", dataTableOutput('tableCli')),
        tabPanel("Prec NAs replaced", 
                 plotOutput('precNA'), tags$hr(),
                 sliderInput("sigmaPrecGRNN", label = "Changing the value of Sigma - Prec", 
                             min = 0.01, 
                             max = 0.9, value = 0.16),
                 numericInput('countPrecTest', 'The size of the test sample - Prec', 69,
                              min = 21, max = 191),
                 verbatimTextOutput("precNAInfo")
                 ),
        tabPanel("Temp NAs replaced", 
                 plotOutput('tempNA'), tags$hr(), 
                 # http://stackoverflow.com/questions/35136029/hide-show-outputs-shiny-r
                 #conditionalPanel("output.sigmaShow", 
                 #                 sliderInput("sigmaGRNN", label = h3("Slider"), min = 0.1, 
                 #                             max = 0.9, value = 0.56)),
                 sliderInput("sigmaTempGRNN", label = "Changing the value of Sigma - Temp", 
                             min = 0.01, 
                                                          max = 0.9, value = 0.24),
                 numericInput('countTempTest', 'The size of the test sample - Temp', 69,
                              min = 21, max = 191),
                 verbatimTextOutput("tempNAInfo")
                 )
        #tabPanel("PlotUpload",
        #         plotOutput('contentsPlotUpload'),
        #         plotOutput('contentsPlotUpload2')),
        #tabPanel("SummaryUpload",verbatimTextOutput("summaryUpload")),
        #tabPanel("TableUpload", dataTableOutput('contentsUpload'))
      )
    )
  )
)),
tabPanel("NA.GRNN.CLI.PLOTLY"),
tabPanel("VS-Oscilloscope"),
tabPanel("DEMO ShinyJS", #http://stackoverflow.com/questions/33020558/embed-iframe-inside-shiny-app
         mainPanel(fluidRow(
           tags$h1("shinyJS Demo"),
           tags$p("You can test out some shinyjs functions below. Choose some R code from the dropdown list, and click Run."),
           tags$hr(),
           htmlOutput("frameShinyJS"),
           tags$hr()
         )
         ) # DEMO ShinyJS
))))