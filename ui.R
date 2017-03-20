# https://www.showmeshiny.com/
# https://shiny.rstudio.com/gallery/file-upload.html
# https://shiny.rstudio.com/gallery/file-download.html
# https://fonts.google.com/

library(shiny)
#data(mtcars)

shinyUI(
fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Anton:400,700');
                    "))
    ),
  #headerPanel("Imputing missing data with neuralnet GRNN R"),
  headerPanel(h1("Imputing missing data with neuralnet GRNN R", 
                 style = "font-family: 'Anton';
        font-weight: 500; line-height: 1.1; 
        color: #4d3a7d;")),
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
      checkboxInput('vscli', 'VS-oscilloscope format .CLI', TRUE),
      tags$hr(),
      checkboxInput('header', 'Header', FALSE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ';'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      selectInput("dataset", "Choose a .CLI dataset:", 
                  choices = c("krest2014", "krest1967", "krest1969"), selected = "krest1967"),
      # R shiny; how to use multiple inputs from selectInput to pass onto 'select' option in dplyr?
      # http://stackoverflow.com/questions/39798042/r-shiny-how-to-use-multiple-inputs-from-selectinput-to-pass-onto-select-optio
      # selectInput("Columns","Columns", names(mtcars), multiple = TRUE),
      tags$hr(),
      selectInput("replaceNA", "Replace NA by:", 
                  choices = c("GRNN", "validANN", "Spline"), selected = "GRNN"),
      tags$hr(),
      downloadButton('downloadData', 'Download result'),
      tags$hr(),
      tags$div("Created by Iljin Victor, 2017", style = "color:green")
    ),
    mainPanel(
      tabsetPanel( # http://rstudio.github.io/shiny/tutorial/#tabsets
        tabPanel("Overview", 
                 #plotOutput('contentsPlot'),
                 plotOutput('plotPrecTemp'),
                 #plotOutput('contentsPlotPrec'),
                 verbatimTextOutput("summaryCli")),
        tabPanel("Table", dataTableOutput('tableCli')),
        tabPanel("Prec NAs replaced", verbatimTextOutput("precNA")),
        tabPanel("Temp NAs replaced", verbatimTextOutput("tempNA"))
        #tabPanel("PlotUpload",
        #         plotOutput('contentsPlotUpload'),
        #         plotOutput('contentsPlotUpload2')),
        #tabPanel("SummaryUpload",verbatimTextOutput("summaryUpload")),
        #tabPanel("TableUpload", dataTableOutput('contentsUpload'))
      )
    )
  )
))