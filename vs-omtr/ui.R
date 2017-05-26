#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(
  tagList(
  navbarPage("VS-OMTR",
  tabPanel("Imputation",           
  fluidPage(theme = shinytheme("cosmo"), #"flatly" #yeti
  
  # Application title
  titlePanel("Upload Multiple Files ->  Imputation NA's-> Downloads ZIP archive"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      fileInput("file1", "Choose .CLI File", multiple = TRUE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv", ".cli", ".CLI")
                ),
      tags$hr(),
      # list formats
      radioButtons("cliFormat", "Format upload .cli files:",
                   c("VS-Pascal" = "vso",
                     "VS-Fortran" = "vsf",
                     "meteo.ru/Aisori" = "aisori")
                   ),
      
      wellPanel(tags$b(""),
      checkboxInput("debug", "View Debug Info", TRUE),
      checkboxInput("statsna", "Stats NA Distribution", FALSE)),
      
      selectInput("replaceNAs", "Replace NAs by:", 
                  choices = c("Select Replacement method", "GRNN-ManualSigma", "GRNN-CrossValidation", "PSO-GRNN","Kalman Smoothing", "mice", "Spline")),
      
      conditionalPanel(
        condition = "input.replaceNAs == 'GRNN-ManualSigma'",
        wellPanel(tags$b(""),
                  sliderInput("sigmaPrecGRNN", label = "Changing Sigma - Prec",
                              min = 0.001, 
                              max = 0.999, value = 0.26))
                  ),
      
      selectInput("cliFormatWrite", "Format download .CLI ZIP archive:", 
                  choices = c("VS-Pascal", "VS-Fortran", "VS-Shiny"), 
                  selected = "VS-Pascal"),
      
      conditionalPanel(
        condition = "input.cliFormatWrite == 'VS-Shiny' && input.cliFormat != 'aisori'",
           numericInput('WMOStation', 'WMO Station', 9999, 
                        min = 1, max = 9999)),
      
      tags$hr(),
      downloadButton('downloadData', 'Save Results to .ZIP archive', class = "butt"),
      tags$hr(),
      tags$div(tags$a(href="mailto:ilynva@gmail.com","Created by Iljin Victor, 2017"))#, style = "color:green")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Stats",
                 conditionalPanel(
                   condition = "input.debug == true",
                   verbatimTextOutput("strFileInput"),
                   verbatimTextOutput("summaryMergeFileInput")),
                 conditionalPanel(
                   condition = "input.statsna == true",
                   verbatimTextOutput("printStatsNA")
                 )),
        tabPanel("Table",
                 dataTableOutput("contents")),
        tabPanel("Plot NA's", 
                 tabsetPanel(
                   tabPanel("Distribution of NAs",
                            plotOutput("plotNADPrec"),
                            plotOutput("plotNADTemp"),
                            column(12, 
                                   dateRangeInput("dates_plotNAD", label = "Date Range")),
                            column(12,
                                   sliderInput("num_plotNAD", label = "Number Range", min = 1, 
                                               max = 24745, value = c(40, 20000), width = '100%'))
                            ),
                   tabPanel("Gapsize of NAs",
                            plotOutput("plotNAGPrec"), 
                            plotOutput("plotNAGTemp")),
                   tabPanel("Distribution of NAs Bar",
                            plotOutput("plotNABPrec"),
                            plotOutput("plotNABTemp")),
                   tabPanel("4"
                            ),
                   tabPanel("5")
                 )       
        ),
        tabPanel("ImputeNAs Temp" # plotNA.imputations
                 
        ),
        tabPanel("ImputeNAs Prec"
                 
        )
      )
    )
  )
)),
tabPanel("About")
) # NavBar
))
