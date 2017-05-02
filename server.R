# http://deanattali.com/blog/building-shiny-apps-tutorial/#4-load-the-dataset
library(shiny)
library(zoo)
library(grnn)
library(ggplot2)
library(reshape2)
library(imputeTS)
require(grt)
require(foreach)
library(shinyjs)
require(lubridate)
library(plotly)

# load IVA_scripts
source("zoo_spline_cli3.R")
source("imputets_kalman.R")
#source("na_grnn_cli01.R")
source("na_grnn_cli3.R")
source("na_grnn.R")
source("global-write.R")
source("global-plot.R")

read.year <- function(fileName) {
  cli_dataset <-read.table(fileName, header=FALSE, sep="")
  return(cli_dataset[1, 3])
}

createDF <- function(fileName){
  cli_dataset <-read.table(fileName, header=FALSE, sep="")
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  cli_dataset[cli_dataset$prec == -9999, 4] <- NA ; prec_vector <- cli_dataset[, 4] 
  cli_dataset[cli_dataset$temp == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5]
  days <- c(1:dim(cli_dataset)[1])
  return(data.frame(days=days, prec=prec_vector, temp=temp_vector))
}
## read global data.frame from disk
file_name <- '1977.cli'; krest1977 <- createDF(file_name)
file_name <- '1967.cli'; krest1967 <- createDF(file_name)
file_name <- '1969.cli'; krest1969 <- createDF(file_name)

YEAR <- read.year(file_name)
##
shinyServer(
  function(input, output, session) {
  # https://github.com/daattali/shinyjs/blob/master/inst/examples/basic/app.R
  # http://deanattali.com/shinyjs/
  
  onclick("toggleAdvanced", toggle(id = "advanced", anim = TRUE))  
    
  observe({ # input$vscli,
    #http://stackoverflow.com/questions/34733147/unable-to-disable-a-shiny-app-radio-button-using-shinyjs
    if(input$vscli == TRUE) {
      shinyjs::disable(selector = "[type=radio][value='VS-R Shiny format .cli']")
      shinyjs::disable(selector = "[type=radio][value='VS-Fortran 5 Classic format .CLI']")
      shinyjs::disable(selector = "[type=radio][value='VS-Fortran 5 China format .CLI']")
      shinyjs::disable(selector = "[type=radio][value='aisori.meteo.ru/ClimateR']")
    }
    else {
      shinyjs::enable(selector = "[type=radio][value='VS-R Shiny format .cli']")
      shinyjs::enable(selector = "[type=radio][value='VS-Fortran 5 Classic format .CLI']")
      shinyjs::enable(selector = "[type=radio][value='VS-Fortran 5 China format .CLI']")
      shinyjs::enable(selector = "[type=radio][value='aisori.meteo.ru/ClimateR']")
    }
  })  
    
  observeEvent(input$button, {
    toggle("countTest")
  })
 
  observeEvent(input$manualSigma, {
    
    
    if(input$manualSigma == TRUE){
      shinyjs::hide("countPrecTest")
      shinyjs::hide("countTempTest")
      shinyjs::show("sigmaPrecGRNN")
      shinyjs::show("sigmaTempGRNN")
    }else{
      shinyjs::hide("sigmaPrecGRNN")
      shinyjs::hide("sigmaTempGRNN")
      shinyjs::show("countPrecTest")
      shinyjs::show("countTempTest")
    }
  })
  #
  values <- reactiveValues()
  values$sigmaShow <- FALSE
  # 
  methodNA <- reactive({
    
  })
  #
  datasetInput <- reactive({
    switch(input$dataset,
           "krest1977" = krest1977,
           "krest1967" = krest1967,
           "krest1969" = krest1969)
  })
  #
  currentFileInput <- reactive({
    inFile <- input$filecli
    
    if (is.null(inFile)){
      datasetInput()
      #return(NULL)
    }else{
      if(input$vscli){
        createDF(inFile$datapath)
      }else{
        read.csv(inFile$datapath, header=input$header, sep=input$sep,
                 quote=input$quote)
      }
    }
  })
  #
  getFileYear <- reactive ({
    inFile <- input$filecli
    if (is.null(inFile)){
      cat("Preloads year", YEAR, "\n")
    }else{
      cat("Uploads year\n"); cat(str(inFile), "\n")
    }
  })
  #
  getFileCli <- reactive({
    inFile <- input$filecli
    if (is.null(inFile)){
      cat("Preloads .CLI", input$dataset, "\n")
    }else{
      cat("Uploads .CLI\n"); cat(str(inFile), "\n")
    }
  })
  #
  getNameFileCli <- reactive({
    inFile <- input$filecli
    if (is.null(inFile)){
      input$dataset
    }else{
      inFile$name
    }
  })
  #
  output$summaryCli <- renderPrint({
    cat(getFileCli(), getFileYear(), "***\n")
      str(currentFileInput())
      cat("***\n")
      summary(currentFileInput())
  })
  ################################
  output$precNA <-renderPlot({
    if(input$replaceNA == "GRNN"){
      if(input$manualSigma == TRUE){
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countPrecTest, input$sigmaPrecGRNN, FALSE, FALSE)
      }else{
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countPrecTest, 0.01, TRUE, FALSE)
      }
      #par(mfrow=c(2,1))
      plotPrecFilled(currentFileInput(), na.na.grnn, 
                     paste0(getNameFileCli(), " - GRNN"))
      #
      #na.na <- na.spline.cli(currentFileInput())
      #plotTempFilled(currentFileInput(), na.na, getNameFileCli())
    }else if(input$replaceNA == "Spline"){
      na.na <- na.spline.cli(currentFileInput())
      plotPrecFilled(currentFileInput(), na.na, paste0(getNameFileCli(), "Spline"))
    }
  })
  
  output$precNAInfo <- renderPrint({
    cat("***\n")
    cat("Method inputation= ", input$replaceNA, "\n")
    cat("***\n")
    summary(na.spline.cli(currentFileInput()))
    
    })
  #
  output$tempNA <-renderPlot({
    if(input$replaceNA == "GRNN"){
      if(input$manualSigma == TRUE){
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countTempTest, input$sigmaTempGRNN, FALSE, FALSE)
      }else{
        na.na.grnn <- na.grnn.cli(currentFileInput()
                                  , input$countTempTest, 0.01, TRUE, FALSE)
      }
      
      #par(mfrow=c(2,1))
      plotTempFilled(currentFileInput(), na.na.grnn, 
                     paste0(getNameFileCli(), " - GRNN"))
      #
      #na.na <- na.spline.cli(currentFileInput())
      #plotTempFilled(currentFileInput(), na.na, getNameFileCli())
    }else if(input$replaceNA == "Spline"){
      na.na <- na.spline.cli(currentFileInput())
      plotTempFilled(currentFileInput(), na.na, paste0(getNameFileCli(), " - Spline"))
    }
  })
  
  output$tempNAInfo <- renderPrint({
    cat("***\n")
    cat("Method inputation= ", input$replaceNA, "\n")
    cat("***\n")
    summary(na.spline.cli(currentFileInput()))
  })
  #################################
  
  output$contentsPlot <- renderPlot({
    # ggvis: Similar to ggplot2, but the plots are focused on being web-based and are more interactive
    # todo plot 2 
    # Plotting two graphs, one below the other, in shiny panel
    # http://stackoverflow.com/questions/33204243/plotting-two-graphs-one-below-the-other-in-shiny-panel
    #plot(datasetInput(), col="red")
    plot(datasetInput(), col="blue")
  })
  
  
  output$plotPrecTemp <- renderPlot({
    #http://stackoverflow.com/questions/27350243/ggplot-line-graph-with-different-line-styles-and-markers
    #How can I plot with 2 different y-axes?
    #http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
    
    ## set up some fake test data
    time <- seq(0,72,12)
    betagal.abs <- c(0.05,0.18,0.25,0.31,0.32,0.34,0.35)
    cell.density <- c(0,1000,2000,3000,4000,5000,6000)
    
    time <- currentFileInput()[,1]
    betagal.abs <- currentFileInput()[,2]; precVect <- currentFileInput()[,2]
    cell.density  <- currentFileInput()[,3]; tempVect <- currentFileInput()[,3]
      
    ## add extra space to right margin of plot within frame
    par(mar=c(5, 4, 4, 6) + 0.1)
    
    ## calculation of minimum maximum value of a vector
    ylimMinMAx <- c(min(tempVect, na.rm=TRUE), max(tempVect, na.rm=TRUE))
    namePlot <- getNameFileCli()
    ## Plot first set of data and draw its axis
    plot(time, tempVect, pch=16, axes=FALSE, ylim=ylimMinMAx, xlab="", ylab="", 
         type="b",col="darkred", main=paste0("Climatic data ", namePlot)) # !!!!!
    axis(2, ylim=ylimMinMAx, col="darkred", col.axis="darkred", las=1)  ## las=1 makes horizontal labels
    mtext(" Temp(c)*10",side=2,line=2.5, col="darkred")
    box()
    
    ## Allow a second plot on the same graph
    par(new=TRUE)
    
    ## calculation of minimum maximum value of a vector
    ylimMinMAx <- c(min(precVect, na.rm=TRUE), max(precVect, na.rm=TRUE))
   
    ## Plot the second plot and put axis scale on right
    plot(time, precVect, pch=15,  xlab="", ylab="", ylim=ylimMinMAx, 
         axes=FALSE, type="b", col="darkblue")
    ## a little farther out (line=4) to make room for labels
    mtext("Prec(mm)*10",side=4,col="darkblue",line=4) 
    axis(4, ylim=ylimMinMAx, col="darkblue",col.axis="darkblue",las=1)
    
    ## Draw the time axis
    axis(1,pretty(range(time),10))
    mtext("Time (days)",side=1,col="black",line=2.5)  
    
    ## Add Legend
    legend("topleft",legend=c("Prec","Temp"),
           text.col=c("darkblue","darkred"),pch=c(16,15),col=c("darkblue","darkred"))
    #qplot(temp, data=datasetInput(), geom="density", alpha=I(.5), 
    #      main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
    #      ylab="Density")
    #plot(datasetInput()[,1],datasetInput()[,3], col="green")
  })
  
  output$contentsPlotPrec <- renderPlot({
    plot(datasetInput()[,1],datasetInput()[,2], col="red")
  })
  # 
  output$tableCli <- renderDataTable({
    currentFileInput()
    #datasetInput()
  }, options = list(lengthMenu = c(16, 30, 50), pageLength = 16))
  
  output$downloadData2 <- downloadHandler(
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  ) 
  
  ##############################
  ### DownloadsData
  ##############################

  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(getNameFileCli(), '.filled', sep='') 
    },
    content = function(file) {
      # filled na.spline.cli(currentFileInput())
      # na.grnn.cli(currentFileInput()
      #, input$countTest, 0.01, TRUE, FALSE)
      write.csv(na.spline.cli(currentFileInput()), file)
      #write.csv(na.grnn.cli(currentFileInput(), input$countTest, 0.01, FALSE, FALSE), file)
    }
  ) # currentFileInput(), na.na, paste0(getNameFileCli()
  output$downloadDataPlotly <- downloadHandler(
    filename = function() { 
      paste0(getNameFileCli(), '.filledPlotly')
    },
    content = function(file) {
      filled.cli <- na.grnn.cli(currentFileInput(), 108, 
                           input$sigmaPrecPlotly, FALSE, FALSE)
      write.table(filled.cli, file)
    }  
  ) # downloadDataPlotly <- downloadHandler(

  output$frameShinyJS <- renderUI({ # https://shiny.rstudio.com/articles/tag-glossary.html
    demoShinyJS <- tags$iframe(src="https://daattali.com/shiny/shinyjs-mini-demo/", height=600, width=900)
    print(demoShinyJS)
    demoShinyJS
  })
  
  ###############################
  ### Plotly
  ###############################
  output$PlotlyPrec <- renderPlotly({
    #plotlyMarkersLines()
    grnn.impute <- na.grnn.cli(currentFileInput(), 106, input$sigmaPrecPlotly, FALSE, FALSE)
    plotlyVect(currentFileInput()$prec, grnn.impute$prec,
               paste0(getNameFileCli(), " Prec - GRNN-R"), "Precipitation in millimeters", "Days")
  }) # output$PlotlyPrec
  
  output$PlotlyTemp <- renderPlotly({
    #plotlyNAMarkersLines()
    grnn.impute <- na.grnn.cli(currentFileInput(), 106, input$sigmaTempPlotly, FALSE, FALSE)
    plotlyVect(currentFileInput()$temp, grnn.impute$temp,
               paste0(getNameFileCli(), " Temp - GRNN-R"), "Temperature in degrees Celsius", "Days")
  }) # output$PlotlyTemp
  
  })
#

