#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(imputeTS)
library(grnn)
library(grt)
library(zoo)
library(mice)
library(VIM)
library(missForest)
library(mi)
library(Hmisc)

library(dplyr)
library(ggplot2)
library(dygraphs)
library(plotly)
library(lubridate)
library(xts)
options(shiny.reactlog=TRUE)
#OS Version definition
Sys.setenv(R_ZIPCMD="/usr/bin/zip")
#Sys.setenv(R_ZIPCMD="C:/Users/IVA/Dropbox/Apps/bin/zip.exe")
#Sys.setenv(R_ZIPCMD="zip.exe")
#load("impute.rda")

station.cli <- read.csv('data/36307.txt', header = FALSE, sep = ";", dec = ".")
station.cli <- station.cli[-c(5, 6, 7, 9, 10, 11, 13, 14)] 
station.cli <- setNames(station.cli, c("station", "year", "month", "day", 'temp','prec'))
vso.cli <- data.frame(station.cli$day, station.cli$month, station.cli$year, station.cli$prec, station.cli$temp)
names(vso.cli) <- c("day", "month", "year", "prec", "temp")

source("na_grnn_vector.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  select.year <- function(years, num){
    year <- years[years$year == num, ]
    return(year)
  }
  select.year.prec <- function(years, num){
    year <- select.year(years, num)
    year.prec <- year$prec
    return(year.prec)
  }
  select.year.temp <- function(years, num){
    year <- select.year(years, num)
    year.temp <- year$temp
    return(year.temp)
  }
  
  length_data <-reactive({
    nrow(data())
  })
  
  data <- reactive({
    #input$dates_plotNADPrec <-
    if (is.null(input$file1)) {
      vso.cli
    }
    else {
      if (input$cliFormat == "vso") {
        cli_merge <- data.frame()
        for (i in 1:nrow(input$file1)) {
          cli <- read.csv(input$file1[[i, 'datapath']],  header=FALSE, sep="")
          names(cli) <- c("day", "month", "year", "prec", "temp")
          cli[cli$prec == -9999, 4] <- NA 
          cli[cli$temp == -9999, 5] <- NA 
          cli_merge <- rbind(cli_merge, cli)
        }
        cli_merge
      }
      else vso.cli
    }
  })
   
  output$strFileInput <- renderPrint({
    if (is.null(input$file1)) {
      cat("Synop Information for 36307 in Erzin, TY, Russian Federation\n")
      cat("http://weather.gladstonefamily.net/site/36307")
    }
    else {
      str(input$file1) 
    }
  })
  output$printStatsNA <- renderPrint({
    if (!is.null(data())) {
      print(statsNA(as.vector(data()$temp), printOnly = TRUE))
    }
  })
  output$summaryMergeFileInput <- renderPrint({
    if (!is.null(data())) {
      cat("------------------------- day --------------------------\n")
      print(summary(data()$day))
      cat("------------------------- month ------------------------\n")
      print(summary(data()$month))
      cat("------------------------- year -------------------------\n")
      print(summary(data()$year))
      cat("------------------------- prec -------------------------\n")
      print(summary(data()$prec))
      cat("------------------------- temp -------------------------\n")
      print(summary(data()$temp))
    }
    
  })
  output$contents <- renderDataTable({
    data()
  })
  
  output$plotNADPrec <- renderPlot({
    inMin <- input$num_plotNAD[1]; inMax <- input$num_plotNAD[2]
    plotNA.distribution(data()$prec[inMin:inMax], colPoints = "steelblue", colBackgroundMV = "indianred2", main = "Distribution of NAs", xlab = "Time", ylab = "prec", pch = 20, cexPoints = 0.8, col = "black")
  })
  output$plotNADPrec1 <- renderPlot({
    inMin <- input$num_plotNAD[1]; inMax <- input$num_plotNAD[2]
    days <- length(data()$temp)
    tt <- seq(as.Date(paste0(min(data()$year),'-01-01')), by='day', length=days-1)
    vals <- data.frame(data()$temp)
    z <- zoo(vals, tt)
    plotNA.distribution(z, colPoints = "steelblue", colBackgroundMV = "indianred2", main = "Distribution of NAs", xlab = "Time", ylab = "prec", pch = 20, cexPoints = 0.8, col = "black")
  })
  output$plotNADTemp <- renderPlot({
    inMin <- input$num_plotNAD[1]; inMax <- input$num_plotNAD[2]
    plotNA.distribution(data()$temp[inMin:inMax], colPoints = "steelblue", colBackgroundMV = "indianred2", main = "Distribution of NAs", xlab = "Time", ylab = "temp", pch = 20, cexPoints = 0.8, col = "black")
  })
  output$plotNAGPrec <- renderPlot({
    plotNA.gapsize(data()$prec, ylab = "prec")
  })
  output$plotNAGTemp <- renderPlot({
    plotNA.gapsize(data()$temp, ylab = "temp")
  })
  
  output$plotNABPrec <-  renderPlot({
    plotNA.distributionBar(data()$prec, ylab = "prec")
  })
  output$plotNABTemp <- renderPlot({
    plotNA.distributionBar(data()$temp, ylab = "temp")
  })
  
  ##############################
  ### PLOT
  ##############################
  
  output$tempNA <-renderPlot({
    days <- length(data()$temp)
    tt <- seq(as.Date(paste0(min(data()$year),'-01-01')), by='day', length=days-1)
    vals <- data.frame(data()$temp)
    z <- zoo(vals, tt)
    plot(z, col="black", type="l", xlab="years", ylab="temp")
  })
  
  plotlyVect <- function(vec.na, vec.impute, vec.title, vec.ysxis, vec.xaxis,
                         mode_line = TRUE, mode_slider = TRUE) {
    days<- 1:length(vec.impute)
    data <- data.frame(days, vec.na, vec.impute)
    if (mode_line) mode.edom <- 'line'#'lines+markers'
    else mode.edom <- 'markers'
    if (mode_slider) {
      p <- plot_ly(data, x = ~days, y= ~vec.impute, name = 'vec.impute', type = 'scatter', mode = mode.edom) %>%
        add_trace(y = ~vec.na, name = 'vec.na', mode = mode.edom) %>%
        layout(title = vec.title,
               xaxis = list(title = vec.xaxis), 
               yaxis = list(title = vec.ysxis),
               legend = list(x = 0, y = 1)) %>%
        rangeslider() 
    }
    else {
      p <- plot_ly(data, x = ~days, y= ~vec.impute, name = 'vec.impute', type = 'scatter', mode = mode.edom) %>%
        add_trace(y = ~vec.na, name = 'vec.na', mode = mode.edom) %>%
        layout(title = vec.title,
               xaxis = list(title = vec.xaxis), 
               yaxis = list(title = vec.ysxis),
               legend = list(x = 0, y = 1))
    }
    
    p 
  }
  
  output$plotlyTempNAs <- renderPlotly({
    #grnn.impute <- na.grnn.vector(data()$temp, 0.1)
    grnn.impute <- na.kalman(data()$temp)
    plotlyVect(data()$temp, grnn.impute,
               paste0(" Temp - GRNN-R"), 
               "Temperature in degrees Celsius", "Days")#, 
              # mode_line = input$mode.edom, mode_slider = input$plotlyShowrangesI)
  }) # 
  
  pMiss <- function(x){
    sum(is.na(x))/length(x)*100
  }
  output$dygraphPercentageNAs <- renderDygraph({
    year<- c(min(data()$year): max(data()$year))
    perc_temp <- vector(); perc_prec <- vector(); j <- 1
    for (i in 1:length(year)) {
      perc_prec[i] <- pMiss(select.year.prec(data(), year[i]))
      perc_temp[i] <- pMiss(select.year.temp(data(), year[i]))
    }
    data.frame(year, perc_temp, perc_prec) %>%
      dygraph(main = "Percentage of missing data") %>%
      dyRangeSelector() %>%
      dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
      dyOptions(connectSeparatedPoints = FALSE, drawPoints = TRUE, pointSize = 2, drawGapEdgePoints = TRUE, strokeWidth = 2) 
       
  })
  
  output$dygraphTempNAs <- renderDygraph({
    
    if (input$replaceNAs == "GRNN-ManualSigma") {
      days <- length(data()$temp)
      tt <- seq(as.Date(paste0(min(data()$year),'-01-01')), by='day', length=days-1)
      vals <- data.frame(temp.imp=na.kalman(data()$temp), temp=data()$temp)
      z <- zoo(vals, tt)
      z %>%
        dygraph(main="GRNN-Imputation Temp") %>% # https://www.rdocumentation.org/packages/dygraphs/versions/1.1.1.4/topics/dyOptions
        #dySeries("temp.imp", drawPoints = FALSE, color = "red") #%>%
        dyAxis("y", label = "temp") %>%
        dyOptions(connectSeparatedPoints = FALSE, strokeWidth = 2) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
        dyOptions(drawGapEdgePoints = TRUE)  %>%
        dyRangeSelector()
    }
    else {
      days <- length(data()$temp)
      tt <- seq(as.Date(paste0(min(data()$year),'-01-01')), by='day', length=days-1)
      vals <- data.frame(data()$temp)
      z <- zoo(vals, tt)
      z %>%
        dygraph(main="Imputation Temp") %>%
        dyAxis("y", label = "temp") %>%
        dyOptions(connectSeparatedPoints = FALSE) %>%
        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
        dyRangeSelector()
    }
  })
  
  ##############################
  ### DownloadsData - Download multiple csv files in a zipped folder in Shiny 
  ### https://stackoverflow.com/questions/28228892/download-multiple-csv-files-in-a-zipped-folder-in-shiny
  ##############################
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("cli_data-", Sys.Date(), ".zip")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tempdir())
      for (i in c(1,2,3,4,5)) {
        path <- paste0("sample_", i, ".csv")
        fs <- c(fs, path)
        write(i*2, path)
      }
      zip(zipfile=fname, files=fs)
    }#,
   # contentType = "application/zip"
  )
  
  output$downloadDataInit <- downloadHandler(
    
    filename = function() {
      paste("cli_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  ) # downloadData <- downloadHandler(
  
})
