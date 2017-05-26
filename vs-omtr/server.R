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
#Sys.setenv(R_ZIPCMD="/usr/bin/zip")
Sys.setenv(R_ZIPCMD="/usr/bin/zip")
#load("impute.rda")

station.cli <- read.csv('data/36307.txt', header = FALSE, sep = ";", dec = ".")
station.cli <- station.cli[-c(5, 6, 7, 9, 10, 11, 13, 14)] 
station.cli <- setNames(station.cli, c("station", "year", "month", "day", 'temp','prec'))
vso.cli <- data.frame(station.cli$day, station.cli$month, station.cli$year, station.cli$prec, station.cli$temp)
names(vso.cli) <- c("day", "month", "year", "prec", "temp")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
    str(input$file1)
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
  ### DownloadsData - Download multiple csv files in a zipped folder in Shiny 
  ### https://stackoverflow.com/questions/28228892/download-multiple-csv-files-in-a-zipped-folder-in-shiny
  ##############################
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cli_data-", Sys.Date(), "zip", sep=".")
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
      #zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
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
