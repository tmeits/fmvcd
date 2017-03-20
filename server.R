# http://deanattali.com/blog/building-shiny-apps-tutorial/#4-load-the-dataset
library(shiny)
library(zoo)
library(grnn)
library(ggplot2)
library(reshape2)
library(imputeTS)
# load iva_scripts

createDF <- function(fileName){
  cli_dataset <-read.table(fileName, header=FALSE, sep="")
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  cli_dataset[cli_dataset$prec == -9999, 4] <- NA ; prec_vector <- cli_dataset[, 4] 
  cli_dataset[cli_dataset$temp == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5]
  days <- c(1:dim(cli_dataset)[1])
  return(data.frame(days=days, prec=prec_vector, temp=temp_vector))
}
## load global data
file_name <- '2014.cli'; krest2014 <- createDF(file_name)
file_name <- '1967.cli'; krest1967 <- createDF(file_name)
file_name <- '1969.cli'; krest1969 <- createDF(file_name)
##
shinyServer(
function(input, output) {
  #
  datasetInput <- reactive({
    switch(input$dataset,
           "krest2014" = krest2014,
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
  getFileCli <- reactive({
    inFile <- input$filecli
    if (is.null(inFile)){
      cat("Preloads .CLI", input$dataset, "\n")
    }else{
      cat("Uploads .CLI\n"); cat(str(inFile), "\n")
    }
  })
  #
  output$summaryCli <-renderPrint({
    cat(getFileCli(), "***\n")
      str(currentFileInput())
      cat("***\n")
      summary(currentFileInput())
  })
  
  output$pwd2 <-renderPrint({
    summary(datasetInput())
  })
  
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
    namePlot <- as.character(getFileCli()[1,1])
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
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste(input$dataset, '.csv', sep='') 
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
})