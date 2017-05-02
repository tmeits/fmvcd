# IVA 2.5.17
library(plotly)
library(dplyr)
library(tidyr)

plotlyMarkersLines <- function(){
  trace_0 <- rnorm(100, mean = 5)
  trace_1 <- rnorm(100, mean = 0)
  trace_2 <- rnorm(100, mean = -5)
  x <- c(1:100)
  
  data <- data.frame(x, trace_0, trace_1, trace_2)
  
  p <- plot_ly(data, x = ~x, y = ~trace_0, name = 'trace 0', type = 'scatter', mode = 'lines') %>%
    add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers') %>%
    add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers') %>%
    layout(title = "title",
             xaxis = list(title = "Days"), 
             yaxis = list(title = "Temp"))
  p
}

plotlyNAMarkersLines <- function(){
  trace_0 <- rnorm(365, mean = 5, sd = 30)
  trace_1 <- trace_0
  trace_3 <- trace_0
  trace_3[sample(length(trace_0), 33)] <- NA
  trace_0[sample(length(trace_0), 33)] <- NA
  
  
  x <- c(1:365)

  data <- data.frame(x, trace_0, trace_1, trace_3)
  colz <- c('#FE8268', '#81E8FE', '#FED681', '#81FED6', '#FE81A9')
  p <- plot_ly(data, x = ~x, 
               y = ~trace_1, name = 'trace 1', type = 'scatter', mode = 'markers') %>% # , hoverinfo = "none"
    add_trace(y = ~trace_0, name = 'trace 0', mode = 'markers') %>%
    add_trace(y = ~trace_3, name = 'trace 3', mode = 'markers') %>%
    layout(title = "title",
           xaxis = list(title = "Days"), 
           yaxis = list(title = "Temp")) %>%
    rangeslider()
  p
}

plotlyVect <- function(vec.na, vec.impute, vec.title, vec.ysxis, vec.xaxis) {
  days<- 1:length(vec.impute)
  data <- data.frame(days, vec.na, vec.impute)
  p <- plot_ly(data, x = ~days, y= ~vec.impute, name = 'vec.impute',type = 'scatter', mode = 'markers') %>%
    add_trace(y = ~vec.na, name = 'vec.na', mode = 'markers') %>%
    layout(title = vec.title,
           xaxis = list(title = vec.xaxis), 
           yaxis = list(title = vec.ysxis)) %>%
    rangeslider()  
  p 
}

if (FALSE){
  plotlyNAMarkersLines()
}
if (FALSE){
  plotlyMarkersLines()
}
if (FALSE) {
  
}