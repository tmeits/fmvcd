#
require(shiny)
filled_path <- "C:/Users/IVA/Dropbox/Apps/"
setwd(filled_path); getwd(); 
file_name <- '2014.cli'
cli_dataset <-read.table(file_name, header=FALSE, sep="")
names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
cli_dataset[cli_dataset$perc == -9999, 4] <- NA ; prec_vector <- cli_dataset[, 4]; 
cli_dataset[cli_dataset$perc == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5];
days <- c(1:dim(cli_dataset)[1])
krest2014 <- data.frame(days=days, prec=prec_vector, temp=temp_vector)
runApp("na_grnn")

# http://jsfiddle.net/amcharts/jUKxs/
# Load, edit, and download a csv file in shiny
