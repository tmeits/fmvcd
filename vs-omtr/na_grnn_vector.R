## Iljin Victor 2017
require(grnn); require(grt); require(foreach); require(imputeTS)

setZeroNegativeNumber <- function(n){
  if(n < 0) return(0)
  else return(n)
}
as.zero.negative <- function(vec){
  return(sapply(vec, setZeroNegativeNumber))
}
#
na.grnn.vector <- function (x, s) {
  if(!is.null(dim(x)) && dim(x)[2] != 1) {
    stop("Input x is not univariate")
  }
  if (!is.numeric(x)) {
    stop("Input x is not numeric") 
  }
  if (!is.numeric(s)) {
    stop("Input s is not numeric") 
  }
  if (sum(is.na(x)) == 0) return(x)
  ## PRE-PROCESSING DATA
  vec.na <- x; vec.na.scale <- grt::scale(vec.na);  
  vec.na.scale.min <- min(vec.na.scale, na.rm = TRUE); 
  vec.na.scale.max <- max(vec.na.scale, na.rm = TRUE); 
  vec.na.index <- which(is.na(vec.na)) 
  vec.na.scale.na.omit <- na.omit(vec.na.scale);
  days <- 1:length(vec.na); days.scale <- grt::scale(days) 
  days.scale.na.omit <- days.scale[-vec.na.index]; # base::scale(days)
  XY <- data.frame(days.scale.na.omit, vec.na.scale.na.omit) # vec.scale.na.omit
  ##
  L <- grnn::learn(XY, variable.column = ncol(XY))
  grnn <- grnn::smooth(L, sigma = s)
  for (i in vec.na.index) {
    #todo РїРѕСЃС‚Р°РІРёС‚СЊ РїСЂРѕРІРµСЂРєСѓ РІС‹С…РѕРґР° Р·Р° min max
    G <- grnn::guess(grnn, days.scale[i, 1])
    if (is.na(G)) { G <- 0 }
    vec.na.scale[i] <- G
    #cat("Guess num= ", i, "\n")
  }
  vec.na.unscale <- grt::unscale(vec.na.scale)
  return(as.vector(vec.na.unscale))
} #end na.grnn.vector

min.mse.grnn <- function(x){
  return(s)
} #end min.mse.grnn
###
if(FALSE){
  ## Only run examples in interactive R sessions
  if ( interactive ( ) ) {
    
    ui <- fluidPage (
      p ( "The first radio button group controls the second" ) ,
      radioButtons ( "inRadioButtons" , "Input radio buttons" ,
                     c ( "Item A" , "Item B" , "Item C" ) ) ,
      radioButtons ( "inRadioButtons2" , "Input radio buttons 2" ,
                     c ( "Item A" , "Item B" , "Item C" ) )
    )
    
    server <- function ( input , output , session ) {
      observe ( {
        x <- input $ inRadioButtons
        
        # Can also set the label and select items
        updateRadioButtons ( session , "inRadioButtons2" ,
                             label = paste ( "radioButtons label" , x ) ,
                             choices = c("1",x) ,
                             selected = x
        )
      } )
    }
    
    shinyApp ( ui , server )
  } 
}