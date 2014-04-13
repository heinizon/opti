install_load <- function (package1, ...) 
{
  # convert arguments to vector
  packages <- c(package1, ...)
  
  for(package in packages){
    
    # if packages exists, load into environment
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package)) 
    
    # if package does not exist, download, and then load
    else {
      install.packages(package)
      do.call('library', list(package))
    }
    
  }
  
}

install_load('shiny', 'xlsx', 'ggplot2', 'scales', 'dplyr')

shinyServer(function(input, output) {
  
  output$data <- renderTable({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile)) {
      dat <- data.frame(
            Line.Item = c('Line Item A','Line Item B','Line Item C'), 
            Media.Cost = c('$2589.14','$2177.04','$2165.78'),
            Total.Conversions = c(126, 107, 111))
      dat
    }

    dat
    
  }) 
  
  output$data_status <- renderText({
    
    infile <- input$datfile
    
    if(is.null(infile)){
      
      return(paste('No data set has been uploaded'))
      
    } else paste('Data successfully uploaded')
    
  })
  
  output$analysis <- renderTable({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if (is.null(infile)) {
      
      dat <- data.frame(
        Line.Item = c('Line Item A','Line Item B','Line Item C'), 
        Media.Cost = c('$2589.14','$2177.04','$2165.78'),
        Total.Conversions = c(126, 107, 111))
      dat
      
    } else {
      
      goal_cpa <- as.numeric(input$goal)
      
      dat %.%
        transform(cpa = ifelse(Total.Conversions == 0,
                               max(Media.Cost),
                               Media.Cost/Total.Conversions)) %.%
        transform(numerator = (1/cpa) - (1/goal_cpa),
                  denominator = sqrt((1/goal_cpa)*(1-(1/goal_cpa))/Media.Cost)) %.%
        transform(z = numerator/denominator) %.%
        transform(classification = ifelse(pnorm(z) < .05, 'Cut', 'OK')) %.%
        select(Line.Item, classification, cpa) %.%
        arrange(classification, cpa)
      
    }
    
  })
        
}) # end ShinyServer I/O



