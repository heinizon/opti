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

shinyServer(function(input, output, session) {
  
  # dynamic variable names
  observe({
    
    infile <- input$datfile
    
    print(infile)
    if(is.null(infile))
      return(NULL)
    
    dt = read.csv(infile$datapath, header = T)
    
    ## Decide later what to do with the data, here we just fill
    updateSelectInput(session, 'dimension', choices = names(dt))
    updateSelectInput(session, 'conversions', choices = names(dt))
    updateSelectInput(session, 'spend', choices = names(dt))
  
  })
  
  # upload tab
  # data status
  output$data_status <- renderText({
    
    infile <- input$datfile
    
    if(is.null(infile)){
      
      return(paste('No data set has been uploaded'))
      
    } else paste('Data successfully uploaded')
    
  }) # end data status
  
  # data tab
  # read in data
  output$data <- renderTable({
    
    infile <- input$datfile    
    
    if(is.null(infile)){
      
      return(NULL)
      
    } else
      
      read.csv(infile$datapath, header = T)
    
  }) # end data tab
  
  # analysis tab
  # running the optimization analysis
  output$analysis <- renderTable({
  
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile)){
      
      return()
      
    } else {
    
    goal_cpa <- as.numeric(input$goal)
    
    spend <- dat[ , input$spend]
    conversions <- dat[ , input$conversions]
    dimension <- dat[ , input$dimension]
    
    cpa <- ifelse(conversions == 0, max(spend), spend/conversions)
    numerator <- (1/cpa) - (1/goal_cpa)
    denominator <- sqrt((1/goal_cpa)*(1-(1/goal_cpa))/spend)
    z <- numerator/denominator
    classification <- ifelse(pnorm(z) < .05, 'Cut', 'OK')
    
    dat <- data.frame(dimension, conversions, spend, cpa, classification) %.%
      arrange(classification, cpa)
    
    return(dat)
    
    }
        
  }) # end optimization analysis

  # analysis tab
  # calculate average cpa
  output$average_cpa <- renderText({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile)) {
    
      return(paste('No data set has been uploaded'))  
      
    } else {
      
      spnd <- dat[ , input$spend]
      conv <- dat[ , input$conversions]
      
      average_cpa <- sum(spnd) / sum(conv)
      paste('Average CPA:', dollar(average_cpa))
    
    }
    
  })
  
}) # end ShinyServer