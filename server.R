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

install_load('shiny', 'gdata', 'ggplot2', 
             'scales', 'dplyr', 'rCharts')

shinyServer(function(input, output, session) {
  
  # dynamic variable names
  observe({
    
    infile <- input$datfile
    
    print(infile)
    if(is.null(infile))
      return(NULL)
    
    dt <- read.csv(infile$datapath, header = T)
    
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
      
      return(NULL)
      
    } else {
        return(Create_Categorization(dat)) #Create_Categorization is a function that returns a dataframe containing
                                          #a classification of Cut, OK, or BreakOut for each row
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
  
  # summary tab
  # calculate summary
  output$classification_summary <- renderTable({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile)){
      
      return(NULL)
      
    } else {
      
      goal_cpa <- as.numeric(input$goal)
      
      dat <- data.frame(spend = dat[, input$spend],
                       conversions = dat[, input$conversions],
                       dimension = dat[, input$dimension]) %.%
        transform(cpa = 
                    ifelse(conversions == 0, max(spend), spend/conversions)) %.%
        transform(numerator = (1/cpa) - (1/goal_cpa),
                  denominator = sqrt((1/goal_cpa)*(1-(1/goal_cpa))/spend)) %.%
        transform(z = numerator/denominator) %.%
        transform(classification = ifelse(pnorm(z) < 0.05, 'Cut', 'OK')) %.%
        group_by(classification) %.%
        dplyr::summarise(spend = sum(spend),
                         conversions = sum(conversions)) %.%
        transform(cpa = spend/conversions) %.%
        return()
      
    }
    
  })
  
  # chart tab
  output$chart1 <- renderChart({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile))
      return(NULL)
    
    spnd <- dat[, input$spend]
    conv <- dat[, input$conversions]
          
    datt <- data.frame(spend = dat[, input$spend],
                       conversions = dat[, input$conversions])
    
    p1 <- rPlot(x = 'spend', y = 'conversions', data = datt, type = 'point')
    
    p1$addParams(height = 400, width = 700, dom = 'chart1')
    
    p1$guides(x = list(title = input$spend))
    p1$guides(y = list(title = input$conversions))
    
    return(p1)
    
  })

  #downloadAnalysis
  #Outputs a csv file when a user hits the Download Analysis button
  #file contains the categories from Create_Categorization function
  output$downloadAnalysis <- downloadHandler(
    filename = function(){
      paste("testfile.csv")
    },
    content = function(file) { 
      infile <- input$datfile
      dat <- read.csv(infile$datapath, header = T)
      
      if(is.null(infile))
        return(NULL)
      write.csv(Create_Categorization(dat), file)
    }
  )#End of downloadAnalysis


  #Create_Categorization
  #Output:  Dataframe containing the classification (OK, Cut, BreakOut) for each row
  #input:  Uploaded data stored as variable 'dat'
  Create_Categorization <- function(dat) {
    goal_cpa <- as.numeric(input$goal)
    
    spend <- dat[, input$spend]
    conversions <- dat[, input$conversions]
    dimension <- dat[, input$dimension]
    cpa <- ifelse(conversions == 0, max(spend), spend/conversions)
    
    dat <- data.frame(spend = dat[, input$spend],
                      conversions = dat[, input$conversions],
                      dimension = dat[, input$dimension],
                      cpa = cpa) %.%
      transform(numerator = (1/cpa) - (1/goal_cpa),
                denominator = sqrt((1/goal_cpa)*(1-(1/goal_cpa))/spend)) %.%
      transform(z = numerator/denominator) %.%
      transform(classification = ifelse(pnorm(z) < 0.05, 'Cut', 'OK')) %.%
      group_by(classification) %.%
      transform(cpa = spend/conversions) %.%
      select(dimension, conversions, spend, cpa, classification) %.%
      arrange(classification, cpa) %.%
      rename.vars(c('dimension', 'conversions', 'spend', 
                    'cpa', 'classification'),
                  c(input$dimension, input$conversions, input$spend, 
                    'CPA', 'Classification'))
  } #End Create_Categorization

}) # end ShinyServer


