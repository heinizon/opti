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
             'scales', 'dplyr', 'rCharts',
             'XLConnect', 'plyr')

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
      # categorize is a function that returns a dataframe containing
      # a classification of Cut, OK, or BreakOut for each row
      
        return(categorize(dat)) 
    }
        
  }) # end optimization analysis

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
  
  # downloadAnalysis
  # outputs a csv file when a user hits the Download Analysis button
  # file contains the categories from categorize function
  output$downloadAnalysis <- downloadHandler(
    
    filename = function(){
      
      paste('optimizerAnalysis_', Sys.time(), '.xlsx', sep = '')
    
      },
    
    content = function(file){
        
        infile <- input$datfile
        dat <- read.csv(infile$datapath, header = T)
        
        if(is.null(infile))
          return(NULL)
        
        fname <- paste(file, "xlsx", sep = ".")
        
        categorized_dat <- categorize(dat)
       
        listData <- plyr::dlply(categorized_dat,
                                plyr::.(Classification))
 

        wb <- XLConnect::loadWorkbook(fname, create = T)
        
        XLConnect::createSheet(wb, name = 'Summary')
        XLConnect::writeWorksheet(wb, cpa_summary(dat), sheet = 'Summary')
       
        for(i in 1:length(listData))
          assign(names(listData)[i], listData[[i]])
        
        # write data to worksheets with formatting ####
        for(theDataCut in names(listData)){
          XLConnect::createSheet(wb, name = theDataCut)
          XLConnect::writeWorksheet(wb, get(theDataCut), sheet = theDataCut)
          XLConnect::setColumnWidth(wb, sheet = theDataCut, 
                                    column = 1:length(get(theDataCut)), width = -1)
        }
        
        XLConnect::saveWorkbook(wb)
        file.rename(fname, file)
        
      }
    
  ) # end downloadAnalysis

  # categorize
  # input: uploaded data stored as variable 'dat'
  # output: dataframe containing the classification (OK, Cut, BreakOut) for each row
  categorize <- function(dat) {
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
  } # end categorize
  
  # summary tab for exported data
  cpa_summary <- function(dat){
    
    goal_cpa <- as.numeric(input$goal)
    
    spend <- dat[, input$spend]
    conversions <- dat[, input$conversions]
    dimension <- dat[, input$dimension]
    cpa <- ifelse(conversions == 0, max(spend), spend/conversions)
    
    datt <- data.frame(spend = spend,
                      conversions = conversions,
                      dimension = dimension,
                      cpa = cpa) %.%
      transform(numerator = (1/cpa) - (1/goal_cpa),
                denominator = sqrt((1/goal_cpa)*(1-(1/goal_cpa))/spend)) %.%
      transform(z = numerator/denominator) %.%
      transform(classification = ifelse(pnorm(z) < 0.05, 'Cut', 'OK')) %.%
      group_by(classification) %.%
      transform(cpa = spend/conversions) %.%
      select(dimension, conversions, spend, cpa, classification) %.%
      arrange(classification, cpa) %.%
      group_by(classification) %.%
      dplyr::summarise(cpa = sum(spend)/sum(conversions),
                       spend = sum(spend),
                       conversions = sum(conversions))
    
    cut_cpa <- datt[which(datt$classification == 'Cut'), 2]
    ok_cpa <- datt[which(datt$classification == 'OK'), 2]
    avg_cpa <- sum(spend)/sum(conversions)
    
    cut_spend <- datt[which(datt$classification == 'Cut'), 3]
    ok_spend <- datt[which(datt$classification == 'OK'), 3]
    all_spend <- sum(spend)
    
    cut_conversions <- datt[which(datt$classification == 'Cut'), 4]
    ok_conversions <- datt[which(datt$classification == 'OK'), 4]
    all_conversions <- sum(conversions)
    
    cpa_summary <- data.frame(
      Group = c('OK', 'Cut', 'Total/Avg', 'Goal'),
      CPA = c(ok_cpa, cut_cpa, avg_cpa, goal_cpa),
      spend = c(ok_spend, cut_spend, all_spend, NA),
      conversions = c(ok_conversions, cut_conversions, all_conversions, NA)) %.%
      rename.vars(c('spend', 'conversions'),
                  c(input$spend, input$conversions))

  }
  
  # conversions chart
  output$conv_chart <- renderChart({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile))
      return(NULL)
    
    goal_cpa <- as.numeric(input$goal)
    
    spend <- dat[, input$spend]
    conversions <- dat[, input$conversions]
    dimension <- dat[, input$dimension]
    cpa <- ifelse(conversions == 0, max(spend), spend/conversions)
    
    dat <- data.frame(spend = spend,
                      conversions = conversions,
                      dimension = dimension,
                      cpa = cpa) %.%
      transform(numerator = (1/cpa) - (1/goal_cpa),
                denominator = sqrt((1/goal_cpa)*(1-(1/goal_cpa))/spend)) %.%
      transform(z = numerator/denominator) %.%
      transform(classification = ifelse(pnorm(z) < 0.05, 'Cut', 'OK')) %.%
      group_by(classification) %.%
      transform(cpa = spend/conversions) %.%
      select(dimension, conversions, spend, cpa, classification) %.%
      arrange(classification, cpa)
    
    CPA <- as.numeric(paste(dat$cpa))
    CPA <- 
      ifelse(is.na(CPA), 
             max(dat$spend), 
             CPA)
    
    cpa <- c()
    for(i in CPA){
      cpa <- c(cpa, ifelse(i == max(dat$spend), 
                           jitter(i), 
                           i))
    }
    dat$CPA <- cpa
    dat$group <- cut(dat$CPA, 
                    breaks = quantile(dat$CPA, 
                                      probs = seq(0, 1, 1/5)),
                    include.lowest= T)

    pdc <- 
      dat %.%
      mutate(allConversions = sum(conversions)) %.%
      dplyr::group_by(group, classification) %.%
      dplyr::mutate(group_conversions = sum(conversions)) %.%
      select(classification, group, group_conversions) %.%
      unique() %.%
      dcast(group ~ classification, fill = 0)
    
    row.names(pdc) <- pdc$group
    pdc$group <- NULL
    
    convChart <- Highcharts$new()
    convChart$chart(type = 'bar',
             inverted = T,
             margin = list(left = 100))
    convChart$xAxis(title = list(text = 'CPA Decile'))
    convChart$yAxis(title = list(text = input$conversions))
    convChart$xAxis(categories = rownames(pdc))
    convChart$data(pdc)
    convChart$addParams(dom = 'conv_chart')
    convChart$plotOptions(series = list(stacking = 'normal'))
    return(convChart)
    
  }) # end of conversion chart
  
  # spend chart
  output$spend_chart <- renderChart({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile))
      return(NULL)
    
    goal_cpa <- as.numeric(input$goal)
    
    spend <- dat[, input$spend]
    conversions <- dat[, input$conversions]
    dimension <- dat[, input$dimension]
    cpa <- ifelse(conversions == 0, max(spend), spend/conversions)
    
    dat <- data.frame(spend = spend,
                      conversions = conversions,
                      dimension = dimension,
                      cpa = cpa) %.%
      transform(numerator = (1/cpa) - (1/goal_cpa),
                denominator = sqrt((1/goal_cpa)*(1-(1/goal_cpa))/spend)) %.%
      transform(z = numerator/denominator) %.%
      transform(classification = ifelse(pnorm(z) < 0.05, 'Cut', 'OK')) %.%
      group_by(classification) %.%
      transform(cpa = spend/conversions) %.%
      select(dimension, conversions, spend, cpa, classification) %.%
      arrange(classification, cpa)
    
    CPA <- as.numeric(paste(dat$cpa))
    CPA <- 
      ifelse(is.na(CPA), 
             max(dat$spend), 
             CPA)
    
    cpa <- c()
    for(i in CPA){
      cpa <- c(cpa, ifelse(i == max(dat$spend), 
                           jitter(i), 
                           i))
    }
    dat$CPA <- cpa
    dat$group <- cut(dat$CPA, 
                     breaks = quantile(dat$CPA, 
                                       probs = seq(0, 1, 1/5)),
                     include.lowest= T)
    
    pds <- 
      dat %.%
      dplyr::mutate(allSpend = sum(spend)) %.%
      dplyr::group_by(group, classification) %.%
      dplyr::mutate(group_spend = sum(spend)) %.%
      select(classification, group, group_spend) %.%
      unique() %.%
      dcast(group ~ classification, fill = 0)
    
    row.names(pds) <- pds$group
    pds$group <- NULL
    
    spendChart <- Highcharts$new()
    spendChart$chart(type = 'bar',
                    inverted = T,
                    margin = list(left = 100))
    spendChart$xAxis(title = list(text = 'CPA Decile'))
    spendChart$yAxis(title = list(text = input$spend))
    spendChart$xAxis(categories = rownames(pds))
    spendChart$data(pds)
    spendChart$addParams(dom = 'spend_chart')
    spendChart$plotOptions(series = list(stacking = 'normal'))
    return(spendChart)
    
  }) # end of spend chart
  
  
}) # end ShinyServer


