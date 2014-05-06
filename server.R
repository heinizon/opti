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
             'XLConnect', 'reshape2')

shinyServer(function(input, output, session) {
  
  # dynamic variable names
  observe({
    
    infile <- input$datfile
    
    print(infile)
    if(is.null(infile))
      return(NULL)
    
    d <- read.csv(infile$datapath, header = T)
    
    updateSelectInput(session, 'dimension', choices = names(d))
    updateSelectInput(session, 'conversions', choices = names(d))
    updateSelectInput(session, 'spend', choices = names(d))
    
  })
  
  # upload tab
  # data status
  output$data_status <- renderText({
    
    infile <- input$datfile
    
    if(is.null(infile)){
      
      return(paste('No data set has been uploaded'))
      
    } else paste('Data successfully uploaded!')
    
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
      # a classification of Under Performing, Within Range, or BreakOut for each row
      dat2 <- categorize(dat)  %.%
        rename.vars(c('dimension', 'spend', 'conversions', 'cpa'), 
                    c(input$dimension, input$spend, input$conversions, 'CPA')) %.%
        return() 
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
      
      dat2 <- categorize(dat)      
      
      summary_dat <- group_by(dat2, classification) %.%
        dplyr::summarise(spend = sum(spend), conversions = sum(conversions)) %.%
        transform(cpa = spend/conversions)        
      
      total_dat <- dplyr::summarise(summary_dat, 
                                    spend = sum(spend), 
                                    conversions = sum(conversions))
      
      total_spend <- total_dat$spend[1]
      total_Conv <- total_dat$conversions[1]
      totals <- data.frame(total_spend = rep(total_spend, nrow(summary_dat)), 
                           total_Conv = rep(total_Conv, nrow(summary_dat)))
      summary_dat <- cbind(summary_dat, totals)
      
      summary_dat <- mutate(summary_dat, 
                            percent.of.spend = spend/total_spend, 
                            percent.of.conv = conversions/total_Conv) %.% 
        mutate(spend = dollar(spend), 
               conversions = comma(conversions), 
               cpa = dollar(cpa)) %.%
        mutate(percent.of.spend = percent(percent.of.spend), 
               percent.of.conv = percent(percent.of.conv)) %.%
        select(classification, spend, conversions, cpa, percent.of.spend, percent.of.conv) %.%
        rename.vars(c('spend', 'conversions', 'cpa', 'percent.of.spend', 'percent.of.conv'), 
                    c(input$spend, input$conversions, 'CPA', '% Spend', 'Conversions %')) %.%
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
      
      summary_cpa <- cpa_summary(dat)
      spend <- sum(dat[, input$spend])
      conversions <- sum(dat[, input$conversions])
      
      total_cpa <- spend/conversions
      total <- data.frame(classification = "Total", 
                          cpa = total_cpa, 
                          spend = spend, 
                          conversions = conversions)
      summary_cpa <- rbind(summary_cpa, total)
      goal_cpa <- data.frame(classification = "Goal", 
                             cpa = as.numeric(input$goal), 
                             spend = NA, 
                             conversions = NA)
      summary_cpa <- rbind(summary_cpa, goal_cpa) %.%
        rename.vars(c('spend', 'conversions', 'cpa'), 
                    c(input$spend, input$conversions, 'CPA'))
      
      categorized_dat <- categorize(dat) %.%
        rename.vars(c('dimension','spend', 'conversions', 'cpa'), 
                    c(input$dimension, input$spend, input$conversions, 'CPA'))
      
      listData <- plyr::dlply(categorized_dat,
                              plyr::.(classification))
      
      wb <- XLConnect::loadWorkbook(fname, create = T)
      
      XLConnect::createSheet(wb, name = 'Summary')
      XLConnect::writeWorksheet(wb, summary_cpa, sheet = 'Summary')
      XLConnect::setColumnWidth(wb, sheet = 'summary', 
                                column = 1:length(cpa_summary(dat)), width = -1)
      
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
  # output: dataframe containing the classification (Within Range, Under Performing, BreakOut) for each row
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
      transform(classification_cut = ifelse(pnorm(z) < 0.05, 'Under Performing', 'Within Range'))
    
    ok_dat <- filter(dat, classification_cut == "Within Range") %.%
      group_by(classification_cut) %.%
      summarise(spend=sum(spend), conv=sum(conversions))
    ok_cpa <- as.numeric(ok_dat$spend[1] / ok_dat$conv[1])
    
    dat <- transform(dat, numerator_bo = (1/cpa) - (1/ok_cpa),
                     denominator_bo = sqrt((1/ok_cpa)*(1-(1/ok_cpa))/spend)) %.%
      transform(z_bo = numerator_bo / denominator_bo) %.%
      transform(classification_bo = ifelse(1 - pnorm(z_bo) < .05, "Out Performing", "Within Range")) %.%
      transform(classification = ifelse(classification_cut == "Within Range", 
                                        ifelse(classification_bo == "Within Range", 
                                               "Within Range", 
                                               "Out Performing"), 
                                        "Under Performing"))
    
    dat <- group_by(dat, classification) %.%
      transform(cpa = spend/conversions) %.%
      select(dimension, conversions, spend, cpa, classification) %.%
      arrange(classification, cpa)
    #       
  } # end categorize
  
  # summary tab for exported data
  cpa_summary <- function(dat){
    
    goal_cpa <- as.numeric(input$goal)
    
    spend <- dat[, input$spend]
    conversions <- dat[, input$conversions]
    dimension <- dat[, input$dimension]
    cpa <- ifelse(conversions == 0, max(spend), spend/conversions)
    
    datt <- categorize(dat) %.%  
      group_by(classification) %.%
      dplyr::summarise(cpa = sum(spend)/sum(conversions),
                       spend = sum(spend),
                       conversions = sum(conversions)) 
    
  }
  
  # summary tab for exported data
  output$ui_cpa_summary <- renderTable({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile))
      return(NULL)
    
    goal_cpa <- as.numeric(input$goal)
    
    spend <- dat[, input$spend]
    conversions <- dat[, input$conversions]
    dimension <- dat[, input$dimension]
    cpa <- spend/conversions
    
    data.frame(
      Dimension = c('Average CPA (w/o Inf)', 'Median CPA', 
                    'Min CPA', 'Max CPA', 'Max (w/o Inf)'),
      CPA = c(mean(cpa[is.finite(cpa)]), median(cpa), 
              min(cpa), max(cpa), max(cpa[is.finite(cpa)]))
      )
  
  })
  
  # categorize_cpa.chart
  # input: uploaded data stored as variable 'dat' & a goal_cpa
  # output: dataframe containing the classification 
  # Within Range, Under Performing, & BreakOut for each row, 
  # with the assumed goal_cpa
  # reason:  Designed for use in plotting the cpa Chart
  # goal_cpa is an input so that we can iterate through
  
  categorize_cpa.chart <- function(dat, goal_cpa) {
    
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
      transform(classification_cut = ifelse(pnorm(z) < 0.05, 'Under Performing', 'Within Range'))
    
    ok_dat <- filter(dat, classification_cut == "Within Range") %.%
      group_by(classification_cut) %.%
      summarise(spend=sum(spend), conv=sum(conversions))
    
    ok_cpa <- as.numeric(ok_dat$spend[1] / ok_dat$conv[1])
    
    dat <- transform(dat, numerator_bo = (1/cpa) - (1/ok_cpa),
                     denominator_bo = sqrt((1/ok_cpa)*(1-(1/ok_cpa))/spend)) %.%
      transform(z_bo = numerator_bo / denominator_bo) %.%
      transform(classification_bo = ifelse(1 - pnorm(z_bo) < .05, 
                                           "Out Performing", 
                                           "Within Range")) %.%
      transform(classification = ifelse(classification_cut == "Within Range", 
                                        ifelse(classification_bo == "Within Range", 
                                               "Within Range", 
                                               "Out Performing"), 
                                        "Under Performing"))
    
    dat <- group_by(dat, classification) %.%
      transform(cpa = spend/conversions) %.%
      select(dimension, conversions, spend, cpa, classification) %.%
      arrange(classification, cpa)
    
  } # end categorize_cpa.chart
  
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
    
    dat <- categorize(dat)
    
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
    convChart$colors('rgba(34, 131, 0, .85)',
                     'rgba(158, 0, 22, .85)', 
                     'rgba(100, 100, 100, .85)')
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
    
    dat <- categorize(dat)
    
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
    spendChart$colors('rgba(34, 131, 0, .85)',
                      'rgba(158, 0, 22, .85)', 
                      'rgba(100, 100, 100, .85)')
    return(spendChart)
    
  }) # end of spend chart
  
  # cpa range chart
  output$cpa_range_chart <- renderChart({
    
    infile <- input$datfile
    dat <- read.csv(infile$datapath, header = T)
    
    if(is.null(infile))
      return(NULL)
    
    goal_cpa <- as.numeric(input$goal)
    goal_cpa_times2 <- goal_cpa * 2
    
    cpa_iterations <- data.frame()
    
    for(i in 1:(goal_cpa_times2)) {
      cpa_iteration <- categorize_cpa.chart(dat, i) %.%
        group_by(classification) %.%
        dplyr::summarise(spend = sum(spend),
                         conversions = sum(conversions)) %.%
        transform(cpa = spend/conversions)
      
      cpa_iteration$goal_cpa <- rep(i, nrow(cpa_iteration))
      
      cpa_iterations <- rbind(cpa_iterations, cpa_iteration)
    }
    
    cpa_iterations_casted <- select(cpa_iterations, classification, spend, goal_cpa) %.%
      filter(!is.na(classification)) %.%
      dcast(goal_cpa ~ classification, value.var = 'spend')
    
    row.names(cpa_iterations_casted) <- cpa_iterations_casted$goal_cpa
    cpa_iterations_casted$goal_cpa <- NULL
    
    # highcharts
    cpa_range_chart <- Highcharts$new()
    cpa_range_chart$chart(type = 'area')
    cpa_range_chart$yAxis(title = list(text = input$spend))
    cpa_range_chart$xAxis(title = list(text = "Goal CPA"),
                          plotLines = list(list(color = 'darkblue',
                                                value = goal_cpa,
                                                width = 3,
                                                dashStyle = 'longdash')))
    cpa_range_chart$data(cpa_iterations_casted)
    cpa_range_chart$plotOptions(area = list(stacking = 'normal',
                                            marker = list(enabled = F)))
    cpa_range_chart$addParams(dom = 'cpa_range_chart')
    cpa_range_chart$tooltip(shared = T,
                            valuePrefix = '$',
                            valueDecimals = 0)
    cpa_range_chart$colors('rgba(158, 0, 22, .85)',
                           'rgba(34, 131, 0, .85)', 
                           'rgba(100, 100, 100, .85)')
    return(cpa_range_chart)
    
    
  }) # end of cpa range chart
  
  
}) # end ShinyServer
