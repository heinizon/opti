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


install_load('shiny', 'rCharts')

shinyUI(pageWithSidebar(
  
  # app title
  headerPanel('Optimization Reporting'),
  
  # goal input
  sidebarPanel(
    h3(textInput('goal', 'Please enter the goal CPA: ', '')),
    selectInput('dimension', 'Dimension:' ,'Dimension'),
    selectInput('conversions', 'Conversions:', 'Conversions'),
    selectInput('spend', 'Spend:', 'Spend'),
    downloadButton('downloadAnalysis', "Download Analysis")
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel('Upload', 
               h3(textOutput('data_status')),
               fileInput('datfile', ''),
               tableOutput('data')
      ),
      
      tabPanel('Analysis',
               tableOutput('analysis')
      ),
      
      tabPanel('Summary',
               tableOutput('classification_summary')
      ),
      
      tabPanel('Conversions Chart',
               showOutput("conv_chart", "Highcharts")
      ),
      
      tabPanel('Spend Chart',
               showOutput("spend_chart", "Highcharts")
      )
      
    ) # end tabSetPanel
    
  )  #end mainPanel
  
)) # ent shinyUI