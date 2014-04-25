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
    selectInput('spend', 'Spend:', 'Spend')
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel('Upload', 
               h3(textOutput('data_status')),
               fileInput('datfile', ''),
               tableOutput('data')
      ),
      
      tabPanel('Analysis',
               h3(textOutput('average_cpa')),
               downloadButton('downloadAnalysis', "Download Analysis"),
               tableOutput('analysis')
      ),
      
      tabPanel('Summary',
               tableOutput('classification_summary')
      ),
      
      tabPanel('Chart',
               showOutput("chart1", "polycharts")
      )
      
    ) # end tabSetPanel
    
  )  #end mainPanel
  
)) # ent shinyUI