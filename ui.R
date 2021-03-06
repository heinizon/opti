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

#
install_load('shiny', 'rCharts')

shinyUI(pageWithSidebar(
  
  # app title
  headerPanel('Optimization Reporting'),
  
  # goal input
  sidebarPanel(
    wellPanel(
      h3(textInput('goal', 'Please enter the goal CPA: ', '')),
      selectInput('dimension', 'Dimension:' ,'Dimension'),
      selectInput('conversions', 'Conversions:', 'Conversions'),
      selectInput('spend', 'Spend:', 'Spend'),
      HTML('<br>'),
      downloadButton('downloadAnalysis', "Download Analysis")
    ),
    wellPanel(
      HTML('<center><h3>CPA Summary</h3><br>'),
      tableOutput('ui_cpa_summary'),
      HTML('</center>')
    )
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel('Upload', 
               h3(textOutput('data_status')),
               fileInput('datfile', ''),
               tableOutput('data')
      ),
      
      tabPanel('Analysis',
               HTML('<br><center>'),
               tableOutput('analysis'),
               HTML('</center>')
      ),
      
      tabPanel('Summary',
               showOutput("cpa_range_chart", "Highcharts"),
               HTML('<br><center>'),
               tableOutput('classification_summary'),
               HTML('</center>')
      ),
      
      tabPanel('Charts',
               showOutput("spend_chart", "Highcharts"),
               showOutput("conv_chart", "Highcharts")
      )
    
    ) # end tabSetPanel
    
  )  #end mainPanel
  
)) # ent shinyUI