frow_e <- fluidRow( 
  box(
    title = "Univariate Plots"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar", height = "500px",width = "500px")
  ),
  box(title = "Variables in the data",status = "primary"
      ,solidHeader = TRUE,background = 'aqua'
      ,collapsible = TRUE,
      selectInput("xaxis","Select the value for x-axis",colnames(data),selected = "")))

ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = "BLACK FRIDAY SALES",titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(id = 'sidebarmenu',
                  menuItem("Univariate Plots",tabName = "uni", icon = icon("bar-chart"))
                  
                  
      )),
    
    
    dashboardBody(
      tabItems(
        tabItem("uni",frow_e)
      )
    ),skin = 'red'
  )
)

server <- function(input, output,session){
  
  
  
  
  output$bar<-renderPlot({
    bar<-table(data[,input$xaxis])
    barplot(bar,col = 'green',ylab = "Count")
  })
  
  
}

shinyApp(ui, server)
