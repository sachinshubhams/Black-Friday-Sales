frow_f <- fluidRow( 
  box(
    title = "Bivariate Chart"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar1", height = "500px",width = "500px")
  ),
  box(title = "Variables in the data",status = "primary"
      ,solidHeader = TRUE,background = 'aqua'
      ,collapsible = TRUE,selectInput("x_axis","Select the value for x-axis",colnames(data),selected = ""),
      selectInput("yaxis","Select the value for y-axis",colnames(data),selected = "Purchase")
  ))

ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = "BLACK FRIDAY SALES",titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(id = 'sidebarmenu',
                  menuItem("Bivariate Plots",
                           icon = icon('bar-chart'),
                           tabName = 'chart1'
                           
                  ))),
    
    
    dashboardBody(
      tabItems(
        tabItem("chart1",frow_f)
        
      )
    ),skin = 'red'
  )
)

server <- function(input, output,session){
  
  output$bar1<-renderPlot({
    bar1<-tapply(data[,input$yaxis], list(data[,input$x_axis]), mean)
    barplot(bar1,col = 'red',ylab = "Purchase Amount")
  })
  
  
}

shinyApp(ui, server)