data<-read.csv(file.choose())


u1 <- data$User_ID
df_uniq1 <- unique(u1)
l1<-length(df_uniq1)

u2 <- data$Product_ID
df_uniq2 <- unique(u2)
l2<-length(df_uniq2)

data$Purchase_millions<-(data$Purchase/1000000)

frow_a <- fluidRow(
  valueBox(sum(data$Purchase_millions),"Total Purchase Amount(in millions)", icon = icon("rupee"),color = 'orange'),
  valueBox(l1,"Total unique users", icon = icon("users"),color = 'purple'),
  valueBox(l2,"Total unique products", icon = icon("product-hunt"),color = 'green'))

frow_b<-fluidRow(box(title = "Dimension of data", tableOutput("data_dim"), width = 15,
                     solidHeader = T, background = "maroon"),
                 (box(title = "Top 5 Observations", tableOutput("data_head"), width = 15,
                      solidHeader = T, background = "navy")),
                 (box(title = "Last 5 Observations", tableOutput("data_tail"), width = 15,
                      solidHeader = T, background = "black")))

ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = "BLACK FRIDAY SALES",titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(id = 'sidebarmenu',
                  # first menu item
                  menuItem("Summary",tabName = "summary", icon = icon("dashboard"))
                  
                  
      )),
    
    
    dashboardBody(
      tabItems(
        tabItem("summary",frow_a,frow_b)
        
      )
    ),skin = 'red'
  )
)

server <- function(input, output,session){
  output$data_head <- renderTable({
    head(data[c(-12,-13)],5)
  })
  
  output$data_tail <- renderTable({
    tail(data[c(-12,-13)],5)
  })
  
  output$data_dim <- renderTable({
    dim.data.frame(data)
  })
}

shinyApp(ui, server)