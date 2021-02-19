frow_d<-fluidRow(
  box(
    title = "Purchase by Product category 1"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar_3")),
  box(
    title = "Purchase by Product category 2"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar_4")),
  box(
    title = "Purchase by Product category 3"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar_5")),
  box(
    title = "Top 10 Products"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar_6")))


ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = "BLACK FRIDAY SALES",titleWidth = 300),
    dashboardSidebar(
      sidebarMenu(id = 'sidebarmenu',
                  menuItem("Product Insights",tabName = "insight_2", icon = icon("dashboard"))
                  
      )),
    
    
    dashboardBody(
      tabItems(
        tabItem("insight_2",frow_d)
        
      )
    ),skin = 'red'
  )
)

df7<-aggregate(data$Purchase~data$Product_Category_1,data = data,FUN = sum)
df8<-aggregate(data$Purchase~data$Product_Category_2,data = data,FUN = sum)
df9<-aggregate(data$Purchase~data$Product_Category_3,data = data,FUN = sum)
df10<-aggregate(data$Purchase~data$Product_ID,data = data,FUN = sum)
df11 <- df10[order(-df10$`data$Purchase`),] 
df11<-head(df11,10)


server <- function(input, output,session){
  
  output$bar_3<-renderPlot({
    ggplot(df7, aes(x=df7$`data$Product_Category_1`, y=df7$`data$Purchase`, fill=df7$`data$Product_Category_1`)) +
      geom_bar(stat="identity")+
      ggtitle("Purchase by Product category 1")+xlab("Product category 1")+
      ylab("Purchase Amount")+labs(fill="Product category levels")
  })
  
  output$bar_4<-renderPlot({
    ggplot(df8, aes(x=df8$`data$Product_Category_2`, y=df8$`data$Purchase`, fill=df8$`data$Product_Category_2`)) +
      geom_bar(stat="identity")+
      ggtitle("Purchase by Product category 2")+xlab("Product category 2")+
      ylab("Purchase Amount")+labs(fill="Product category levels")
  })
  
  output$bar_5<-renderPlot({
    ggplot(df9, aes(x=df9$`data$Product_Category_3`, y=df9$`data$Purchase`, fill=df9$`data$Product_Category_3`)) +
      geom_bar(stat="identity")+
      ggtitle("Purchase by Product category 3")+xlab("Product category 3")+
      ylab("Purchase Amount")+labs(fill="Product category levels")
  })
  
  output$bar_6<-renderPlot({
    ggplot(df11, aes(x=df11$`data$Product_ID`, y=df11$`data$Purchase`, fill=df11$`data$Product_ID`)) +
      geom_bar(stat="identity")+
      ggtitle("Top 10 Products")+xlab("Product Ids")+
      ylab("Purchase Amount")+labs(fill="Product category levels")
  })
  
  
}

shinyApp(ui, server)