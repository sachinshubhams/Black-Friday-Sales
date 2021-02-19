frow_c <- fluidRow( 
  box(
    title = "Purchase amount by Males and Females"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("pie_1")
  ),
  box(
    title = "Purchase amount by Age intervals"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("pie_2")),
  box(
    title = "Purchase amount by Marital Status"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("pie_3")),
  box(
    title = "Purchase amount by City categories"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("pie_4")),
  box(
    title = "Purchase by Stay in Current city"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar_1")),
  box(
    title = "Purchase by Occupation"
    ,status = "primary"
    ,solidHeader = TRUE,background = 'purple'
    ,collapsible = TRUE 
    ,plotOutput("bar_2")))

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
                  
                  menuItem("Demographics Insights",tabName = "insight", icon = icon("dashboard"))
      )),
    
    
    dashboardBody(
      tabItems(
        tabItem("insight",frow_c)
        
      )
    ),skin = 'red'
  )
)

df1<-aggregate(data$Purchase~data$Gender,data = data,FUN = sum)
df2<-aggregate(data$Purchase~data$Age,data = data,FUN = sum)
df3<-aggregate(data$Purchase~data$Marital_Status,data = data,FUN = sum)
df4<-aggregate(data$Purchase~data$City_Category,data = data,FUN = sum)
df5<-aggregate(data$Purchase~data$Stay_In_Current_City_Years,data = data,FUN = sum)
df6<-aggregate(data$Purchase~data$Occupation,data = data,FUN = sum)

server <- function(input, output,session){
  
  
  
  output$pie_1<-renderPlot({
    ggplot(df1, aes(x="", y=df1$`data$Purchase`, fill=df1$`data$Gender`))+
      geom_bar(position ="fill" ,width = 1, stat = "identity")+coord_polar("y", start=0)+
      geom_text(aes(label=scales::percent(df1$`data$Purchase`/sum(df1$`data$Purchase`))),
                stat='identity',position=position_fill(vjust=0.5))+ggtitle("Purchase amount by Males and Females")+
      ylab("")+labs(fill="Gender levels")
  })
  
  output$pie_2<-renderPlot({
    ggplot(df2, aes(x="", y=df2$`data$Purchase`, fill=df2$`data$Age`))+
      geom_bar(position ="fill" ,width = 1, stat = "identity")+coord_polar("y", start=0)+
      geom_text(aes(label=scales::percent(df2$`data$Purchase`/sum(df2$`data$Purchase`))),
                stat='identity',position=position_fill(vjust=0.5))+ggtitle("Purchase amount by Age intervals")+
      ylab("")+labs(fill="Age levels")
  })
  
  output$pie_3<-renderPlot({
    ggplot(df3, aes(x="", y=df3$`data$Purchase`, fill=df3$`data$Marital_Status`))+
      geom_bar(position ="fill" ,width = 1, stat = "identity")+coord_polar("y", start=0)+
      geom_text(aes(label=scales::percent(df3$`data$Purchase`/sum(df3$`data$Purchase`))),
                stat='identity',position=position_fill(vjust=0.5))+ggtitle("Purchase amount by Marital Status")+
      ylab("")+labs(fill="Marital Status Levels")
  })
  
  output$pie_4<-renderPlot({
    ggplot(df4, aes(x="", y=df4$`data$Purchase`, fill=df4$`data$City_Category`))+
      geom_bar(position ="fill" ,width = 1, stat = "identity")+coord_polar("y", start=0)+
      geom_text(aes(label=scales::percent(df4$`data$Purchase`/sum(df4$`data$Purchase`))),
                stat='identity',position=position_fill(vjust=0.5))+ggtitle("Purchase amount by City categories")+
      ylab("")+labs(fill="City Categories")
  })
  
  output$bar_1<-renderPlot({
    ggplot(df5, aes(x="", y=df5$`data$Purchase`, fill=df5$`data$Stay_In_Current_City_Years`)) +
      geom_bar(position ="fill" ,width = 1,stat="identity")+
      geom_text(aes(label=scales::percent(df5$`data$Purchase`/sum(df5$`data$Purchase`))),
                stat='identity',position=position_fill(vjust= 0.5))+
      ggtitle("Purchase by Stay in Current city")+xlab("Years of Stay in Current City")+
      labs(fill="Years in City")
  })
  
  output$bar_2<-renderPlot({
    ggplot(df6, aes(x=df6$`data$Occupation`, y=df6$`data$Purchase`, fill=df6$`data$Occupation`)) +
      geom_bar(stat="identity")+
      ggtitle("Purchase by Occupation")+xlab("Occupation Levels")+
      ylab("Purchase Amount")+labs(fill="Occupation Levels")
  })
  
  
}

shinyApp(ui, server)