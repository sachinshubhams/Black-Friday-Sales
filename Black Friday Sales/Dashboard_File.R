data<-read.csv(file.choose())
library(graphics)
library(shinydashboard)
library(ggplot2)

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
      selectInput("xaxis","Select the value for x-axis",colnames(data),selected = ""))
)

frow_b<-fluidRow(box(title = "Dimension of data", tableOutput("data_dim"), width = 15,
                     solidHeader = T, background = "maroon"),
                 (box(title = "Top 5 Observations", tableOutput("data_head"), width = 15,
                      solidHeader = T, background = "navy")),
                 (box(title = "Last 5 Observations", tableOutput("data_tail"), width = 15,
                      solidHeader = T, background = "black"))
)

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


# The UI Part


ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = "BLACK FRIDAY SALES",titleWidth = 300,
                    dropdownMenu(type = "messages",
                                 messageItem(
                                   from = "Sachin Shubham",
                                   message = "Check my articles here",
                                   href = ''
                                 ),
                                 messageItem(
                                   from = "Black friday sales Datahack",
                                   message = "How do I register?",
                                   icon = icon("question"),
                                   href = ''
                                 ))),
    dashboardSidebar(
      sidebarMenu(id = 'sidebarmenu',
                  # first menu item
                  menuItem("Summary",tabName = "summary", icon = icon("dashboard")),
                  menuItem("Demographics Insights",tabName = "insight", icon = icon("dashboard")),
                  menuItem("Product Insights",tabName = "insight_2", icon = icon("dashboard")),
                  menuItem("Univariate Plots",tabName = "uni", icon = icon("bar-chart")),
                  # second menu item with 2 sub menus
                  menuItem("Bivariate Plots",
                           icon = icon('bar-chart'),
                           tabName = 'chart1'
                           
                  ))),
    
    
    dashboardBody(
      tabItems(
        tabItem("summary",frow_a,frow_b),
        tabItem("insight",frow_c),
        tabItem("insight_2",frow_d),
        tabItem("uni",frow_e),
        tabItem("chart1",frow_f)
        
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
df7<-aggregate(data$Purchase~data$Product_Category_1,data = data,FUN = sum)
df8<-aggregate(data$Purchase~data$Product_Category_2,data = data,FUN = sum)
df9<-aggregate(data$Purchase~data$Product_Category_3,data = data,FUN = sum)
df10<-aggregate(data$Purchase~data$Product_ID,data = data,FUN = sum)
df11 <- df10[order(-df10$`data$Purchase`),] 
df11<-head(df11,10)


#The server part

shinyServer <- function(input, output,session){
  
  output$data_head <- renderTable({
    head(data[c(-12,-13)],5)
  })
  
  output$data_tail <- renderTable({
    tail(data[c(-12,-13)],5)
  })
  
  output$data_dim <- renderTable({
    dim.data.frame(data)
  })
  
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
  
  output$bar<-renderPlot({
    bar<-table(data[,input$xaxis])
    barplot(bar,col = 'green')
  })
  
  output$bar1<-renderPlot({
    bar1<-tapply(data[,input$yaxis], list(data[,input$x_axis]), mean)
    barplot(bar1,col = 'red',ylab = "Purchase Amount")
  })
  
  
}

shinyApp(ui, shinyServer)
