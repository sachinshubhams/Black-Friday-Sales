ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title = "BLACK FRIDAY SALES",titleWidth = 300,
                    dropdownMenu(type = "messages",
                                 messageItem(
                                   from = "Harshit Gupta",
                                   message = "Check my articles here",
                                   href = 'https://www.analyticsvidhya.com/blog/author/harshit_gupta_6349/'
                                 ),
                                 messageItem(
                                   from = "Black friday sales Datahack",
                                   message = "How do I register?",
                                   icon = icon("question"),
                                   href = 'https://datahack.analyticsvidhya.com/contest/black-friday/'
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
        tabItem("summary"),
        tabItem("insight"),
        tabItem("insight_2"),
        tabItem("uni"),
        tabItem("chart1")
        
      )
    ),skin = 'red'
  )
)

server <- function(input, output,session){}

shinyApp(ui, server)