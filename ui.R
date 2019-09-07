source("ui_env.R", local=TRUE)

source("ui_info.R", local=TRUE)
source("ui_Point.R", local=TRUE)



dashboardPage(
  dashboardHeader(title = "JLeague2019"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", icon=icon("info"), tabName="tab_info"
      ),
      menuItem("Point", icon=icon("line-chart"), tabName="tab_Point"
      )
      
    )
  ),
  dashboardBody(
    #tag$script(HTML(strJavaScript01)),
    
    tabItems(
      tabItem_info,
      tabItem_Point
      
    )
  ),
  skin="blue"
)