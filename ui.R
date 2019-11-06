source("ui_env.R", local=TRUE)
source("ui_info.R", local=TRUE)
source("ui_Point.R", local=TRUE)
source("ui_ResultAll.R", local=TRUE)
source("ui_ResultTeam.R", local=TRUE)

source('ui_javaScript.R', local = TRUE)

dashboardPage(
  dashboardHeader(title = "JLeague2019"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", icon=icon("info"), tabName="tab_info"
      ),
      menuItem(menuStrPoint, icon=icon("line-chart"), tabName="tab_Point"
      ),
      menuItem(menuStrResult, icon=icon("table"), 
               menuSubItem(menuStrResultAll,tabName="tab_ResultAll"),
               menuSubItem(menuStrResultTeam, tabName="tab_ResultTeam")
      )
      
    )
  ),
  dashboardBody(
    tags$script(HTML(strJavaScript01)),
    tabItems(
      tabItem_info,
      tabItem_Point,
      tabItem_ResultAll,
      tabItem_ResultTeam
      
    )
  ),
  skin="blue"
)