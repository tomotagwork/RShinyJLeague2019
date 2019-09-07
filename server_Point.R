

output$plotly_point <- renderPlotly({
  plotPoint <- ggplot2::ggplot(dfTeamData_plot, aes(x=as.Date(matchday), y=target_team_cumpoint, color=target_team)) + 
    geom_line() +   geom_point() +
    labs(x="date", y="points") +
    scale_x_date(labels=date_format("%Y/%m/%d")) +
    scale_color_manual(
      name = "team",
      values = c(
        札幌 = "#D6000F",
        仙台 = "#ffcc33",
        鹿島 = "#af011c",
        浦和 = "#e60412",
        FC東京 = "#000080",
        川崎Ｆ = "#27bfe5",
        横浜FM = "#0000ff",
        湘南 = "#6DBA2E",
        松本 = "#006600", 
        清水 = "#FF9809", 
        磐田 = "#6b9ad3", 
        名古屋 = "#D80C18",
        Ｇ大阪 = "#0E3192",
        Ｃ大阪 = "#F21E8C",
        神戸 = "#9D0020",
        鳥栖 = "#3aadde",
        広島 = "#666666",
        大分 = "#000080"
      )
    )
  
  ggplotly(plotPoint)
  
})

