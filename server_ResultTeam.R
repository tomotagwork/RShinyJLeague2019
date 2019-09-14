output$uiOutput_ResultTeam <- renderUI({
  listTabBoxVal      <- list(id="tabBox_ResultTeam", title="", width = "100%")
  for (i in 1:length(teamList)) {
  #for (i in 1:1) {
    targetTeam<-teamList[i]
    listTabBoxVal[[i+3]] <- tabPanel(targetTeam, "",
                                     tags$head(tags$style(type='text/css', paste0('#dataTable_ResultTeam',targetTeam,'{ overflow-x: scroll; }'))),
                                     #textOutput(paste0("textOut",targetTeam), ),
                                     dataTableOutput(paste0("dataTable_ResultTeam",targetTeam)))
  }

  do.call(tabBox, listTabBoxVal)
  
})

purrr::iwalk(listTeamData, ~ {
  #tempTextName <- paste0("textOut",.y)
  #output[[tempTextName]] <- renderText({
  #  paste0("TTTT", .y)
  #})
  
  temp_dataTabelOutputName <- paste0("dataTable_ResultTeam",.y)
  output[[temp_dataTabelOutputName]] <- renderDataTable({
    #dfTempTeam <- listTeamData[[.y]]
    dfTempTeam <- .x
    dfTempTeam$score <- ifelse(dfTempTeam$score != "vs", 
                               paste0('<a href="', dfTempTeam$match_link, '"target="_blank">', dfTempTeam$score, '</a>'), 
                               dfTempTeam$score)
    dfTempTeam$attendances <- as.numeric(stringr::str_replace(dfTempTeam$attendances, ",", ""))

    dfTempTeam <- dplyr::select(dfTempTeam, section, matchday_org, kickofftime, home, score, away, stadium, attendances, broadcast)
    return(dfTempTeam)
  },server=FALSE,
    rownames=FALSE, 
    escape=FALSE,
    colnames=c("節","試合日", "kickoff", "home","score","away","stadium","入場者数","放送"),
    extensions=c("FixedColumns"),
    filter='top',
    options=list(pageLength = 10,
                 searchHighlight = TRUE,
                 #ordering=FALSE,
                 #autoWidth = TRUE, columnDefs = list(list(width = '80px', targets = c(1, 10:43)),
                 #                                     list(className = 'dt-center', targets = c(0:43))),
                 #fixedColumns = list(leftColumns = 2),
                 dom = 'lftipr', scrollX = TRUE
    )
  )
  
})

