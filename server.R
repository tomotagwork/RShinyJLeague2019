# server.R
source("server_env.R", local=TRUE)

### shinyServer ###################################################################
shinyServer(function(input, output, session){
  
  #makeReactiveBinding("listUploadFiles_Menu01")
  
  # get data
  targetUrl_game<-"http://data.j-league.or.jp/SFMS01/search?competition_years=2019&competition_frame_ids=1&tv_relay_station_name="
  dfTableOriginal_game <- readHTMLTable(targetUrl_game, header = FALSE, which=1, stringsAsFactors = FALSE)
  
  # create master table
  dfTableMaster <- dfTableOriginal_game
  colnames(dfTableMaster) <- c("year","competition","section","matchday_org","kickofftime","home","score","away","stadium","attendances","broadcast")
  
  # add section_num column
  dfTableMaster$section_num <- stringr::str_sub(dfTableMaster$section, 1,-4)
  #dfTableMaster$section_num <- stringi::stri_trans_nfkc(stringr::str_sub(dfTableMaster$section_num, 2,-2))
  
  # add matchday_format column
  dfTableMaster$matchday <- lubridate::parse_date_time2(paste0(dfTableMaster$year,"/",stringr::str_sub(dfTableMaster$matchday_org,1,5)), "%Y/%m/%d", tz="Asia/Tokyo")
  
  # add home_score / away_score column
  dfTableMaster$home_score <- as.numeric(stringr::str_split(dfTableMaster$score, "-", simplify=TRUE)[,1])
  dfTableMaster$away_score <- as.numeric(stringr::str_split(dfTableMaster$score, "-", simplify=TRUE)[,2])
  
  # add score_diff column
  dfTableMaster$score_diff <- dfTableMaster$home_score - dfTableMaster$away_score
  
  # add home_point / away_point column
  dfTableMaster$home_point <- ifelse(dfTableMaster$score_diff > 0, 3, (ifelse(dfTableMaster$score_diff == 0, 1, 0)))
  dfTableMaster$away_point <- ifelse(dfTableMaster$score_diff < 0, 3, (ifelse(dfTableMaster$score_diff == 0, 1, 0)))
  
  # add Link info
  data <- read_html(targetUrl_game)
  strURLPrefix <- "https://data.j-league.or.jp"
  listMatchLink <- paste0(strURLPrefix, 
                          html_nodes(data,".al-c") %>% html_nodes("a") %>% html_attr("href"))
  listMatchLink <- c(listMatchLink, rep("", nrow(dfTableMaster)-length(listMatchLink)))
  dfTableMaster$match_link <-listMatchLink
  
  # By team
  
  listTeamData <- list()
  dfTeamData <- data.frame()
  
  for (i in 1:length(teamList)) {
    #print(teamList[i])
    targetTeam<-teamList[i]
    # Home game
    dfTempTeam_home <- dplyr::filter(dfTableMaster, home==targetTeam)
    dfTempTeam_home$target_team <- targetTeam
    dfTempTeam_home$target_team_HA <- "Home"
    dfTempTeam_home$target_team_Opponent <- dfTempTeam_home$away
    dfTempTeam_home$target_team_Result <- ifelse(dfTempTeam_home$home_point == 3, strWin, (ifelse(dfTempTeam_home$home_point == 1, strDraw, strLose)))
    dfTempTeam_home$target_team_point <- dfTempTeam_home$home_point
    dfTempTeam_home$target_team_goalfor <- dfTempTeam_home$home_score
    dfTempTeam_home$target_team_goalagainst <- dfTempTeam_home$away_score
    dfTempTeam_home$target_team_matchdetail <- 
      ifelse(is.na(dfTempTeam_home$home_score), 
             paste0(dfTempTeam_home$target_team_Opponent,"(H)_", dfTempTeam_home$matchday),
             paste0(dfTempTeam_home$target_team_Opponent,"(H)_", dfTempTeam_home$home_score, dfTempTeam_home$target_team_Result, dfTempTeam_home$away_score))
    
    # Away game
    dfTempTeam_away <- dplyr::filter(dfTableMaster, away==targetTeam)
    dfTempTeam_away$target_team <- targetTeam
    dfTempTeam_away$target_team_HA <- "Away"
    dfTempTeam_away$target_team_Opponent <- dfTempTeam_away$home
    dfTempTeam_away$target_team_Result <- ifelse(dfTempTeam_away$away_point == 3, strWin, (ifelse(dfTempTeam_away$away_point == 1, strDraw, strLose)))
    dfTempTeam_away$target_team_point <- dfTempTeam_away$away_point
    dfTempTeam_away$target_team_goalfor <- dfTempTeam_away$away_score
    dfTempTeam_away$target_team_goalagainst <- dfTempTeam_away$home_score
    dfTempTeam_away$target_team_matchdetail <- 
      ifelse(is.na(dfTempTeam_away$home_score), 
             paste0(dfTempTeam_away$target_team_Opponent,"(A)_", dfTempTeam_away$matchday),
             paste0(dfTempTeam_away$target_team_Opponent,"(A)_", dfTempTeam_away$home_score, dfTempTeam_away$target_team_Result, dfTempTeam_away$away_score))
    
    # Merger Home and Away games
    dfTempTeam <- rbind(dfTempTeam_home, dfTempTeam_away) %>% dplyr::arrange(matchday)
    
    # Add cumsum colum 
    dfTempTeam$target_team_cumpoint <- cumsum(dfTempTeam$target_team_point)
    dfTempTeam$target_team_cumgoalfor <- cumsum(dfTempTeam$target_team_goalfor)
    dfTempTeam$target_team_cumgoalagainst <- cumsum(dfTempTeam$target_team_goalagainst)
    dfTempTeam$target_team_cumgoaldiff <- dfTempTeam$target_team_cumgoalfor - dfTempTeam$target_team_cumgoalagainst
    
    # Add matchday + section column for display
    # dfTempTeam$target_team_matchday_disp <- paste0(dfTempTeam$matchday_org, " ",dfTempTeam$section_num)
    
    # Add to dfTeamData
    dfTeamData <- rbind(dfTeamData, dfTempTeam)
    
    # Add to listTeamData
    listTeamData[[targetTeam]] <- dfTempTeam
    
  }
  
  
  # create data table for standings
  dfTeamStandings <- plyr::ddply(dfTeamData, .(target_team), summarize, 
                                 point=sum(target_team_point, na.rm=TRUE), 
                                 goalfor=sum(target_team_goalfor, na.rm=TRUE),
                                 goalagainst=sum(target_team_goalagainst, na.rm=TRUE))
  dfTeamStandings$goaldiff <- dfTeamStandings$goalfor - dfTeamStandings$goalagainst
  
  
  dfWinCount <- dfTeamData %>% dplyr::group_by(target_team, target_team_Result) %>% dplyr::summarise(count=n()) %>% dplyr::filter(!is.na(target_team_Result))
  dfWinCount <- dcast(dfWinCount, target_team ~ target_team_Result, value.var="count")
  colnames(dfWinCount) <- c("target_team", "draw", "lose", "win")
  dfWinCount$num_of_matches <- dfWinCount$win + dfWinCount$draw + dfWinCount$lose
  
  dfTeamStandings <- dplyr::left_join(dfTeamStandings, dfWinCount, by="target_team")
  
  dfTeamStandings <- dplyr::arrange(dfTeamStandings, desc(point), desc(goaldiff), desc(goalfor))
  dfTeamStandings$ranking <- c(1:18)
  
  dfTeamStandings <- dplyr::select(dfTeamStandings, ranking, target_team, point, num_of_matches, win, draw, lose, goalfor, goalagainst, goaldiff)
  
  # get team order list
  teamOrder <- dfTeamStandings$target_team
  
  
  source("server_Point.R", local=TRUE)
  source("server_ResultAll.R", local=TRUE)
  source("server_ResultTeam.R", local=TRUE)
  
})