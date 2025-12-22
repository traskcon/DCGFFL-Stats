library(tidyverse)
library(stringr)

# Function Definitions ---------------------------------------------------------

set.coltypes <- function(df,types) {
  for (i in 1:length(df)){
    FUN <- switch(types[i],character = as.character,
                          integer = as.integer,
                          numeric = as.numeric)
    df[,i] <- FUN(df[,i])
  }
  df
}

# 1. Read Data and Generate Stats ----------------------------------------------

pbp_data <- read_csv("DCGFFL_Plays.csv")

stat_list <- c("passer","receiver","rusher","tackler","passRush1","blocker1",
               "passRush2","blocker2","passRush3","blocker3")

players <- unique(as.vector(as.matrix(pbp_data[,stat_list])))
players <- players[!sapply(players, is.na)]
stats <- c("playerName","games","passAtt","passComp","passYard","passTD","passXP",
          "passINT","recTarget","recR","recYard","recTD","recXP","rushAtt","rushYard",
          "rushTD","rushXP","prPressure","prSnaps","prRate","prBPR","prUPR","prSack",
          "ppPressure","ppSnaps","ppPRA","ppSack","defTackle","defINT",
          "penaltyCount","penaltyYard")
stat_types <- c("character","integer","integer","integer","integer","integer",
                "integer","integer","integer","integer","integer","integer",
                "integer","integer","integer","integer","integer","numeric",
                "integer","numeric","numeric","numeric","integer","numeric",
                "integer","numeric","integer","integer","integer","integer","integer")

player_stats <- data.frame(matrix(nrow=0,ncol=length(stats)))
colnames(player_stats) <- stats
player_stats <- set.coltypes(player_stats, stat_types)

for (player in players) {
  # Filter data down to plays specific player was involved in
  player_data <- pbp_data |>
    filter(if_any(all_of(stat_list), ~ . == player))
  # Extract player stats from player plays
  pGames <- length(unique(player_data$gameId))
  # Passing Stats
  list2env(as.list(filter(player_data, passer == player) |>
    summarise(pPassAtt = n(), pComp = sum(reception, na.rm=T), 
              pPassYard = sum(yardsGained, na.rm=T),
              pPassTD = sum(str_detect(playDescription,regex("^(?=.*TOUCHDOWN)(?!.*INTERCEPTED)."))),
              pPassXP = sum(str_detect(playDescription,"GOOD")),
              pPassINT = sum(str_detect(playDescription,"INTERCEPTED")))), 
    envir=.GlobalEnv)
  # Receiving Stats
  list2env(as.list(filter(player_data, receiver == player) |>
    summarise(pTargets = n(), pRec = sum(reception, na.rm=T), 
              pRecYard = sum(yardsGained, na.rm=T),
              pRecTD = sum(str_detect(playDescription,regex("^(?=.*TOUCHDOWN)(?!.*INTERCEPTED)."))),
              pRecXP = sum(str_detect(playDescription,"GOOD")))),
    envir = .GlobalEnv)
  # Rushing Stats  
  list2env(as.list(filter(player_data, rusher == player) |>
    summarise(pCarries = n(), pRushYard = sum(yardsGained, na.rm=T),
              pRushTD = sum(str_detect(playDescription,"TOUCHDOWN")),
              pRushXP = sum(str_detect(playDescription,"GOOD")))),
    envir = .GlobalEnv)
  # Reshape data for pass rush and pass protection
  
  trench_data <- reshape(player_data, direction="long",idvar=c("gameId","playId"),
                         varying=list(c("passRush1","passRush2","passRush3"),
                                      c("blocker1","blocker2","blocker3"),
                                      c("pressure1","pressure2","pressure3"),
                                      c("sack1","sack2","sack3")),
                         v.names=c("passRush","blocker","pressure","sack"), timevar="matchup")
  # Pass rush stats
  list2env(as.list(filter(trench_data, passRush == player) |>
    summarise(pPRSnaps = n(), pPressures = sum(pressure, na.rm=T), pSacks = sum(sack, na.rm=T),
              pPressureRate = pPressures/pPRSnaps,
              pBPR = mean(pressure[!is.na(blocker)]),
              pUPR = mean(pressure[is.na(blocker)]))),
    envir = .GlobalEnv)
  # Pass protection stats
  list2env(as.list(filter(trench_data, blocker == player) |>
    summarise(pPPSnaps = n(), pPA = sum(pressure, na.rm=T), pSA = sum(sack, na.rm=T),
              pPRA = pPA/pPPSnaps)),
    envir = .GlobalEnv)
  # Misc Stats (Defense & Penalties)
  pTackles <- nrow(filter(player_data, tackler == player))
  pDefINT <- nrow(filter(player_data, str_detect(playDescription,
                                                 paste0("INTERCEPTED by ",player))))
  list2env(as.list(filter(player_data, str_detect(playDescription,
                                          regex(paste0("PENALTY.*",player)))) |>
    summarise(pPenalties = n(), pPenYard = sum(abs(penaltyYards), na.rm=T))),
    envir = .GlobalEnv)
  # Add player stats to dataframe
  player_stats <- add_row(player_stats, playerName = player,games = pGames, 
                          passAtt = pPassAtt, passComp = pComp, passYard = pPassYard,
                          passTD = pPassTD, passXP = pPassXP, passINT = pPassINT,
                          recTarget = pTargets, recR = pRec, recYard = pRecYard,
                          recTD = pRecTD, recXP = pRecXP, rushAtt = pCarries,
                          rushYard = pRushYard, rushTD = pRushTD, rushXP = pRushXP,
                          prPressure = pPressures, prSnaps = pPRSnaps, 
                          prRate = pPressureRate, prBPR = pBPR, prUPR = pUPR,
                          prSack = pSacks, ppPressure = pPA, ppSnaps = pPPSnaps,
                          ppPRA = pPRA, ppSack = pSA, defTackle = pTackles,
                          defINT = pDefINT, penaltyCount = pPenalties,
                          penaltyYard = pPenYard)
  }

# Calculate Stats for Radar plot
player_stats <- player_stats |>
  mutate(explosiveness = (rushYard + recYard)/(rushAtt + recR),
         possession = recR/recTarget)
