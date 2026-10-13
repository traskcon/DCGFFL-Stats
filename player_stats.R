suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# ---- Config ------------------------------------------------------------------
PLAYS_PATH          <- "./data/DCGFFL_F26_plays.csv"
OUT_DIR             <- "."
INCLUDE_CONVERSIONS <- TRUE  # TRUE = count extra-point / two-point plays in player stats
NAME_FIXES          <- c()  # For ailiasing players (Ex. same first initial.Lastname on different teams)

STAT_COLS <- c(
  "pass_att", "pass_cmp", "pass_yds", "pass_td", "pass_int",
  "targets", "receptions", "rec_yds", "rec_td",
  "rush_att", "rush_yds", "rush_td",
  "tackles", "def_int", "def_sacks"
)

# ---- Load and clean ----------------------------------------------------------
plays <- read_csv(
  PLAYS_PATH,
  col_types = cols(
    .default    = col_character(),
    yardsGained = col_double(),
    reception   = col_double()
  ),
  na = c("", "NA"),
  show_col_types = FALSE
)

clean_name <- function(x) {
  x <- na_if(str_squish(x), "")
  coalesce(unname(NAME_FIXES[x]), x)
}

plays <- plays |>
  mutate(
    across(c(passer, receiver, rusher, tackler), clean_name),
    desc          = playDescription,
    is_conversion = str_detect(desc, "^\\((Extra Point|Two Point Attempt)\\)")
  )

if (!INCLUDE_CONVERSIONS) plays <- filter(plays, !is_conversion)

# ---- Play-level flags --------------------------------------------------------
plays <- plays |>
  mutate(
    intercepted = str_detect(desc, fixed("INTERCEPTED")),
    sacked      = str_detect(desc, regex("sacked", ignore_case = TRUE)),
    # An accepted (not declined) penalty. When one nullifies the play, the log
    # leaves the result field blank: reception for passes, yardsGained for sacks.
    live_pen    = str_detect(desc, "PENALT(Y|IES)") & !str_detect(desc, regex("declined", ignore_case = TRUE)),
    
    # An accepted penalty that wipes out a pick is logged with PENALTY + blank reception
    # Additionally, interceptions only count on conversions if they are returned for 2-points
    is_int      = intercepted & !(live_pen & is.na(reception))
                              & !(str_detect(desc, "^\\((Extra Point|Two Point Attempt)\\)") & !str_detect(desc, fixed("TOUCHDOWN"))),
    is_sack     = sacked & !(live_pen & is.na(yardsGained)),
    completion  = reception %in% 1,
    
    # Defender credited with the pick: the name right after "INTERCEPTED by"
    int_by      = clean_name(str_match(desc, "INTERCEPTED by ([A-Za-z]+\\.[A-Za-z'-]+)")[, 2]),
    # Pick-sixes say TOUCHDOWN but belong to the defense
    touchdown   = str_detect(desc, fixed("TOUCHDOWN")) & !intercepted,
    extra_point = str_detect(desc, fixed("POINT GOOD")),
    
    # Completions with blank yardsGained: recover yardage from the text
    parsed_yds   = as.numeric(str_match(desc, "for (-?\\d+) yards?")[, 2]),
    yards        = if_else(is.na(yardsGained) & completion, parsed_yds, yardsGained),
    
    # Catch-and-lateral: pass/receiver get yardage stated before "LATERAL",
    # the remainder goes to the rusher (lateral recipient)
    lateral_pass = playType %in% "pass" & completion & !is.na(receiver) & !is.na(rusher),
    pre_lateral  = str_sub(desc, 1, str_locate(desc, "LATERAL")[, 1] - 1),
    pass_part    = coalesce(as.numeric(str_match(pre_lateral, "for (-?\\d+) yards?")[, 2]), 0),
    rec_yds      = if_else(lateral_pass, pass_part, yards),
    rush_yds     = if_else(lateral_pass, yards - pass_part, yards)
  )

# ---- Stat events by role -----------------------------------------------------
passing <- plays |>
  filter(!is.na(passer), !is.na(reception)) |>
  transmute(
    gameId, player = passer, team = possessionTeam,
    pass_att = 1L,
    pass_cmp = as.integer(completion),
    pass_yds = if_else(completion, rec_yds, 0),
    pass_td  = as.integer(completion & touchdown),
    pass_xp  = as.integer(completion & extra_point),
    pass_pts = if_else(completion, points, 0),
    pass_int = as.integer(is_int)
  )

receiving <- plays |>
  filter(!is.na(receiver), !is.na(reception)) |>
  transmute(
    gameId, player = receiver, team = possessionTeam,
    targets    = 1L,
    receptions = as.integer(completion),
    rec_yds    = if_else(completion, rec_yds, 0),
    rec_td     = as.integer(completion & touchdown & !lateral_pass)
  )

rushing <- plays |>
  filter(!is.na(rusher), !is.na(yards)) |>   # blank yardsGained = carry erased by a penalty
  transmute(
    gameId, player = rusher, team = possessionTeam,
    rush_att = 1L,
    rush_yds = rush_yds,
    rush_td  = as.integer(touchdown & (playType %in% "run" | lateral_pass))
  )

# On interceptions and punt returns the tackler is on the possession team
tackles <- plays |>
  filter(!is.na(tackler)) |>
  transmute(
    gameId, player = tackler,
    team = if_else(intercepted | playType %in% "punt", possessionTeam, defensiveTeam),
    tackles = 1L
  )

# Defensive interceptions (valid picks only)
def_ints <- plays %>%
  filter(is_int, !is.na(int_by)) %>%
  transmute(gameId, player = int_by, team = defensiveTeam, def_int = 1L)

# Sacks: the tackler on a non-nullified "sacked" play
def_sacks <- plays %>%
  filter(is_sack, !is.na(tackler)) %>%
  transmute(gameId, player = tackler, team = defensiveTeam, def_sacks = 1L)

events <- bind_rows(passing, receiving, rushing, tackles, def_ints, def_sacks)

# ---- Team label (stats are keyed on player name) -----------------------------
player_team <- events |>
  count(player, team, name = "n") |>
  group_by(player) |>
  summarise(n_teams = n(), team = team[which.max(n)], .groups = "drop")

multi_team <- filter(player_team, n_teams > 1)
if (nrow(multi_team) > 0) {
  warning("Players matched to more than one team (name collision or data error): ",
          paste(multi_team$player, collapse = ", "), call. = FALSE)
}

# ---- Aggregate ---------------------------------------------------------------
game_stats <- events |>
  group_by(gameId, player) |>
  summarise(across(all_of(STAT_COLS), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  left_join(select(player_team, player, team), by = "player") |>
  select(gameId, player, team, all_of(STAT_COLS)) |>
  arrange(gameId, team, player)

season_stats <- game_stats |>
  group_by(player, team) |>
  summarise(games = n_distinct(gameId), across(all_of(STAT_COLS), sum), .groups = "drop") |>
  arrange(team, player)

# ---- Sanity check and output -------------------------------------------------
cmp <- sum(game_stats$pass_cmp)
rec <- sum(game_stats$receptions)
if (cmp != rec) warning("Completions (", cmp, ") != receptions (", rec, ")", call. = FALSE)

thrown <- sum(game_stats$pass_int)
caught <- sum(game_stats$def_int)
if (thrown != caught) {
  warning("Interceptions thrown (", thrown, ") != interceptions credited to defenders (",
          caught, ")", call. = FALSE)
}

no_sacker <- filter(plays, is_sack, is.na(tackler))
if (nrow(no_sacker) > 0) {
  warning("Sacks with no tackler listed (no credit given): ",
          paste0(no_sacker$gameId, " #", no_sacker$playId, collapse = ", "), call. = FALSE)
}

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write_csv(game_stats,   file.path(OUT_DIR, "DCGFFL_F26_player_game_stats.csv"))
write_csv(season_stats, file.path(OUT_DIR, "DCGFFL_F26_player_season_stats.csv"))

print(season_stats, n = 10)