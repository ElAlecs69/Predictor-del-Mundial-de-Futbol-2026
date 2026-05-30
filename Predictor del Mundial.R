# ==============================
# 1) LIBRERÍAS
# ==============================
install.packages(c("tidyverse", "janitor", "lubridate"))

library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)

# ==============================
# 2) CARGA DE DATOS
# ==============================
setwd("D:/Descargas/Programas en R")

results <- read.csv("results.csv")
goals <- read.csv("goalscorers.csv")
shootouts <- read.csv("shootouts.csv")
teams_form <- read.csv("teams_form.csv")
teams_features <- read.csv("teams_match_features.csv")
players <- read.csv("player_aggregates.csv")

# ==============================
# 3) LIMPIEZA GENERAL
# ==============================
limpiar <- function(df){
  df %>%
    clean_names() %>%
    mutate(across(where(is.character), str_trim)) %>%
    filter(if_any(everything(), ~ !is.na(.))) %>%
    distinct()
}

results <- limpiar(results)
teams_form <- limpiar(teams_form)
teams_features <- limpiar(teams_features)
players <- limpiar(players)

# ==============================
# 4) NORMALIZAR NOMBRES
# ==============================
teams_features <- teams_features %>%
  rename(
    home_team = X_home_team,
    away_team = X_away_team,
    date = X_date
  )

results <- results %>%
  mutate(
    home_team = str_to_upper(home_team),
    away_team = str_to_upper(away_team)
  )

teams_form <- teams_form %>%
  mutate(team = str_to_upper(team))

players <- players %>%
  mutate(country = str_to_upper(country))

# ==============================
# 5) FECHAS
# ==============================
teams_features$date <- as.Date(teams_features$date)

results$date <- as.Date(results$date)
teams_form$match_date <- as.Date(teams_form$match_date)

# ==============================
# 6) FEATURES BASE
# ==============================
results <- results %>%
  mutate(
    goal_diff = home_score - away_score,
    total_goals = home_score + away_score,
    result = case_when(
      home_score > away_score ~ 1,
      home_score < away_score ~ -1,
      TRUE ~ 0
    )
  )

# ==============================
# 7) JOIN FORM ()
# ==============================
dataset <- dataset %>%
  left_join(
    teams_features,
    by = c("home_team", "away_team", "date")
  )
# ==============================
# 7) JOIN FORM (HOME)
# ==============================
dataset <- results %>%
  left_join(
    teams_form,
    by = c("home_team" = "team", "date" = "match_date")
  ) %>%
  rename(
    home_avg_goals_scored = avg_goals_scored,
    home_avg_goals_conceded = avg_goals_conceded,
    home_win_rate = win_rate
  )

# ==============================
# 8) JOIN FORM (AWAY)
# ==============================
dataset <- dataset %>%
  left_join(
    teams_form,
    by = c("away_team" = "team", "date" = "match_date")
  ) %>%
  rename(
    away_avg_goals_scored = avg_goals_scored,
    away_avg_goals_conceded = avg_goals_conceded,
    away_win_rate = win_rate
  )

# ==============================
# 9) JOIN PLAYERS (HOME)
# ==============================
dataset <- dataset %>%
  left_join(players, by = c("home_team" = "country")) %>%
  rename(
    home_avg_overall_players = avg_overall,
    home_max_overall_players = max_overall
  )

# ==============================
# 10) JOIN PLAYERS (AWAY)
# ==============================
dataset <- dataset %>%
  left_join(players, by = c("away_team" = "country")) %>%
  rename(
    away_avg_overall_players = avg_overall,
    away_max_overall_players = max_overall
  )

# ==============================
# 11) FEATURES AVANZADAS
# ==============================
# (ya vienen alineadas por partido)
dataset <- bind_cols(dataset, teams_features)

# ==============================
# 12) LIMPIEZA FINAL
# ==============================
dataset <- dataset %>%
  mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))))

# ==============================
# 13) ELIMINAR DUPLICADOS
# ==============================
dataset <- dataset %>%
  distinct()

# ==============================
# 14) EXPORTAR DATASET FINAL
# ==============================
write.csv(dataset, "dataset_modelo.csv", row.names = FALSE)

# ==============================
# 15) VERIFICACIÓN
# ==============================
print(dim(dataset))
print(head(dataset))