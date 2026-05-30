# ==============================
# 1) LIBRERÍAS
# ==============================
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)

# ==============================
# 2) CARGAR DATASET
# ==============================
goals <- read.csv("goalscorers.csv")

# ==============================
# 3) LIMPIEZA BÁSICA
# ==============================
goals <- goals %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct()

# ==============================
# 4) NORMALIZAR TEXTO
# ==============================
goals <- goals %>%
  mutate(
    home_team = str_to_upper(home_team),
    away_team = str_to_upper(away_team),
    team = str_to_upper(team),
    scorer = str_to_upper(scorer)
  )

# ==============================
# 5) FECHAS
# ==============================
goals$date <- as.Date(goals$date)

# ==============================
# 6) LIMPIEZA DE VARIABLES
# ==============================

# minuto como número
goals <- goals %>%
  mutate(minute = as.numeric(minute))

# own_goal y penalty a binario
goals <- goals %>%
  mutate(
    own_goal = ifelse(own_goal == TRUE | own_goal == "TRUE", 1, 0),
    penalty = ifelse(penalty == TRUE | penalty == "TRUE", 1, 0)
  )

# ==============================
# 7) FEATURES POR PARTIDO
# ==============================

# goles por equipo en cada partido
goals_match <- goals %>%
  group_by(date, home_team, away_team, team) %>%
  summarise(
    goals_scored = n(),
    penalties = sum(penalty),
    own_goals = sum(own_goal),
    .groups = "drop"
  )

# ==============================
# 8) SEPARAR HOME / AWAY
# ==============================

home_goals <- goals_match %>%
  filter(team == home_team) %>%
  rename(
    home_goals_scored = goals_scored,
    home_penalties = penalties,
    home_own_goals = own_goals
  ) %>%
  select(-team)

away_goals <- goals_match %>%
  filter(team == away_team) %>%
  rename(
    away_goals_scored = goals_scored,
    away_penalties = penalties,
    away_own_goals = own_goals
  ) %>%
  select(-team)

# ==============================
# 9) UNIR HOME Y AWAY
# ==============================
goals_features <- full_join(
  home_goals,
  away_goals,
  by = c("date", "home_team", "away_team")
)

# ==============================
# 10) MANEJO DE NA
# ==============================
goals_features <- goals_features %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# ==============================
# 11) FEATURES AVANZADAS
# ==============================

goals_features <- goals_features %>%
  mutate(
    goal_diff_real = home_goals_scored - away_goals_scored,
    total_goals_real = home_goals_scored + away_goals_scored,
    penalty_ratio_home = home_penalties / pmax(home_goals_scored, 1),
    penalty_ratio_away = away_penalties / pmax(away_goals_scored, 1)
  )

# ==============================
# 12) EXPORTAR
# ==============================
write.csv(goals_features, "goals_features.csv", row.names = FALSE)

# ==============================
# 13) VERIFICACIÓN
# ==============================
print(dim(goals_features))
print(head(goals_features))