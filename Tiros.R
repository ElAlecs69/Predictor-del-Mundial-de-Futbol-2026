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
shootouts <- read.csv("shootouts.csv")

# ==============================
# 3) LIMPIEZA BÁSICA
# ==============================
shootouts <- shootouts %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct()

# ==============================
# 4) NORMALIZAR NOMBRES
# ==============================
shootouts <- shootouts %>%
  mutate(
    home_team = str_to_upper(home_team),
    away_team = str_to_upper(away_team),
    winner = str_to_upper(winner)
  )

# ==============================
# 5) FECHAS
# ==============================
shootouts <- shootouts %>%
  mutate(date = as.Date(date))

# ==============================
# 6) VARIABLES CLAVE
# ==============================
shootouts <- shootouts %>%
  mutate(
    # siempre hubo penales si está en este dataset
    is_shootout = 1,
    
    # quién ganó
    home_win_pen = ifelse(winner == home_team, 1, 0),
    away_win_pen = ifelse(winner == away_team, 1, 0)
  )

# ==============================
# 7) LIMPIEZA DE first_shooter
# ==============================
shootouts <- shootouts %>%
  mutate(first_shooter = str_to_upper(first_shooter))

# ==============================
# 8) VARIABLES ADICIONALES
# ==============================
shootouts <- shootouts %>%
  mutate(
    first_shooter_home = ifelse(first_shooter == home_team, 1, 0),
    first_shooter_away = ifelse(first_shooter == away_team, 1, 0)
  )

# ==============================
# 9) SELECCIÓN FINAL
# ==============================
shootouts_clean <- shootouts %>%
  select(
    date,
    home_team,
    away_team,
    is_shootout,
    home_win_pen,
    away_win_pen,
    first_shooter_home,
    first_shooter_away
  )

# ==============================
# 10) EXPORTAR
# ==============================
write.csv(shootouts_clean, "shootouts_features.csv", row.names = FALSE)

# ==============================
# 11) VERIFICACIÓN
# ==============================
print(dim(shootouts_clean))
print(head(shootouts_clean))