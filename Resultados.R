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
results <- read.csv("results.csv")

# ==============================
# 3) LIMPIEZA BÁSICA
# ==============================
results <- results %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct()

# ==============================
# 4) NORMALIZAR NOMBRES
# ==============================
results <- results %>%
  mutate(
    home_team = str_to_upper(home_team),
    away_team = str_to_upper(away_team),
    tournament = str_to_upper(tournament),
    country = str_to_upper(country),
    city = str_to_upper(city)
  )

# ==============================
# 5) FECHAS
# ==============================
results <- results %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    day = day(date)
  )

# ==============================
# 6) VARIABLES BASE (CLAVE)
# ==============================
results <- results %>%
  mutate(
    goal_diff = home_score - away_score,
    total_goals = home_score + away_score,
    
    # variable objetivo (ML)
    result = case_when(
      home_score > away_score ~ 1,
      home_score < away_score ~ -1,
      TRUE ~ 0
    )
  )

# ==============================
# 7) VARIABLES CONTEXTUALES
# ==============================
results <- results %>%
  mutate(
    is_neutral = ifelse(neutral == TRUE, 1, 0),
    
    # localía
    home_advantage = ifelse(neutral == FALSE, 1, 0)
  )

# ==============================
# 8) TIPO DE TORNEO
# ==============================
results <- results %>%
  mutate(
    is_world_cup = ifelse(str_detect(tournament, "WORLD CUP"), 1, 0),
    is_friendly = ifelse(str_detect(tournament, "FRIENDLY"), 1, 0),
    is_qualifier = ifelse(str_detect(tournament, "QUALIFYING"), 1, 0)
  )

# ==============================
# 9) FILTRADO (OPCIONAL)
# ==============================
# quitar partidos muy antiguos si quieres
#results <- results %>%
#  filter(year >= 2000)

# ==============================
# 10) FEATURES AVANZADAS
# ==============================
results <- results %>%
  mutate(
    high_scoring = ifelse(total_goals >= 3, 1, 0),
    close_match = ifelse(abs(goal_diff) <= 1, 1, 0)
  )

# ==============================
# 11) ORDENAR
# ==============================
results <- results %>%
  arrange(date)

# ==============================
# 12) EXPORTAR
# ==============================
write.csv(results, "results_clean.csv", row.names = FALSE)

# ==============================
# 13) VERIFICACIÓN
# ==============================
print(dim(results))
print(head(results))