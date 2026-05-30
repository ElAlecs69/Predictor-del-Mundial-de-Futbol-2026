# ==============================
# 1) LIBRERÍAS
# ==============================
library(tidyverse)
library(janitor)
library(stringr)

# ==============================
# 2) CARGAR DATASET
# ==============================
players <- read.csv("player_aggregates.csv")

# ==============================
# 3) LIMPIEZA BÁSICA
# ==============================
players <- players %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct()

# ==============================
# 4) VERIFICAR COLUMNAS (DEBUG)
# ==============================
print(names(players))
# Debe incluir:
# country, fifa_version, num_players, avg_overall, ...

# ==============================
# 5) NORMALIZAR NOMBRES
# ==============================
players <- players %>%
  mutate(country = str_to_upper(country))

# ==============================
# 6) ASEGURAR TIPOS NUMÉRICOS
# ==============================
players <- players %>%
  mutate(across(
    c(num_players, avg_overall, max_overall, avg_pace,
      avg_shooting, avg_passing, avg_dribbling,
      avg_defending, avg_physic,
      avg_attack_overall, avg_defense_overall),
    as.numeric
  ))

# ==============================
# 7) FEATURES DERIVADAS
# ==============================
players <- players %>%
  mutate(
    overall_strength = avg_overall,
    attack_strength = avg_attack_overall,
    defense_strength = avg_defense_overall,
    
    # balance táctico
    balance = attack_strength - defense_strength,
    
    # profundidad plantilla
    depth = max_overall - avg_overall,
    
    # calidad por jugador
    quality_per_player = avg_overall / pmax(num_players, 1)
  )

# ==============================
# 8) SELECCIÓN FINAL (SIN ERRORES)
# ==============================
players_clean <- players %>%
  select(
    country,
    fifa_version,
    num_players,
    overall_strength,
    max_overall,
    attack_strength,
    defense_strength,
    avg_pace,
    avg_shooting,
    avg_passing,
    avg_dribbling,
    avg_defending,
    avg_physic,
    balance,
    depth,
    quality_per_player
  )

# ==============================
# 9) MANEJO DE NA
# ==============================
players_clean <- players_clean %>%
  mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))))

# ==============================
# 10) EXPORTAR
# ==============================
write.csv(players_clean, "players_features.csv", row.names = FALSE)

# ==============================
# 11) VERIFICACIÓN
# ==============================
print(dim(players_clean))
print(head(players_clean))