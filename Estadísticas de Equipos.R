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
teams_form <- read.csv("teams_form.csv")

# ==============================
# 3) LIMPIEZA BÁSICA
# ==============================
teams_form <- teams_form %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct()

# ==============================
# 4) VERIFICAR COLUMNAS
# ==============================
print(names(teams_form))
# Esperado:
# team, match_date, avg_goals_scored, avg_goals_conceded, win_rate

# ==============================
# 5) NORMALIZAR NOMBRES
# ==============================
teams_form <- teams_form %>%
  mutate(team = str_to_upper(team))

# ==============================
# 6) FECHAS
# ==============================
teams_form <- teams_form %>%
  mutate(match_date = as.Date(match_date))

# ==============================
# 7) LIMPIEZA NUMÉRICA
# ==============================
teams_form <- teams_form %>%
  mutate(across(
    c(avg_goals_scored, avg_goals_conceded, win_rate),
    as.numeric
  ))

# ==============================
# 8) FEATURES DERIVADAS
# ==============================
teams_form <- teams_form %>%
  mutate(
    # diferencia ofensiva/defensiva
    goal_balance = avg_goals_scored - avg_goals_conceded,
    
    # eficiencia ofensiva
    scoring_efficiency = avg_goals_scored * win_rate,
    
    # vulnerabilidad defensiva
    defensive_risk = avg_goals_conceded * (1 - win_rate)
  )

# ==============================
# 9) LIMPIEZA DE NA
# ==============================
teams_form <- teams_form %>%
  mutate(across(where(is.numeric), ~ replace_na(., median(., na.rm = TRUE))))

# ==============================
# 10) ORDENAR
# ==============================
teams_form <- teams_form %>%
  arrange(team, match_date)

# ==============================
# 11) EXPORTAR
# ==============================
write.csv(teams_form, "teams_form_clean.csv", row.names = FALSE)

# ==============================
# 12) VERIFICACIÓN
# ==============================
print(dim(teams_form))
print(head(teams_form))