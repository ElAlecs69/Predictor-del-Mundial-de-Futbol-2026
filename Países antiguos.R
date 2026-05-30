# ==============================
# 1) LIBRERÍAS
# ==============================
library(tidyverse)
library(janitor)
library(stringr)

# ==============================
# 2) CARGAR DATASET
# ==============================
former_names <- read.csv("former_names.csv")

# ==============================
# 3) LIMPIEZA BÁSICA
# ==============================
former_names <- former_names %>%
  clean_names() %>%
  mutate(across(where(is.character), str_trim)) %>%
  distinct()

# ==============================
# 4) VER ESTRUCTURA
# ==============================
names(former_names)



former_names <- former_names %>%
  rename(
    old_name = 1,
    new_name = 2
  )



former_names <- former_names %>%
  mutate(
    old_name = str_to_upper(old_name),
    new_name = str_to_upper(new_name)
  )



former_names <- former_names %>%
  filter(!is.na(old_name), !is.na(new_name)) %>%
  filter(old_name != "")



normalizar_equipos <- function(df, columna) {
  df %>%
    left_join(former_names, by = setNames("old_name", columna)) %>%
    mutate(
      !!columna := ifelse(!is.na(new_name), new_name, .data[[columna]])
    ) %>%
    select(-new_name)
}



write.csv(former_names, "former_names_clean.csv", row.names = FALSE)



