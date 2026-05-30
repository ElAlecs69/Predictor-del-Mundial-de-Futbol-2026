# ==============================
# 1) LIBRERÍAS
# ==============================
library(tidyverse)
library(stringr)

former_names <- read.csv("former_names_clean.csv")

# ==============================
# 2) VISIÓN GENERAL
# ==============================
glimpse(former_names)

n_total <- nrow(former_names)
n_unique_old <- n_distinct(former_names$old_name)
n_unique_new <- n_distinct(former_names$new_name)

cat("Total mappings:", n_total, "\n")
cat("Unique old names:", n_unique_old, "\n")
cat("Unique new names:", n_unique_new, "\n")


# ¿Un old_name apunta a varios new_name?
ambiguos <- former_names %>%
  group_by(old_name) %>%
  summarise(n = n_distinct(new_name)) %>%
  filter(n > 1)

print(ambiguos)



consolidacion <- former_names %>%
  count(new_name) %>%
  arrange(desc(n))

head(consolidacion, 10)


ggplot(consolidacion[1:15,], aes(x = reorder(new_name, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Consolidación histórica de selecciones",
    x = "Selección actual",
    y = "Número de nombres históricos"
  ) +
  theme_minimal()



former_names <- former_names %>%
  mutate(
    old_length = str_length(old_name),
    new_length = str_length(new_name)
  )

ggplot(former_names, aes(x = old_length, y = new_length)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Complejidad de transformación de nombres",
    x = "Longitud nombre antiguo",
    y = "Longitud nombre nuevo"
  ) +
  theme_minimal()



library(igraph)

edges <- former_names %>%
  select(old_name, new_name)

g <- graph_from_data_frame(edges)

plot(g, vertex.size = 5, vertex.label.cex = 0.6)



identicos <- former_names %>%
  filter(old_name == new_name)

print(identicos)



# antes de normalizar
unique_results <- unique(results$home_team)

# después
results_norm <- normalizar_equipos(results, "home_team")

reduccion <- length(unique(results$home_team)) - length(unique(results_norm$home_team))

cat("Reducción de categorías:", reduccion)



before_after <- data.frame(
  tipo = c("Antes", "Después"),
  valores = c(
    length(unique(results$home_team)),
    length(unique(results_norm$home_team))
  )
)

ggplot(before_after, aes(x = tipo, y = valores, fill = tipo)) +
  geom_col() +
  labs(
    title = "Reducción de cardinalidad de equipos",
    y = "Número de categorías"
  ) +
  theme_minimal()



