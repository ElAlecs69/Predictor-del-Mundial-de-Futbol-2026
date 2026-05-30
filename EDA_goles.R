install.packages("moments")

library(tidyverse)
library(lubridate)

goals_features <- read.csv("goals_features.csv")

glimpse(goals_features)



ggplot(goals_features, aes(x = total_goals_real)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(
    title = "Distribución de goles por partido",
    x = "Total de goles",
    y = "Frecuencia"
  ) +
  theme_minimal()



goals_features %>%
  summarise(
    home_avg = mean(home_goals_scored),
    away_avg = mean(away_goals_scored)
  )



ggplot(goals_features, aes(x = home_goals_scored, y = away_goals_scored)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Relación goles home vs away",
    x = "Goles local",
    y = "Goles visitante"
  ) +
  theme_minimal()



ggplot(goals_features, aes(x = goal_diff_real)) +
  geom_histogram(binwidth = 1, fill = "darkgreen") +
  labs(
    title = "Distribución diferencia de goles",
    x = "Diferencia (home - away)"
  ) +
  theme_minimal()



ggplot(goals_features, aes(x = penalty_ratio_home)) +
  geom_density(fill = "red", alpha = 0.4) +
  labs(
    title = "Dependencia de penales (local)",
    x = "Proporción de goles por penal"
  )

ggplot(goals_features, aes(x = penalty_ratio_away)) +
  geom_density(fill = "blue", alpha = 0.4)



library(corrplot)

num_data <- goals_features %>%
  select(where(is.numeric))

cor_matrix <- cor(num_data)

corrplot(cor_matrix, method = "color", type = "upper")



goals_features$date <- as.Date(goals_features$date)

goals_features %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(avg_goals = mean(total_goals_real)) %>%
  ggplot(aes(x = year, y = avg_goals)) +
  geom_line() +
  labs(
    title = "Evolución del promedio de goles",
    y = "Goles promedio"
  )




extremos <- goals_features %>%
  filter(total_goals_real >= 6)

print(head(extremos))

ggplot(goals_features, aes(x = total_goals_real)) +
  geom_boxplot()



goals_features_long <- goals_features %>%
  pivot_longer(
    cols = c(home_goals_scored, away_goals_scored),
    names_to = "tipo",
    values_to = "goles"
  )

ggplot(goals_features_long, aes(x = goles, fill = tipo)) +
  geom_density(alpha = 0.5)



library(cluster)

k_data <- goals_features %>%
  select(home_goals_scored, away_goals_scored, total_goals_real)

kmeans_result <- kmeans(k_data, centers = 3)

goals_features$cluster <- kmeans_result$cluster

ggplot(goals_features, aes(x = home_goals_scored, y = away_goals_scored, color = factor(cluster))) +
  geom_point()



# ==============================
# 0) LIBRERÍAS
# ==============================
library(moments)   # skewness, kurtosis
library(corrplot)

# ==============================
# 1) FUNCIÓN MODA
# ==============================
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ==============================
# 2) FUNCIÓN EDA AVANZADO
# ==============================
eda_avanzado <- function(df, nombre = "dataset") {
  
  cat("=====================================\n")
  cat("EDA AVANZADO:", nombre, "\n")
  cat("=====================================\n\n")
  
  # ==============================
  # NUMÉRICAS
  # ==============================
  num_df <- df %>% select(where(is.numeric))
  
  # ==============================
  # ESTADÍSTICAS DESCRIPTIVAS
  # ==============================
  resumen <- num_df %>%
    summarise(across(everything(), list(
      media = ~mean(., na.rm = TRUE),
      mediana = ~median(., na.rm = TRUE),
      varianza = ~var(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      minimo = ~min(., na.rm = TRUE),
      maximo = ~max(., na.rm = TRUE),
      skewness = ~skewness(., na.rm = TRUE),
      kurtosis = ~kurtosis(., na.rm = TRUE)
    )))
  
  print("Resumen estadístico:")
  print(resumen)
  
  # ==============================
  # MODA (por columna)
  # ==============================
  modas <- sapply(num_df, moda)
  
  cat("\nModa por variable:\n")
  print(modas)
  
  # ==============================
  # MATRIZ DE COVARIANZA
  # ==============================
  cov_matrix <- cov(num_df, use = "complete.obs")
  
  cat("\nMatriz de covarianza:\n")
  print(cov_matrix)
  
  # ==============================
  # MATRIZ DE CORRELACIÓN
  # ==============================
  cor_matrix <- cor(num_df, use = "complete.obs")
  
  cat("\nMatriz de correlación:\n")
  print(cor_matrix)
  
  corrplot(cor_matrix, method = "color", type = "upper")
  
  # ==============================
  # OUTLIERS (IQR)
  # ==============================
  outliers_iqr <- num_df %>%
    summarise(across(everything(), ~{
      Q1 <- quantile(., 0.25, na.rm = TRUE)
      Q3 <- quantile(., 0.75, na.rm = TRUE)
      IQR_val <- Q3 - Q1
      sum(. < (Q1 - 1.5 * IQR_val) | . > (Q3 + 1.5 * IQR_val), na.rm = TRUE)
    }))
  
  cat("\nNúmero de outliers por variable (IQR):\n")
  print(outliers_iqr)
  
  # ==============================
  # VISUALIZACIONES
  # ==============================
  
  # Histogramas
  num_df %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30, fill = "steelblue") +
    facet_wrap(~name, scales = "free") +
    theme_minimal() +
    labs(title = paste("Histogramas -"))
  
  # Boxplots
  num_df %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = name, y = value)) +
    geom_boxplot(fill = "orange") +
    coord_flip() +
    theme_minimal() +
    labs(title = paste("Boxplots -"))
  
  # Pairplot
  pairs(num_df)
  
  cat("\nEDA completado.\n")
}