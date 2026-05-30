library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)

players <- read.csv("players_features.csv")

num_df <- players %>% select(where(is.numeric))



# Moda
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

stats_central <- num_df %>%
  summarise(across(everything(), list(
    media = ~mean(.),
    mediana = ~median(.)
  )))

modas <- sapply(num_df, moda)

print(stats_central)
print(modas)



num_df %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Distribución de variables (media/mediana)")



stats_dispersion <- num_df %>%
  summarise(across(everything(), list(
    varianza = ~var(.),
    sd = ~sd(.)
  )))

print(stats_dispersion)



num_df %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(fill = "orange") +
  coord_flip() +
  labs(title = "Dispersión (varianza y desviación estándar)")



cor_matrix <- cor(num_df)

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper")




ggpairs(num_df)



cov_matrix <- cov(num_df)

print(cov_matrix)



skewness <- function(x) {
  m <- mean(x)
  s <- sd(x)
  mean(((x - m)/s)^3)
}

kurtosis <- function(x) {
  m <- mean(x)
  s <- sd(x)
  mean(((x - m)/s)^4) - 3
}

shape_stats <- num_df %>%
  summarise(across(everything(), list(
    skewness = ~skewness(.),
    kurtosis = ~kurtosis(.)
  )))

print(shape_stats)



outliers <- num_df %>%
  summarise(across(everything(), ~{
    Q1 <- quantile(., 0.25)
    Q3 <- quantile(., 0.75)
    IQR_val <- Q3 - Q1
    sum(. < (Q1 - 1.5*IQR_val) | . > (Q3 + 1.5*IQR_val))
  }))

print(outliers)



num_df %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Outliers por variable")



players <- players %>%
  mutate(
    categoria = case_when(
      overall_strength > 80 ~ "elite",
      overall_strength > 70 ~ "medio",
      TRUE ~ "bajo"
    )
  )

# predicción dummy
players$pred <- sample(players$categoria)

conf_matrix <- table(players$categoria, players$pred)

print(conf_matrix)




conf_df <- as.data.frame(conf_matrix)

ggplot(conf_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(title = "Matriz de confusión")



ggplot(players, aes(x = attack_strength, y = defense_strength)) +
  geom_point(alpha = 0.5) +
  labs(title = "Ataque vs Defensa")

ggplot(players, aes(x = overall_strength, y = depth)) +
  geom_point() +
  labs(title = "Calidad vs profundidad")



set.seed(123)

kmeans_res <- kmeans(num_df, centers = 3)

players$cluster <- kmeans_res$cluster

ggplot(players, aes(x = attack_strength, y = defense_strength, color = factor(cluster))) +
  geom_point()



