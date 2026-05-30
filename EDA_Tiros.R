library(tidyverse)
library(corrplot)
library(GGally)

shootouts <- read.csv("shootouts_features.csv")

num_df <- shootouts %>% select(where(is.numeric))



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
  geom_bar(fill = "steelblue") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Distribución de variables binarias")



stats_dispersion <- num_df %>%
  summarise(across(everything(), list(
    varianza = ~var(.),
    sd = ~sd(.)
  )))

print(stats_dispersion)



cor_matrix <- cor(num_df)

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper")



ggpairs(num_df)



cov_matrix <- cov(num_df)

print(cov_matrix)



balance <- num_df %>%
  summarise(across(everything(), ~mean(.)))

print(balance)



balance %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_col(fill = "red") +
  labs(title = "Proporción de eventos (1)")



# predicción naive (quién gana más)
shootouts$pred <- ifelse(shootouts$home_win_pen == 1, 1, 0)

conf_matrix <- table(
  Real = shootouts$home_win_pen,
  Pred = shootouts$pred
)

print(conf_matrix)



conf_df <- as.data.frame(conf_matrix)

ggplot(conf_df, aes(Real, Pred, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(title = "Matriz de confusión")



set.seed(123)

kmeans_res <- kmeans(num_df, centers = 2)

shootouts$cluster <- kmeans_res$cluster



ggplot(shootouts, aes(x = home_win_pen, y = away_win_pen, color = factor(cluster))) +
  geom_jitter(width = 0.1, height = 0.1) +
  labs(title = "Clusters en penales")



shootouts %>%
  summarise(
    home_win_rate = mean(home_win_pen),
    away_win_rate = mean(away_win_pen)
  )



shootouts %>%
  summarise(
    home = mean(home_win_pen),
    away = mean(away_win_pen)
  ) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_col() +
  labs(title = "Probabilidad de ganar en penales")



shootouts %>%
  summarise(
    first_home_win = mean(home_win_pen[first_shooter_home == 1]),
    first_away_win = mean(away_win_pen[first_shooter_away == 1])
  )



shootouts %>%
  mutate(first = ifelse(first_shooter_home == 1, "home", "away")) %>%
  group_by(first) %>%
  summarise(win_rate = mean(home_win_pen)) %>%
  ggplot(aes(x = first, y = win_rate)) +
  geom_col() +
  labs(title = "Impacto de tirar primero")



