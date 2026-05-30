library(tidyverse)
library(corrplot)
library(GGally)

teams_form <- read.csv("teams_form_clean.csv")

num_df <- teams_form %>% select(where(is.numeric))



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
  labs(title = "Distribución (media y mediana)")



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
  labs(title = "Dispersión (varianza y desviación)")



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



teams_form <- teams_form %>%
  mutate(
    categoria = case_when(
      win_rate > 0.7 ~ "alta",
      win_rate > 0.4 ~ "media",
      TRUE ~ "baja"
    )
  )

set.seed(123)
teams_form$pred <- sample(teams_form$categoria)

conf_matrix <- table(teams_form$categoria, teams_form$pred)

print(conf_matrix)



conf_df <- as.data.frame(conf_matrix)

ggplot(conf_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(title = "Matriz de confusión")



set.seed(123)

kmeans_res <- kmeans(num_df, centers = 3)

teams_form$cluster <- kmeans_res$cluster



ggplot(teams_form, aes(x = avg_goals_scored, y = win_rate)) +
  geom_point() +
  labs(title = "Goles vs Win Rate")

ggplot(teams_form, aes(x = goal_balance, y = win_rate)) +
  geom_point() +
  labs(title = "Balance vs Win Rate")


