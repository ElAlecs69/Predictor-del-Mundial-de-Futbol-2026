library(tidyverse)
library(corrplot)
library(GGally)

results <- read.csv("results_clean.csv")

num_df <- results %>% select(where(is.numeric))



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
  labs(title = "Distribución de variables")




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
  labs(title = "Dispersión de variables")



cor_matrix <- cor(num_df)

print(cor_matrix)

corrplot(cor_matrix, method = "color", type = "upper")




ggpairs(num_df)




cov_matrix <- cov(num_df)

print(cov_matrix)




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
  facet_wrap(~name, scales = "free")



results %>%
  group_by(year) %>%
  summarise(avg_goals = mean(total_goals)) %>%
  ggplot(aes(x = year, y = avg_goals)) +
  geom_line() +
  labs(title = "Evolución de goles en el tiempo")




results %>%
  summarise(
    home_avg = mean(home_score),
    away_avg = mean(away_score)
  )

ggplot(results, aes(x = home_score, y = away_score)) +
  geom_point(alpha = 0.3) +
  labs(title = "Goles local vs visitante")



# modelo simple baseline
results$pred <- ifelse(results$home_score > results$away_score, 1,
                       ifelse(results$home_score < results$away_score, -1, 0))

conf_matrix <- table(results$result, results$pred)

print(conf_matrix)




conf_df <- as.data.frame(conf_matrix)

ggplot(conf_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  labs(title = "Matriz de confusión")




set.seed(123)

kmeans_res <- kmeans(num_df, centers = 3)

results$cluster <- kmeans_res$cluster




ggplot(results, aes(x = home_score, y = away_score, color = factor(cluster))) +
  geom_point() +
  labs(title = "Clusters de partidos")




ggplot(results, aes(x = goal_diff, y = total_goals)) +
  geom_point() +
  labs(title = "Diferencia vs total de goles")



ggplot(results, aes(x = home_advantage, y = total_goals)) +
  geom_boxplot() +
  labs(title = "Impacto de localía")



