# ============================================================
# REGRESIONES LINEALES PARA PREDICCION DE PARTIDOS / MUNDIAL
# Datos esperados en la misma carpeta del script:
# - teams_match_features*.csv
# - teams_form_clean*.csv
# - results_clean*.csv
# - players_features*.csv
# - goals_features*.csv
# - shootouts_features*.csv
# - former_names_clean*.csv
# ============================================================

# ------------------------------
# 0) Paquetes
# ------------------------------
required_packages <- c("readr", "dplyr", "stringr", "lubridate", "purrr", "broom", "ggplot2", "tibble")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(broom)
library(ggplot2)
library(tibble)

set.seed(123)

# ------------------------------
# 1) Configuracion
# ------------------------------
BASE_DIR <- "."
OUT_DIR <- "outputs_linear_R"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

pick_file <- function(pattern) {
  files <- list.files(BASE_DIR, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop(paste("No se encontro ningun archivo con patron:", pattern))
  }
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

norm_team <- function(x) {
  x %>% as.character() %>% str_squish() %>% str_to_upper()
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}

r2_score <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  1 - ss_res / ss_tot
}

median_impute <- function(df, cols, medians = NULL) {
  df2 <- df

  if (is.null(medians)) {
    medians <- sapply(cols, function(col) {
      val <- suppressWarnings(median(df2[[col]], na.rm = TRUE))
      ifelse(is.finite(val), val, 0)
    })
  }

  for (col in cols) {
    df2[[col]] <- as.numeric(df2[[col]])
    df2[[col]][is.na(df2[[col]])] <- medians[[col]]
  }

  list(data = df2, medians = medians)
}

clamp <- function(x, lower, upper) {
  pmax(lower, pmin(upper, x))
}

# ------------------------------
# 2) Carga de los 7 CSV
# ------------------------------
teams_match_file <- pick_file("^teams_match_features.*\\.csv$")
teams_form_file  <- pick_file("^teams_form_clean.*\\.csv$")
shootouts_file   <- pick_file("^shootouts_features.*\\.csv$")
results_file     <- pick_file("^results_clean.*\\.csv$")
players_file     <- pick_file("^players_features.*\\.csv$")
goals_file       <- pick_file("^goals_features.*\\.csv$")
former_file      <- pick_file("^former_names_clean.*\\.csv$")

teams_match <- read_csv(teams_match_file, show_col_types = FALSE)
teams_form  <- read_csv(teams_form_file, show_col_types = FALSE)
shootouts   <- read_csv(shootouts_file, show_col_types = FALSE)
results     <- read_csv(results_file, show_col_types = FALSE)
players     <- read_csv(players_file, show_col_types = FALSE)
goals       <- read_csv(goals_file, show_col_types = FALSE)
former_names <- read_csv(former_file, show_col_types = FALSE)

cat("Archivos cargados:\n")
cat("-", teams_match_file, "\n")
cat("-", teams_form_file, "\n")
cat("-", shootouts_file, "\n")
cat("-", results_file, "\n")
cat("-", players_file, "\n")
cat("-", goals_file, "\n")
cat("-", former_file, "\n\n")

# ------------------------------
# 3) Preparacion del dataset principal
# ------------------------------
# teams_match_features es la matriz principal porque ya resume:
# - fuerza Elo
# - fuerza de plantilla
# - forma reciente
# - banderas del tipo de partido
# - goles reales del partido

tm <- teams_match %>%
  mutate(
    match_date = as.Date(`_date`),
    home_goals = as.numeric(home_goals),
    away_goals = as.numeric(away_goals),

    # Variables objetivo continuas o semilineales
    goal_diff = home_goals - away_goals,
    total_goals = home_goals + away_goals,
    home_expected_points = case_when(
      goal_diff > 0 ~ 3,
      goal_diff == 0 ~ 1,
      goal_diff < 0 ~ 0,
      TRUE ~ NA_real_
    ),
    away_expected_points = case_when(
      goal_diff < 0 ~ 3,
      goal_diff == 0 ~ 1,
      goal_diff > 0 ~ 0,
      TRUE ~ NA_real_
    ),

    # Variables comparativas estables
    form_win_rate_diff = home_form_win_rate - away_form_win_rate,
    form_scored_diff = home_form_scored - away_form_scored,
    form_conceded_diff = home_form_conceded - away_form_conceded,
    attack_vs_defense_home = home_avg_attack - away_avg_defense,
    attack_vs_defense_away = away_avg_attack - home_avg_defense,

    is_neutral = as.numeric(is_neutral),
    is_world_cup = as.numeric(is_world_cup),
    is_continental = as.numeric(is_continental)
  )

model_df <- tm %>%
  filter(
    match_date >= as.Date("2000-01-01"),
    !is.na(home_goals),
    !is.na(away_goals),
    !is.na(goal_diff)
  )

# Division temporal, no aleatoria.
# Esto evita entrenar con informacion posterior al periodo de prueba.
# NOTA: quantile() no opera directamente sobre Date en algunas versiones de R,
# porque internamente intenta multiplicar objetos Date. Por eso se convierte a
# numerico y despues se regresa a Date.
temporal_split_date <- function(date_vector, probs = 0.80) {
  date_vector <- as.Date(date_vector)

  if (all(is.na(date_vector))) {
    stop("No hay fechas validas para hacer la division temporal.")
  }

  split_numeric <- stats::quantile(
    as.numeric(date_vector),
    probs = probs,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  as.Date(round(split_numeric), origin = "1970-01-01")
}

model_df <- model_df %>% arrange(match_date)
split_date <- temporal_split_date(model_df$match_date, probs = 0.80)

train_df <- model_df %>% filter(match_date < split_date)
test_df  <- model_df %>% filter(match_date >= split_date)

# Respaldo: si muchas filas comparten la fecha de corte y alguna particion queda
# vacia o demasiado pequena, se usa corte por indice manteniendo el orden temporal.
if (nrow(train_df) < 10 || nrow(test_df) < 10) {
  cutoff_idx <- floor(0.80 * nrow(model_df))
  train_df <- model_df[seq_len(cutoff_idx), ]
  test_df  <- model_df[(cutoff_idx + 1):nrow(model_df), ]
  split_date <- min(test_df$match_date, na.rm = TRUE)
}

cat("Filas entrenamiento:", nrow(train_df), "\n")
cat("Filas prueba:", nrow(test_df), "\n")
cat("Fecha de corte temporal:", as.character(split_date), "\n\n")

# ------------------------------
# 4) Variables lineales seleccionadas
# ------------------------------
# Se usan variables diferenciales porque son mas estables e interpretables:
# - elo_diff: diferencia de fuerza historica
# - overall_diff: diferencia de calidad de plantilla
# - attack_diff / defense_diff: diferencias por zonas
# - diferencias de forma reciente
# - banderas del contexto del partido

predictors_diff <- c(
  "elo_diff",
  "overall_diff",
  "attack_diff",
  "defense_diff",
  "form_win_rate_diff",
  "form_scored_diff",
  "form_conceded_diff",
  "is_neutral",
  "is_world_cup",
  "is_continental"
)

predictors_goals_home <- c(
  "home_elo",
  "away_elo",
  "attack_vs_defense_home",
  "home_form_scored",
  "away_form_conceded",
  "home_form_win_rate",
  "away_form_win_rate",
  "is_neutral",
  "is_world_cup",
  "is_continental"
)

predictors_goals_away <- c(
  "home_elo",
  "away_elo",
  "attack_vs_defense_away",
  "away_form_scored",
  "home_form_conceded",
  "away_form_win_rate",
  "home_form_win_rate",
  "is_neutral",
  "is_world_cup",
  "is_continental"
)

predictors_total_goals <- c(
  "home_elo",
  "away_elo",
  "home_avg_attack",
  "away_avg_attack",
  "home_avg_defense",
  "away_avg_defense",
  "home_form_scored",
  "away_form_scored",
  "home_form_conceded",
  "away_form_conceded",
  "is_neutral",
  "is_world_cup",
  "is_continental"
)

model_specs <- list(
  lm_goal_diff = list(
    target = "goal_diff",
    predictors = predictors_diff,
    recommended = TRUE,
    interpretation = "Diferencia esperada de goles: home_goals - away_goals. Es la regresion lineal principal."
  ),
  lm_home_goals = list(
    target = "home_goals",
    predictors = predictors_goals_home,
    recommended = TRUE,
    interpretation = "Goles esperados del local. Es conteo, pero funciona como baseline lineal."
  ),
  lm_away_goals = list(
    target = "away_goals",
    predictors = predictors_goals_away,
    recommended = TRUE,
    interpretation = "Goles esperados del visitante. Es conteo, pero funciona como baseline lineal."
  ),
  lm_home_expected_points = list(
    target = "home_expected_points",
    predictors = predictors_diff,
    recommended = TRUE,
    interpretation = "Puntos esperados del local en escala 0-3. Aunque sale de una clase, la media esperada es interpretable."
  ),
  lm_away_expected_points = list(
    target = "away_expected_points",
    predictors = predictors_diff,
    recommended = TRUE,
    interpretation = "Puntos esperados del visitante en escala 0-3. Complementa al modelo de puntos del local."
  ),
  lm_total_goals = list(
    target = "total_goals",
    predictors = predictors_total_goals,
    recommended = FALSE,
    interpretation = "Total de goles. Se deja como diagnostico: suele ser menos lineal y mas ruidoso."
  )
)

# ------------------------------
# 5) Funcion para entrenar y evaluar LM
# ------------------------------
fit_lm_model <- function(model_name, spec, train_df, test_df) {
  target <- spec$target
  predictors <- spec$predictors

  needed_cols <- c(target, predictors)
  missing_cols <- setdiff(needed_cols, names(train_df))
  if (length(missing_cols) > 0) {
    stop(paste("Faltan columnas para", model_name, ":", paste(missing_cols, collapse = ", ")))
  }

  train_clean <- train_df %>% filter(!is.na(.data[[target]]))
  test_clean  <- test_df %>% filter(!is.na(.data[[target]]))

  imp_train <- median_impute(train_clean, predictors)
  train_imp <- imp_train$data
  test_imp <- median_impute(test_clean, predictors, imp_train$medians)$data

  form <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  model <- lm(form, data = train_imp)

  pred <- as.numeric(predict(model, newdata = test_imp))

  # Las predicciones se evaluan crudas para revisar el comportamiento real de lm.
  metrics <- tibble(
    model_name = model_name,
    target = target,
    n_train = nrow(train_imp),
    n_test = nrow(test_imp),
    MAE = mae(test_imp[[target]], pred),
    RMSE = rmse(test_imp[[target]], pred),
    R2 = r2_score(test_imp[[target]], pred),
    recommended_by_design = spec$recommended,
    interpretation = spec$interpretation
  )

  predictions <- test_imp %>%
    transmute(
      model_name = model_name,
      target = target,
      date = match_date,
      home_team = `_home_team`,
      away_team = `_away_team`,
      actual = .data[[target]],
      predicted = pred,
      residual = actual - predicted
    )

  coefficients <- broom::tidy(model) %>%
    mutate(
      model_name = model_name,
      target = target,
      .before = 1
    )

  list(
    model = model,
    medians = imp_train$medians,
    metrics = metrics,
    predictions = predictions,
    coefficients = coefficients,
    spec = spec
  )
}

# ------------------------------
# 6) Entrenamiento de todas las regresiones lineales
# ------------------------------
linear_models <- imap(
  model_specs,
  ~ fit_lm_model(.y, .x, train_df = train_df, test_df = test_df)
)

metrics_all <- bind_rows(map(linear_models, "metrics")) %>%
  arrange(desc(R2)) %>%
  mutate(
    recommended_empirical = R2 >= 0.10,
    decision = case_when(
      recommended_by_design & recommended_empirical ~ "USAR",
      recommended_by_design & !recommended_empirical ~ "REVISAR",
      !recommended_by_design & recommended_empirical ~ "DIAGNOSTICO",
      TRUE ~ "NO_RECOMENDADA"
    )
  )

coefficients_all <- bind_rows(map(linear_models, "coefficients"))
predictions_all <- bind_rows(map(linear_models, "predictions"))

write_csv(metrics_all, file.path(OUT_DIR, "linear_regression_metrics_by_target.csv"))
write_csv(coefficients_all, file.path(OUT_DIR, "linear_regression_coefficients_by_target.csv"))
write_csv(predictions_all, file.path(OUT_DIR, "linear_regression_test_predictions.csv"))
saveRDS(linear_models, file.path(OUT_DIR, "linear_models.rds"))

cat("Metricas de regresiones lineales:\n")
print(metrics_all %>% select(model_name, target, MAE, RMSE, R2, decision))
cat("\nArchivos guardados en:", OUT_DIR, "\n\n")

# ------------------------------
# 7) GRAFICAS DE LAS REGRESIONES LINEALES
# ------------------------------
# Antes el script guardaba principalmente tablas CSV. Esta seccion genera graficas
# para visualizar el ajuste de cada regresion: real vs predicho, residuos,
# distribucion de residuos, Q-Q plot y coeficientes.

PLOTS_DIR <- file.path(OUT_DIR, "plots_linear_regressions")
if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE)

safe_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9_\\-]+", "_") %>%
    str_replace_all("_+", "_")
}

add_metrics_subtitle <- function(model_name) {
  row <- metrics_all %>% filter(.data$model_name == model_name) %>% slice(1)
  if (nrow(row) == 0) return(NULL)

  paste0(
    "MAE = ", round(row$MAE, 3),
    " | RMSE = ", round(row$RMSE, 3),
    " | R2 = ", round(row$R2, 3),
    " | Decision: ", row$decision
  )
}

plot_actual_vs_predicted <- function(pred_df, model_name) {
  target_name <- unique(pred_df$target)[1]

  min_val <- min(c(pred_df$actual, pred_df$predicted), na.rm = TRUE)
  max_val <- max(c(pred_df$actual, pred_df$predicted), na.rm = TRUE)

  p <- ggplot(pred_df, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.35) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    coord_cartesian(xlim = c(min_val, max_val), ylim = c(min_val, max_val)) +
    labs(
      title = paste("Real vs predicho -", model_name),
      subtitle = add_metrics_subtitle(model_name),
      x = paste("Valor real de", target_name),
      y = paste("Valor predicho de", target_name)
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, paste0("01_actual_vs_predicted_", safe_filename(model_name), ".png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 150
  )
}

plot_residuals_vs_predicted <- function(pred_df, model_name) {
  p <- ggplot(pred_df, aes(x = predicted, y = residual)) +
    geom_point(alpha = 0.35) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Residuos vs prediccion -", model_name),
      subtitle = "Idealmente los puntos deben estar dispersos aleatoriamente alrededor de 0.",
      x = "Prediccion",
      y = "Residual = real - predicho"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, paste0("02_residuals_vs_predicted_", safe_filename(model_name), ".png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 150
  )
}

plot_residual_histogram <- function(pred_df, model_name) {
  p <- ggplot(pred_df, aes(x = residual)) +
    geom_histogram(bins = 35, alpha = 0.75) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Distribucion de residuos -", model_name),
      subtitle = "Una distribucion centrada cerca de 0 indica menor sesgo promedio.",
      x = "Residual = real - predicho",
      y = "Frecuencia"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, paste0("03_residual_histogram_", safe_filename(model_name), ".png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 150
  )
}

plot_qq_residuals <- function(pred_df, model_name) {
  p <- ggplot(pred_df, aes(sample = residual)) +
    stat_qq(alpha = 0.45) +
    stat_qq_line(linetype = "dashed") +
    labs(
      title = paste("Q-Q plot de residuos -", model_name),
      subtitle = "Evalua visualmente si los residuos se aproximan a normalidad.",
      x = "Cuantiles teoricos",
      y = "Cuantiles muestrales"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, paste0("04_qq_residuals_", safe_filename(model_name), ".png")),
    plot = p,
    width = 8,
    height = 6,
    dpi = 150
  )
}

plot_coefficients <- function(coef_df, model_name) {
  coef_plot_df <- coef_df %>%
    filter(model_name == !!model_name, term != "(Intercept)") %>%
    mutate(
      conf_low = estimate - 1.96 * std.error,
      conf_high = estimate + 1.96 * std.error,
      term = reorder(term, estimate)
    )

  if (nrow(coef_plot_df) == 0) return(NULL)

  p <- ggplot(coef_plot_df, aes(x = term, y = estimate)) +
    geom_col(alpha = 0.75) +
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Coeficientes lineales -", model_name),
      subtitle = "Barras con intervalo aproximado de 95%. Excluye intercepto.",
      x = "Variable predictora",
      y = "Coeficiente estimado"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, paste0("05_coefficients_", safe_filename(model_name), ".png")),
    plot = p,
    width = 9,
    height = 6,
    dpi = 150
  )
}

plot_top_residual_matches <- function(pred_df, model_name, n = 20) {
  top_df <- pred_df %>%
    mutate(
      match_label = paste0(as.character(date), " | ", home_team, " vs ", away_team),
      abs_residual = abs(residual)
    ) %>%
    arrange(desc(abs_residual)) %>%
    slice_head(n = n) %>%
    mutate(match_label = reorder(match_label, abs_residual))

  p <- ggplot(top_df, aes(x = match_label, y = abs_residual)) +
    geom_col(alpha = 0.75) +
    coord_flip() +
    labs(
      title = paste("Mayores errores absolutos -", model_name),
      subtitle = "Partidos donde la regresion lineal fallo mas dentro del conjunto de prueba.",
      x = "Partido",
      y = "Error absoluto"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, paste0("06_top_abs_residuals_", safe_filename(model_name), ".png")),
    plot = p,
    width = 10,
    height = 7,
    dpi = 150
  )
}

# Graficas individuales por modelo.
for (nm in names(linear_models)) {
  pred_df <- linear_models[[nm]]$predictions

  plot_actual_vs_predicted(pred_df, nm)
  plot_residuals_vs_predicted(pred_df, nm)
  plot_residual_histogram(pred_df, nm)
  plot_qq_residuals(pred_df, nm)
  plot_coefficients(coefficients_all, nm)
  plot_top_residual_matches(pred_df, nm, n = 20)
}

# Graficas comparativas entre objetivos.
metrics_plot_df <- metrics_all %>%
  mutate(
    model_name = reorder(model_name, R2),
    decision = factor(decision, levels = c("USAR", "REVISAR", "DIAGNOSTICO", "NO_RECOMENDADA"))
  )

p_r2 <- ggplot(metrics_plot_df, aes(x = model_name, y = R2)) +
  geom_col(alpha = 0.75) +
  coord_flip() +
  labs(
    title = "Comparacion de R2 por regresion lineal",
    subtitle = "Mayor R2 implica mejor capacidad explicativa fuera de entrenamiento.",
    x = "Modelo",
    y = "R2"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(PLOTS_DIR, "00_model_comparison_R2.png"),
  plot = p_r2,
  width = 8,
  height = 5,
  dpi = 150
)

p_rmse <- ggplot(metrics_plot_df, aes(x = reorder(model_name, RMSE), y = RMSE)) +
  geom_col(alpha = 0.75) +
  coord_flip() +
  labs(
    title = "Comparacion de RMSE por regresion lineal",
    subtitle = "Menor RMSE implica menor error promedio cuadratico.",
    x = "Modelo",
    y = "RMSE"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(PLOTS_DIR, "00_model_comparison_RMSE.png"),
  plot = p_rmse,
  width = 8,
  height = 5,
  dpi = 150
)

p_mae <- ggplot(metrics_plot_df, aes(x = reorder(model_name, MAE), y = MAE)) +
  geom_col(alpha = 0.75) +
  coord_flip() +
  labs(
    title = "Comparacion de MAE por regresion lineal",
    subtitle = "Menor MAE implica menor error absoluto promedio.",
    x = "Modelo",
    y = "MAE"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(PLOTS_DIR, "00_model_comparison_MAE.png"),
  plot = p_mae,
  width = 8,
  height = 5,
  dpi = 150
)

cat("Graficas de regresiones lineales guardadas en:\n")
cat(PLOTS_DIR, "\n\n")

# ------------------------------
# 8) Construccion opcional de features para partidos del Mundial 2026
# ------------------------------
# Esta parte usa los 7 CSV para crear una tabla de prediccion sobre fixtures futuros.
# Los modelos lineales necesitan las mismas columnas que se usaron al entrenar.

# Ultimo Elo disponible por equipo desde teams_match_features.
latest_elo_home <- teams_match %>%
  transmute(
    team = norm_team(`_home_team`),
    date = as.Date(`_date`),
    elo = as.numeric(home_elo)
  )

latest_elo_away <- teams_match %>%
  transmute(
    team = norm_team(`_away_team`),
    date = as.Date(`_date`),
    elo = as.numeric(away_elo)
  )

latest_elo <- bind_rows(latest_elo_home, latest_elo_away) %>%
  filter(!is.na(team), !is.na(date), !is.na(elo)) %>%
  arrange(team, date) %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Ultima forma disponible por seleccion.
latest_form <- teams_form %>%
  mutate(
    team = norm_team(team),
    match_date = as.Date(match_date)
  ) %>%
  arrange(team, match_date) %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%
  ungroup()

# Ultima version FIFA disponible por pais.
latest_players <- players %>%
  mutate(team = norm_team(country)) %>%
  arrange(team, fifa_version) %>%
  group_by(team) %>%
  slice_tail(n = 1) %>%
  ungroup()

home_elo_tbl <- latest_elo %>% transmute(home_key = team, home_elo = elo)
away_elo_tbl <- latest_elo %>% transmute(away_key = team, away_elo = elo)

home_form_tbl <- latest_form %>%
  transmute(
    home_key = team,
    home_form_scored = avg_goals_scored,
    home_form_conceded = avg_goals_conceded,
    home_form_win_rate = win_rate
  )

away_form_tbl <- latest_form %>%
  transmute(
    away_key = team,
    away_form_scored = avg_goals_scored,
    away_form_conceded = avg_goals_conceded,
    away_form_win_rate = win_rate
  )

home_player_tbl <- latest_players %>%
  transmute(
    home_key = team,
    home_avg_overall = overall_strength,
    home_max_overall = max_overall,
    home_avg_attack = attack_strength,
    home_avg_defense = defense_strength,
    home_avg_pace = avg_pace,
    home_avg_shooting = avg_shooting,
    home_avg_passing = avg_passing
  )

away_player_tbl <- latest_players %>%
  transmute(
    away_key = team,
    away_avg_overall = overall_strength,
    away_max_overall = max_overall,
    away_avg_attack = attack_strength,
    away_avg_defense = defense_strength,
    away_avg_pace = avg_pace,
    away_avg_shooting = avg_shooting,
    away_avg_passing = avg_passing
  )

# Fixtures futuros del Mundial: filas sin marcador.
wc_fixtures <- results %>%
  mutate(
    date = as.Date(date),
    home_key = norm_team(home_team),
    away_key = norm_team(away_team),
    tournament_key = str_to_upper(tournament)
  ) %>%
  filter(
    tournament_key == "FIFA WORLD CUP",
    year(date) == 2026,
    is.na(home_score),
    is.na(away_score)
  )

if (nrow(wc_fixtures) > 0) {
  future_features <- wc_fixtures %>%
    left_join(home_elo_tbl, by = "home_key") %>%
    left_join(away_elo_tbl, by = "away_key") %>%
    left_join(home_form_tbl, by = "home_key") %>%
    left_join(away_form_tbl, by = "away_key") %>%
    left_join(home_player_tbl, by = "home_key") %>%
    left_join(away_player_tbl, by = "away_key") %>%
    mutate(
      elo_diff = home_elo - away_elo,
      overall_diff = home_avg_overall - away_avg_overall,
      attack_diff = home_avg_attack - away_avg_attack,
      defense_diff = home_avg_defense - away_avg_defense,
      form_win_rate_diff = home_form_win_rate - away_form_win_rate,
      form_scored_diff = home_form_scored - away_form_scored,
      form_conceded_diff = home_form_conceded - away_form_conceded,
      attack_vs_defense_home = home_avg_attack - away_avg_defense,
      attack_vs_defense_away = away_avg_attack - home_avg_defense,
      is_neutral = as.numeric(is_neutral),
      is_world_cup = 1,
      is_continental = 0
    )

  future_predictions <- future_features %>%
    transmute(
      date,
      home_team,
      away_team,
      city,
      country,
      is_neutral
    )

  for (nm in names(linear_models)) {
    spec <- linear_models[[nm]]$spec
    predictors <- spec$predictors
    target <- spec$target

    future_imp <- median_impute(
      future_features,
      predictors,
      linear_models[[nm]]$medians
    )$data

    pred <- as.numeric(predict(linear_models[[nm]]$model, newdata = future_imp))

    # Ajustes logicos solo para reporte de partidos futuros.
    if (target %in% c("home_goals", "away_goals", "total_goals")) {
      pred <- pmax(0, pred)
    }
    if (target %in% c("home_expected_points", "away_expected_points")) {
      pred <- clamp(pred, 0, 3)
    }

    future_predictions[[paste0("pred_", target)]] <- pred
  }

  future_predictions <- future_predictions %>%
    mutate(
      predicted_result_lm = case_when(
        pred_goal_diff > 0.20 ~ "HOME_WIN",
        pred_goal_diff < -0.20 ~ "AWAY_WIN",
        TRUE ~ "DRAW"
      ),
      predicted_winner_no_draw_lm = case_when(
        pred_goal_diff >= 0 ~ home_team,
        TRUE ~ away_team
      ),
      predicted_score_lm = paste0(
        round(pred_home_goals, 2),
        " - ",
        round(pred_away_goals, 2)
      )
    )

  write_csv(
    future_predictions,
    file.path(OUT_DIR, "worldcup_2026_linear_predictions_extra_targets.csv")
  )

  cat("Predicciones lineales para Mundial 2026 guardadas en:\n")
  cat(file.path(OUT_DIR, "worldcup_2026_linear_predictions_extra_targets.csv"), "\n")
} else {
  cat("No se encontraron fixtures futuros del Mundial 2026 sin marcador en results_clean.\n")
}


# ------------------------------
# 8.1) Graficas para predicciones lineales del Mundial 2026
# ------------------------------
if (exists("future_predictions") && nrow(future_predictions) > 0) {
  wc_plots_df <- future_predictions %>%
    mutate(
      match_label = paste0(home_team, " vs ", away_team),
      abs_goal_diff = abs(pred_goal_diff),
      favored_team = ifelse(pred_goal_diff >= 0, home_team, away_team),
      favorite_margin = abs(pred_goal_diff)
    )

  p_wc_margin <- wc_plots_df %>%
    arrange(desc(favorite_margin)) %>%
    slice_head(n = 25) %>%
    mutate(match_label = reorder(match_label, favorite_margin)) %>%
    ggplot(aes(x = match_label, y = favorite_margin)) +
    geom_col(alpha = 0.75) +
    coord_flip() +
    labs(
      title = "Mundial 2026: partidos con mayor margen lineal esperado",
      subtitle = "Basado en pred_goal_diff absoluto. Es una lectura de superioridad esperada, no una probabilidad.",
      x = "Partido",
      y = "Margen esperado de goles"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, "worldcup_2026_top_expected_goal_margins_lm.png"),
    plot = p_wc_margin,
    width = 10,
    height = 7,
    dpi = 150
  )

  p_wc_goal_diff <- wc_plots_df %>%
    arrange(desc(pred_goal_diff)) %>%
    mutate(match_label = reorder(match_label, pred_goal_diff)) %>%
    ggplot(aes(x = match_label, y = pred_goal_diff)) +
    geom_col(alpha = 0.75) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Mundial 2026: goal_diff predicho por partido",
      subtitle = "Valor positivo favorece al local; valor negativo favorece al visitante.",
      x = "Partido",
      y = "pred_goal_diff"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, "worldcup_2026_predicted_goal_diff_lm.png"),
    plot = p_wc_goal_diff,
    width = 11,
    height = 12,
    dpi = 150
  )

  wc_team_points_lm <- wc_plots_df %>%
    transmute(team = home_team, expected_points = pred_home_expected_points) %>%
    bind_rows(
      wc_plots_df %>% transmute(team = away_team, expected_points = pred_away_expected_points)
    ) %>%
    group_by(team) %>%
    summarise(
      expected_points = sum(expected_points, na.rm = TRUE),
      matches = n(),
      expected_points_per_match = expected_points / matches,
      .groups = "drop"
    ) %>%
    arrange(desc(expected_points))

  write_csv(wc_team_points_lm, file.path(OUT_DIR, "worldcup_2026_linear_team_expected_points.csv"))

  p_wc_points <- wc_team_points_lm %>%
    slice_head(n = 25) %>%
    mutate(team = reorder(team, expected_points)) %>%
    ggplot(aes(x = team, y = expected_points)) +
    geom_col(alpha = 0.75) +
    coord_flip() +
    labs(
      title = "Mundial 2026: puntos esperados por seleccion segun regresiones lineales",
      subtitle = "Suma de pred_home_expected_points y pred_away_expected_points en fase de grupos.",
      x = "Seleccion",
      y = "Puntos esperados"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOTS_DIR, "worldcup_2026_linear_team_expected_points.png"),
    plot = p_wc_points,
    width = 9,
    height = 7,
    dpi = 150
  )

  cat("Graficas lineales del Mundial 2026 guardadas en:\n")
  cat(PLOTS_DIR, "\n")
  cat("Ranking lineal de puntos esperados guardado en:\n")
  cat(file.path(OUT_DIR, "worldcup_2026_linear_team_expected_points.csv"), "\n")
}

# ------------------------------
# 9) Nota metodologica automatica
# ------------------------------
cat("\nLectura recomendada:\n")
cat("- goal_diff es la variable lineal principal porque resume superioridad relativa.\n")
cat("- home_goals y away_goals son utiles como regresiones auxiliares para marcador esperado.\n")
cat("- home_expected_points y away_expected_points son utiles para ranking de grupos.\n")
cat("- total_goals queda como diagnostico porque suele ser mucho mas ruidosa y menos lineal.\n")
