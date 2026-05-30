# ============================================================
# CLASIFICADOR PARA PREDICCION DE RESULTADOS DE PARTIDOS
# Mundial / selecciones nacionales
#
# Modelos entrenados:
# 1) Regresion logistica multinomial
# 2) ExtraTrees usando ranger con splitrule = "extratrees"
# 3) Ensamble por promedio de probabilidades
#
# Objetivo de clasificacion:
# - HOME_WIN: gana el local
# - DRAW: empate
# - AWAY_WIN: gana el visitante
#
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
required_packages <- c(
  "readr", "dplyr", "stringr", "lubridate", "tibble",
  "purrr", "nnet", "ranger", "jsonlite"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tibble)
library(purrr)
library(nnet)
library(ranger)
library(jsonlite)

set.seed(123)

# ------------------------------
# 1) Configuracion general
# ------------------------------
BASE_DIR <- "."
OUT_DIR <- "outputs_classifier_R"
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

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# ------------------------------
# 2) Metricas y utilidades
# ------------------------------
accuracy_score <- function(actual, predicted) {
  mean(as.character(actual) == as.character(predicted), na.rm = TRUE)
}

log_loss_multiclass <- function(actual, prob_matrix, eps = 1e-15) {
  actual <- as.character(actual)
  prob_matrix <- as.matrix(prob_matrix)
  prob_matrix <- pmin(pmax(prob_matrix, eps), 1 - eps)

  idx <- match(actual, colnames(prob_matrix))
  valid <- !is.na(idx)

  -mean(log(prob_matrix[cbind(which(valid), idx[valid])]), na.rm = TRUE)
}

make_confusion_matrix <- function(actual, predicted, levels_order) {
  actual <- factor(as.character(actual), levels = levels_order)
  predicted <- factor(as.character(predicted), levels = levels_order)

  as.data.frame.matrix(table(actual = actual, predicted = predicted)) %>%
    rownames_to_column("actual")
}

balanced_case_weights <- function(y) {
  y_chr <- as.character(y)
  tab <- table(y_chr)
  n <- length(y_chr)
  k <- length(tab)
  class_w <- n / (k * as.numeric(tab))
  names(class_w) <- names(tab)
  as.numeric(class_w[y_chr])
}

complete_prob_cols <- function(probs, levels_order) {
  probs <- as.data.frame(probs)

  # Si predict() devuelve vector porque solo hay 2 clases, convertir a matriz.
  if (ncol(probs) == 1 && !all(levels_order %in% names(probs))) {
    stop("Las probabilidades no tienen columnas de clases reconocibles.")
  }

  for (lvl in levels_order) {
    if (!lvl %in% names(probs)) probs[[lvl]] <- 0
  }

  probs <- probs[, levels_order, drop = FALSE]
  row_sums <- rowSums(probs)
  row_sums[row_sums == 0 | is.na(row_sums)] <- 1
  probs <- probs / row_sums
  probs
}

class_from_probs <- function(prob_matrix) {
  prob_matrix <- as.matrix(prob_matrix)
  colnames(prob_matrix)[max.col(prob_matrix, ties.method = "first")]
}

# ------------------------------
# 3) Preprocesamiento numerico
# ------------------------------
fit_numeric_preprocess <- function(df, cols, scale_data = FALSE) {
  medians <- sapply(cols, function(col) {
    val <- suppressWarnings(median(safe_numeric(df[[col]]), na.rm = TRUE))
    ifelse(is.finite(val), val, 0)
  })

  df_imp <- df
  for (col in cols) {
    df_imp[[col]] <- safe_numeric(df_imp[[col]])
    df_imp[[col]][is.na(df_imp[[col]])] <- medians[[col]]
  }

  means <- rep(0, length(cols))
  sds <- rep(1, length(cols))
  names(means) <- cols
  names(sds) <- cols

  if (scale_data) {
    means <- sapply(cols, function(col) mean(df_imp[[col]], na.rm = TRUE))
    sds <- sapply(cols, function(col) stats::sd(df_imp[[col]], na.rm = TRUE))
    sds[!is.finite(sds) | sds == 0] <- 1
  }

  list(cols = cols, medians = medians, means = means, sds = sds, scale_data = scale_data)
}

apply_numeric_preprocess <- function(df, prep) {
  out <- df[, prep$cols, drop = FALSE]

  for (col in prep$cols) {
    out[[col]] <- safe_numeric(out[[col]])
    out[[col]][is.na(out[[col]])] <- prep$medians[[col]]

    if (prep$scale_data) {
      out[[col]] <- (out[[col]] - prep$means[[col]]) / prep$sds[[col]]
    }
  }

  out
}

# ------------------------------
# 4) Cargar los 7 CSV
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

# Nota metodologica:
# teams_match_features es la matriz principal de entrenamiento porque ya integra
# fuerza Elo, atributos de plantilla, forma reciente y contexto del partido.
# Los otros CSV quedan cargados para reconstruir features de fixtures futuros.

# ------------------------------
# 5) Preparar matriz de entrenamiento
# ------------------------------
class_levels <- c("AWAY_WIN", "DRAW", "HOME_WIN")

tm <- teams_match %>%
  mutate(
    match_date = as.Date(`_date`),
    home_goals = safe_numeric(home_goals),
    away_goals = safe_numeric(away_goals),
    goal_diff = home_goals - away_goals,
    outcome = case_when(
      goal_diff > 0 ~ "HOME_WIN",
      goal_diff == 0 ~ "DRAW",
      goal_diff < 0 ~ "AWAY_WIN",
      TRUE ~ NA_character_
    ),
    outcome = factor(outcome, levels = class_levels)
  )

excluded_cols <- c(
  "home_goals", "away_goals", "_home_team", "_away_team",
  "_date", "_tournament", "match_date", "goal_diff", "outcome"
)

feature_cols <- setdiff(names(tm), excluded_cols)
feature_cols <- feature_cols[sapply(tm[feature_cols], is.numeric)]

model_df <- tm %>%
  filter(
    match_date >= as.Date("2000-01-01"),
    !is.na(outcome),
    !is.na(goal_diff)
  )

# Division temporal: evita entrenar con informacion posterior al periodo de prueba.
split_num <- quantile(as.numeric(model_df$match_date), probs = 0.80, na.rm = TRUE)
split_date <- as.Date(split_num, origin = "1970-01-01")

train_df <- model_df %>% filter(match_date < split_date)
test_df  <- model_df %>% filter(match_date >= split_date)

cat("Filas de entrenamiento:", nrow(train_df), "\n")
cat("Filas de prueba:", nrow(test_df), "\n")
cat("Fecha de corte temporal:", as.character(split_date), "\n")
cat("Numero de variables predictoras:", length(feature_cols), "\n\n")

write_csv(tibble(feature = feature_cols), file.path(OUT_DIR, "classifier_feature_columns_R.csv"))

# ------------------------------
# 6) Preprocesamiento separado por modelo
# ------------------------------
# Logistic multinomial: imputacion + estandarizacion.
prep_logit <- fit_numeric_preprocess(train_df, feature_cols, scale_data = TRUE)
x_train_logit <- apply_numeric_preprocess(train_df, prep_logit)
x_test_logit  <- apply_numeric_preprocess(test_df, prep_logit)

# ExtraTrees: imputacion, sin estandarizar.
prep_tree <- fit_numeric_preprocess(train_df, feature_cols, scale_data = FALSE)
x_train_tree <- apply_numeric_preprocess(train_df, prep_tree)
x_test_tree  <- apply_numeric_preprocess(test_df, prep_tree)

y_train <- train_df$outcome
y_test  <- test_df$outcome
case_w <- balanced_case_weights(y_train)

# ------------------------------
# 7) Modelo 1: regresion logistica multinomial
# ------------------------------
# En Python se uso LogisticRegression(max_iter=500, class_weight="balanced").
# En R, nnet::multinom permite pesos de clase mediante el argumento weights.
# decay agrega regularizacion L2 ligera para mejorar estabilidad numerica.

logit_train_tbl <- bind_cols(tibble(outcome = y_train), as_tibble(x_train_logit))

modelo_logit <- nnet::multinom(
  outcome ~ .,
  data = logit_train_tbl,
  weights = case_w,
  maxit = 500,
  decay = 1e-4,
  trace = FALSE
)

prob_logit <- predict(modelo_logit, newdata = as_tibble(x_test_logit), type = "probs")
prob_logit <- complete_prob_cols(prob_logit, class_levels)
pred_logit <- class_from_probs(prob_logit)

acc_logit <- accuracy_score(y_test, pred_logit)
ll_logit <- log_loss_multiclass(y_test, prob_logit)

# ------------------------------
# 8) Modelo 2: ExtraTrees con ranger
# ------------------------------
# Equivalente metodologico del ExtraTreesClassifier de sklearn:
# - splitrule = "extratrees"
# - probability = TRUE para obtener probabilidades
# - pesos balanceados por clase

extra_train_tbl <- bind_cols(tibble(outcome = y_train), as_tibble(x_train_tree))

mtry_val <- max(1, floor(sqrt(length(feature_cols))))

modelo_extra <- ranger::ranger(
  outcome ~ .,
  data = extra_train_tbl,
  probability = TRUE,
  num.trees = 120,
  mtry = mtry_val,
  splitrule = "extratrees",
  max.depth = 10,
  min.node.size = 20,
  importance = "permutation",
  case.weights = case_w,
  seed = 123
)

prob_extra <- predict(modelo_extra, data = as_tibble(x_test_tree))$predictions
prob_extra <- complete_prob_cols(prob_extra, class_levels)
pred_extra <- class_from_probs(prob_extra)

acc_extra <- accuracy_score(y_test, pred_extra)
ll_extra <- log_loss_multiclass(y_test, prob_extra)

# ------------------------------
# 9) Ensamble: promedio de probabilidades
# ------------------------------
prob_ensemble <- (as.matrix(prob_logit) + as.matrix(prob_extra)) / 2
prob_ensemble <- complete_prob_cols(prob_ensemble, class_levels)
pred_ensemble <- class_from_probs(prob_ensemble)

acc_ensemble <- accuracy_score(y_test, pred_ensemble)
ll_ensemble <- log_loss_multiclass(y_test, prob_ensemble)

# ------------------------------
# 10) Reportes de evaluacion
# ------------------------------
metrics <- tibble(
  model = c("multinomial_logistic", "extra_trees", "ensemble_avg_probs"),
  accuracy = c(acc_logit, acc_extra, acc_ensemble),
  log_loss = c(ll_logit, ll_extra, ll_ensemble),
  train_rows = nrow(train_df),
  test_rows = nrow(test_df),
  split_date = as.character(split_date),
  n_features = length(feature_cols)
)

print(metrics)

write_csv(metrics, file.path(OUT_DIR, "classifier_metrics_R.csv"))
write_json(as.list(metrics), file.path(OUT_DIR, "classifier_metrics_R.json"), pretty = TRUE, auto_unbox = TRUE)

conf_logit <- make_confusion_matrix(y_test, pred_logit, class_levels)
conf_extra <- make_confusion_matrix(y_test, pred_extra, class_levels)
conf_ensemble <- make_confusion_matrix(y_test, pred_ensemble, class_levels)

write_csv(conf_logit, file.path(OUT_DIR, "confusion_logit_R.csv"))
write_csv(conf_extra, file.path(OUT_DIR, "confusion_extra_trees_R.csv"))
write_csv(conf_ensemble, file.path(OUT_DIR, "confusion_ensemble_R.csv"))

test_predictions <- test_df %>%
  transmute(
    match_date,
    home_team = `_home_team`,
    away_team = `_away_team`,
    tournament = `_tournament`,
    actual_result = as.character(outcome),
    p_away_win_logit = prob_logit$AWAY_WIN,
    p_draw_logit = prob_logit$DRAW,
    p_home_win_logit = prob_logit$HOME_WIN,
    pred_logit = pred_logit,
    p_away_win_extra = prob_extra$AWAY_WIN,
    p_draw_extra = prob_extra$DRAW,
    p_home_win_extra = prob_extra$HOME_WIN,
    pred_extra = pred_extra,
    p_away_win_ensemble = prob_ensemble$AWAY_WIN,
    p_draw_ensemble = prob_ensemble$DRAW,
    p_home_win_ensemble = prob_ensemble$HOME_WIN,
    pred_ensemble = pred_ensemble
  )

write_csv(test_predictions, file.path(OUT_DIR, "classifier_test_predictions_R.csv"))

# Importancia de variables ExtraTrees
if (!is.null(modelo_extra$variable.importance)) {
  importance_tbl <- tibble(
    feature = names(modelo_extra$variable.importance),
    importance = as.numeric(modelo_extra$variable.importance)
  ) %>%
    arrange(desc(importance))

  write_csv(importance_tbl, file.path(OUT_DIR, "extra_trees_feature_importance_R.csv"))
}

# Coeficientes de la regresion logistica multinomial
logit_coef <- as.matrix(coef(modelo_logit))
logit_coef_tbl <- as.data.frame(as.table(logit_coef)) %>%
  as_tibble() %>%
  rename(class = Var1, feature = Var2, coefficient = Freq) %>%
  arrange(class, desc(abs(coefficient)))

write_csv(logit_coef_tbl, file.path(OUT_DIR, "logit_coefficients_R.csv"))

# Guardar artefactos entrenados
classifier_artifacts <- list(
  modelo_logit = modelo_logit,
  modelo_extra = modelo_extra,
  prep_logit = prep_logit,
  prep_tree = prep_tree,
  feature_cols = feature_cols,
  class_levels = class_levels,
  split_date = split_date,
  metrics = metrics
)

saveRDS(classifier_artifacts, file.path(OUT_DIR, "worldcup_classifier_models_R.rds"))

# ------------------------------
# 11) Funcion reutilizable para predecir nuevos partidos
# ------------------------------
predict_classifier <- function(new_df, artifacts = classifier_artifacts) {
  x_logit <- apply_numeric_preprocess(new_df, artifacts$prep_logit)
  x_tree  <- apply_numeric_preprocess(new_df, artifacts$prep_tree)

  p_logit <- predict(artifacts$modelo_logit, newdata = as_tibble(x_logit), type = "probs")
  p_logit <- complete_prob_cols(p_logit, artifacts$class_levels)

  p_extra <- predict(artifacts$modelo_extra, data = as_tibble(x_tree))$predictions
  p_extra <- complete_prob_cols(p_extra, artifacts$class_levels)

  p_ens <- (as.matrix(p_logit) + as.matrix(p_extra)) / 2
  p_ens <- complete_prob_cols(p_ens, artifacts$class_levels)

  tibble(
    p_away_win = p_ens$AWAY_WIN,
    p_draw = p_ens$DRAW,
    p_home_win = p_ens$HOME_WIN,
    predicted_result = class_from_probs(p_ens)
  )
}

# ------------------------------
# 12) Construir features para fixtures futuros del Mundial si existen
# ------------------------------
# results_clean trae partidos futuros con home_score y away_score en NA.
# Para esos fixtures se reconstruyen features actuales desde:
# - ultimo Elo observado en teams_match_features
# - ultima forma en teams_form_clean
# - ultima plantilla disponible en players_features
#
# Esto es aproximado, pero reproduce la logica de inferencia del pipeline:
# usar el estado mas reciente disponible antes del torneo.

build_future_fixture_features <- function(fixtures, teams_match, teams_form, players) {
  # Ultimo Elo observado por seleccion
  home_elo_state <- teams_match %>%
    transmute(
      team = norm_team(`_home_team`),
      state_date = as.Date(`_date`),
      elo = safe_numeric(home_elo)
    )

  away_elo_state <- teams_match %>%
    transmute(
      team = norm_team(`_away_team`),
      state_date = as.Date(`_date`),
      elo = safe_numeric(away_elo)
    )

  elo_latest <- bind_rows(home_elo_state, away_elo_state) %>%
    filter(!is.na(team), !is.na(state_date)) %>%
    arrange(team, desc(state_date)) %>%
    group_by(team) %>%
    slice(1) %>%
    ungroup()

  home_elo_tbl <- elo_latest %>%
    transmute(home_team_norm = team, home_elo = elo)

  away_elo_tbl <- elo_latest %>%
    transmute(away_team_norm = team, away_elo = elo)

  # Ultimos datos FIFA/jugadores por seleccion
  players_latest <- players %>%
    mutate(team = norm_team(country)) %>%
    arrange(team, desc(fifa_version)) %>%
    group_by(team) %>%
    slice(1) %>%
    ungroup()

  home_players <- players_latest %>%
    transmute(
      home_team_norm = team,
      home_avg_overall = safe_numeric(overall_strength),
      home_max_overall = safe_numeric(max_overall),
      home_avg_attack = safe_numeric(attack_strength),
      home_avg_defense = safe_numeric(defense_strength),
      home_avg_pace = safe_numeric(avg_pace),
      home_avg_shooting = safe_numeric(avg_shooting),
      home_avg_passing = safe_numeric(avg_passing)
    )

  away_players <- players_latest %>%
    transmute(
      away_team_norm = team,
      away_avg_overall = safe_numeric(overall_strength),
      away_max_overall = safe_numeric(max_overall),
      away_avg_attack = safe_numeric(attack_strength),
      away_avg_defense = safe_numeric(defense_strength),
      away_avg_pace = safe_numeric(avg_pace),
      away_avg_shooting = safe_numeric(avg_shooting),
      away_avg_passing = safe_numeric(avg_passing)
    )

  # Ultima forma reciente por seleccion
  form_latest <- teams_form %>%
    mutate(
      team = norm_team(team),
      form_date = as.Date(match_date)
    ) %>%
    arrange(team, desc(form_date)) %>%
    group_by(team) %>%
    slice(1) %>%
    ungroup()

  home_form <- form_latest %>%
    transmute(
      home_team_norm = team,
      home_form_scored = safe_numeric(avg_goals_scored),
      home_form_conceded = safe_numeric(avg_goals_conceded),
      home_form_win_rate = safe_numeric(win_rate)
    )

  away_form <- form_latest %>%
    transmute(
      away_team_norm = team,
      away_form_scored = safe_numeric(avg_goals_scored),
      away_form_conceded = safe_numeric(avg_goals_conceded),
      away_form_win_rate = safe_numeric(win_rate)
    )

  fixture_features <- fixtures %>%
    mutate(
      fixture_date = as.Date(date),
      home_team_norm = norm_team(home_team),
      away_team_norm = norm_team(away_team),
      is_neutral = as.numeric(is_neutral),
      is_world_cup = as.numeric(is_world_cup),
      is_continental = 0
    ) %>%
    left_join(home_elo_tbl, by = "home_team_norm") %>%
    left_join(away_elo_tbl, by = "away_team_norm") %>%
    left_join(home_players, by = "home_team_norm") %>%
    left_join(away_players, by = "away_team_norm") %>%
    left_join(home_form, by = "home_team_norm") %>%
    left_join(away_form, by = "away_team_norm") %>%
    mutate(
      elo_diff = home_elo - away_elo,
      overall_diff = home_avg_overall - away_avg_overall,
      attack_diff = home_avg_attack - away_avg_attack,
      defense_diff = home_avg_defense - away_avg_defense
    )

  # Asegurar que todas las columnas de entrenamiento existan.
  for (col in feature_cols) {
    if (!col %in% names(fixture_features)) fixture_features[[col]] <- NA_real_
  }

  fixture_features
}

future_worldcup_fixtures <- results %>%
  filter(
    is_world_cup == 1,
    is.na(home_score),
    is.na(away_score)
  )

if (nrow(future_worldcup_fixtures) > 0) {
  future_features <- build_future_fixture_features(
    fixtures = future_worldcup_fixtures,
    teams_match = teams_match,
    teams_form = teams_form,
    players = players
  )

  future_probs <- predict_classifier(future_features)

  future_predictions <- future_features %>%
    transmute(
      date = fixture_date,
      home_team,
      away_team,
      tournament,
      city,
      country,
      is_neutral
    ) %>%
    bind_cols(future_probs) %>%
    mutate(
      predicted_match_winner_no_draw = if_else(
        p_home_win >= p_away_win,
        home_team,
        away_team
      ),
      confidence_no_draw = pmax(p_home_win, p_away_win),
      expected_home_points = 3 * p_home_win + 1 * p_draw,
      expected_away_points = 3 * p_away_win + 1 * p_draw
    ) %>%
    arrange(date, home_team)

  write_csv(future_predictions, file.path(OUT_DIR, "worldcup_future_classifier_predictions_R.csv"))

  cat("\nPredicciones futuras del Mundial exportadas en:\n")
  cat(file.path(OUT_DIR, "worldcup_future_classifier_predictions_R.csv"), "\n")


  # ------------------------------
  # 13) Ranking por pais desde el clasificador
  # ------------------------------
  # El clasificador predice probabilidades por partido. Para obtener una
  # probabilidad/fuerza por seleccion se agregan las probabilidades de sus
  # 3 partidos de fase de grupos:
  # - expected_points: suma de puntos esperados.
  # - avg_win_probability: promedio de probabilidad de victoria por partido.
  # - score: indice 0-1 normalizado para ordenar favoritos.

  worldcup_groups_2026 <- tribble(
    ~team_norm, ~group,
    "MEXICO", "A", "SOUTH AFRICA", "A", "SOUTH KOREA", "A", "CZECH REPUBLIC", "A",
    "CANADA", "B", "QATAR", "B", "SWITZERLAND", "B", "BOSNIA AND HERZEGOVINA", "B",
    "UNITED STATES", "C", "PARAGUAY", "C", "AUSTRALIA", "C", "TURKEY", "C",
    "BRAZIL", "D", "MOROCCO", "D", "HAITI", "D", "SCOTLAND", "D",
    "GERMANY", "E", "CURAÇAO", "E", "IVORY COAST", "E", "ECUADOR", "E",
    "NETHERLANDS", "F", "JAPAN", "F", "SWEDEN", "F", "TUNISIA", "F",
    "BELGIUM", "G", "EGYPT", "G", "IRAN", "G", "NEW ZEALAND", "G",
    "SPAIN", "H", "CAPE VERDE", "H", "SAUDI ARABIA", "H", "URUGUAY", "H",
    "FRANCE", "I", "SENEGAL", "I", "IRAQ", "I", "NORWAY", "I",
    "ARGENTINA", "J", "ALGERIA", "J", "AUSTRIA", "J", "JORDAN", "J",
    "PORTUGAL", "K", "UZBEKISTAN", "K", "COLOMBIA", "K", "DR CONGO", "K",
    "ENGLAND", "L", "CROATIA", "L", "GHANA", "L", "PANAMA", "L"
  )

  minmax01 <- function(x) {
    lo <- min(x, na.rm = TRUE)
    hi <- max(x, na.rm = TRUE)
    if (!is.finite(lo) || !is.finite(hi) || hi == lo) return(rep(0.5, length(x)))
    (x - lo) / (hi - lo)
  }

  home_team_probs <- future_predictions %>%
    transmute(
      team = home_team,
      team_norm = norm_team(home_team),
      p_win = p_home_win,
      p_draw = p_draw,
      p_loss = p_away_win,
      expected_points = expected_home_points
    )

  away_team_probs <- future_predictions %>%
    transmute(
      team = away_team,
      team_norm = norm_team(away_team),
      p_win = p_away_win,
      p_draw = p_draw,
      p_loss = p_home_win,
      expected_points = expected_away_points
    )

  worldcup_team_probabilities <- bind_rows(home_team_probs, away_team_probs) %>%
    left_join(worldcup_groups_2026, by = "team_norm") %>%
    group_by(team_norm, group) %>%
    summarise(
      team = first(team),
      matches = n(),
      expected_points = sum(expected_points, na.rm = TRUE),
      expected_points_per_match = expected_points / matches,
      avg_win_probability = mean(p_win, na.rm = TRUE),
      min_win_probability = min(p_win, na.rm = TRUE),
      max_win_probability = max(p_win, na.rm = TRUE),
      avg_draw_probability = mean(p_draw, na.rm = TRUE),
      avg_loss_probability = mean(p_loss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Score de fuerza de grupo basado SOLO en el clasificador.
      # 70% puntos esperados normalizados + 30% probabilidad media de victoria.
      raw_classifier_strength = 0.70 * (expected_points_per_match / 3) +
        0.30 * avg_win_probability,
      score = minmax01(raw_classifier_strength)
    ) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number()) %>%
    select(
      rank, team, group, score,
      expected_points, expected_points_per_match,
      avg_win_probability, min_win_probability, max_win_probability,
      avg_draw_probability, avg_loss_probability, matches,
      raw_classifier_strength
    )

  worldcup_country_ranking_compact <- worldcup_team_probabilities %>%
    transmute(
      Rank = rank,
      Equipo = str_to_title(team),
      Grupo = group,
      Score = round(score, 3)
    )

  write_csv(
    worldcup_team_probabilities,
    file.path(OUT_DIR, "worldcup_team_probabilities_classifier_R.csv")
  )

  write_csv(
    worldcup_country_ranking_compact,
    file.path(OUT_DIR, "worldcup_country_ranking_classifier_R.csv")
  )

  cat("\nRanking por pais exportado en:\n")
  cat(file.path(OUT_DIR, "worldcup_country_ranking_classifier_R.csv"), "\n")
  cat(file.path(OUT_DIR, "worldcup_team_probabilities_classifier_R.csv"), "\n")

} else {
  cat("\nNo se encontraron fixtures futuros del Mundial con scores NA en results_clean.\n")
}

cat("\nProceso terminado. Archivos guardados en:", OUT_DIR, "\n")
