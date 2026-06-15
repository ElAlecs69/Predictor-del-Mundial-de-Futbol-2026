# ============================================================
# PCA - ANALISIS DE COMPONENTES PRINCIPALES
# Proyecto: Prediccion Mundial FIFA 2026
#
# Entradas esperadas en la misma carpeta del script:
# - teams_match_features*.csv
# - teams_form_clean*.csv
# - results_clean*.csv
# - players_features*.csv
# - goals_features*.csv
# - shootouts_features*.csv
# - former_names_clean*.csv
#
# Salidas principales:
# - outputs_pca_R/pca_summary.csv
# - outputs_pca_R/<dataset>/explained_variance.csv
# - outputs_pca_R/<dataset>/loadings.csv
# - outputs_pca_R/<dataset>/scores.csv
# - outputs_pca_R/<dataset>/top_loadings.csv
# - outputs_pca_R/<dataset>/plots/*.png
# ============================================================

# ------------------------------
# 0) Paquetes
# ------------------------------
required_packages <- c(
  "readr", "dplyr", "stringr", "tibble", "purrr",
  "ggplot2", "tidyr", "jsonlite"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(purrr)
library(ggplot2)
library(tidyr)
library(jsonlite)
library(grid)

set.seed(123)

# ------------------------------
# 1) Configuracion
# ------------------------------
BASE_DIR <- "."
OUT_DIR <- "outputs_pca_R"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# Maximo de puntos para graficas de scores.
# Los CSV de scores se guardan completos; solo se muestrean las graficas para no saturar R.
PLOT_SAMPLE_N <- 5000

pick_file <- function(pattern) {
  files <- list.files(BASE_DIR, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    warning(paste("No se encontro ningun archivo con patron:", pattern))
    return(NA_character_)
  }
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_filename <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9_\\-]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")
}

norm_team <- function(x) {
  x %>% as.character() %>% str_squish() %>% str_to_upper()
}

# ------------------------------
# 2) Funciones auxiliares PCA
# ------------------------------
median_impute_matrix <- function(df_num) {
  out <- df_num

  medians <- sapply(names(out), function(col) {
    val <- suppressWarnings(median(out[[col]], na.rm = TRUE))
    ifelse(is.finite(val), val, 0)
  })

  for (col in names(out)) {
    out[[col]] <- safe_numeric(out[[col]])
    out[[col]][is.na(out[[col]])] <- medians[[col]]
  }

  list(data = out, medians = medians)
}

remove_bad_numeric_columns <- function(df_num, min_complete_ratio = 0.50) {
  # Elimina columnas con demasiados NA, infinitos, varianza cero o columna constante.
  cleaned <- df_num %>%
    mutate(across(everything(), ~ safe_numeric(.x))) %>%
    mutate(across(everything(), ~ ifelse(is.infinite(.x), NA_real_, .x)))

  complete_ratio <- sapply(cleaned, function(x) mean(!is.na(x)))
  keep_complete <- names(complete_ratio)[complete_ratio >= min_complete_ratio]

  cleaned <- cleaned[, keep_complete, drop = FALSE]

  if (ncol(cleaned) == 0) {
    return(list(data = cleaned, removed = tibble(feature = character(), reason = character())))
  }

  vars <- sapply(cleaned, function(x) {
    x2 <- x[!is.na(x)]
    if (length(x2) <= 1) return(0)
    var(x2)
  })

  keep_var <- names(vars)[is.finite(vars) & vars > 0]
  removed_complete <- setdiff(names(df_num), keep_complete)
  removed_var <- setdiff(names(cleaned), keep_var)

  removed <- bind_rows(
    tibble(feature = removed_complete, reason = "too_many_missing_values"),
    tibble(feature = removed_var, reason = "zero_variance_or_constant")
  )

  list(data = cleaned[, keep_var, drop = FALSE], removed = removed)
}

choose_color_column <- function(label_df) {
  # Elige automaticamente una columna categorica pequena para colorear graficas.
  if (ncol(label_df) == 0) return(NULL)

  candidates <- names(label_df)
  for (col in candidates) {
    vals <- label_df[[col]]
    n_unique <- length(unique(vals[!is.na(vals)]))
    if (n_unique >= 2 && n_unique <= 12) return(col)
  }
  NULL
}

plot_scree <- function(explained, dataset_dir, dataset_name) {
  p <- ggplot(explained, aes(x = component_index, y = variance_pct)) +
    geom_col(alpha = 0.75) +
    geom_line(aes(y = cumulative_pct), linewidth = 0.8) +
    geom_point(aes(y = cumulative_pct), size = 1.5) +
    scale_x_continuous(breaks = explained$component_index) +
    labs(
      title = paste("PCA - Varianza explicada:", dataset_name),
      subtitle = "Barras = varianza por componente; linea = varianza acumulada.",
      x = "Componente principal",
      y = "Porcentaje de varianza explicada"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(dataset_dir, "plots", "01_scree_variance.png"),
    plot = p,
    width = 9,
    height = 5,
    dpi = 150
  )
}

plot_scores <- function(scores, labels, explained, dataset_dir, dataset_name, sample_n = PLOT_SAMPLE_N) {
  if (!all(c("PC1", "PC2") %in% names(scores))) return(NULL)

  plot_df <- bind_cols(scores %>% select(row_id, PC1, PC2), labels)
  if (nrow(plot_df) > sample_n) {
    plot_df <- plot_df %>% slice_sample(n = sample_n)
  }

  color_col <- choose_color_column(labels)

  if (!is.null(color_col) && color_col %in% names(plot_df)) {
    p <- ggplot(plot_df, aes(x = PC1, y = PC2, color = .data[[color_col]])) +
      geom_point(alpha = 0.55, size = 1.4) +
      labs(
        title = paste("PCA - Scores PC1 vs PC2:", dataset_name),
        subtitle = paste0(
          "PC1 = ", round(explained$variance_pct[1], 2),
          "% | PC2 = ", round(explained$variance_pct[2], 2),
          "% | color = ", color_col
        ),
        x = "PC1",
        y = "PC2",
        color = color_col
      ) +
      theme_minimal()
  } else {
    p <- ggplot(plot_df, aes(x = PC1, y = PC2)) +
      geom_point(alpha = 0.45, size = 1.2) +
      labs(
        title = paste("PCA - Scores PC1 vs PC2:", dataset_name),
        subtitle = paste0(
          "PC1 = ", round(explained$variance_pct[1], 2),
          "% | PC2 = ", round(explained$variance_pct[2], 2),
          "%"
        ),
        x = "PC1",
        y = "PC2"
      ) +
      theme_minimal()
  }

  ggsave(
    filename = file.path(dataset_dir, "plots", "02_scores_PC1_PC2.png"),
    plot = p,
    width = 8,
    height = 6,
    dpi = 150
  )
}

plot_loadings_for_pc <- function(loadings_long, pc_name, dataset_dir, dataset_name, top_n = 15) {
  pc_df <- loadings_long %>%
    filter(component == pc_name) %>%
    mutate(abs_loading = abs(loading)) %>%
    arrange(desc(abs_loading)) %>%
    slice_head(n = top_n) %>%
    mutate(feature = reorder(feature, loading))

  if (nrow(pc_df) == 0) return(NULL)

  p <- ggplot(pc_df, aes(x = feature, y = loading)) +
    geom_col(alpha = 0.75) +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("PCA - Cargas principales", pc_name, ":", dataset_name),
      subtitle = "Variables con mayor contribucion absoluta al componente.",
      x = "Variable",
      y = "Loading / carga"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(dataset_dir, "plots", paste0("03_top_loadings_", pc_name, ".png")),
    plot = p,
    width = 9,
    height = 6,
    dpi = 150
  )
}

plot_biplot <- function(scores, loadings, explained, dataset_dir, dataset_name, sample_n = PLOT_SAMPLE_N) {
  if (!all(c("PC1", "PC2") %in% names(scores)) || !all(c("PC1", "PC2") %in% names(loadings))) {
    return(NULL)
  }

  scores_plot <- scores %>% select(PC1, PC2)
  if (nrow(scores_plot) > sample_n) {
    scores_plot <- scores_plot %>% slice_sample(n = sample_n)
  }

  loading_plot <- loadings %>%
    mutate(contribution = sqrt(PC1^2 + PC2^2)) %>%
    arrange(desc(contribution)) %>%
    slice_head(n = 12)

  # Factor de escala para que las flechas se vean sobre el espacio de scores.
  sx <- max(abs(scores_plot$PC1), na.rm = TRUE)
  sy <- max(abs(scores_plot$PC2), na.rm = TRUE)
  sf <- min(sx, sy) * 0.75

  loading_plot <- loading_plot %>%
    mutate(
      xend = PC1 * sf,
      yend = PC2 * sf
    )

  p <- ggplot(scores_plot, aes(x = PC1, y = PC2)) +
    geom_point(alpha = 0.25, size = 1) +
    geom_segment(
      data = loading_plot,
      aes(x = 0, y = 0, xend = xend, yend = yend),
      arrow = arrow(length = unit(0.18, "cm")),
      inherit.aes = FALSE
    ) +
    geom_text(
      data = loading_plot,
      aes(x = xend, y = yend, label = feature),
      inherit.aes = FALSE,
      size = 3,
      vjust = -0.4
    ) +
    labs(
      title = paste("PCA - Biplot PC1 vs PC2:", dataset_name),
      subtitle = paste0(
        "Puntos = observaciones; flechas = variables. PC1 ",
        round(explained$variance_pct[1], 2), "%, PC2 ",
        round(explained$variance_pct[2], 2), "%."
      ),
      x = "PC1",
      y = "PC2"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(dataset_dir, "plots", "04_biplot_PC1_PC2.png"),
    plot = p,
    width = 9,
    height = 7,
    dpi = 150
  )
}

run_pca_for_dataset <- function(dataset_name, df, numeric_cols = NULL, label_cols = NULL, exclude_cols = NULL, min_complete_ratio = 0.50) {
  message("\n========== PCA: ", dataset_name, " ==========")

  dataset_dir <- file.path(OUT_DIR, safe_filename(dataset_name))
  plots_dir <- file.path(dataset_dir, "plots")
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

  if (is.null(numeric_cols)) {
    numeric_cols <- names(df)[sapply(df, is.numeric)]
  }

  numeric_cols <- setdiff(numeric_cols, exclude_cols)
  numeric_cols <- intersect(numeric_cols, names(df))

  label_cols <- intersect(label_cols, names(df))
  labels <- df[, label_cols, drop = FALSE] %>% as_tibble()

  if (length(numeric_cols) < 2) {
    warning("Dataset ", dataset_name, " no tiene suficientes variables numericas para PCA.")
    write_csv(
      tibble(dataset = dataset_name, status = "skipped", reason = "less_than_two_numeric_columns"),
      file.path(dataset_dir, "pca_skipped.csv")
    )
    return(tibble(
      dataset = dataset_name,
      status = "skipped",
      rows = nrow(df),
      original_numeric_features = length(numeric_cols),
      used_features = 0,
      PC1_variance_pct = NA_real_,
      PC2_variance_pct = NA_real_,
      cumulative_PC2_pct = NA_real_,
      components_80pct = NA_integer_,
      components_90pct = NA_integer_
    ))
  }

  raw_num <- df[, numeric_cols, drop = FALSE] %>% as_tibble()
  cleaned <- remove_bad_numeric_columns(raw_num, min_complete_ratio = min_complete_ratio)

  if (nrow(cleaned$removed) > 0) {
    write_csv(cleaned$removed, file.path(dataset_dir, "removed_features.csv"))
  } else {
    write_csv(tibble(feature = character(), reason = character()), file.path(dataset_dir, "removed_features.csv"))
  }

  if (ncol(cleaned$data) < 2) {
    warning("Dataset ", dataset_name, " quedo con menos de 2 variables utiles despues de limpieza.")
    write_csv(
      tibble(dataset = dataset_name, status = "skipped", reason = "less_than_two_valid_features_after_cleaning"),
      file.path(dataset_dir, "pca_skipped.csv")
    )
    return(tibble(
      dataset = dataset_name,
      status = "skipped",
      rows = nrow(df),
      original_numeric_features = length(numeric_cols),
      used_features = ncol(cleaned$data),
      PC1_variance_pct = NA_real_,
      PC2_variance_pct = NA_real_,
      cumulative_PC2_pct = NA_real_,
      components_80pct = NA_integer_,
      components_90pct = NA_integer_
    ))
  }

  imp <- median_impute_matrix(cleaned$data)
  X <- as.matrix(imp$data)

  # PCA con estandarizacion: necesario porque las variables tienen escalas distintas.
  pca <- prcomp(X, center = TRUE, scale. = TRUE)

  eigenvalues <- pca$sdev^2
  explained <- tibble(
    component_index = seq_along(eigenvalues),
    component = paste0("PC", component_index),
    eigenvalue = eigenvalues,
    variance_prop = eigenvalues / sum(eigenvalues),
    variance_pct = variance_prop * 100,
    cumulative_prop = cumsum(variance_prop),
    cumulative_pct = cumulative_prop * 100
  )

  loadings <- as.data.frame(pca$rotation) %>%
    rownames_to_column("feature") %>%
    as_tibble()

  loadings_long <- loadings %>%
    pivot_longer(
      cols = starts_with("PC"),
      names_to = "component",
      values_to = "loading"
    ) %>%
    mutate(abs_loading = abs(loading)) %>%
    arrange(component, desc(abs_loading))

  scores <- as.data.frame(pca$x) %>%
    as_tibble() %>%
    mutate(row_id = row_number(), .before = 1)

  scores_with_labels <- bind_cols(scores, labels)

  top_loadings <- loadings_long %>%
    group_by(component) %>%
    slice_max(order_by = abs_loading, n = 10, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(component, desc(abs_loading))

  write_csv(explained, file.path(dataset_dir, "explained_variance.csv"))
  write_csv(loadings, file.path(dataset_dir, "loadings.csv"))
  write_csv(loadings_long, file.path(dataset_dir, "loadings_long.csv"))
  write_csv(top_loadings, file.path(dataset_dir, "top_loadings.csv"))
  write_csv(scores_with_labels, file.path(dataset_dir, "scores.csv"))
  write_csv(tibble(feature = names(imp$medians), median_used = as.numeric(imp$medians)), file.path(dataset_dir, "imputation_medians.csv"))
  saveRDS(pca, file.path(dataset_dir, "pca_model.rds"))

  # Graficas
  plot_scree(explained, dataset_dir, dataset_name)
  plot_scores(scores, labels, explained, dataset_dir, dataset_name)
  plot_loadings_for_pc(loadings_long, "PC1", dataset_dir, dataset_name, top_n = 15)
  plot_loadings_for_pc(loadings_long, "PC2", dataset_dir, dataset_name, top_n = 15)
  plot_biplot(scores, loadings, explained, dataset_dir, dataset_name)

  components_80 <- explained %>% filter(cumulative_prop >= 0.80) %>% slice(1) %>% pull(component_index)
  components_90 <- explained %>% filter(cumulative_prop >= 0.90) %>% slice(1) %>% pull(component_index)

  if (length(components_80) == 0) components_80 <- NA_integer_
  if (length(components_90) == 0) components_90 <- NA_integer_

  tibble(
    dataset = dataset_name,
    status = "ok",
    rows = nrow(df),
    original_numeric_features = length(numeric_cols),
    used_features = ncol(X),
    removed_features = nrow(cleaned$removed),
    PC1_variance_pct = explained$variance_pct[1],
    PC2_variance_pct = ifelse(nrow(explained) >= 2, explained$variance_pct[2], NA_real_),
    cumulative_PC2_pct = ifelse(nrow(explained) >= 2, explained$cumulative_pct[2], NA_real_),
    components_80pct = components_80,
    components_90pct = components_90
  )
}

# ------------------------------
# 3) Carga de CSV
# ------------------------------
files <- list(
  teams_form = pick_file("^teams_form_clean.*\\.csv$"),
  shootouts = pick_file("^shootouts_features.*\\.csv$"),
  teams_match = pick_file("^teams_match_features.*\\.csv$"),
  results = pick_file("^results_clean.*\\.csv$"),
  players = pick_file("^players_features.*\\.csv$"),
  goals = pick_file("^goals_features.*\\.csv$"),
  former_names = pick_file("^former_names_clean.*\\.csv$")
)

print(files)

read_if_exists <- function(path) {
  if (is.na(path)) return(NULL)
  read_csv(path, show_col_types = FALSE)
}

teams_form <- read_if_exists(files$teams_form)
shootouts <- read_if_exists(files$shootouts)
teams_match <- read_if_exists(files$teams_match)
results_clean <- read_if_exists(files$results)
players <- read_if_exists(files$players)
goals <- read_if_exists(files$goals)
former_names <- read_if_exists(files$former_names)

write_json(files, file.path(OUT_DIR, "input_files_used.json"), pretty = TRUE, auto_unbox = TRUE)

# ------------------------------
# 4) Configuracion de PCA por dataset
# ------------------------------
summaries <- list()

# 4.1 PCA de forma reciente de equipos
# Variables: goles anotados/recibidos, tasa de victoria, balance, eficiencia ofensiva y riesgo defensivo.
if (!is.null(teams_form)) {
  summaries$teams_form <- run_pca_for_dataset(
    dataset_name = "teams_form_recent_performance",
    df = teams_form,
    label_cols = c("team", "match_date"),
    exclude_cols = c(),
    min_complete_ratio = 0.50
  )
}

# 4.2 PCA de tandas de penales
# is_shootout suele ser constante; el script la elimina automaticamente por varianza cero.
if (!is.null(shootouts)) {
  summaries$shootouts <- run_pca_for_dataset(
    dataset_name = "shootouts_penalty_features",
    df = shootouts,
    label_cols = c("date", "home_team", "away_team"),
    exclude_cols = c(),
    min_complete_ratio = 0.50
  )
}

# 4.3 PCA de variables predictoras de partidos
# Se excluyen goles reales y columnas identificadoras para evitar fuga de informacion.
if (!is.null(teams_match)) {
  teams_match <- teams_match %>%
    mutate(
      goal_diff = safe_numeric(home_goals) - safe_numeric(away_goals),
      total_goals = safe_numeric(home_goals) + safe_numeric(away_goals)
    )

  summaries$teams_match_predictors <- run_pca_for_dataset(
    dataset_name = "teams_match_predictors_no_leakage",
    df = teams_match,
    label_cols = c("_date", "_home_team", "_away_team", "_tournament"),
    exclude_cols = c(
      "home_goals", "away_goals", "goal_diff", "total_goals"
    ),
    min_complete_ratio = 0.50
  )
}

# 4.4 PCA de resultados historicos
# Se excluyen year/month/day si se quiere analizar rendimiento y no temporalidad pura.
if (!is.null(results_clean)) {
  summaries$results <- run_pca_for_dataset(
    dataset_name = "results_match_outcomes_context",
    df = results_clean,
    label_cols = c("date", "home_team", "away_team", "tournament"),
    exclude_cols = c("year", "month", "day"),
    min_complete_ratio = 0.50
  )
}

# 4.5 PCA de calidad de jugadores/plantilla
# Se excluye fifa_version porque es identificador temporal de version, no rendimiento directo.
if (!is.null(players)) {
  summaries$players <- run_pca_for_dataset(
    dataset_name = "players_squad_strength",
    df = players,
    label_cols = c("country", "fifa_version"),
    exclude_cols = c("fifa_version"),
    min_complete_ratio = 0.50
  )
}

# 4.6 PCA de eventos de goles
if (!is.null(goals)) {
  summaries$goals <- run_pca_for_dataset(
    dataset_name = "goals_event_features",
    df = goals,
    label_cols = c("date", "home_team", "away_team"),
    exclude_cols = c(),
    min_complete_ratio = 0.50
  )
}

# 4.7 former_names
# No se aplica PCA porque contiene variables categoricas/textuales de normalizacion de nombres.
if (!is.null(former_names)) {
  former_dir <- file.path(OUT_DIR, "former_names_clean_skipped")
  if (!dir.exists(former_dir)) dir.create(former_dir, recursive = TRUE)
  write_csv(
    tibble(
      dataset = "former_names_clean",
      status = "skipped",
      reason = "No se aplica PCA porque contiene principalmente variables categoricas/textuales de normalizacion de nombres.",
      rows = nrow(former_names),
      columns = ncol(former_names)
    ),
    file.path(former_dir, "pca_skipped.csv")
  )
  summaries$former_names <- tibble(
    dataset = "former_names_clean",
    status = "skipped",
    rows = nrow(former_names),
    original_numeric_features = 0,
    used_features = 0,
    removed_features = NA_integer_,
    PC1_variance_pct = NA_real_,
    PC2_variance_pct = NA_real_,
    cumulative_PC2_pct = NA_real_,
    components_80pct = NA_integer_,
    components_90pct = NA_integer_
  )
}

# ------------------------------
# 5) Exportar resumen global
# ------------------------------
pca_summary <- bind_rows(summaries)
write_csv(pca_summary, file.path(OUT_DIR, "pca_summary.csv"))

# Archivo metodologico
methodology <- list(
  method = "Principal Component Analysis (PCA)",
  implementation = "stats::prcomp",
  center = TRUE,
  scale = TRUE,
  imputation = "Median imputation fitted per dataset before PCA",
  zero_variance_columns = "Removed",
  missing_value_rule = "Columns with less than 50% complete values are removed",
  plot_sample_n = PLOT_SAMPLE_N,
  notes = c(
    "PCA is unsupervised and does not use target labels.",
    "For teams_match_features, target/result columns such as home_goals and away_goals were excluded to avoid leakage.",
    "former_names_clean is not suitable for PCA because it is categorical/textual."
  )
)

write_json(methodology, file.path(OUT_DIR, "pca_methodology.json"), pretty = TRUE, auto_unbox = TRUE)

cat("\n============================================================\n")
cat("PCA terminado.\n")
cat("Archivos guardados en:", OUT_DIR, "\n")
cat("Resumen global:", file.path(OUT_DIR, "pca_summary.csv"), "\n")
cat("============================================================\n\n")

print(pca_summary)
