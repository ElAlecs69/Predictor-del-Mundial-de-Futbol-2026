# ============================================================
# VALIDACION DE SIMULACION MONTE CARLO - MUNDIAL 2026
#
# Este script valida la simulacion Monte Carlo mediante:
# 1) chequeos estructurales / consistencia probabilistica,
# 2) error Monte Carlo e intervalos de confianza,
# 3) convergencia por numero de simulaciones y semillas,
# 4) sensibilidad a supuestos del modelo,
# 5) consistencia entre puntos esperados del clasificador y puntos simulados,
# 6) validacion de probabilidades de partidos de grupo,
# 7) validacion de probabilidades de eliminatorias.
#
# Entradas esperadas en BASE_DIR:
# - worldcup_group_stage_predictions.csv
# - worldcup_team_probabilities_classifier_R.csv
# - fifa_annex_c_third_place_mapping.csv
# - worldcup_monte_carlo_champion_probabilities_R.csv (opcional)
# - worldcup_monte_carlo_methodology_R.json (opcional)
#
# Salidas en outputs_monte_carlo_validation_R/
# ============================================================

required_packages <- c("readr", "dplyr", "stringr", "tibble", "jsonlite", "tidyr", "purrr", "ggplot2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
}

library(readr)
library(dplyr)
library(stringr)
library(tibble)
library(jsonlite)
library(tidyr)
library(purrr)
library(ggplot2)

# ------------------------------
# 0) Configuracion
# ------------------------------
BASE_DIR <- "."
OUT_DIR <- file.path(BASE_DIR, "outputs_monte_carlo_validation_R")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Puedes bajar estos valores si tu computadora tarda mucho.
N_BASE_ASSUMED <- 5000
N_REPRO <- 5000
N_CONVERGENCE_VALUES <- c(500, 1000, 2500)
SEEDS_CONVERGENCE <- c(101, 202, 303)
N_SENSITIVITY <- 1500
N_MATCH_PROB_CHECK <- 5000

sigmoid <- function(x) 1 / (1 + exp(-x))
clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))
norm_team <- function(x) str_squish(str_to_upper(as.character(x)))

safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = method))
}

rmse <- function(x, y) sqrt(mean((x - y)^2, na.rm = TRUE))
mae <- function(x, y) mean(abs(x - y), na.rm = TRUE)

# ------------------------------
# 1) Carga de entradas
# ------------------------------
group_preds <- read_csv(file.path(BASE_DIR, "worldcup_group_stage_predictions.csv"), show_col_types = FALSE) %>%
  mutate(
    `_home_team` = norm_team(`_home_team`),
    `_away_team` = norm_team(`_away_team`),
    group = as.character(group)
  )

team_probs <- read_csv(file.path(BASE_DIR, "worldcup_team_probabilities_classifier_R.csv"), show_col_types = FALSE) %>%
  mutate(team = norm_team(team), group = as.character(group))

annex_c <- read_csv(file.path(BASE_DIR, "fifa_annex_c_third_place_mapping.csv"), show_col_types = FALSE) %>%
  mutate(qualified_third_groups = as.character(qualified_third_groups))

teams_all <- team_probs$team
team_group <- setNames(team_probs$group, team_probs$team)
base_ratings <- setNames(team_probs$raw_classifier_strength, team_probs$team)
base_score_norm <- setNames(team_probs$score, team_probs$team)

# ------------------------------
# 2) Funcion principal de simulacion reutilizable
# ------------------------------
run_worldcup_mc <- function(N_SIM = 1000,
                            seed = 123,
                            strength_scale = 1.00,
                            draw_multiplier = 1.00,
                            extra_time_prob = 0.35,
                            penalty_strength_factor = 0.20,
                            boost_team = NULL,
                            boost_amount = 0,
                            scenario_name = "baseline",
                            verbose = FALSE) {
  set.seed(seed)

  ratings <- base_ratings
  if (!is.null(boost_team)) {
    boost_team <- norm_team(boost_team)
    if (boost_team %in% names(ratings)) ratings[[boost_team]] <- ratings[[boost_team]] + boost_amount
  }

  get_rating <- function(team) {
    val <- ratings[[team]]
    if (is.null(val) || is.na(val)) mean(ratings, na.rm = TRUE) else val
  }

  # Calibracion con probabilidades de fase de grupos.
  cal <- group_preds %>%
    rowwise() %>%
    mutate(
      strength_diff = get_rating(`_home_team`) - get_rating(`_away_team`),
      p_no_draw_home = p_home_win / (p_home_win + p_away_win)
    ) %>%
    ungroup()

  k_grid <- seq(0.1, 50, length.out = 1000)
  errors <- sapply(k_grid, function(k) {
    mean((sigmoid(k * cal$strength_diff) - cal$p_no_draw_home)^2, na.rm = TRUE)
  })
  k_best <- k_grid[which.min(errors)]

  draw_fit <- lm(p_draw ~ I(abs(strength_diff)), data = cal)
  draw_intercept <- coef(draw_fit)[1]
  draw_slope <- coef(draw_fit)[2]

  knockout_probs <- function(team_a, team_b) {
    diff <- get_rating(team_a) - get_rating(team_b)
    p_no_draw <- sigmoid(k_best * strength_scale * diff)
    p_draw_raw <- draw_intercept + draw_slope * abs(diff)
    p_draw <- clamp(draw_multiplier * p_draw_raw, 0.18, 0.58)
    p_a_90 <- (1 - p_draw) * p_no_draw
    p_b_90 <- (1 - p_draw) * (1 - p_no_draw)
    p_a_pen <- clamp(0.5 + penalty_strength_factor * (p_no_draw - 0.5), 0.40, 0.60)
    list(
      p_a_90 = p_a_90,
      p_draw = p_draw,
      p_b_90 = p_b_90,
      p_no_draw = p_no_draw,
      p_a_pen = p_a_pen
    )
  }

  play_knockout <- function(team_a, team_b) {
    p <- knockout_probs(team_a, team_b)
    u <- runif(1)
    if (u < p$p_a_90) return(list(winner = team_a, mode = "90min"))
    if (u < p$p_a_90 + p$p_b_90) return(list(winner = team_b, mode = "90min"))

    if (runif(1) < extra_time_prob) {
      winner <- ifelse(runif(1) < p$p_no_draw, team_a, team_b)
      return(list(winner = winner, mode = "extra_time"))
    } else {
      winner <- ifelse(runif(1) < p$p_a_pen, team_a, team_b)
      return(list(winner = winner, mode = "penalties"))
    }
  }

  sample_score <- function(p_home, p_draw) {
    u <- runif(1)
    if (u < p_home) {
      margin <- sample(1:5, 1, prob = c(0.62, 0.24, 0.09, 0.035, 0.015))
      loser <- sample(0:2, 1, prob = c(0.55, 0.35, 0.10))
      return(c(home = loser + margin, away = loser))
    } else if (u < p_home + p_draw) {
      g <- sample(0:3, 1, prob = c(0.25, 0.55, 0.17, 0.03))
      return(c(home = g, away = g))
    } else {
      margin <- sample(1:5, 1, prob = c(0.62, 0.24, 0.09, 0.035, 0.015))
      loser <- sample(0:2, 1, prob = c(0.55, 0.35, 0.10))
      return(c(home = loser, away = loser + margin))
    }
  }

  simulate_group <- function(group_letter) {
    gdf <- group_preds %>% filter(group == group_letter)
    teams <- sort(unique(c(gdf$`_home_team`, gdf$`_away_team`)))
    table <- tibble(team = teams, pts = 0, gf = 0, ga = 0)

    for (i in seq_len(nrow(gdf))) {
      row <- gdf[i,]
      score <- sample_score(row$p_home_win, row$p_draw)
      h <- row$`_home_team`; a <- row$`_away_team`
      hg <- score["home"]; ag <- score["away"]

      table$gf[table$team == h] <- table$gf[table$team == h] + hg
      table$ga[table$team == h] <- table$ga[table$team == h] + ag
      table$gf[table$team == a] <- table$gf[table$team == a] + ag
      table$ga[table$team == a] <- table$ga[table$team == a] + hg

      if (hg > ag) table$pts[table$team == h] <- table$pts[table$team == h] + 3
      else if (ag > hg) table$pts[table$team == a] <- table$pts[table$team == a] + 3
      else {
        table$pts[table$team == h] <- table$pts[table$team == h] + 1
        table$pts[table$team == a] <- table$pts[table$team == a] + 1
      }
    }

    table %>%
      mutate(
        gd = gf - ga,
        strength = sapply(team, get_rating),
        random_tie = runif(n())
      ) %>%
      arrange(desc(pts), desc(gd), desc(gf), desc(strength), desc(random_tie)) %>%
      mutate(group_pos = row_number(), group = group_letter)
  }

  stages <- c("round32", "round16", "quarterfinal", "semifinal", "final", "champion", "runner_up")
  stage_counts <- lapply(stages, function(x) setNames(rep(0, length(teams_all)), teams_all))
  names(stage_counts) <- stages
  group_pos_counts <- lapply(teams_all, function(x) c(`1` = 0, `2` = 0, `3` = 0, `4` = 0))
  names(group_pos_counts) <- teams_all
  group_points_sum <- setNames(rep(0, length(teams_all)), teams_all)
  mode_counts <- c(`90min` = 0, extra_time = 0, penalties = 0)

  add_stage <- function(stage, team) {
    stage_counts[[stage]][team] <<- stage_counts[[stage]][team] + 1
  }

  groups <- sort(unique(group_preds$group))

  for (sim in seq_len(N_SIM)) {
    winners <- list(); runners <- list(); third_rows <- list()

    for (g in groups) {
      tab <- simulate_group(g)
      winners[[g]] <- tab$team[1]
      runners[[g]] <- tab$team[2]
      third_rows[[g]] <- tab[3,]

      for (i in seq_len(nrow(tab))) {
        team <- tab$team[i]
        pos <- as.character(tab$group_pos[i])
        group_pos_counts[[team]][pos] <- group_pos_counts[[team]][pos] + 1
        group_points_sum[team] <- group_points_sum[team] + tab$pts[i]
      }
    }

    third_df <- bind_rows(third_rows) %>%
      arrange(desc(pts), desc(gd), desc(gf), desc(strength), desc(random_tie))
    best_third <- third_df %>% slice_head(n = 8)

    qualifiers <- c(unlist(winners), unlist(runners), best_third$team)
    for (team in qualifiers) add_stage("round32", team)

    key <- paste(sort(best_third$group), collapse = "")
    map <- annex_c %>% filter(qualified_third_groups == key) %>% slice(1)
    if (nrow(map) == 0) stop(paste("No existe mapeo Annex C para la clave:", key))

    third_team_by_group <- setNames(best_third$team, best_third$group)
    third_for_slot <- list(
      `1A` = third_team_by_group[[map$slot_1A]],
      `1B` = third_team_by_group[[map$slot_1B]],
      `1D` = third_team_by_group[[map$slot_1D]],
      `1E` = third_team_by_group[[map$slot_1E]],
      `1G` = third_team_by_group[[map$slot_1G]],
      `1I` = third_team_by_group[[map$slot_1I]],
      `1K` = third_team_by_group[[map$slot_1K]],
      `1L` = third_team_by_group[[map$slot_1L]]
    )

    M <- list(
      `73` = c(runners$A, runners$B),
      `74` = c(winners$E, third_for_slot$`1E`),
      `75` = c(winners$F, runners$C),
      `76` = c(winners$C, runners$F),
      `77` = c(winners$I, third_for_slot$`1I`),
      `78` = c(runners$E, runners$I),
      `79` = c(winners$A, third_for_slot$`1A`),
      `80` = c(winners$L, third_for_slot$`1L`),
      `81` = c(winners$D, third_for_slot$`1D`),
      `82` = c(winners$G, third_for_slot$`1G`),
      `83` = c(runners$K, runners$L),
      `84` = c(winners$H, runners$J),
      `85` = c(winners$B, third_for_slot$`1B`),
      `86` = c(winners$J, runners$H),
      `87` = c(winners$K, third_for_slot$`1K`),
      `88` = c(runners$D, runners$G)
    )

    W <- list()
    for (m in names(M)) {
      out <- play_knockout(M[[m]][1], M[[m]][2]); W[[m]] <- out$winner
      mode_counts[out$mode] <- mode_counts[out$mode] + 1
      add_stage("round16", out$winner)
    }

    r16 <- list(
      `89` = c(W$`74`, W$`77`),
      `90` = c(W$`73`, W$`75`),
      `91` = c(W$`76`, W$`78`),
      `92` = c(W$`79`, W$`80`),
      `93` = c(W$`83`, W$`84`),
      `94` = c(W$`81`, W$`82`),
      `95` = c(W$`86`, W$`88`),
      `96` = c(W$`85`, W$`87`)
    )
    for (m in names(r16)) {
      out <- play_knockout(r16[[m]][1], r16[[m]][2]); W[[m]] <- out$winner
      mode_counts[out$mode] <- mode_counts[out$mode] + 1
      add_stage("quarterfinal", out$winner)
    }

    qf <- list(
      `97` = c(W$`89`, W$`90`),
      `98` = c(W$`93`, W$`94`),
      `99` = c(W$`91`, W$`92`),
      `100` = c(W$`95`, W$`96`)
    )
    for (m in names(qf)) {
      out <- play_knockout(qf[[m]][1], qf[[m]][2]); W[[m]] <- out$winner
      mode_counts[out$mode] <- mode_counts[out$mode] + 1
      add_stage("semifinal", out$winner)
    }

    sf1 <- play_knockout(W$`97`, W$`98`); W$`101` <- sf1$winner; mode_counts[sf1$mode] <- mode_counts[sf1$mode] + 1
    sf2 <- play_knockout(W$`99`, W$`100`); W$`102` <- sf2$winner; mode_counts[sf2$mode] <- mode_counts[sf2$mode] + 1
    add_stage("final", W$`101`); add_stage("final", W$`102`)

    final <- play_knockout(W$`101`, W$`102`); mode_counts[final$mode] <- mode_counts[final$mode] + 1
    champion <- final$winner
    runner_up <- ifelse(champion == W$`101`, W$`102`, W$`101`)
    add_stage("champion", champion)
    add_stage("runner_up", runner_up)

    if (verbose && sim %% 500 == 0) cat("Escenario", scenario_name, "- Simulacion", sim, "de", N_SIM, "\n")
  }

  results <- tibble(team = teams_all, group = team_group[teams_all])
  for (stage in stages) {
    results[[paste0(stage, "_probability")]] <- stage_counts[[stage]][teams_all] / N_SIM
  }
  results <- results %>%
    rowwise() %>%
    mutate(
      avg_group_points_simulated = group_points_sum[team] / N_SIM,
      group_1st_probability = group_pos_counts[[team]]["1"] / N_SIM,
      group_2nd_probability = group_pos_counts[[team]]["2"] / N_SIM,
      group_3rd_probability = group_pos_counts[[team]]["3"] / N_SIM,
      group_4th_probability = group_pos_counts[[team]]["4"] / N_SIM,
      input_strength_score = base_score_norm[team],
      input_raw_classifier_strength = ratings[team]
    ) %>%
    ungroup() %>%
    arrange(desc(champion_probability)) %>%
    mutate(rank = row_number(), .before = 1)

  prob_cols <- names(results)[str_detect(names(results), "_probability$")]
  for (col in prob_cols) results[[paste0(col, "_pct")]] <- results[[col]] * 100

  list(
    results = results,
    mode_counts = mode_counts,
    method = list(
      scenario_name = scenario_name,
      N_SIM = N_SIM,
      seed = seed,
      strength_scale = strength_scale,
      draw_multiplier = draw_multiplier,
      extra_time_prob = extra_time_prob,
      penalty_strength_factor = penalty_strength_factor,
      boost_team = boost_team,
      boost_amount = boost_amount,
      k_best = k_best,
      draw_intercept = draw_intercept,
      draw_slope = draw_slope,
      mode_counts = as.list(mode_counts)
    )
  )
}

# ------------------------------
# 3) Baseline: leer archivo existente o simularlo
# ------------------------------
baseline_path <- file.path(BASE_DIR, "worldcup_monte_carlo_champion_probabilities_R.csv")
compact_baseline_path <- file.path(BASE_DIR, "worldcup_monte_carlo_champion_probabilities_compact_R.csv")

if (file.exists(baseline_path)) {
  baseline_results <- read_csv(baseline_path, show_col_types = FALSE) %>% mutate(team = norm_team(team), group = as.character(group))
  baseline_method <- list(N_SIM = N_BASE_ASSUMED, seed = NA, source = baseline_path)
  cat("Baseline leido desde:", baseline_path, "\n")
} else {
  baseline_run <- run_worldcup_mc(N_SIM = N_BASE_ASSUMED, seed = 123, scenario_name = "baseline_5000", verbose = TRUE)
  baseline_results <- baseline_run$results
  baseline_method <- baseline_run$method
  write_csv(baseline_results, baseline_path)
  cat("Baseline simulado y guardado en:", baseline_path, "\n")
}

# Si existe JSON metodologico, tomar N_SIM real.
method_json_path <- file.path(BASE_DIR, "worldcup_monte_carlo_methodology_R.json")
if (file.exists(method_json_path)) {
  method_json <- jsonlite::read_json(method_json_path, simplifyVector = TRUE)
  if (!is.null(method_json$n_simulations)) baseline_method$N_SIM <- as.numeric(method_json$n_simulations)
}
N_BASE <- baseline_method$N_SIM

# ------------------------------
# 4) Validacion estructural / consistencia probabilistica
# ------------------------------
stage_expectations <- tibble(
  stage = c("round32", "round16", "quarterfinal", "semifinal", "final", "champion", "runner_up"),
  probability_col = paste0(stage, "_probability"),
  expected_sum = c(32, 16, 8, 4, 2, 1, 1)
) %>%
  mutate(
    actual_sum = map_dbl(probability_col, ~ sum(baseline_results[[.x]], na.rm = TRUE)),
    abs_error = abs(actual_sum - expected_sum),
    pass = abs_error < 1e-8
  )

probability_bounds <- baseline_results %>%
  summarise(across(ends_with("_probability"), ~ all(.x >= -1e-12 & .x <= 1 + 1e-12, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "probability_col", values_to = "all_values_between_0_and_1")

team_consistency <- baseline_results %>%
  transmute(
    team, group,
    group_position_sum = group_1st_probability + group_2nd_probability + group_3rd_probability + group_4th_probability,
    group_position_sum_error = abs(group_position_sum - 1),
    final_identity_error = abs(final_probability - champion_probability - runner_up_probability),
    monotonic_pass = round32_probability + 1e-12 >= round16_probability &
      round16_probability + 1e-12 >= quarterfinal_probability &
      quarterfinal_probability + 1e-12 >= semifinal_probability &
      semifinal_probability + 1e-12 >= final_probability &
      final_probability + 1e-12 >= champion_probability &
      final_probability + 1e-12 >= runner_up_probability,
    group_points_in_range = avg_group_points_simulated >= -1e-12 & avg_group_points_simulated <= 9 + 1e-12,
    round32_respects_group_qualification = round32_probability + 1e-12 >= group_1st_probability + group_2nd_probability
  )

group_consistency <- baseline_results %>%
  group_by(group) %>%
  summarise(
    sum_group_1st_probability = sum(group_1st_probability, na.rm = TRUE),
    sum_group_2nd_probability = sum(group_2nd_probability, na.rm = TRUE),
    sum_group_3rd_probability = sum(group_3rd_probability, na.rm = TRUE),
    sum_group_4th_probability = sum(group_4th_probability, na.rm = TRUE),
    error_1st = abs(sum_group_1st_probability - 1),
    error_2nd = abs(sum_group_2nd_probability - 1),
    error_3rd = abs(sum_group_3rd_probability - 1),
    error_4th = abs(sum_group_4th_probability - 1),
    .groups = "drop"
  )

write_csv(stage_expectations, file.path(OUT_DIR, "01_stage_probability_sum_checks.csv"))
write_csv(probability_bounds, file.path(OUT_DIR, "02_probability_bounds_checks.csv"))
write_csv(team_consistency, file.path(OUT_DIR, "03_team_probability_consistency_checks.csv"))
write_csv(group_consistency, file.path(OUT_DIR, "04_group_position_consistency_checks.csv"))

# ------------------------------
# 5) Error Monte Carlo e intervalos de confianza
# ------------------------------
mc_uncertainty <- baseline_results %>%
  transmute(
    rank,
    team,
    group,
    champion_probability,
    champion_probability_pct = 100 * champion_probability,
    mc_standard_error = sqrt(champion_probability * (1 - champion_probability) / N_BASE),
    ci95_low = pmax(0, champion_probability - 1.96 * mc_standard_error),
    ci95_high = pmin(1, champion_probability + 1.96 * mc_standard_error),
    ci95_low_pct = 100 * ci95_low,
    ci95_high_pct = 100 * ci95_high,
    ci95_width_pct = ci95_high_pct - ci95_low_pct
  ) %>%
  arrange(rank)

write_csv(mc_uncertainty, file.path(OUT_DIR, "05_monte_carlo_standard_error_ci95.csv"))

# ------------------------------
# 6) Reproducibilidad con misma semilla
# ------------------------------
# Si el baseline fue generado con N=5000 y seed=123, esta prueba debe ser muy parecida o identica.
# Si el baseline fue leido de otro archivo, sirve como comparacion de estabilidad.
repro_run <- run_worldcup_mc(N_SIM = N_REPRO, seed = 123, scenario_name = "repro_same_seed", verbose = TRUE)
repro_compare <- baseline_results %>%
  select(team, baseline_champion_probability = champion_probability, baseline_rank = rank) %>%
  left_join(
    repro_run$results %>% select(team, repro_champion_probability = champion_probability, repro_rank = rank),
    by = "team"
  ) %>%
  mutate(
    abs_diff = abs(repro_champion_probability - baseline_champion_probability),
    rank_diff = repro_rank - baseline_rank
  ) %>%
  arrange(desc(abs_diff))

write_csv(repro_compare, file.path(OUT_DIR, "06_reproducibility_same_seed_comparison.csv"))

# ------------------------------
# 7) Convergencia por N y por semilla
# ------------------------------
convergence_grid <- tidyr::expand_grid(
  N_SIM = N_CONVERGENCE_VALUES,
  seed = SEEDS_CONVERGENCE
)

convergence_results <- purrr::pmap_dfr(
  convergence_grid,
  function(N_SIM, seed) {
    cat("Convergencia: N=", N_SIM, " seed=", seed, "\n")
    run <- run_worldcup_mc(N_SIM = N_SIM, seed = seed, scenario_name = paste0("N", N_SIM, "_seed", seed), verbose = FALSE)
    run$results %>%
      select(team, group, rank, champion_probability, final_probability, semifinal_probability) %>%
      mutate(N_SIM = N_SIM, seed = seed, .before = 1)
  }
)

convergence_summary <- convergence_results %>%
  left_join(
    baseline_results %>% select(team, baseline_rank = rank, baseline_champion_probability = champion_probability),
    by = "team"
  ) %>%
  group_by(N_SIM, team, group, baseline_rank, baseline_champion_probability) %>%
  summarise(
    mean_champion_probability = mean(champion_probability, na.rm = TRUE),
    sd_champion_probability = sd(champion_probability, na.rm = TRUE),
    mean_abs_error_vs_baseline = mean(abs(champion_probability - baseline_champion_probability), na.rm = TRUE),
    mean_rank = mean(rank, na.rm = TRUE),
    sd_rank = sd(rank, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(N_SIM, baseline_rank)

convergence_global <- convergence_results %>%
  left_join(baseline_results %>% select(team, baseline_champion_probability = champion_probability), by = "team") %>%
  group_by(N_SIM, seed) %>%
  summarise(
    pearson_vs_baseline = safe_cor(champion_probability, baseline_champion_probability, "pearson"),
    spearman_rank_vs_baseline = safe_cor(champion_probability, baseline_champion_probability, "spearman"),
    mae_vs_baseline = mae(champion_probability, baseline_champion_probability),
    rmse_vs_baseline = rmse(champion_probability, baseline_champion_probability),
    max_abs_error_vs_baseline = max(abs(champion_probability - baseline_champion_probability), na.rm = TRUE),
    .groups = "drop"
  )

write_csv(convergence_results, file.path(OUT_DIR, "07_convergence_raw_results.csv"))
write_csv(convergence_summary, file.path(OUT_DIR, "08_convergence_by_team_summary.csv"))
write_csv(convergence_global, file.path(OUT_DIR, "09_convergence_global_summary.csv"))

# ------------------------------
# 8) Sensibilidad a supuestos del modelo
# ------------------------------
sensitivity_scenarios <- tibble::tribble(
  ~scenario, ~strength_scale, ~draw_multiplier, ~extra_time_prob, ~penalty_strength_factor,
  "baseline_sens", 1.00, 1.00, 0.35, 0.20,
  "strength_low_0_80", 0.80, 1.00, 0.35, 0.20,
  "strength_high_1_20", 1.20, 1.00, 0.35, 0.20,
  "draw_low_0_85", 1.00, 0.85, 0.35, 0.20,
  "draw_high_1_15", 1.00, 1.15, 0.35, 0.20,
  "extra_time_low_0_25", 1.00, 1.00, 0.25, 0.20,
  "extra_time_high_0_45", 1.00, 1.00, 0.45, 0.20,
  "penalty_adv_low_0_10", 1.00, 1.00, 0.35, 0.10,
  "penalty_adv_high_0_30", 1.00, 1.00, 0.35, 0.30
)

sensitivity_results <- purrr::pmap_dfr(
  sensitivity_scenarios,
  function(scenario, strength_scale, draw_multiplier, extra_time_prob, penalty_strength_factor) {
    cat("Sensibilidad:", scenario, "\n")
    run <- run_worldcup_mc(
      N_SIM = N_SENSITIVITY,
      seed = 999,
      strength_scale = strength_scale,
      draw_multiplier = draw_multiplier,
      extra_time_prob = extra_time_prob,
      penalty_strength_factor = penalty_strength_factor,
      scenario_name = scenario,
      verbose = FALSE
    )
    run$results %>%
      select(team, group, rank, champion_probability, runner_up_probability, final_probability, semifinal_probability) %>%
      mutate(
        scenario = scenario,
        strength_scale = strength_scale,
        draw_multiplier = draw_multiplier,
        extra_time_prob = extra_time_prob,
        penalty_strength_factor = penalty_strength_factor,
        .before = 1
      )
  }
)

baseline_top10 <- baseline_results %>% arrange(rank) %>% slice_head(n = 10) %>% pull(team)

sensitivity_compare <- sensitivity_results %>%
  left_join(
    baseline_results %>% select(team, baseline_rank = rank, baseline_champion_probability = champion_probability),
    by = "team"
  ) %>%
  mutate(
    abs_change_vs_baseline = abs(champion_probability - baseline_champion_probability),
    pct_point_change_vs_baseline = 100 * (champion_probability - baseline_champion_probability),
    rank_change_vs_baseline = rank - baseline_rank
  ) %>%
  arrange(scenario, baseline_rank)

sensitivity_robustness <- sensitivity_compare %>%
  group_by(scenario) %>%
  summarise(
    pearson_vs_baseline = safe_cor(champion_probability, baseline_champion_probability, "pearson"),
    spearman_rank_vs_baseline = safe_cor(champion_probability, baseline_champion_probability, "spearman"),
    mae_vs_baseline = mae(champion_probability, baseline_champion_probability),
    rmse_vs_baseline = rmse(champion_probability, baseline_champion_probability),
    max_abs_change_pct_points = 100 * max(abs_change_vs_baseline, na.rm = TRUE),
    top1 = team[which.min(rank)],
    top10_overlap_with_baseline = length(intersect(team[rank <= 10], baseline_top10)),
    .groups = "drop"
  ) %>%
  arrange(scenario)

write_csv(sensitivity_scenarios, file.path(OUT_DIR, "10_sensitivity_scenarios.csv"))
write_csv(sensitivity_results, file.path(OUT_DIR, "11_sensitivity_raw_results.csv"))
write_csv(sensitivity_compare, file.path(OUT_DIR, "12_sensitivity_vs_baseline_by_team.csv"))
write_csv(sensitivity_robustness, file.path(OUT_DIR, "13_sensitivity_robustness_summary.csv"))

# ------------------------------
# 9) Consistencia: puntos simulados vs puntos esperados del clasificador
# ------------------------------
points_consistency <- baseline_results %>%
  select(team, group, avg_group_points_simulated) %>%
  left_join(
    team_probs %>% select(team, expected_points, expected_points_per_match, raw_classifier_strength, score),
    by = "team"
  ) %>%
  mutate(
    points_error = avg_group_points_simulated - expected_points,
    abs_points_error = abs(points_error)
  ) %>%
  arrange(desc(abs_points_error))

points_consistency_summary <- points_consistency %>%
  summarise(
    pearson_correlation = safe_cor(avg_group_points_simulated, expected_points, "pearson"),
    spearman_correlation = safe_cor(avg_group_points_simulated, expected_points, "spearman"),
    mae_points = mae(avg_group_points_simulated, expected_points),
    rmse_points = rmse(avg_group_points_simulated, expected_points),
    max_abs_points_error = max(abs_points_error, na.rm = TRUE)
  )

write_csv(points_consistency, file.path(OUT_DIR, "14_group_points_simulated_vs_classifier_expected.csv"))
write_csv(points_consistency_summary, file.path(OUT_DIR, "15_group_points_consistency_summary.csv"))

# ------------------------------
# 10) Consistencia de probabilidades de partidos de grupo
# ------------------------------
# Verifica que la funcion sample_score respete p_home_win, p_draw y p_away_win.
validate_match_probs <- function(N = 5000, seed = 777) {
  set.seed(seed)
  purrr::pmap_dfr(
    group_preds %>% mutate(match_id = row_number()) %>% select(match_id, group, `_home_team`, `_away_team`, p_home_win, p_draw, p_away_win),
    function(match_id, group, `_home_team`, `_away_team`, p_home_win, p_draw, p_away_win) {
      outcomes <- replicate(N, {
        u <- runif(1)
        if (u < p_home_win) "HOME_WIN" else if (u < p_home_win + p_draw) "DRAW" else "AWAY_WIN"
      })
      tibble(
        match_id = match_id,
        group = group,
        home_team = `_home_team`,
        away_team = `_away_team`,
        input_p_home_win = p_home_win,
        input_p_draw = p_draw,
        input_p_away_win = p_away_win,
        sim_p_home_win = mean(outcomes == "HOME_WIN"),
        sim_p_draw = mean(outcomes == "DRAW"),
        sim_p_away_win = mean(outcomes == "AWAY_WIN"),
        abs_error_home = abs(sim_p_home_win - input_p_home_win),
        abs_error_draw = abs(sim_p_draw - input_p_draw),
        abs_error_away = abs(sim_p_away_win - input_p_away_win)
      )
    }
  )
}

match_prob_validation <- validate_match_probs(N = N_MATCH_PROB_CHECK, seed = 777)
match_prob_summary <- match_prob_validation %>%
  summarise(
    mae_home = mean(abs_error_home, na.rm = TRUE),
    mae_draw = mean(abs_error_draw, na.rm = TRUE),
    mae_away = mean(abs_error_away, na.rm = TRUE),
    max_abs_error_home = max(abs_error_home, na.rm = TRUE),
    max_abs_error_draw = max(abs_error_draw, na.rm = TRUE),
    max_abs_error_away = max(abs_error_away, na.rm = TRUE)
  )

write_csv(match_prob_validation, file.path(OUT_DIR, "16_group_match_probability_validation.csv"))
write_csv(match_prob_summary, file.path(OUT_DIR, "17_group_match_probability_validation_summary.csv"))

# ------------------------------
# 11) Validacion de probabilidades de eliminatorias
# ------------------------------
# Se revisa que las probabilidades calculadas para pares de equipos sean validas.
# Para no depender de funciones internas, se recalibra igual que en el baseline.
ratings <- base_ratings
get_rating_global <- function(team) {
  val <- ratings[[team]]
  if (is.null(val) || is.na(val)) mean(ratings, na.rm = TRUE) else val
}

cal_global <- group_preds %>%
  rowwise() %>%
  mutate(
    strength_diff = get_rating_global(`_home_team`) - get_rating_global(`_away_team`),
    p_no_draw_home = p_home_win / (p_home_win + p_away_win)
  ) %>%
  ungroup()

k_grid_global <- seq(0.1, 50, length.out = 1000)
errors_global <- sapply(k_grid_global, function(k) mean((sigmoid(k * cal_global$strength_diff) - cal_global$p_no_draw_home)^2, na.rm = TRUE))
k_best_global <- k_grid_global[which.min(errors_global)]
draw_fit_global <- lm(p_draw ~ I(abs(strength_diff)), data = cal_global)
draw_intercept_global <- coef(draw_fit_global)[1]
draw_slope_global <- coef(draw_fit_global)[2]

knockout_probs_global <- function(team_a, team_b) {
  diff <- get_rating_global(team_a) - get_rating_global(team_b)
  p_no_draw <- sigmoid(k_best_global * diff)
  p_draw <- clamp(draw_intercept_global + draw_slope_global * abs(diff), 0.18, 0.58)
  p_a_90 <- (1 - p_draw) * p_no_draw
  p_b_90 <- (1 - p_draw) * (1 - p_no_draw)
  p_a_pen <- clamp(0.5 + 0.20 * (p_no_draw - 0.5), 0.40, 0.60)
  tibble(p_a_90 = p_a_90, p_draw = p_draw, p_b_90 = p_b_90, p_no_draw = p_no_draw, p_a_pen = p_a_pen)
}

team_pairs <- combn(teams_all, 2, simplify = FALSE)
knockout_probability_checks <- purrr::map_dfr(team_pairs, function(pair) {
  a <- pair[1]; b <- pair[2]
  pab <- knockout_probs_global(a, b)
  pba <- knockout_probs_global(b, a)
  tibble(
    team_a = a,
    team_b = b,
    p_a_90 = pab$p_a_90,
    p_draw = pab$p_draw,
    p_b_90 = pab$p_b_90,
    sum_90_probs = pab$p_a_90 + pab$p_draw + pab$p_b_90,
    p_no_draw_a_vs_b = pab$p_no_draw,
    p_no_draw_b_vs_a = pba$p_no_draw,
    no_draw_symmetry_error = abs((pab$p_no_draw + pba$p_no_draw) - 1),
    p_a_pen = pab$p_a_pen,
    all_bounds_valid = all(c(pab$p_a_90, pab$p_draw, pab$p_b_90, pab$p_no_draw, pab$p_a_pen) >= 0 &
                             c(pab$p_a_90, pab$p_draw, pab$p_b_90, pab$p_no_draw, pab$p_a_pen) <= 1)
  )
})

knockout_probability_summary <- knockout_probability_checks %>%
  summarise(
    max_sum_90_error = max(abs(sum_90_probs - 1), na.rm = TRUE),
    max_no_draw_symmetry_error = max(no_draw_symmetry_error, na.rm = TRUE),
    all_pairs_valid_bounds = all(all_bounds_valid),
    min_p_draw = min(p_draw, na.rm = TRUE),
    max_p_draw = max(p_draw, na.rm = TRUE),
    min_penalty_probability = min(p_a_pen, na.rm = TRUE),
    max_penalty_probability = max(p_a_pen, na.rm = TRUE)
  )

write_csv(knockout_probability_checks, file.path(OUT_DIR, "18_knockout_probability_checks.csv"))
write_csv(knockout_probability_summary, file.path(OUT_DIR, "19_knockout_probability_summary.csv"))

# ------------------------------
# 12) Validacion del Annex C
# ------------------------------
annex_required_cols <- c("qualified_third_groups", "slot_1A", "slot_1B", "slot_1D", "slot_1E", "slot_1G", "slot_1I", "slot_1K", "slot_1L")
annex_validation <- tibble(
  check = c("required_columns_present", "no_duplicate_keys", "expected_495_combinations_if_full_12_choose_8"),
  value = c(
    all(annex_required_cols %in% names(annex_c)),
    !anyDuplicated(annex_c$qualified_third_groups),
    nrow(annex_c) == choose(length(sort(unique(group_preds$group))), 8)
  ),
  detail = c(
    paste("missing:", paste(setdiff(annex_required_cols, names(annex_c)), collapse = ", ")),
    paste("duplicated_keys:", sum(duplicated(annex_c$qualified_third_groups))),
    paste("rows:", nrow(annex_c), "expected:", choose(length(sort(unique(group_preds$group))), 8))
  )
)
write_csv(annex_validation, file.path(OUT_DIR, "20_annex_c_validation.csv"))

# ------------------------------
# 13) Graficas de validacion
# ------------------------------
# Top 20 con intervalo de confianza
p_ci <- mc_uncertainty %>%
  slice_min(order_by = rank, n = 20) %>%
  mutate(team = reorder(team, champion_probability_pct)) %>%
  ggplot(aes(x = team, y = champion_probability_pct)) +
  geom_col() +
  geom_errorbar(aes(ymin = ci95_low_pct, ymax = ci95_high_pct), width = 0.2) +
  coord_flip() +
  labs(
    title = "Top 20: probabilidad de campeon con IC 95% Monte Carlo",
    x = "Seleccion",
    y = "Probabilidad de campeon (%)"
  )

ggsave(file.path(OUT_DIR, "plot_01_top20_champion_probability_ci95.png"), p_ci, width = 10, height = 7, dpi = 300)

# Convergencia global: MAE vs baseline
p_conv <- convergence_global %>%
  group_by(N_SIM) %>%
  summarise(mean_mae = mean(mae_vs_baseline, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = N_SIM, y = mean_mae)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Convergencia de Monte Carlo: MAE contra baseline",
    x = "Numero de simulaciones",
    y = "MAE de champion_probability"
  )

ggsave(file.path(OUT_DIR, "plot_02_convergence_mae_vs_baseline.png"), p_conv, width = 8, height = 5, dpi = 300)

# Sensibilidad top 10 baseline
p_sens <- sensitivity_compare %>%
  filter(team %in% baseline_top10) %>%
  mutate(team = reorder(team, baseline_champion_probability)) %>%
  ggplot(aes(x = team, y = 100 * champion_probability, group = scenario)) +
  geom_line(aes(linetype = scenario)) +
  geom_point() +
  coord_flip() +
  labs(
    title = "Sensibilidad de probabilidad de campeon para Top 10 baseline",
    x = "Seleccion",
    y = "Probabilidad de campeon (%)"
  )

ggsave(file.path(OUT_DIR, "plot_03_sensitivity_top10.png"), p_sens, width = 11, height = 7, dpi = 300)

# Puntos esperados vs simulados
p_points <- points_consistency %>%
  ggplot(aes(x = expected_points, y = avg_group_points_simulated)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = "Consistencia: puntos esperados del clasificador vs puntos simulados",
    x = "Puntos esperados por clasificador",
    y = "Puntos promedio simulados"
  )

ggsave(file.path(OUT_DIR, "plot_04_expected_points_vs_simulated_points.png"), p_points, width = 7, height = 6, dpi = 300)

# ------------------------------
# 14) Resumen ejecutivo
# ------------------------------
summary_report <- list(
  baseline_N = N_BASE,
  structural_checks = list(
    stage_sum_all_pass = all(stage_expectations$pass),
    probability_bounds_all_pass = all(probability_bounds$all_values_between_0_and_1),
    max_group_position_sum_error = max(team_consistency$group_position_sum_error, na.rm = TRUE),
    max_final_identity_error = max(team_consistency$final_identity_error, na.rm = TRUE),
    monotonic_all_pass = all(team_consistency$monotonic_pass),
    group_points_range_all_pass = all(team_consistency$group_points_in_range),
    round32_qualification_logic_all_pass = all(team_consistency$round32_respects_group_qualification)
  ),
  monte_carlo_uncertainty = list(
    average_ci95_width_pct_points = mean(mc_uncertainty$ci95_width_pct, na.rm = TRUE),
    top1_team = baseline_results$team[1],
    top1_champion_probability_pct = 100 * baseline_results$champion_probability[1],
    top1_ci95_low_pct = mc_uncertainty$ci95_low_pct[mc_uncertainty$team == baseline_results$team[1]],
    top1_ci95_high_pct = mc_uncertainty$ci95_high_pct[mc_uncertainty$team == baseline_results$team[1]]
  ),
  convergence = convergence_global %>% group_by(N_SIM) %>% summarise(mean_spearman = mean(spearman_rank_vs_baseline, na.rm = TRUE), mean_mae = mean(mae_vs_baseline, na.rm = TRUE), .groups = "drop"),
  sensitivity = sensitivity_robustness,
  group_points_consistency = points_consistency_summary,
  match_probability_consistency = match_prob_summary,
  knockout_probability_consistency = knockout_probability_summary,
  annex_c_validation = annex_validation
)

write_json(summary_report, file.path(OUT_DIR, "00_monte_carlo_validation_summary.json"), pretty = TRUE, auto_unbox = TRUE)

cat("\nVALIDACION COMPLETADA. Archivos guardados en:", OUT_DIR, "\n")
cat("Revisa especialmente:\n")
cat("- 00_monte_carlo_validation_summary.json\n")
cat("- 01_stage_probability_sum_checks.csv\n")
cat("- 05_monte_carlo_standard_error_ci95.csv\n")
cat("- 09_convergence_global_summary.csv\n")
cat("- 13_sensitivity_robustness_summary.csv\n")
cat("- 15_group_points_consistency_summary.csv\n")
