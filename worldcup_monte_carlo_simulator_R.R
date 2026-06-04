# ============================================================
# SIMULACION MONTE CARLO PARA PROBABILIDAD DE CAMPEON MUNDIAL
# Entradas esperadas en la misma carpeta:
# - worldcup_group_stage_predictions.csv
# - worldcup_team_probabilities_classifier_R.csv
# - fifa_annex_c_third_place_mapping.csv
#
# Salidas:
# - worldcup_monte_carlo_champion_probabilities_R.csv
# - worldcup_monte_carlo_champion_probabilities_compact_R.csv
# - worldcup_monte_carlo_methodology_R.json
# ============================================================

required_packages <- c("readr", "dplyr", "stringr", "tibble", "jsonlite")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
}
library(readr); library(dplyr); library(stringr); library(tibble); library(jsonlite)

set.seed(123)
BASE_DIR <- "."
N_SIM <- 5000

sigmoid <- function(x) 1 / (1 + exp(-x))
clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))
norm_team <- function(x) str_squish(str_to_upper(as.character(x)))

# ------------------------------
# 1) Carga de datos
# ------------------------------
group_preds <- read_csv(file.path(BASE_DIR, "worldcup_group_stage_predictions.csv"), show_col_types = FALSE) %>%
  mutate(
    `_home_team` = norm_team(`_home_team`),
    `_away_team` = norm_team(`_away_team`),
    group = as.character(group)
  )

team_probs <- read_csv(file.path(BASE_DIR, "worldcup_team_probabilities_classifier_R.csv"), show_col_types = FALSE) %>%
  mutate(team = norm_team(team), group = as.character(group))

annex_c <- read_csv(file.path(BASE_DIR, "fifa_annex_c_third_place_mapping.csv"), show_col_types = FALSE)

ratings <- setNames(team_probs$raw_classifier_strength, team_probs$team)
score_norm <- setNames(team_probs$score, team_probs$team)
team_group <- setNames(team_probs$group, team_probs$team)
teams_all <- team_probs$team
get_rating <- function(team) {
  val <- ratings[[team]]
  if (is.null(val) || is.na(val)) mean(team_probs$raw_classifier_strength, na.rm = TRUE) else val
}

# ------------------------------
# 2) Calibracion simple para eliminatorias
# ------------------------------
# El clasificador solo predice partidos de grupo. Para simular eliminatorias entre
# cualquier par posible, se calibra una funcion de probabilidad de ganador usando
# la diferencia de fuerza entre selecciones.
cal <- group_preds %>%
  rowwise() %>%
  mutate(
    strength_diff = get_rating(`_home_team`) - get_rating(`_away_team`),
    p_no_draw_home = p_home_win / (p_home_win + p_away_win)
  ) %>%
  ungroup()

k_grid <- seq(0.1, 50, length.out = 1000)
errors <- sapply(k_grid, function(k) mean((sigmoid(k * cal$strength_diff) - cal$p_no_draw_home)^2, na.rm = TRUE))
k_best <- k_grid[which.min(errors)]

draw_fit <- lm(p_draw ~ I(abs(strength_diff)), data = cal)
draw_intercept <- coef(draw_fit)[1]
draw_slope <- coef(draw_fit)[2]

knockout_probs <- function(team_a, team_b) {
  diff <- get_rating(team_a) - get_rating(team_b)
  p_no_draw <- sigmoid(k_best * diff)
  p_draw <- clamp(draw_intercept + draw_slope * abs(diff), 0.18, 0.58)
  p_a_90 <- (1 - p_draw) * p_no_draw
  p_b_90 <- (1 - p_draw) * (1 - p_no_draw)
  p_a_pen <- clamp(0.5 + 0.20 * (p_no_draw - 0.5), 0.40, 0.60)
  list(p_a_90 = p_a_90, p_draw = p_draw, p_b_90 = p_b_90, p_no_draw = p_no_draw, p_a_pen = p_a_pen)
}

play_knockout <- function(team_a, team_b) {
  p <- knockout_probs(team_a, team_b)
  u <- runif(1)
  if (u < p$p_a_90) return(list(winner = team_a, mode = "90min"))
  if (u < p$p_a_90 + p$p_b_90) return(list(winner = team_b, mode = "90min"))

  # Empate en 90 minutos: tiempo extra y, si persiste el empate, penales.
  if (runif(1) < 0.35) {
    winner <- ifelse(runif(1) < p$p_no_draw, team_a, team_b)
    return(list(winner = winner, mode = "extra_time"))
  } else {
    winner <- ifelse(runif(1) < p$p_a_pen, team_a, team_b)
    return(list(winner = winner, mode = "penalties"))
  }
}

# ------------------------------
# 3) Simulacion de fase de grupos
# ------------------------------
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

# ------------------------------
# 4) Contadores
# ------------------------------
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

# ------------------------------
# 5) Monte Carlo: grupos + eliminatorias
# ------------------------------
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

  # Round of 32
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

  r16 <- list(`89`=c(W$`74`,W$`77`), `90`=c(W$`73`,W$`75`), `91`=c(W$`76`,W$`78`), `92`=c(W$`79`,W$`80`), `93`=c(W$`83`,W$`84`), `94`=c(W$`81`,W$`82`), `95`=c(W$`86`,W$`88`), `96`=c(W$`85`,W$`87`))
  for (m in names(r16)) {
    out <- play_knockout(r16[[m]][1], r16[[m]][2]); W[[m]] <- out$winner
    mode_counts[out$mode] <- mode_counts[out$mode] + 1
    add_stage("quarterfinal", out$winner)
  }

  qf <- list(`97`=c(W$`89`,W$`90`), `98`=c(W$`93`,W$`94`), `99`=c(W$`91`,W$`92`), `100`=c(W$`95`,W$`96`))
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
  
  if (sim %% 100 == 0) {
    cat("Simulación:", sim, "de", N_SIM, "\n")
  }
}

# ------------------------------
# 6) Exportar resultados
# ------------------------------
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
    input_strength_score = score_norm[team],
    input_raw_classifier_strength = ratings[team]
  ) %>%
  ungroup() %>%
  arrange(desc(champion_probability)) %>%
  mutate(rank = row_number(), .before = 1)

prob_cols <- names(results)[str_detect(names(results), "_probability$")]
for (col in prob_cols) results[[paste0(col, "_pct")]] <- results[[col]] * 100

write_csv(results, file.path(BASE_DIR, "worldcup_monte_carlo_champion_probabilities_R.csv"))
write_csv(
  results %>% select(rank, team, group, champion_probability, champion_probability_pct, runner_up_probability, final_probability, semifinal_probability, quarterfinal_probability, round16_probability, round32_probability, avg_group_points_simulated),
  file.path(BASE_DIR, "worldcup_monte_carlo_champion_probabilities_compact_R.csv")
)

method <- list(
  n_simulations = N_SIM,
  seed = 123,
  inputs = c("worldcup_group_stage_predictions.csv", "worldcup_team_probabilities_classifier_R.csv", "fifa_annex_c_third_place_mapping.csv"),
  no_draw_winner_model = list(sigmoid_slope_k = k_best),
  regulation_draw_model = list(intercept = draw_intercept, slope_abs_strength_diff = draw_slope, clamp = c(0.18, 0.58)),
  extra_time_penalties = list(draws_after_90_resolved_in_extra_time_probability = 0.35, penalty_strength_compression = "0.5 + 0.20*(p_no_draw - 0.5), clipped [0.40, 0.60]"),
  knockout_resolution_counts = as.list(mode_counts)
)
write_json(method, file.path(BASE_DIR, "worldcup_monte_carlo_methodology_R.json"), pretty = TRUE, auto_unbox = TRUE)

print(results %>% select(rank, team, group, champion_probability_pct, final_probability, semifinal_probability) %>% head(20))
