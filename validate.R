library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)

dir.create("validation", showWarnings = FALSE)

# read files

weekly_files <- file.path("recoded-files", paste0("w", 1:30, ".csv"))
weekly_data <- lapply(weekly_files, function(file)
  read_csv(file, show_col_types = FALSE))

baseline_data <- read_csv("recoded-files/baseline.csv", show_col_types = FALSE)

wave <- file.path("recoded-files", paste0("wave", 1:12, ".csv"))
wave_data <- lapply(wave, function(file)
  read_csv(file, show_col_types = FALSE))

all_surveys <- c(list(baseline_data), weekly_data, wave_data)
wave_numbers <- c(0L, seq_len(30), 31:42)
wave_groups <- c("baseline", rep("weekly", 30), rep("additional", 12))

ids <- paste0("s", 1:47)

################################### weekly and baseline

###################### network validation
sn_cols <- grep("^s[0-9]+$", names(weekly_data[[1]]), value = TRUE)


make_network <- function(dat, ids, sn_cols) {
  dat <- dat[match(ids, as.character(dat$id)), ]
  
  y <- as.matrix(dat[, sn_cols, drop = FALSE])
  storage.mode(y) <- "numeric"
  
  y <- ifelse(is.na(y), NA_real_, ifelse(y > 2, 1, 0))
  
  rownames(y) <- ids
  colnames(y) <- ids
  
  diag(y) <- 0
  
  y
}


ids <- as.character(weekly_data[[1]]$id)

stopifnot(length(sn_cols) == length(ids),!anyDuplicated(ids),!anyNA(ids))

networks <- lapply(all_data, make_network, ids = ids, sn_cols = sn_cols)



structure_summary <- map_dfr(seq_along(networks), function(t) {
  y <- networks[[t]]
  
  tibble(
    week = t,
    n_nodes = nrow(y),
    missing_edges = sum(is.na(y[row(y) != col(y)])) / nrow(y) / (nrow(y) -
                                                                   1),
    n_ties = sum(y == 1, na.rm = TRUE) / nrow(y) / (nrow(y) - 1),
    asymmetric_edges = sum(y != t(y), na.rm = TRUE) / nrow(y) / (nrow(y) -
                                                                   1)
  )
})

print(structure_summary)


network_stats <- function(y, week) {
  diag(y) <- NA_real_
  
  observed <- !is.na(y)
  ties <- observed & y == 1
  
  density <- if (sum(observed) > 0) {
    sum(ties) / sum(observed)
  } else
    NA_real_
  
  mutual_observed <- observed & t(observed)
  eligible_ties <- ties & mutual_observed
  
  reciprocity <- if (sum(eligible_ties) > 0) {
    sum(eligible_ties & t(ties)) /
      sum(eligible_ties)
  } else
    NA_real_
  
  tibble(
    week = week,
    density = density,
    reciprocity = reciprocity,
    n_ties = sum(ties)
  )
}

stats <- map2_dfr(networks, seq_along(networks), network_stats)


persistence <- map_dfr(1:29, function(t) {
  y1 <- networks[[t]]
  y2 <- networks[[t + 1]]
  
  diag(y1) <- NA_real_
  diag(y2) <- NA_real_
  
  common <- !is.na(y1) & !is.na(y2)
  
  existing <- common & y1 == 1
  retained <- existing & y2 == 1
  
  tibble(week = t + 1, persistence = if (sum(existing) > 0) {
    sum(retained) / sum(existing)
  } else
    NA_real_)
})

stats <- left_join(stats, persistence, by = "week")


plot_data <- stats %>%
  select(week, density, reciprocity, persistence) %>%
  pivot_longer(cols = -week,
               names_to = "metric",
               values_to = "value")

p <- ggplot(plot_data, aes(week, value)) +
  geom_line() +
  geom_point(size = 1.5) +
  facet_wrap(~ metric, ncol = 1, scales = "free_y") +
  labs(x = "Survey week", y = "Proportion") +
  theme_classic()

ggsave("validation/network_validation.pdf",
       p,
       width = 7,
       height = 7)


write_csv(stats, "validation/network_statistics.csv")

write_csv(structure_summary,
          "validation/network_structure_checks.csv")







plot_data <- structure_summary %>%
  pivot_longer(cols = -week,
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = factor(
    metric,
    levels = c("n_nodes", "n_ties", "missing_edges", "asymmetric_edges"),
    labels = c(
      "Number of Nodes",
      "Number of Edges",
      "Missing edges",
      "Asymmetric edges"
    )
  ))

p <- ggplot(plot_data, aes(x = week, y = value)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~ metric, ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = seq(1, 30, 5)) +
  labs(x = "Week", y = NULL) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.spacing = grid::unit(1.2, "lines")
  )

print(p)

ggsave("validation/network_structure.pdf",
       p,
       width = 9,
       height = 6)


########################## covariate validation



# baseline variables
baseline_vars <- c(
  "age",
  "sex",
  "height",
  "weight",
  "score",
  "smoking",
  "drinking",
  paste0("BF", 1:20),
  paste0("PHQ", 1:7),
  paste0("L", 1:10),
  paste0("H", 1:4)
)

# weekly variables
weekly_vars <- c(
  "smoke",
  "smoke_days",
  "smoke_num",
  "drink",
  "drink_days",
  "drink_num",
  "drink_3",
  "drink_4",
  "drink_5",
  "friends",
  "contact",
  "social",
  "happy",
  "lonely",
  "pressure",
  "sleep",
  "sleep_hours",
  "study_hours",
  "friends_hours"
)



# missingness

baseline_missing <- baseline %>%
  select(all_of(baseline_vars)) %>%
  summarise(across(everything(), ~ mean(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_rate")

p_baseline <- ggplot(baseline_missing, aes(x = reorder(variable, missing_rate), y = missing_rate)) +
  geom_col() +
  coord_flip() +
  labs(x = "Baseline Covariate", y = "Proportion Missing") +
  theme_classic(base_size = 12)

print(p_baseline)

ggsave(
  "validation/baseline_covariate_missingness.pdf",
  p_baseline,
  width = 8,
  height = 9
)

.

weekly <- map2_dfr(weekly_data, 1:30, ~ mutate(.x, week = .y))

weekly_main_vars <- c(
  "smoke",
  "drink",
  "friends",
  "contact",
  "social",
  "happy",
  "lonely",
  "pressure",
  "sleep",
  "sleep_hours",
  "study_hours",
  "friends_hours"
)

weekly_missing <- weekly %>%
  group_by(week) %>%
  summarise(across(all_of(weekly_main_vars), ~ mean(is.na(.x))), .groups = "drop") %>%
  pivot_longer(cols = -week,
               names_to = "variable",
               values_to = "missing_rate")

# Plot as a heatmap
p_weekly <- ggplot(weekly_missing, aes(x = week, y = variable, fill = missing_rate)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white",
    high = "steelblue",
    limits = c(0, 1),
    name = "Missing"
  ) +
  scale_x_continuous(breaks = seq(1, 30, 5)) +
  labs(x = "Survey Week", y = NULL) +
  theme_classic(base_size = 12) +
  geom_text(aes(label = scales::percent(missing_rate, accuracy = 1)),
            size = 2.5,
            color = "black")

print(p_weekly)

ggsave(
  "validation/weekly_covariate_missingness.pdf",
  p_weekly,
  width = 10,
  height = 5
)


###################################### wave data

has_network <- function(dat) {
  if (is.null(dat) || !('id' %in% names(dat)))
    return(FALSE)
  sn <- intersect(ids, names(dat))
  if (length(sn) == 0)
    return(FALSE)
  respondent <- as.character(dat$id)
  any(vapply(sn, function(target) {
    vals <- suppressWarnings(as.numeric(as.character(dat[[target]])))
    any(!is.na(vals) &
          !is.na(respondent) & respondent %in% ids & respondent != target)
  }, logical(1)))
}

make_network <- function(dat) {
  if (anyDuplicated(na.omit(as.character(dat$id)))) {
    warning('Duplicate IDs found; first occurrence is used')
    dat <- dat[!duplicated(as.character(dat$id)), , drop = FALSE]
  }
  dat <- dat[match(ids, as.character(dat$id)), , drop = FALSE]
  y <- matrix(NA_real_, length(ids), length(ids), dimnames = list(ids, ids))
  sn <- intersect(ids, names(dat))
  if (length(sn)) {
    x <- as.matrix(dat[, sn, drop = FALSE])
    x <- matrix(
      suppressWarnings(as.numeric(x)),
      nrow = nrow(x),
      ncol = ncol(x),
      dimnames = dimnames(x)
    )
    y[, sn] <- ifelse(is.na(x), NA_real_, ifelse(x > 2, 1, 0))
  }
  diag(y) <- NA_real_
  y
}

available <- vapply(wave_data, has_network, logical(1))
network_availability <- tibble(
  wave = 12,
  group = wave_groups,
  network_available = available,
  n_network_columns = vapply(wave data, function(dat) {
    if (is.null(dat))
      return(0L)
    length(intersect(ids, names(dat)))
  }, integer(1))
)
