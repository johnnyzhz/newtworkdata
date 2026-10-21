library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)


#### preprocessing
dir.create("validation", recursive = TRUE, showWarnings = FALSE)
input_dir <- ("recoded-files/")

# w1-30
weekly_files <- file.path(input_dir, paste0("w", 1:30, ".csv"))
weekly_data <- lapply(weekly_files, read_csv, show_col_types = FALSE)

# baseline data
baseline_data <- read_csv(file.path(input_dir, "baseline.csv"), show_col_types = FALSE)

# 10week
wave_files <- file.path(input_dir, paste0("10week", 1:9, ".csv"))
wave_data <- lapply(wave_files, read_csv, show_col_types = FALSE)

# grad data, final data
grad_data <- read_csv(file.path(input_dir, "graduation.csv"), show_col_types = FALSE)
final_data <- read_csv(file.path(input_dir, "final.csv"), show_col_types = FALSE)

rename_alcohol <- function(df) {
  names(df) <- sub("^drink", "alcohol", names(df))
  df
}

weekly_data <- lapply(weekly_data, rename_alcohol)

baseline_data <- dplyr::rename(baseline_data, alcohol = any_of("drinking"))
grad_data <- rename_alcohol(grad_data)
final_data <- rename_alcohol(final_data)
wave_data <- lapply(wave_data, rename_alcohol)


output_dir <- "recoded-files-final/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Save
names(weekly_data) <- paste0("w", 1:30)
mapply(
  \(df, name) write_csv(df, file.path(output_dir, paste0(name, ".csv"))),
  weekly_data, names(weekly_data)
)

write_csv(baseline_data, file.path(output_dir, "baseline.csv"))
write_csv(grad_data, file.path(output_dir, "graduation.csv"))
write_csv(final_data, file.path(output_dir, "final.csv"))

names(wave_data) <- c(paste0("10week", 1:9))
mapply(
  \(df, name) write_csv(df, file.path(output_dir, paste0(name, ".csv"))),
  wave_data, names(wave_data)
)



ids <- paste0("s", 1:47)
all_surveys <- c(list(baseline_data),
                 weekly_data,
                 wave_data,
                 list(grad_data, final_data))
collection_dates <- as.Date(
  c(
    "2021-09-26",
    as.character(seq(
      as.Date("2021-10-03"), by = "7 days", length.out = 30
    )),
    "2021-12-05",
    "2022-02-15",
    "2022-05-02",
    "2022-06-26",
    "2022-08-27",
    "2022-11-06",
    "2023-03-11",
    "2023-05-21",
    "2023-08-13",
    "2025-07-03",
    "2025-10-17"
  )
)
metadata <- tibble(
  timepoint = c(
    "baseline",
    paste0("week", 1:30),
    paste0("10week", 1:9),
    "graduation",
    "final"
  ),
  group = c("baseline", rep("weekly", 30), rep("additional", 11)),
  collection_date = collection_dates,
  days_since_baseline = as.integer(collection_dates - as.Date("2021-09-26")),
  survey_order = seq_along(all_surveys)
)
# stopifnot(nrow(metadata) == length(all_surveys), !anyNA(metadata$collection_date))
write_csv(metadata, "validation/survey_collection_dates.csv")

normalize_blanks <- function(dat) {
  dat[] <- lapply(dat, function(x) {
    if (is.character(x)) {
      x[trimws(x) == ""] <- NA_character_
    }
    x
  })
  dat
}
all_surveys <- lapply(all_surveys, normalize_blanks)


######## vis
has_network <- function(dat) {
  if (!"id" %in% names(dat)) {
    return(FALSE)
  }
  sn <- intersect(ids, names(dat))
  if (!length(sn)) {
    return(FALSE)
  }
  respondents <- as.character(dat$id)
  any(vapply(sn, function(target) {
    vals <- suppressWarnings(as.numeric(as.character(dat[[target]])))
    any(!is.na(vals) & !is.na(respondents) & respondents %in% ids &
          respondents != target)
  }, logical(1)))
}

make_network <- function(dat) {
  respondent_ids <- as.character(dat$id)
  if (anyDuplicated(respondent_ids[!is.na(respondent_ids)])) {
    warning("Duplicate respondent IDs; keeping first occurrence")
    dat <- dat[!duplicated(respondent_ids), , drop = FALSE]
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

metadata$network_available <- vapply(all_surveys, has_network, logical(1))
metadata$n_network_columns <- vapply(all_surveys, function(dat) {
  length(intersect(ids, names(dat)))
}, integer(1))
# write_csv(metadata, 'validation/network_availability.csv')

# sort by time
network_meta <- metadata %>%
  filter(network_available) %>%
  arrange(collection_date, survey_order)
networks <- lapply(network_meta$survey_order, function(i) {
  make_network(all_surveys[[i]])
})
names(networks) <- network_meta$timepoint

network_stats <- function(y, timepoint, group, collection_date) {
  observed <- !is.na(y)
  ties <- observed & y == 1
  eligible <- ties & observed & t(observed)
  n_possible <- nrow(y) * (nrow(y) - 1)
  tibble(
    timepoint = timepoint,
    group = group,
    collection_date = collection_date,
    nodes = nrow(y),
    missing_edges = sum(!observed) / n_possible,
    edges = sum(ties),
    edge_proportion = sum(ties) / n_possible,
    asymmetric_edges = sum((y != t(y)) &
                             observed &
                             t(observed), na.rm = TRUE) / n_possible,
    density = if (sum(observed) > 0) {
      sum(ties) / sum(observed)
    } else {
      NA
    },
    reciprocity = if (sum(eligible) > 0) {
      sum(eligible & t(ties)) / sum(eligible)
    } else {
      NA
    }
  )
}

if (length(networks)) {
  stats <- pmap_dfr(
    list(
      networks,
      network_meta$timepoint,
      network_meta$group,
      network_meta$collection_date
    ),
    network_stats
  )
  persistence <- if (length(networks) >= 2) {
    map_dfr(seq_len(length(networks) - 1), function(i) {
      y1 <- networks[[i]]
      y2 <- networks[[i + 1]]
      common <- !is.na(y1) & !is.na(y2)
      existing <- common & y1 == 1
      retained <- existing & y2 == 1
      tibble(
        timepoint = network_meta$timepoint[i + 1],
        previous_timepoint = network_meta$timepoint[i],
        previous_date = network_meta$collection_date[i],
        days_elapsed = as.integer(
          network_meta$collection_date[i + 1] -
            network_meta$collection_date[i]
        ),
        persistence = if (sum(existing) > 0) {
          sum(retained) / sum(existing)
        } else {
          NA_real_
        }
      )
    })
  } else {
    tibble(
      timepoint = character(),
      previous_timepoint = character(),
      previous_date = as.Date(character()),
      days_elapsed = integer(),
      persistence = double()
    )
  }
  stats <- left_join(stats, persistence, by = "timepoint")
  # write_csv(stats, 'validation/network_statistics.csv')
  # write_csv(
  #   stats %>% select(
  #     timepoint,
  #     collection_date,
  #     group,
  #     n_nodes,
  #     missing_edges,
  #     n_ties,
  #     tie_proportion,
  #     asymmetric_edges
  #   ),
  #   'validation/network_structure_checks.csv'
  # )
  
  # Equally spaced network survey occasions, in chronological order.
  # Dates are retained in CSV outputs and persistence calculations.
  network_labels <- network_meta$timepoint
  stats <- stats %>%
    mutate(plot_index = match(timepoint, network_labels))
  axis_scale <- scale_x_continuous(
    breaks = seq_along(network_labels),
    labels = network_labels,
    guide = guide_axis(angle = 90, check.overlap = TRUE)
  )
  
  p1 <- stats %>%
    select(plot_index, reciprocity, persistence) %>%
    pivot_longer(c(reciprocity, persistence),
                 names_to = "metric",
                 values_to = "value") %>%
    ggplot(aes(plot_index, value)) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 1.5, na.rm = TRUE) +
    facet_wrap( ~ metric, ncol = 1, scales = "free_y") +
    axis_scale +
    labs(x = "data collection", y = "proportion") +
    theme_classic()
  ggsave("validation/network_validation.pdf",
         p1,
         width = 12,
         height = 8)
  
  p2 <- stats %>%
    select(plot_index,
           nodes,
           edge_proportion,
           missing_edges,
           asymmetric_edges) %>%
    pivot_longer(-plot_index, names_to = "metric", values_to = "value") %>%
    ggplot(aes(plot_index, value)) +
    geom_line(na.rm = TRUE) +
    geom_point(size = 1.5, na.rm = TRUE) +
    facet_wrap( ~ metric, ncol = 2, scales = "free_y") +
    axis_scale +
    labs(x = "data collection", y = NULL) +
    theme_classic(base_size = 12)
  ggsave("validation/network_structure.pdf",
         p2,
         width = 12,
         height = 7)
} else {
  message("No observed network data; skipping network statistics and plots.")
}

baseline_vars <- c(
  "age",
  "sex",
  "height",
  "weight",
  "score",
  "smoking",
  "alcohol",
  paste0("BF", 1:20),
  paste0("PHQ", 1:7),
  paste0("L", 1:10),
  paste0("H", 1:4)
)
weekly_vars <- c(
  "smoke",
  "smoke_days",
  "smoke_num",
  "alcohol",
  "alcohol_days",
  "alcohol_num",
  "alcohol_3",
  "alcohol_4",
  "alcohol_5",
  "friends",
  "contact",
  "social",
  "happy",
  "lonely",
  "pressure",
  "sleep",
  "sleep_hours",
  "study_hours",
  "friends_hours",
  "social_media",
  "social_media_class",
  "friend_conflict",
  "anxiety",
  "graduation_plan"
)

# rename vars
wave_demo_vars <- c("age", "gender", "height", "weight", "score", "smoke", "alcohol")
wave_personality_vars <- paste0("BF", 1:20)
wave_depression_vars <- paste0("PHD", 1:7)
wave_loneliness_vars <- paste0("L", 1:10)
wave_happiness_vars <- paste0("H", 1:4)
wave_extra_vars <- c(
  paste0("SAS", 1:8),
  "friend_similarity",
  "social_activity",
  "social_quality",
  "social_media",
  "social_media_class",
  "friend_conflict",
  "anxiety",
  "graduation_plan",
  "graduation_other",
  "graduation_location",
  "employment_field"
)
wave_vars <- unique(
  c(
    wave_demo_vars,
    wave_personality_vars,
    wave_depression_vars,
    wave_loneliness_vars,
    wave_happiness_vars,
    wave_extra_vars
  )
)

missing_for_survey <- function(dat,
                               timepoint,
                               group,
                               collection_date,
                               vars) {
  map_dfr(vars, function(v) {
    present <- v %in% names(dat)
    tibble(
      timepoint = timepoint,
      group = group,
      collection_date = collection_date,
      variable = v,
      administered = present,
      missing_rate = if (present && nrow(dat) > 0) {
        mean(is.na(dat[[v]]))
      } else {
        NA_real_
      }
    )
  })
}

baseline_missing <- missing_for_survey(all_surveys[[1]],
                                       "baseline",
                                       "baseline",
                                       collection_dates[1],
                                       baseline_vars)
# write_csv(baseline_missing, 'validation/baseline_covariate_missingness.csv')
p_base <- baseline_missing %>%
  filter(administered) %>%
  ggplot(aes(reorder(variable, missing_rate), missing_rate)) +
  geom_col() +
  coord_flip() +
  labs(x = "baseline covariates", y = "proportion missing") +
  theme_classic(base_size = 12)
ggsave(
  "validation/baseline_covariate_missingness.pdf",
  p_base,
  width = 8,
  height = 9
)

followup_missing <- pmap_dfr(list(
  all_surveys[-1],
  metadata$timepoint[-1],
  metadata$group[-1],
  metadata$collection_date[-1]
), function(dat, timepoint, group, collection_date) {
  vars <- if (group == "weekly") {
    weekly_vars
  } else {
    wave_vars
  }
  missing_for_survey(dat, timepoint, group, collection_date, vars)
})
# write_csv(followup_missing, 'validation/followup_covariate_missingness.csv')

# heatmap for covs for all timepoints
followup_order <- metadata %>%
  filter(group != "baseline") %>%
  arrange(collection_date, survey_order) %>%
  pull(timepoint)

followup_missing <- followup_missing %>%
  mutate(timepoint = factor(timepoint, levels = followup_order))

missing_plot <- function(dat, title) {
  ggplot(dat, aes(timepoint, variable, fill = missing_rate)) +
    geom_tile(color = "grey90") +
    scale_fill_gradient(
      low = "white",
      high = "steelblue",
      limits = c(0, 1),
      na.value = "grey85",
      name = "Missing"
    ) +
    geom_text(aes(label = ifelse(
      administered,
      scales::percent(missing_rate, accuracy = 1),
      "—"
    )), size = 0.8) +
    labs(
      x = "data collection",
      y = NULL,
      title = title,
      caption = "grey = variable absent from survey; columns ordered by data collection date."
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ))
}

p_followup <- missing_plot(followup_missing, "longitudinal covariates missingness")
ggsave(
  "validation/followup_covariate_missingness.pdf",
  p_followup,
  width = 15,
  height = 11
)


# additional data collection covs
p_additional <- missing_plot(filter(followup_missing, group == "additional"),
                             "wave covariate missingness") +
  scale_x_discrete(drop = TRUE)
ggsave(
  "validation/additional_wave_covariate_missingness.pdf",
  p_additional,
  width = 13,
  height = 14
)

p_weekly <- missing_plot(filter(followup_missing, group == "weekly"),
                         "weekly covariate missingness") +
  scale_x_discrete(drop = TRUE)
ggsave(
  "validation/weekly_covariate_missingness.pdf",
  p_weekly,
  width = 11,
  height = 7
)

