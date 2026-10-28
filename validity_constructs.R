
library(tidyverse)
library(psych)


input_dir <- "recoded-files-final"
output_dir <- "covariate-validation"

dir.create(output_dir, showWarnings = FALSE)
dir.create(file.path(output_dir, "distributions"),
           showWarnings = FALSE)
dir.create(file.path(output_dir, "factor_analysis"),
           showWarnings = FALSE)

# Expected dataset names
dataset_names <- c(
  "baseline",
  paste0("10week", 1:9),
  "graduation",
  "final"
)

# Read available CSV files
file_paths <- file.path(
  input_dir,
  paste0(dataset_names, ".csv")
)

names(file_paths) <- dataset_names

available <- file.exists(file_paths)

if (any(!available)) {
  message(
    "Missing files: ",
    paste(names(file_paths)[!available], collapse = ", ")
  )
}

file_paths <- file_paths[available]

if (length(file_paths) == 0) {
  stop("No matching CSV files found.")
}

data_list <- lapply(
  file_paths,
  function(path) {
    read_csv(
      path,
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    )
  }
)

# scale validation
scales <- list(
  Extraversion = c("BF1", "BF6", "BF11", "BF16"),
  Agreeableness = c("BF4", "BF9", "BF14", "BF19"),
  Conscientiousness = c("BF2", "BF7", "BF12", "BF17"),
  Neuroticism = c("BF5", "BF10", "BF15", "BF20"),
  Imagination = c("BF3", "BF8", "BF13", "BF18"),
  PHQ = paste0("PHQ", 1:7),
  Loneliness = paste0("L", 1:10),
  Happiness = paste0("H", 1:4),
  Social_Assurance = paste0("SAS", 1:8)
)

scale_ranges <- list(
  Extraversion = c(1, 5),
  Agreeableness = c(1, 5),
  Conscientiousness = c(1, 5),
  Neuroticism = c(1, 5),
  Imagination = c(1, 5),
  PHQ = c(1, 4),
  Loneliness = c(1, 4),
  Happiness = c(1, 7),
  Social_Assurance = c(1, 6)
)



reverse_items <- list(
  Extraversion = c("BF1", "BF6"),
  Agreeableness = c("BF4", "BF9"),
  Conscientiousness = c("BF2", "BF17"),
  Neuroticism = c("BF15", "BF20"),
  Imagination = c("BF3", "BF8", "BF13"),
  PHQ = character(0),
  Loneliness = character(0),
  Happiness = c("H4"),
  Social_Assurance = character(0)
)


prepare_scale <- function(dat, scale_name) {
  
  items <- scales[[scale_name]]
  
  if (!all(items %in% names(dat))) {
    return(NULL)
  }
  
  x <- dat %>%
    select(all_of(items)) %>%
    mutate(
      across(
        everything(),
        ~ suppressWarnings(as.numeric(.x))
      )
    )
  
  limits <- scale_ranges[[scale_name]]

  
  # Reverse score specified items
  reversed <- reverse_items[[scale_name]]
  
  for (item in reversed) {
    if (!item %in% names(x)) {
      stop(paste("Unknown reverse-scored item:", item))
    }
    
    x[[item]] <- sum(limits) - x[[item]]
  }
  
  x
}


calculate_alpha <- function(x, dataset, scale_name) {
  
  complete_x <- x[complete.cases(x), , drop = FALSE]
  
  n <- nrow(complete_x)
  k <- ncol(complete_x)
  

  if (n < 10 || k < 2 ||
      any(sapply(complete_x, sd) == 0)) {
    return(tibble(
      dataset = dataset,
      scale = scale_name,
      n = n,
      n_items = k,
      alpha = NA_real_,
      standardized_alpha = NA_real_
    ))
  }
  
  result <- tryCatch(
    psych::alpha(
      complete_x,
      check.keys = FALSE,
      warnings = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    return(tibble(
      dataset = dataset,
      scale = scale_name,
      n = n,
      n_items = k,
      alpha = NA_real_,
      standardized_alpha = NA_real_
    ))
  }
  
  tibble(
    dataset = dataset,
    scale = scale_name,
    n = n,
    n_items = k,
    alpha = result$total$raw_alpha,
    standardized_alpha = result$total$std.alpha
  )
}

calculate_descriptives <- function(x, dataset, scale_name) {
  
  # Require complete responses for the scale score
  scores <- rowMeans(x)
  scores[rowSums(is.na(x)) > 0] <- NA_real_
  
  valid <- scores[!is.na(scores)]
  
  tibble(
    dataset = dataset,
    scale = scale_name,
    n_total = nrow(x),
    n_valid = length(valid),
    n_missing = sum(is.na(scores)),
    missing_pct = mean(is.na(scores)) * 100,
    mean = if (length(valid) > 0) mean(valid) else NA_real_,
    sd = if (length(valid) > 1) sd(valid) else NA_real_,
    median = if (length(valid) > 0) median(valid) else NA_real_,
    min = if (length(valid) > 0) min(valid) else NA_real_,
    max = if (length(valid) > 0) max(valid) else NA_real_
  )
}




alpha_results <- list()
descriptive_results <- list()
score_results <- list()

for (dataset_name in names(data_list)) {
  
  dat <- data_list[[dataset_name]]
  
  message("Processing: ", dataset_name)
  
  for (scale_name in names(scales)) {
    
    x <- prepare_scale(dat, scale_name)
    
    # Skip scales not measured in this dataset
    if (is.null(x)) next
    
    # Cronbach's alpha
    alpha_results[[length(alpha_results) + 1]] <-
      calculate_alpha(x, dataset_name, scale_name)
    
    # Descriptive statistics
    descriptive_results[[length(descriptive_results) + 1]] <-
      calculate_descriptives(x, dataset_name, scale_name)
    
    scores <- rowMeans(x)
    scores[rowSums(is.na(x)) > 0] <- NA_real_
    
    score_results[[length(score_results) + 1]] <-
      tibble(
        dataset = dataset_name,
        row = seq_len(nrow(x)),
        id = if ("id" %in% names(dat)) dat$id
        else as.character(seq_len(nrow(x))),
        scale = scale_name,
        score = scores
      )
    
  }
  
}

# Combine outputs safely
alpha_table <- bind_rows(alpha_results)
descriptive_table <- bind_rows(descriptive_results)
score_data <- bind_rows(score_results)

# Save results
write_csv(
  alpha_table,
  file.path(output_dir, "cronbach_alpha.csv")
)

write_csv(
  descriptive_table,
  file.path(output_dir, "scale_descriptives.csv")
)


write_csv(
  score_data,
  file.path(output_dir, "participant_scale_scores.csv")
)

score_data

if (nrow(score_data) > 0) {
  
  p_distribution <- score_data %>%
    filter(!is.na(score)) %>%
    ggplot(aes(x = score)) +
    geom_histogram(
      bins = 12,
      fill = "steelblue",
      color = "white"
    ) +
    facet_wrap(~scale, scales = "free") +
    labs(
      x = "mean scale score",
      y = "frequency"
    ) +
    theme_minimal()+
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold")
    )+
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
  
  ggsave(
    file.path(
      output_dir,
      "distributions",
      "scale_distributions.png"
    ),
    p_distribution,
    width = 12,
    height = 8,
    dpi = 300
  )
  
  # Distribution across measurement occasions
  p_box <- score_data %>%
    filter(!is.na(score)) %>%
    ggplot(
      aes(x = dataset, y = score)
    ) +
    geom_boxplot(fill = "lightblue") +
    facet_wrap(~scale, scales = "free_y") +
    labs(
      x = "timepoint",
      y = "mean scale score"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        size = 6
      )
    )+
    theme_minimal(base_size = 11) +
    theme(
      strip.text = element_text(face = "bold")
    )+
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
  
  ggsave(
    file.path(
      output_dir,
      "distributions",
      "scale_boxplots.png"
    ),
    p_box,
    width = 16,
    height = 9,
    dpi = 300
  )
}


alpha_summary <- alpha_table %>%
  group_by(scale) %>%
  summarise(
    n_datasets = sum(!is.na(alpha)),
    mean_alpha = mean(alpha, na.rm = TRUE),
    median_alpha = median(alpha, na.rm = TRUE),
    min_alpha = if (all(is.na(alpha))) NA_real_
    else min(alpha, na.rm = TRUE),
    max_alpha = if (all(is.na(alpha))) NA_real_
    else max(alpha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric) & !matches("^n_"),
      ~ replace(.x, is.nan(.x), NA_real_)
    )
  )

write_csv(
  alpha_summary,
  file.path(output_dir, "alpha_summary.csv")
)




print(alpha_summary, n = Inf)



dataset_order <- c(
  "baseline",
  paste0("10week", 1:9),
  "graduation",
  "final"
)

# Plot Cronbach's alpha
p_alpha <- alpha_table %>%
  filter(!is.na(alpha)) %>%
  mutate(
    dataset = factor(dataset, levels = dataset_order)
  ) %>%
  filter(!is.na(dataset)) %>%
  ggplot(aes(x = dataset, y = alpha, group = 1)) +
  geom_point() +
  geom_line() +
  facet_wrap(~scale) +
  geom_hline(
    yintercept = 0.7,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    x = "timepoint",
    y = "Cronbach's alpha"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 7
    )
  )+
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold")
  )+
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


ggsave(
  file.path(output_dir, "alpha_by_dataset.png"),
  p_alpha,
  width = 14,
  height = 8,
  dpi = 300
)



############### additional variables


baseline_vars <- c(
  "age", "sex", "height", "weight",
  "score", "smoking", "alcohol"
)

tenweek_vars <- c(
  "age", "gender", "height", "weight",
  "score", "smoke", "alcohol"
)

grad_final_vars <- c(
  "age", "gender", "height", "weight",
  "score", "smoke", "alcohol",
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


# combine data
longitudinal_data <- map_dfr(
  dataset_order,
  function(dataset_name) {
    
    dat <- data_list[[dataset_name]]
    
    if (is.null(dat)) return(tibble())
    
    vars <- if (dataset_name == "baseline") {
      baseline_vars
    } else if (str_detect(dataset_name, "^10week")) {
      tenweek_vars
    } else {
      grad_final_vars
    }
    
    selected <- intersect(vars, names(dat))
    
    if (length(selected) == 0) return(tibble())
    
    dat %>%
      select(all_of(selected)) %>%
      mutate(
        across(everything(), as.character),
        dataset = dataset_name
      ) %>%
      pivot_longer(
        cols = -dataset,
        names_to = "variable",
        values_to = "value"
      )
  }
)
longitudinal_data <- longitudinal_data %>%
  mutate(
    variable = recode(
      variable,
      "sex" = "gender",
      "smoking" = "smoke"
    ),
    dataset = factor(
      dataset,
      levels = dataset_order,
      ordered = TRUE
    ),
    time = as.numeric(dataset),
    value = na_if(str_trim(value), "")
  ) %>%
  filter(
    !variable %in% c(
      "age", "gender", "score",
      "graduation_plan",
      "graduation_other",
      "graduation_location",
      "employment_field"
    )
  )

continuous_vars <- c(
  "height",
  "weight",
  "friend_similarity",
  "social_activity",
  "social_quality",
  "social_media",
  "social_media_class",
  "friend_conflict",
  "anxiety"
)

categorical_vars <- c(
  "smoke",
  "alcohol"
)

longitudinal_data <- longitudinal_data %>%
  mutate(
    value = if_else(
      variable == "height" & suppressWarnings(as.numeric(value)) > 200,
      NA_character_,
      value
    )
  )


longitudinal_data <- longitudinal_data %>%
  mutate(
    value = if_else(
      variable == "height" & suppressWarnings(as.numeric(value)) < 140,
      NA_character_,
      value
    )
  )

continuous_summary <- longitudinal_data %>%
  filter(variable %in% continuous_vars) %>%
  mutate(
    value_num = suppressWarnings(as.numeric(value))
  ) %>%
  group_by(dataset, time, variable) %>%
  summarise(
    n = sum(!is.na(value_num)),
    mean = if (n > 0) {
      mean(value_num, na.rm = TRUE)
    } else {
      NA_real_
    },
    sd = if (n > 1) {
      sd(value_num, na.rm = TRUE)
    } else {
      NA_real_
    },
    se = sd / sqrt(n),
    lower = if (n > 1) {
      mean - qt(0.975, n - 1) * se
    } else {
      NA_real_
    },
    upper = if (n > 1) {
      mean + qt(0.975, n - 1) * se
    } else {
      NA_real_
    },
    .groups = "drop"
  )


categorical_summary <- longitudinal_data %>%
  filter(
    variable %in% categorical_vars,
    !is.na(value)
  ) %>%
  count(
    dataset, time, variable, value,
    name = "n"
  ) %>%
  group_by(dataset, time, variable) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup()


p_cont <- continuous_summary %>%
  filter(!is.na(mean)) %>%
  ggplot(
    aes(x = time, y = mean, group = variable)
  ) +
  geom_line(
    color = "steelblue",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_point(
    color = "steelblue",
    size = 1.8
  ) +
  facet_wrap(
    ~variable,
    scales = "free_y",
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq_along(dataset_order),
    labels = dataset_order
  ) +
  labs(
    x = "timtpoint",
    y = "mean"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 7
    ),
    strip.text = element_text(face = "bold")
  )+
theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

p_cont <- p_cont +
  scale_y_continuous(expand = expansion(mult = 2))

# categorical plot
p_cat <- categorical_summary %>%
  ggplot(
    aes(
      x = time,
      y = proportion,
      color = value,
      group = interaction(variable, value)
    )
  ) +
  geom_line(
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_point(size = 1.8) +
  facet_wrap(
    ~variable,
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq_along(dataset_order),
    labels = dataset_order
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "timepoint",
    y = "proportion",
    color = "response",
    title = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size = 7
    ),
    strip.text = element_text(face = "bold")
  )+
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# baseline plot
baseline_demographics <- data_list[["baseline"]] %>%
  transmute(
    age = suppressWarnings(as.numeric(age)),
    gender = as.character(sex),
    score = suppressWarnings(as.numeric(score))
  )

p_age <- ggplot(baseline_demographics, aes(x = age)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "age",
    x = "age",
    y = "grequency"
  ) +
  theme_minimal()

p_gender <- baseline_demographics %>%
  filter(!is.na(gender), gender != "") %>%
  count(gender) %>%
  ggplot(aes(x = gender, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "gender",
    x = "gender",
    y = "frequency"
  ) +
  theme_minimal()

p_score <- ggplot(baseline_demographics, aes(x = score)) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    x = "score",
    y = "frequency"
  ) +
  theme_minimal()

p_baseline <- p_age + p_gender + p_score 
print(p_baseline)



ggsave(
  file.path(output_dir, "baseline_demographics.png"),
  p_baseline,
  width = 12,
  height = 4,
  dpi = 300
)




# save

ggsave(
  file.path(output_dir, "longitudinal_cont_covariates.png"),
  p_cont,
  width = 16,
  height = 18,
  dpi = 300
)



ggsave(
  file.path(output_dir, "longitudinal_cat_covariates.png"),
  p_cat,
  width = 16,
  height = 8,
  dpi = 300
)



