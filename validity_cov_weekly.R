
library(tidyverse)
library(patchwork)


input_dir <- "recoded-files-final"
output_dir <- "covariate-weekly"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dataset_names <- paste0("w", 1:30)

file_paths <- file.path(
  input_dir,
  paste0(dataset_names, ".csv")
)

names(file_paths) <- dataset_names


data_list <- lapply(
  file_paths[file.exists(file_paths)],
  function(path) {
    read_csv(
      path,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
  }
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
  "friends_hours"
)


weekly_long <- map_dfr(
  dataset_names,
  function(dataset_name) {
    
    dat <- data_list[[dataset_name]]
    
    if (is.null(dat)) return(tibble())
    
    available_vars <- intersect(weekly_vars, names(dat))
    
    if (length(available_vars) == 0) return(tibble())
    
    dat %>%
      select(any_of(available_vars)) %>%
      mutate(
        across(everything(), as.character),
        week = as.integer(str_remove(dataset_name, "w"))
      ) %>%
      pivot_longer(
        cols = -week,
        names_to = "variable",
        values_to = "value"
      ) %>%
      mutate(
        value = na_if(str_trim(value), "")
      )
  }
)


# Continuous / ordinal variables
continuous_vars <- c(
  "smoke_days",
  "smoke_num",
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
  "friends_hours"
)

# Categorical variables
categorical_vars <- c(
  "smoke",
  "alcohol"
)


continuous_summary <- weekly_long %>%
  filter(variable %in% continuous_vars) %>%
  mutate(
    value_num = suppressWarnings(as.numeric(value))
  ) %>%
  group_by(week, variable) %>%
  summarise(
    n = sum(!is.na(value_num)),
    mean = if (n > 0) mean(value_num, na.rm = TRUE)
    else NA_real_,
    sd = if (n > 1) sd(value_num, na.rm = TRUE)
    else NA_real_,
    se = sd / sqrt(n),
    .groups = "drop"
  )


categorical_summary <- weekly_long %>%
  filter(
    variable %in% categorical_vars,
    !is.na(value)
  ) %>%
  count(week, variable, value, name = "n") %>%
  group_by(week, variable) %>%
  mutate(
    proportion = n / sum(n)
  ) %>%
  ungroup()

p_cont <- continuous_summary %>%
  filter(!is.na(mean)) %>%
  ggplot(aes(x = week, y = mean)) +
  geom_line(
    color = "steelblue",
    linewidth = 0.8
  ) +
  geom_point(
    color = "steelblue",
    size = 1.5
  ) +
  facet_wrap(
    ~variable,
    scales = "free_y",
    ncol = 4
  ) +
  scale_x_continuous(
    breaks = seq(1, 30, by = 5)
  ) +
  scale_y_continuous(
    expand = expansion(mult = 0.05)
  ) +
  labs(
    x = "week",
    y = "mean"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold")
  )+
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

p_cat <- categorical_summary %>%
  ggplot(
    aes(
      x = week,
      y = proportion,
      color = value,
      group = value
    )
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~variable,
    ncol = 3
  ) +
  scale_x_continuous(
    breaks = seq(1, 30, by = 5)
  ) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "week",
    y = "proportion",
    color = "response"
  ) +
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


p_all <- p_cont / p_cat +
  plot_layout(heights = c(3, 1))

print(p_all)




ggsave(
  file.path(output_dir, "weekly_continuous.png"),
  p_cont,
  width = 16,
  height = 12
)

ggsave(
  file.path(output_dir, "weekly_categorical.png"),
  p_cat,
  width = 12,
  height = 5
)

