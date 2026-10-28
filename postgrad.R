library(tidyverse)
library(scales)

# Load datasets (adjust paths as needed)
grad <- read_csv("recoded-files-final/graduation_english.csv")
final <- read_csv("recoded-files-final/final_english.csv")

postgrad_vars <- c(
  "graduation_plan",
  "graduation_location",
  "employment_field"
)

# Combine datasets
postgrad_data <- bind_rows(
  grad %>% mutate(survey = "Graduation"),
  final %>% mutate(survey = "Final follow-up")
) %>%
  select(survey, any_of(postgrad_vars)) %>%
  mutate(across(all_of(postgrad_vars), as.character)) %>%
  pivot_longer(
    cols = all_of(postgrad_vars),
    names_to = "question",
    values_to = "response"
  ) %>%
  filter(!is.na(response), response != "")

# Calculate percentages
plot_data <- postgrad_data %>%
  count(survey, question, response) %>%
  group_by(survey, question) %>%
  mutate(percentage = 100 * n / sum(n)) %>%
  ungroup()

# Plot
ggplot(plot_data,
       aes(x = response, y = percentage, fill = survey)) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  facet_wrap(~ question, scales = "free_x", ncol = 2) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    x = "Response category",
    y = "Percentage",
    fill = "Survey"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  "postgraduation_responses.png",
  width = 10,
  height = 6,
  dpi = 300
)

