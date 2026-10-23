library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)



# read data
attention_data <- imap_dfr(raw_data, function(dat, dataset_name) {
  
  if (nrow(dat) < 2) return(tibble())
  
  # Find attention-check columns using row 1
  attention_cols <- which(
    str_detect(
      as.character(unlist(dat[1, ], use.names = FALSE)),
      fixed("在参与本次调查中，我")
    ) %in% TRUE
  )
  
  if (length(attention_cols) == 0) return(tibble())
  
# get attention data (only weekly has, and not all weekly)
  map_dfr(attention_cols, function(j) {
    tibble(
      dataset = dataset_name,
      row = 2:nrow(dat),
      item = names(dat)[j],
      response = as.character(dat[[j]][-1])
    )
  })
})

# 2. Recode responses
attention_data <- attention_data %>%
  mutate(
    response = str_trim(response),
    attention = case_when(
      response %in% c("1", "不认真") ~ 1L,
      response %in% c("2", "一般认真") ~ 2L,
      response %in% c("3", "比较认真") ~ 3L,
      response %in% c("4", "非常认真") ~ 4L,
      TRUE ~ NA_integer_
    ),
    attention_label = factor(
      attention,
      levels = 1:4,
      labels = c(
        "not attentive",
        "moderately attentive",
        "attentive",
        "very attentive"
      )
    )
  )


write_csv(attention_data, "attention-results/attention.csv")

# analysis
attention_summary <- attention_data %>%
  count(attention_label, .drop = FALSE) %>%
  mutate(
    percent = round(100 * n / sum(n), 2)
  )

print(attention_summary)


attention_by_dataset <- attention_data %>%
  filter(!is.na(attention)) %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    # mean= mean(attention),
    # sd_attention = sd(attention),
    not_attentive = mean(attention == 1),
    moderately_attentive = mean(attention == 2),
    attentive = mean(attention == 3),
    very_attentive = mean(attention == 4),
    .groups = "drop"
  )


print(attention_by_dataset, n = Inf)

write_csv(
  attention_by_dataset,
  "guangzhou_attention_summary.csv"
)
