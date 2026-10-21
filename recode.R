library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)


input_dir <- "clean-files"
output_dir <- "recoded-files"

dir.create(output_dir, showWarnings = FALSE)

# funcs
clean_blanks <- function(dat) {
  dat %>%
    mutate(across(where(is.character), ~ na_if(trimws(.x), "")))
}

to_numeric <- function(dat, vars) {
  vars <- intersect(vars, names(dat))
  
  dat %>%
    mutate(across(all_of(vars), ~ suppressWarnings(as.numeric(.x))))
}

# baseline

baseline_file <- file.path(input_dir, "baseline-clean.csv")

baseline <- read_csv(baseline_file, show_col_types = FALSE)

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

baseline_clean <- baseline %>%
  clean_blanks() %>%
  to_numeric(baseline_vars) %>%
  mutate(
    sex = case_when(sex == 1 ~ 0, sex == 2 ~ 1, sex == 3 ~ 2, sex == 4 ~ NA),
    smoking = if_else(smoking %in% 1:5, smoking - 1, NA),
    drinking = if_else(drinking %in% 1:5, drinking - 1, NA)
  )

write_csv(baseline_clean, file.path(output_dir, "baseline.csv"), na = "")

# weekly
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

clean_weekly <- function(dat) {
  dat <- dat %>%
    clean_blanks() %>%
    to_numeric(weekly_vars)
  
  dat %>%
    mutate(
      # 1 = yes, 2 = no, 3 = prefer not
      smoke = case_when(smoke == 1 ~ 1, smoke == 2 ~ 0, TRUE ~ NA),
      
      drink = case_when(drink == 1 ~ 1, drink == 2 ~ 0, TRUE ~ NA),
      
      # Conditional smoking questions
      smoke_num = if_else(smoke == 1, smoke_num - 1, NA),
    )
}


# get all weekly surveys
weekly_files <- list.files(
  input_dir,
  pattern = "^w[0-9]+.*\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)


# Extract week number from filenames
week_numbers <- as.integer(str_match(
  basename(weekly_files),
  regex("(?:week|w)[_-]?(\\d+)", ignore_case = TRUE)
)[, 2])

# Validate filenames and number of surveys
stopifnot(
  length(weekly_files) == 30,!anyNA(week_numbers),
  setequal(week_numbers, 1:30),!anyDuplicated(week_numbers)
)

# Sort by week
ord <- order(week_numbers)
weekly_files <- weekly_files[ord]
week_numbers <- week_numbers[ord]

# Clean all weekly surveys
weekly_data <- map2(weekly_files, week_numbers, function(file, week) {
  dat <- read_csv(file, show_col_types = FALSE)
  
  cleaned <- clean_weekly(dat)
  
  # Save individual weekly dataset
  write_csv(cleaned, file.path(output_dir, paste0("w", week, ".csv")), na = "")
  
  # Add time identifier for longitudinal data
  cleaned %>%
    mutate(week = week, .before = 1)
})







######### recode wave data


recode_wave <- function(dat) {
  dat <- dat %>%
    clean_blanks()
  
  dat <- dat %>%
    mutate(across(any_of(c("smoke", "drink")), ~ .x - 1))
  
  
  
  if ("social_media_class" %in% names(dat)) {
    dat <- dat %>%
      mutate(
        social_media = case_when(
          social_media == 1 ~ 1,
          social_media == 4 ~ 2,
          social_media == 5 ~ 3,
          social_media == 6 ~ 4,
          TRUE ~ NA_real_
        )
      )
    
    dat <- dat %>%
      mutate(
        social_media_class = case_when(
          social_media_class == 1 ~ 1,
          social_media_class == 4 ~ 2,
          social_media_class == 5 ~ 3,
          social_media_class == 6 ~ 4,
          social_media_class == 7 ~ 5,
          TRUE ~ NA
        )
      )
    
    dat <- dat %>%
      mutate(across(any_of(c(
        "friend_conflict", "anxiety"
      )), ~ case_when(.x == 1 ~ 1, .x == 4 ~ 2, .x == 5 ~ 3, TRUE ~ NA)))
    
    dat <- dat %>%
      mutate(
        graduation_plan = case_when(
          graduation_plan %in% 1:4 ~ graduation_plan,
          graduation_plan == 5 ~ NA,
          TRUE ~ NA
        )
      )
  }
  
  
  
  return(dat)
  
}


# rename data
wave_names <- c(
  paste0("10week", 1:9),
  "graduation",
  "final"
)

# Process waves 2–12 (exclude wave 1)
for (wave in 2:12) {
  
  file <- file.path(input_dir, paste0("wave", wave, "-clean.csv"))
  
  dat <- read_csv(file, show_col_types = FALSE)
  
  recoded_wave <- recode_wave(dat)
  
  new_name <- wave_names[wave - 1]
  
  write_csv(
    recoded_wave,
    file.path(output_dir, paste0(new_name, ".csv")),
    na = ""
  )
}
