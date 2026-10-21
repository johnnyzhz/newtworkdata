#### weekly

# Participant IDs
cnames <- paste0("s", 1:47)

# Covariates
co_vars <- c(
  "Q6",
  "Q2",
  "Q3",
  "Q4",
  "Q5",
  "Q7",
  "Q8_1",
  "Q8_2",
  "Q8_3",
  "Q20",
  "Q21",
  "Q22",
  "Q23",
  "Q24",
  "Q26",
  "Q27",
  "Q16_1",
  "Q17_1",
  "Q18_1"
)

co_names <- c(
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

# Create output directory
dir.create("clean-files", showWarnings = FALSE)

cleanfunc <- function(filename, week) {
  dat <- read.csv(filename, fileEncoding = "UTF-8", check.names = T)
  # id is in column 84 before week 11 and 85 after
  id_col <- "Q21.1"
  
  sn_cols <- 37:83
  
  
  sn_vars <- c(id_col, colnames(dat)[sn_cols])
  
  sn_names <- c("id", sub(".* - (s[0-9]+)$", "\\1", as.character(unlist(dat[1, sn_cols]))))
  
  clean <- dat[, c(sn_vars, co_vars), drop = FALSE]
  colnames(clean) <- c(sn_names, co_names)
  
  
  clean <- clean[-c(1, 2), , drop = FALSE]
  
  
  recode_map <- c(
    "13" = 1,
    "14" = 2,
    "15" = 3,
    "16" = 4,
    "17" = 5,
    "19" = 0,
    "20" = NA
  )
  
  network_cols <- 2:48
  
  for (j in network_cols) {
    x <- as.character(clean[[j]])
    matched <- x %in% names(recode_map)
    x[matched] <- as.character(recode_map[x[matched]])
    clean[[j]] <- suppressWarnings(as.numeric(x))
  }
  
  # remove dup
  duplicate_cols <- c("s2.1", "s12.1", "s22.1", "s32.1", "s42.1")
  
  clean <- clean[, !names(clean) %in% duplicate_cols, drop = FALSE]
  
  
  missing_cols <- setdiff(cnames, names(clean))
  
  for (col in missing_cols) {
    clean[[col]] <- NA
  }
  
  # Keep first observation for duplicate IDs
  clean <- clean[!duplicated(clean$id), , drop = FALSE]
  
  
  clean <- merge(
    data.frame(id = cnames),
    clean,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Sort by participant ID
  clean <- clean[match(cnames, clean$id), , drop = FALSE]
  
  # Standardize column order
  clean <- clean[, c("id", cnames, co_names), drop = FALSE]
  
  rownames(clean) <- NULL
  
  # Convert all empty to NA
  clean[] <- lapply(clean, function(x) {
    if (is.character(x)) {
      x[trimws(x) == ""] <- NA
    }
    x
  })
  
  
  return(clean)
}

# apply the clean function
weekly_data <- vector("list", 30)
names(weekly_data) <- paste0("w", 1:30)

for (week in 1:30) {
  input_file <- paste0("anon/guangzhou_w", week, "_anonymous.csv")
  
  if (!file.exists(input_file)) {
    warning("Missing file: ", input_file)
    next
  }
  
  wclean <- cleanfunc(input_file, week)
  
  weekly_data[[week]] <- wclean
  
  output_file <- paste0("clean-files/w", week, "-clean.csv")
  
  write.csv(wclean, output_file, row.names = FALSE, na = "")
  
  cat("Completed week", week, "\n")
}


################ baseline


bl <- read.csv(
  "anon/guangzhou_baseline_anonymous.csv",
  fileEncoding = "UTF-8",
  check.names = FALSE
)

# Network variables
snvars <- colnames(bl)[c(19, 27:73)]
snnames <- c("id", as.character(unlist(bl[1, 27:73])))

# Covariates
covars <- c(
  "Q1",
  "Q3",
  "Q4",
  "Q5",
  "Q6",
  "Q7",
  "Q8",
  "Q10",
  "Q54.1",
  "Q55",
  "Q56",
  "Q57",
  "Q58",
  "Q59",
  "Q60",
  "Q61",
  "Q62",
  "Q63",
  "Q64",
  "Q65",
  "Q66",
  "Q67",
  "Q68",
  "Q69",
  "Q70",
  "Q71",
  "Q72",
  "Q33",
  "Q73",
  "Q74",
  "Q75",
  "Q76",
  "Q77",
  "Q78",
  "Q41",
  "Q79",
  "Q80",
  "Q81",
  "Q83",
  "Q84",
  "Q85",
  "Q86",
  "Q87",
  "Q88",
  "Q51_1",
  "Q52_1",
  "Q53_1",
  "Q54_1"
)


#
# conames <- c(
#   "age", "sex", "height", "weight", "score",
#   "smoking", "drinking", "talk_little",
#   "forget", "abstract_notion", "interest_problems",
#   "upset", "attention", "procrastinate",
#   "lack_imagination", "interest_others",
#   "moodswing", "party_chat", "organized",
#   "abstract_opinion", "empathy_feeling",
#   "relax", "party_atmos", "mess",
#   "lively_imagination", "empathy_emotion",
#   "gloomy", "lack_motivation", "depressed",
#   "sleep", "tired", "eat", "focus", "action",
#   "tasks_alone", "talk_to", "lonely_bear",
#   "understand", "call", "lonely_complete",
#   "communicate", "company", "make_friends",
#   "isolate", "happy_overall", "happy_compare",
#   "overall_happy_person", "overall_unhappy_person"
# )


conames <- c(
  # Demographics
  "age",
  "sex",
  "height",
  "weight",
  "score",
  "smoking",
  "drinking",
  
  # Big Five personality items
  "BF1",
  "BF2",
  "BF3",
  "BF4",
  "BF5",
  "BF6",
  "BF7",
  "BF8",
  "BF9",
  "BF10",
  "BF11",
  "BF12",
  "BF13",
  "BF14",
  "BF15",
  "BF16",
  "BF17",
  "BF18",
  "BF19",
  "BF20",
  
  # PHQ
  "PHQ1",
  "PHQ2",
  "PHQ3",
  "PHQ4",
  "PHQ5",
  "PHQ6",
  "PHQ7",
  
  # UCLA loneliness
  "L1",
  "L2",
  "L3",
  "L4",
  "L5",
  "L6",
  "L7",
  "L8",
  "L9",
  "L10",
  
  # Happiness items
  "H1",
  "H2",
  "H3",
  "H4"
)


# Select variables
baseline <- bl[, c(snvars, covars), drop = FALSE]
colnames(baseline) <- c(snnames, conames)

# Remove metadata row
baseline <- baseline[-1, , drop = FALSE]


recode_map <- c(
  "1" = 1,
  "2" = 2,
  "4" = 3,
  "5" = 4,
  "7" = 5,
  "8" = 0,
  # Self
  "9" = NA # Refused to respond
)

for (j in 2:48) {
  x <- as.character(baseline[[j]])
  matched <- x %in% names(recode_map)
  x[matched] <- as.character(recode_map[x[matched]])
  baseline[[j]] <- suppressWarnings(as.numeric(x))
}

# fix id

cnames <- paste0("s", 1:47)

# Remove duplicate network columns
baseline <- baseline[, !duplicated(names(baseline)), drop = FALSE]

# Add missing network columns
for (col in setdiff(cnames, names(baseline))) {
  baseline[[col]] <- NA
}

# Remove duplicate participants
baseline <- baseline[!duplicated(baseline$id), , drop = FALSE]

# Add missing participants
baseline <- merge(
  data.frame(id = cnames),
  baseline,
  by = "id",
  all.x = TRUE,
  sort = FALSE
)

# Sort participants
baseline <- baseline[match(cnames, baseline$id), , drop = FALSE]

# Standardize column order
baseline <- baseline[, c("id", cnames, conames), drop = FALSE]

rownames(baseline) <- NULL

# save
# Convert all empty to NA

if (is.character(baseline)) {
  baseline[trimws(baseline) == ""] <- NA
}


write.csv(baseline,
          "clean-files/baseline-clean.csv",
          row.names = FALSE,
          na = "")


##################### wavw data


cnames <- paste0("s", 1:47)

# Demographics
demo_vars <- c("Q1", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8")

demo_names <- c("age", "gender", "height", "weight", "score", "smoke", "drink")

# Mini-IPIP personality (20 items)
personality_vars <- c("Q10", paste0("Q", 54:72))

personality_names <- paste0("BF", 1:20)

# Depression (7 items)
depression_vars <- c("Q33", paste0("Q", 73:78))

depression_names <- paste0("PHD", 1:7)

# Loneliness (10 items)
loneliness_vars <- c("Q41", "Q79", "Q80", "Q81", paste0("Q", 83:88))

loneliness_names <- paste0("L", 1:10)

# Subjective happiness (4 items)
happiness_vars <- c("Q51_1", "Q52_1", "Q53_1", "Q54_1")

happiness_names <- paste0("H", 1:4)


# additional for wave11,12
extra_vars <- c(
  paste0("sas_", 1:8),
  "Q59.1",
  "Q60.2",
  "Q61_1",
  "Q62.1",
  "Q63.1",
  "Q64.1",
  "Q65.1",
  "Q66.1",
  "Q66_4_TEXT",
  "Q68.1",
  "Q69.1"
)

extra_names <- c(
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


wave_cleanfunc <- function(filename) {
  dat <- read.csv(
    filename,
    fileEncoding = "UTF-8",
    check.names = TRUE,
    stringsAsFactors = FALSE
  )
  
  
  names(dat) <- make.unique(names(dat))
  
  id_col <- which(grepl("请输入您的名字", trimws(as.character(unlist(
    dat[1, ]
  ))), fixed = TRUE))
  
  
  question_labels <- as.character(unlist(dat[1, ]))
  sn_cols <- which(grepl("请评价你和下面学生之间的关系", trimws(question_labels)))
  
  
  if (length(sn_cols) > 0) {
    sn_names <- sub(".* - (s[0-9]+)$", "\\1", question_labels[sn_cols])
    
    # Keep valid participants
    valid_sn <- sn_names %in% cnames
    sn_cols <- sn_cols[valid_sn]
    sn_names <- sn_names[valid_sn]
    
    # Remove duplicate columns
    keep <- !duplicated(sn_names)
    sn_cols <- sn_cols[keep]
    sn_names <- sn_names[keep]
  } else {
    sn_names <- character(0)
  }
  
  
  # Create clean dataset
  clean <- data.frame(id = dat[, id_col], stringsAsFactors = FALSE)
  
  # Add network variables
  for (j in seq_along(sn_cols)) {
    clean[[sn_names[j]]] <- dat[[sn_cols[j]]]
  }
  
  
  # Add covariates
  for (j in seq_along(co_vars)) {
    if (co_vars[j] %in% names(dat)) {
      clean[[co_names[j]]] <- dat[[co_vars[j]]]
    } else {
      clean[[co_names[j]]] <- NA
    }
  }
  
  # Remove questionnaire metadata rows
  clean <- clean[-c(1, 2), , drop = FALSE]
  
  # Convert empty strings to NA
  clean[] <- lapply(clean, function(x) {
    if (is.character(x)) {
      x <- trimws(x)
      x[x == ""] <- NA
    }
    x
  })
  
  # Recode network variables
  clean <- clean[, colSums(!is.na(clean)) > 0, drop = FALSE]
  
  recode_map <- c(
    "13" = 1,
    "14" = 2,
    "15" = 3,
    "16" = 4,
    "17" = 5,
    "19" = 0,
    "20" = NA,
    "8" = 0,
    "9" = NA
  )
  
  has_sn <- length(sn_cols) > 0 &&
    any(sapply(clean[intersect(sn_names, names(clean))], function(x) {
      any(!is.na(x) & trimws(as.character(x)) != "")
    }))
  
  
  if (has_sn) {
    for (col in sn_names) {
      x <- as.character(clean[[col]])
      
      matched <- x %in% names(recode_map)
      
      x[matched] <- as.character(recode_map[x[matched]])
      
      clean[[col]] <- suppressWarnings(as.numeric(x))
    }
    
    
    #  Keep only valid participants
    clean <- clean[!is.na(clean$id) & clean$id %in% cnames, , drop = FALSE]
    
    # Keep first observation for duplicate IDs
    clean <- clean[!duplicated(clean$id), , drop = FALSE]
    
    # fill missing
    if (length(sn_cols) > 0) {
      missing_cols <- setdiff(cnames, names(clean))
      
      for (col in missing_cols) {
        clean[[col]] <- NA
      }
    }
    
    # Reorder and add missing participants
    clean <- clean[match(cnames, clean$id), , drop = FALSE]
    
    clean$id <- cnames
    
    # Standardize column order
    clean <- clean[, intersect(c("id", cnames, co_names), names(clean)), drop = FALSE]
  }
  
  
  return(clean)
}


# apply the clean function
wave_data <- vector("list", 12)
names(wave_data) <- paste0("wave", 1:12)

for (wave in 1:12) {
  if (wave > 10) {
    co_names <- c(
      demo_names,
      personality_names,
      depression_names,
      loneliness_names,
      happiness_names,
      extra_names
    )
    
    co_vars <- c(
      demo_vars,
      personality_vars,
      depression_vars,
      loneliness_vars,
      happiness_vars,
      extra_vars
    )
  } else {
    co_vars <- c(demo_vars,
                 personality_vars,
                 depression_vars,
                 loneliness_vars,
                 happiness_vars)
    
    co_names <- c(
      demo_names,
      personality_names,
      depression_names,
      loneliness_names,
      happiness_names
    )
  }
  
  input_file <- paste0("anon/guangzhou_wave", wave, "_anonymous.csv")
  
  if (!file.exists(input_file)) {
    warning("Missing file: ", input_file)
    next
  }
  
  wclean <- wave_cleanfunc(input_file)
  
  wave_data[[wave]] <- wclean
  
  output_file <- paste0("clean-files/wave", wave, "-clean.csv")
  
  write.csv(wclean, output_file, row.names = FALSE, na = "")
  
  cat("Completed wave", wave, "\n")
}
