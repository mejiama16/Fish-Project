#### Preamble ####
# Purpose: Cleans the data - mainly removes missing values
# Author: Mariana Garcia Mejia
# Date: 6 October 2026
# Contact: mariana.garcia@mail.utoronto.ca
# Pre-requisites: The `tidyverse` package must be installed

#### Workspace setup ####
library(tidyverse)
library(here)

# Load the data
load("data/processed_AnalysisData.Rdata")
processed_data

# Preview of the data
head(processed_data)
glimpse(processed_data)
names(processed_data)

# Identify flagged frequency columns
# Frequency columns are named F45, F45.5, F46, ...
freq_cols <- grep("^F[0-9]+(\\.[0-9]+)?$", names(df), value = TRUE)
freqs     <- as.numeric(sub("^F", "", freq_cols))

low  <- freq_cols[freqs >= 90  & freqs <= 170]   # 161 columns
high <- freq_cols[freqs >= 173 & freqs <= 260]   # 175 columns
flagged <- c(low, high)                          # 336 columns with missing values

# Remove rows that have at least one NA in any of the columns
# Only the flagged columns are checked; NAs in any other column are ignored.
n_before <- nrow(df)

df_clean <- df %>%
  filter(if_all(all_of(flagged), ~ !is.na(.x)))

n_removed <- n_before - nrow(df_clean)
cat("Rows before:", n_before,
    "| after:", nrow(df_clean),
    "| removed:", n_removed, "\n")      # expect 4242 removed

stopifnot(n_removed == 4242)             # stops with an error if it doesn't match

# Sanity checks
# No NAs left in the flagged columns
sum(is.na(df_clean[, flagged]))          # should be 0

# What did we lose, per species? (rows and fish)
before <- df       %>% group_by(species) %>%
  summarise(rows_before = n(), fish_before = n_distinct(fishNum))
after  <- df_clean %>% group_by(species) %>%
  summarise(rows_after  = n(), fish_after  = n_distinct(fishNum))

left_join(before, after, by = "species") %>%
  mutate(rows_lost_pct = round(100 * (1 - rows_after / rows_before), 1))

## Check all frequency columns
# All frequency columns (the 426 matching F45, F45.5, ...)
freq_cols <- grep("^F[0-9]+(\\.[0-9]+)?$", names(df_clean), value = TRUE)
length(freq_cols)                                  # should be 426

# Total NAs across all frequency columns
sum(is.na(df_clean[, freq_cols]))                  # should be 0

# Columns that still have any NA (should be empty)
na_by_col <- colSums(is.na(df_clean[, freq_cols]))
na_by_col[na_by_col > 0]                           # expect: named integer(0)

# Rows that still have an NA in any frequency column (should be 0)
sum(!complete.cases(df_clean[, freq_cols]))

### Save the clean data 
write_csv(df_clean, here("data/clean.csv"))
