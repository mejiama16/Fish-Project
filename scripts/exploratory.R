#### Preamble ####
# Purpose: Loads the data from the repository: Fish Tether Experiment. 
# Author: Mariana Garcia Mejia
# Date: 16 Sep 2026
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

# Basic information
nrow(processed_data)
ncol(processed_data)

# What types of fish do we have?
processed_data |> group_by(species) |> count()
# Lake trout, Lake White Fish, Small Mouth Bass 
# Total rows = 14,575

# Class Imbalance? How many unique fish do we have for each specie?
print(processed_data |> group_by(fishNum) |> count(), n=60)
# 44 individual fish

# How many frequencies are there?
freq_cols <- processed_data |> select(matches("^F\\d+(\\.\\d+)?$")) |> names()
length(freq_cols)

# Check range
summary(df[freq_cols[10:20]])   # first few, since printing all ~1000 at once is unreadable

# Save the data 
write_csv(processed_data, here("data/analysis.csv"))

