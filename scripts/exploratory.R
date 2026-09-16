#### Preamble ####
# Purpose: Loads the data from the repository: Fish Tether Experiment. 
# Author: Mariana Garcia Mejia
# Date: 16 Sep 2026
# Contact: mariana.garcia@mail.utoronto.ca
# Pre-requisites: The `tidyverse` package must be installed

#### Workspace setup ####
library(tidyverse)


# Load the data
load("data/processed_AnalysisData.Rdata")
processed_data

# Preview of the data
head(processed_data)
glimpse(processed_data)

# Count of main components
processed_data |> group_by(fishNum) |> count()
# Question there is a different number of measures for each fish.

# Count of number of unique fish for each species
processed_data |> group_by(fishNum) |> group_by(species) |> count()
