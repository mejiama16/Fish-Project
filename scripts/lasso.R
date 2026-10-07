#### Preamble ####
# Purpose: Trains the Lasso Model
# Author: Mariana Garcia Mejia
# Date: 6 October 2026
# Contact: mariana.garcia@mail.utoronto.ca
# Pre-requisites: The `tidyverse` package must be installed

# Load the data
df_clean <- read_csv(here("data/clean.csv"))

# STEP 1: SPLIT TRAINING/VALIDATION AND TESTING SETS
# Classes (binary)
classes <- c("lakeTrout", "lakeWhitefish")

df_model <- df_clean %>%
  filter(species %in% classes)


# Check the first structure
# One row per fish: which species it is and how many pings it has.
fish_tbl <- df_model %>%
  group_by(fishNum, species) %>%
  summarise(n_pings = n(), .groups = "drop")

# How many fish per species do you have AFTER removing rows?
# Removing rows with missing bands may have eliminated some fish entirely.
fish_tbl %>% count(species)

# Pings per fish: this will vary a lot, which is why row counts per split
# will differ.
fish_tbl %>% group_by(species) %>%
  summarise(min = min(n_pings), median = median(n_pings), max = max(n_pings))

# Split train/validation/test by fish
# Key idea: we shuffle FISH IDs, not rows, so all pings from one fish always
# land in the same set. That prevents the model from being tested on
# individuals it has already seen during training (data leakage).
#
# We do it separately within each species (stratified) so that both LT and LW
# appear in every set. With so few fish, a random split could easily put zero
# LW fish in the test set.

# Make the split reproducible
set.seed(16)

assign_split <- function(ids, props = c(train = 0.5, val = 0.3, test = 0.2)) {
  n <- length(ids)
  ids <- ids[sample.int(n)]                 # shuffle the fish IDs
  n_train <- round(props["train"] * n)
  n_val   <- round(props["val"]   * n)
  tibble(
    fishNum = ids,
    split   = c(rep("train", n_train),
                rep("val",   n_val),
                rep("test",  n - n_train - n_val))   # whatever is left
  )
}

# Run the function once per species and stack the results
fish_split <- map_dfr(classes, function(sp) {
  ids <- fish_tbl$fishNum[fish_tbl$species == sp]
  assign_split(ids) %>% mutate(species = sp)
})

# Attach the split label to every ping of each fish
df_model <- df_model %>%
  left_join(fish_split %>% select(fishNum, split), by = "fishNum")

# Check the split
# Fish per split and species: each cell should be at least 1, ideally 2+
fish_split %>% count(split, species) %>%
  pivot_wider(names_from = species, values_from = n)

# Pings per split and species: expect these to be uneven, since fish differ
# in how many pings they have
df_model %>% count(split, species) %>%
  pivot_wider(names_from = species, values_from = n)

# No fish should appear in more than one split (should return 0 rows)
df_model %>% distinct(fishNum, split) %>% count(fishNum) %>% filter(n > 1)

## Think about doing leave-one-fish-out
# Train model on 19, test on 1, repeat 20 times

# STEP 2: CHOOSE PREDICTOR COLUMNS
# Only the frequency columns go into the model. 
freq_cols <- grep("^F[0-9]+(\\.[0-9]+)?$", names(df_model), value = TRUE)
length(freq_cols)    # should be 426

# Separate the three sets
df_train <- df_model %>% filter(split == "train")
df_val   <- df_model %>% filter(split == "val")
df_test  <- df_model %>% filter(split == "test")

#nrow(df_train)
#nrow(df_val)
#nrow(df_test)

# Build design matrices
# glmnet needs a plain numeric matrix, not a tibble.
X_train_raw <- df_train %>% select(all_of(freq_cols)) %>% as.matrix()
X_val_raw   <- df_val   %>% select(all_of(freq_cols)) %>% as.matrix()
X_test_raw  <- df_test  %>% select(all_of(freq_cols)) %>% as.matrix()

# Standardize training statistics only
# Lasso penalizes the size of coefficients, so columns on different scales would
# be penalized unequally. Standardizing (mean 0, SD 1) puts all frequencies on
# the same footing.
# Important: the means and SDs come from the training pings ONLY. Validation and
# test sets are transformed with those same numbers. If we computed them on all
# the data, information from the test fish would leak into training.
mu  <- colMeans(X_train_raw)
sdv <- apply(X_train_raw, 2, sd)

# A column with SD = 0 (constant) would give NaN/Inf when we divide by it.
any(sdv == 0)    # should be FALSE. If TRUE, tell me and we'll drop those columns.

standardize <- function(X) {
  X <- sweep(X, 2, mu,  FUN = "-")    # subtract the training mean of each column
  X <- sweep(X, 2, sdv, FUN = "/")    # divide by the training SD of each column
  X
}

X_train <- standardize(X_train_raw)
X_val   <- standardize(X_val_raw)
X_test  <- standardize(X_test_raw)

# Build the response matrix
# glmnet's logistic regression models the probability of the SECOND factor level.
# I'm putting lakeTrout second, so predicted probabilities mean P(lake trout).
lvls <- c("lakeWhitefish", "lakeTrout")
y_train <- factor(df_train$species, levels = lvls)
y_val   <- factor(df_val$species,   levels = lvls)
y_test  <- factor(df_test$species,  levels = lvls)

# Keep the fish IDs aligned with the rows; we need them for grouped CV folds
# and for fish-level evaluation later.
fish_train <- df_train$fishNum
fish_val   <- df_val$fishNum
fish_test  <- df_test$fishNum

# ---- 2f. Weights: every fish counts equally, and the classes are balanced ----
# Fish have very different numbers of pings. Without weights, a fish with 1,500
# pings would dominate one with 50. Two steps:
#   1) each ping gets 1 / (pings in its fish), so every fish has total weight 1
#   2) divide by the number of fish in its class, so LT and LW have equal total
#      weight even though there are 6 LT fish and 4 LW fish
# Finally rescale so the average weight is 1 (keeps lambda on a familiar scale).
pings_per_fish <- table(fish_train)
fish_class <- df_train %>% distinct(fishNum, species)
fish_per_class <- table(fish_class$species)

w_train <- 1 / (as.numeric(pings_per_fish[as.character(fish_train)]) *
                  as.numeric(fish_per_class[as.character(df_train$species)]))
w_train <- w_train * length(w_train) / sum(w_train)

# Sanity checks
dim(X_train); dim(X_val); dim(X_test)    # columns should all be 426

# Training columns should now have mean ~0 and SD ~1
summary(colMeans(X_train))                    # all near 0
summary(apply(X_train, 2, sd))                # all near 1
# Val/test will NOT be exactly 0/1, which is expected and correct.

# No missing or infinite values anywhere
c(sum(!is.finite(X_train)), sum(!is.finite(X_val)), sum(!is.finite(X_test)))   # 0 0 0

# Each class should now carry equal total weight in training (both ~ N/2)
tapply(w_train, y_train, sum)

# Each fish should carry equal weight within its class
tibble(fishNum = fish_train, species = df_train$species, w = w_train) %>%
  group_by(species, fishNum) %>% summarise(total_w = sum(w), .groups = "drop")

## STEP 3: CROSS VALIDATION
library(glmnet)

# ---- 3a. Build the cross-validation folds BY FISH ---------------------------
# cv.glmnet normally assigns pings to folds at random, which would put pings
# from the same fish on both sides of a fold. That makes the CV error far too
# optimistic and picks a lambda that is too small. So we assign whole fish to
# folds ourselves and pass them in through `foldid`.
#
# We use 4 folds (not 5): with only 4 LW fish, 4 folds lets every fold hold
# exactly one LW fish, so each fold contains both species.
# (I said 5 folds of 2 fish earlier; 4 folds works better with these counts.)
set.seed(16)
n_folds <- 4

fish_info <- df_train %>% distinct(fishNum, species)   # one row per training fish

fish_info <- fish_info %>%
  group_by(species) %>%
  # Within each species, spread fish evenly over the folds, in random order
  mutate(fold = sample(rep_len(1:n_folds, n()))) %>%
  ungroup()

# Give every PING the fold number of its fish
foldid <- fish_info$fold[match(fish_train, fish_info$fishNum)]

# Check: fish per fold and species. Each fold should have both species.
fish_info %>% count(fold, species) %>%
  pivot_wider(names_from = species, values_from = n, values_fill = 0)

# ---- 3b. Run cross-validated lasso ------------------------------------------
# family = "binomial" -> logistic regression
# alpha = 1           -> lasso penalty (alpha = 0 would be ridge)
# weights = w_train   -> fish-equalized, class-balanced weights from Step 2
# foldid = foldid     -> the fish-based folds from above
# standardize = FALSE -> we already standardized in Step 2
# type.measure = "deviance" -> CV criterion: weighted logistic loss
cv_fit <- cv.glmnet(
  x = X_train,
  y = y_train,
  family = "binomial",
  alpha = 1,
  weights = w_train,
  foldid = foldid,
  standardize = FALSE,
  type.measure = "deviance"
)

# ---- 3c. Look at the result -------------------------------------------------
# The plot shows CV error against log(lambda). The two dashed lines are:
#   lambda.min = lambda with the lowest CV error
#   lambda.1se = the largest lambda within 1 standard error of the minimum
#                (a simpler model that performs about as well)
plot(cv_fit)

cv_fit$lambda.min
cv_fit$lambda.1se

# ---- 3d. How many frequencies does each choice keep? ------------------------
# Lasso sets many coefficients to exactly zero; the non-zero ones are the
# "selected" frequencies. (The intercept is excluded from the count.)
n_selected <- function(s) {
  b <- coef(cv_fit, s = s)
  sum(b[-1, 1] != 0)
}
n_selected("lambda.min")
n_selected("lambda.1se")

# Which frequencies are selected at lambda.1se, and with what sign/size?
b <- coef(cv_fit, s = "lambda.1se")
selected <- tibble(freq = rownames(b)[-1], coef = b[-1, 1]) %>%
  filter(coef != 0) %>%
  arrange(desc(abs(coef)))
selected

# Diagnostics
# 1. How many frequencies are kept at lambda.min? (nonzero here means the minimum
#    is not the empty model)
n_selected("lambda.min")

# 2. Compare the best CV error with the intercept-only error. The largest lambda
#    on the path gives (almost) the empty model.
tibble(lambda = cv_fit$lambda,
       cv_dev = cv_fit$cvm,
       cv_se  = cv_fit$cvsd,
       n_nonzero = cv_fit$nzero) %>%
  slice(c(1, which.min(cv_dev), nrow(.)))   # first = empty model, then best, then smallest lambda

# 3. The CV curve. Look at its shape: is the dip below the leftmost value tiny
#    compared with the error bars?
plot(cv_fit)

# 4. Per-fold errors, so you can see whether one fold drives everything.
#    (Fold id from Step 3a; this refits the 4 folds at lambda.min.)
fold_check <- map_dfr(1:n_folds, function(k) {
  tr <- foldid != k
  fit <- glmnet(X_train[tr, ], y_train[tr], family = "binomial", alpha = 1,
                weights = w_train[tr], standardize = FALSE,
                lambda = cv_fit$lambda.min)
  p <- as.numeric(predict(fit, X_train[!tr, ], type = "response"))
  tibble(fold = k,
         held_out = paste(unique(fish_train[!tr]), collapse = ","),
         species = paste(unique(as.character(y_train[!tr])), collapse = ","),
         mean_p_LT = mean(p))
})
fold_check

# Most important features!!!
b <- coef(cv_fit, s = "lambda.min")

imp <- tibble(freq_name = rownames(b)[-1], coef = b[-1, 1]) %>%
  filter(coef != 0) %>%
  mutate(kHz = as.numeric(sub("^F", "", freq_name))) %>%
  arrange(desc(abs(coef)))

head(imp, 15)      # the top 15 frequencies
nrow(imp)          # how many were kept

# Extra Plot: where in the spectrum are the selected frequencies?
# Positive coef = higher TS pushes toward lake trout; negative = toward whitefish
ggplot(imp, aes(kHz, coef)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_segment(aes(xend = kHz, yend = 0)) +
  geom_point() +
  labs(x = "Frequency (kHz)", y = "Standardized lasso coefficient",
       title = "Frequencies selected by lasso (lambda.min)")

# One spectrum per fish, averaged in the linear domain (then back to dB)
fish_means <- df_model %>%
  select(fishNum, species, all_of(freq_cols)) %>%
  mutate(across(all_of(freq_cols), ~ 10^(.x / 10))) %>%
  group_by(fishNum, species) %>%
  summarise(across(all_of(freq_cols), mean), .groups = "drop") %>%
  mutate(across(all_of(freq_cols), ~ 10 * log10(.x)))

is_LT <- fish_means$species == "lakeTrout"

auc_tbl <- map_dfr(freq_cols, function(f) {
  x <- fish_means[[f]]
  w <- suppressWarnings(wilcox.test(x[is_LT], x[!is_LT]))
  tibble(kHz = as.numeric(sub("^F", "", f)),
         auc = unname(w$statistic) / (sum(is_LT) * sum(!is_LT)),
         p   = w$p.value)
})

ggplot(auc_tbl, aes(kHz, auc)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_line() +
  labs(x = "Frequency (kHz)", y = "AUC, LT vs LW (fish level)",
       title = "Per-frequency separation of lake trout and lake whitefish")

auc_tbl %>% arrange(desc(abs(auc - 0.5))) %>% head(10)
