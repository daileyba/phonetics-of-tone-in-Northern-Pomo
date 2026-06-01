## -----------------------------------------------------------------------------
library(mgcv)
library(tidyverse)
library(dplyr)
library(gridExtra)  # for arranging plots side by side


## -----------------------------------------------------------------------------
timeseries_no_NA_df <- read.csv("data/timeseries.csv")  # update path as needed


## -----------------------------------------------------------------------------
timeseries_no_NA_df <- timeseries_no_NA_df[!grepl("\\*", timeseries_no_NA_df$WORD_ID), ]


## -----------------------------------------------------------------------------
# Convert ALL categorical variables to factors ONCE
timeseries_no_NA_df$TONE <- as.factor(timeseries_no_NA_df$TONE)

timeseries_no_NA_df$PREV_WORD_TONE <- as.factor(timeseries_no_NA_df$PREV_WORD_TONE)
timeseries_no_NA_df$NEXT_WORD_TONE <- as.factor(timeseries_no_NA_df$NEXT_WORD_TONE)
timeseries_no_NA_df$SYLL_STRUCTURE <- as.factor(timeseries_no_NA_df$SYLL_STRUCTURE)
timeseries_no_NA_df$VOWEL_HEIGHT <- as.factor(timeseries_no_NA_df$VOWEL_HEIGHT)
timeseries_no_NA_df$VOWEL_FRONTNESS <- as.factor(timeseries_no_NA_df$VOWEL_FRONTNESS)
timeseries_no_NA_df$PREV_LARYNGEAL_CLASS <- as.factor(timeseries_no_NA_df$PREV_LARYNGEAL_CLASS)
timeseries_no_NA_df$NEXT_LARYNGEAL_CLASS <- as.factor(timeseries_no_NA_df$NEXT_LARYNGEAL_CLASS)
timeseries_no_NA_df$WORD_ID <- as.factor(timeseries_no_NA_df$WORD_ID)
timeseries_no_NA_df$PHRASE_ID <- as.factor(timeseries_no_NA_df$PHRASE_ID)
timeseries_no_NA_df$SENTENCE_ID <- as.factor(timeseries_no_NA_df$SENTENCE_ID)
timeseries_no_NA_df$FILE_ID <- as.factor(timeseries_no_NA_df$FILE_ID)

#set factor level order for LARYNGEAL_CLASS variables
timeseries_no_NA_df$PREV_LARYNGEAL_CLASS <- factor(timeseries_no_NA_df$PREV_LARYNGEAL_CLASS, levels = c("voiced", "voiceless", "aspirated", "glottalic"))
timeseries_no_NA_df$NEXT_LARYNGEAL_CLASS <- factor(timeseries_no_NA_df$NEXT_LARYNGEAL_CLASS, levels = c("voiced", "voiceless", "aspirated", "glottalic", "PAU"))

#set factor level order for PREV/NEXT TONE variables
timeseries_no_NA_df$PREV_WORD_TONE <- factor(timeseries_no_NA_df$PREV_WORD_TONE, levels = c("H", "L", "N", "PAU"))
timeseries_no_NA_df$NEXT_WORD_TONE <- factor(timeseries_no_NA_df$NEXT_WORD_TONE, levels = c("H", "L", "N", "PAU"))

#set factor level order for SYLL_STRUCTURE
timeseries_no_NA_dfSYLL_STRUCTURE <- factor(timeseries_no_NA_df$SYLL_STRUCTURE, levels = c("cvv", "cvc"))
timeseries_no_NA_df$SYLL_STRUCTURE <- relevel(timeseries_no_NA_df$SYLL_STRUCTURE, ref="cvv")

# Verify
print("Factor levels:")
print(paste("TONE:", levels(timeseries_no_NA_df$TONE)))
print(paste("PREV_WORD_TONE:", levels(timeseries_no_NA_df$PREV_WORD_TONE)))


## -----------------------------------------------------------------------------
#Visualization of raw data

#Visualizing contour with two tones assumed

# Prepare data
plot_data <- timeseries_no_NA_glott_collapse_df %>%
  mutate(f0_hz = exp(LOG_F0)) %>%
  filter(TONE %in% c("H", "L"))

# Calculate mean trajectory
plot_summary <- plot_data %>%
  group_by(NORM_TIME_STANDARD, TONE) %>%
  summarise(
    mean_f0 = mean(f0_hz, na.rm = TRUE),
    se_f0 = sd(f0_hz, na.rm = TRUE) / sqrt(n()),
    lower = mean_f0 - 1.96 * se_f0,
    upper = mean_f0 + 1.96 * se_f0,
    .groups = "drop"
  )

# Calculate y-axis limits based on mean ± 1.5 SD (tighter zoom)
overall_mean <- mean(plot_data$f0_hz, na.rm = TRUE)
overall_sd <- sd(plot_data$f0_hz, na.rm = TRUE)
#zoom_floor <- overall_mean - 1.5 * overall_sd
#zoom_ceiling <- overall_mean + 1.5 * overall_sd

zoom_floor <- 125
zoom_ceiling <- 150

# Combined plot: spaghetti + mean
p_combined <- ggplot() +
  # Individual trajectories (behind)
  geom_line(data = plot_data,
            aes(x = NORM_TIME_STANDARD, y = f0_hz,
                group = interaction(WORD_ID, PHRASE_ID, FILE_ID),
                color = TONE),
            alpha = 0.1, linewidth = 0.3) +
  # Confidence ribbon
  geom_ribbon(data = plot_summary,
              aes(x = NORM_TIME_STANDARD, ymin = lower, ymax = upper, fill = TONE),
              alpha = 0.3) +
  # Mean line (on top)
  geom_line(data = plot_summary,
            aes(x = NORM_TIME_STANDARD, y = mean_f0, color = TONE),
            linewidth = 2) +
  scale_color_manual(values = c("H" = "orange", "L" = "purple")) +
  scale_fill_manual(values = c("H" = "orange", "L" = "purple")) +
  scale_y_continuous(limits = c(zoom_floor, zoom_ceiling)) +
  labs(
    title = "Observed F0 Contours by Tone",
    subtitle = "Thin lines: individual words | Thick lines: mean ± 95% CI",
    x = "Normalized Time",
    y = "F0 (Hz)"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(p_combined)

# Summary statistics with slopes and start/end
cat("\n=== OBSERVED DATA SUMMARY ===\n\n")

cat("H TONE (raw data):\n")
h_summary <- plot_summary %>%
  filter(TONE == "H") %>%
  summarise(
    start_f0 = mean_f0[1],
    end_f0 = mean_f0[n()],
    max_f0 = max(mean_f0),
    max_f0_time = NORM_TIME_STANDARD[which.max(mean_f0)],
    min_f0 = min(mean_f0),
    min_f0_time = NORM_TIME_STANDARD[which.min(mean_f0)],
    slope = end_f0 - start_f0,
    range = max_f0 - min_f0,
    mean_f0 = mean(mean_f0)
  )

h_raw <- plot_data %>%
  filter(TONE == "H") %>%
  summarise(
    overall_mean = mean(f0_hz, na.rm = TRUE),
    median_f0 = median(f0_hz, na.rm = TRUE),
    sd_f0 = sd(f0_hz, na.rm = TRUE),
    n_obs = n(),
    n_words = n_distinct(WORD_ID, PHRASE_ID, FILE_ID)
  )

cat("  Start F0:", round(h_summary$start_f0, 2), "Hz\n")
cat("  End F0:", round(h_summary$end_f0, 2), "Hz\n")
cat("  Max F0:", round(h_summary$max_f0, 2), "Hz (at time", round(h_summary$max_f0_time, 3), ")\n")
cat("  Min F0:", round(h_summary$min_f0, 2), "Hz (at time", round(h_summary$min_f0_time, 3), ")\n")
cat("  Range (max-min):", round(h_summary$range, 2), "Hz\n")
cat("  Slope (end-start):", round(h_summary$slope, 2), "Hz\n")
cat("  Mean F0 (trajectory):", round(h_summary$mean_f0, 2), "Hz\n")
cat("  Mean F0 (all obs):", round(h_raw$overall_mean, 2), "Hz\n")
cat("  Median F0:", round(h_raw$median_f0, 2), "Hz\n")
cat("  SD:", round(h_raw$sd_f0, 2), "Hz\n")
cat("  N observations:", h_raw$n_obs, "\n")
cat("  N words:", h_raw$n_words, "\n\n")

cat("L TONE (raw data):\n")
l_summary <- plot_summary %>%
  filter(TONE == "L") %>%
  summarise(
    start_f0 = mean_f0[1],
    end_f0 = mean_f0[n()],
    max_f0 = max(mean_f0),
    max_f0_time = NORM_TIME_STANDARD[which.max(mean_f0)],
    min_f0 = min(mean_f0),
    min_f0_time = NORM_TIME_STANDARD[which.min(mean_f0)],
    slope = end_f0 - start_f0,
    range = max_f0 - min_f0,
    mean_f0 = mean(mean_f0)
  )

l_raw <- plot_data %>%
  filter(TONE == "L") %>%
  summarise(
    overall_mean = mean(f0_hz, na.rm = TRUE),
    median_f0 = median(f0_hz, na.rm = TRUE),
    sd_f0 = sd(f0_hz, na.rm = TRUE),
    n_obs = n(),
    n_words = n_distinct(WORD_ID, PHRASE_ID, FILE_ID)
  )

cat("  Start F0:", round(l_summary$start_f0, 2), "Hz\n")
cat("  End F0:", round(l_summary$end_f0, 2), "Hz\n")
cat("  Max F0:", round(l_summary$max_f0, 2), "Hz (at time", round(l_summary$max_f0_time, 3), ")\n")
cat("  Min F0:", round(l_summary$min_f0, 2), "Hz (at time", round(l_summary$min_f0_time, 3), ")\n")
cat("  Range (max-min):", round(l_summary$range, 2), "Hz\n")
cat("  Slope (end-start):", round(l_summary$slope, 2), "Hz\n")
cat("  Mean F0 (trajectory):", round(l_summary$mean_f0, 2), "Hz\n")
cat("  Mean F0 (all obs):", round(l_raw$overall_mean, 2), "Hz\n")
cat("  Median F0:", round(l_raw$median_f0, 2), "Hz\n")
cat("  SD:", round(l_raw$sd_f0, 2), "Hz\n")
cat("  N observations:", l_raw$n_obs, "\n")
cat("  N words:", l_raw$n_words, "\n\n")

cat("DIFFERENCE (H - L, raw data):\n")
cat("  Start difference:", round(h_summary$start_f0 - l_summary$start_f0, 2), "Hz\n")
cat("  End difference:", round(h_summary$end_f0 - l_summary$end_f0, 2), "Hz\n")
cat("  Max difference:", round(h_summary$max_f0 - l_summary$max_f0, 2), "Hz\n")
cat("  Min difference:", round(h_summary$min_f0 - l_summary$min_f0, 2), "Hz\n")
cat("  Mean difference:", round(h_raw$overall_mean - l_raw$overall_mean, 2), "Hz\n")
cat("  Median difference:", round(h_raw$median_f0 - l_raw$median_f0, 2), "Hz\n")
cat("  Slope difference:", round(h_summary$slope - l_summary$slope, 2), "Hz\n")


## -----------------------------------------------------------------------------
##### MODELING ######


## -----------------------------------------------------------------------------
# model syntax

model <- bam(DEPENDENT_VARIABLE ~ INDEPENDENT_VARIABLES * INTERACTION + SMOOTH + TENSOR_STUFF + RANDOM_EFFECTS,
             data = #your dataset
             discrete = #default?
             method = #distribution_type/model_assumptions/model_tyep
             )

#complex model example:
#LSA final model

model_no_ar <- bam(
  LOG_F0 ~ TONE * PREV_WORD_TONE + TONE * NEXT_WORD_TONE +
         VOWEL_HEIGHT + SEG_DURATION + TONE * SYLL_STRUCTURE +
         TONE * PREV_LARYNGEAL_CLASS + TONE * NEXT_LARYNGEAL_CLASS +
         RELATIVE_WORD_POSITION_IN_PHRASE * NUM_OF_WORDS_IN_PHRASE +
         s(NORM_TIME_STANDARD, by=TONE, k=6) +
         s(NORM_TIME_STANDARD, by=interaction(TONE, PREV_WORD_TONE), k=6) +
         s(NORM_TIME_STANDARD, by=interaction(TONE, NEXT_WORD_TONE), k=6) +
         #s(NORM_TIME_STANDARD, by=SYLL_STRUCTURE, k=6) +
         s(NORM_TIME_STANDARD, by=PREV_LARYNGEAL_CLASS, k=6) +
         s(NORM_TIME_STANDARD, by=NEXT_LARYNGEAL_CLASS, k=6) +
         s(PHRASE_ID, bs="re"),
  data = timeseries_no_NA_glott_collapse_df,
  discrete = TRUE,
  method = "fREML"
)


## -----------------------------------------------------------------------------
###### TESTING MODELS ######


## -----------------------------------------------------------------------------
#comparing AIC
#AIC measures the relative amount of information lost when a given model is used to represent the data generating process
#It's calculated from two components — the likelihood of the model (how well it fits the data) and a penalty for the number of parameters. The formula is:
#AIC = 2k − 2ln(L)
#where k is the number of parameters and L is the likelihood.
#lower number is better


#turn into a function
print(AIC(model_1, model_2, model_n))


## -----------------------------------------------------------------------------
# Function to analyze and summarize concurvity from a GAM model
analyze_concurvity <- function(gam_model, threshold = 0.8, moderate_threshold = 0.5) {

  cat("=== CONCURVITY ANALYSIS ===\n\n")
  cat("Note: Focusing on PAIRWISE concurvity, which is the meaningful metric.\n")
  cat("(Full concurvity with all terms combined is often high and less informative)\n\n")

  # Get pairwise concurvity measures
  conc_pair <- concurvity(gam_model, full = FALSE)

  # Handle different output formats from concurvity()
  # Check if it's a list or array
  if (is.list(conc_pair)) {
    # List format: conc_pair$worst, conc_pair$observed, conc_pair$estimate
    pair_obs <- conc_pair$observed
    pair_est <- conc_pair$estimate
    pair_worst <- conc_pair$worst
  } else if (is.array(conc_pair) && length(dim(conc_pair)) == 3) {
    # 3D array format
    pair_obs <- conc_pair[, , "observed"]
    pair_est <- conc_pair[, , "estimate"]
    pair_worst <- conc_pair[, , "worst"]
  } else {
    # Fallback - just use whatever we got as observed
    pair_obs <- conc_pair
    pair_est <- NULL
    pair_worst <- NULL
  }

  # 1. OBSERVED pairwise concurvity (most important)
  cat("1. PAIRWISE OBSERVED CONCURVITY:\n")
  cat("   (Based on actual relationships in your data)\n\n")

  # Find high pairwise concurvities
  high_pairs <- which(pair_obs > threshold & pair_obs < 1, arr.ind = TRUE)
  moderate_pairs <- which(pair_obs > moderate_threshold & pair_obs <= threshold, arr.ind = TRUE)

  # Remove duplicates (keep only upper triangle)
  if (nrow(high_pairs) > 0) {
    high_pairs <- high_pairs[high_pairs[,1] < high_pairs[,2], , drop = FALSE]
  }
  if (nrow(moderate_pairs) > 0) {
    moderate_pairs <- moderate_pairs[moderate_pairs[,1] < moderate_pairs[,2], , drop = FALSE]
  }

  # High concurvity pairs
  if (nrow(high_pairs) > 0) {
    cat("   ⚠️  HIGH CONCURVITY (>", threshold, "):\n")

    if (!is.null(pair_est)) {
      pair_df_high <- data.frame(
        Term1 = rownames(pair_obs)[high_pairs[,1]],
        Term2 = colnames(pair_obs)[high_pairs[,2]],
        Observed = round(pair_obs[high_pairs], 3),
        Estimate = round(pair_est[high_pairs], 3)
      )
    } else {
      pair_df_high <- data.frame(
        Term1 = rownames(pair_obs)[high_pairs[,1]],
        Term2 = colnames(pair_obs)[high_pairs[,2]],
        Observed = round(pair_obs[high_pairs], 3)
      )
    }
    pair_df_high <- pair_df_high[order(-pair_df_high$Observed), ]
    print(pair_df_high, row.names = FALSE)
    cat("\n")
  } else {
    cat("   ✓ No high concurvity pairs (>", threshold, ")\n\n")
  }

  # Moderate concurvity pairs
  if (nrow(moderate_pairs) > 0) {
    cat("   ⚡ MODERATE CONCURVITY (", moderate_threshold, "-", threshold, "):\n")
    cat("   (Worth monitoring, may not require action)\n")

    if (!is.null(pair_est)) {
      pair_df_mod <- data.frame(
        Term1 = rownames(pair_obs)[moderate_pairs[,1]],
        Term2 = colnames(pair_obs)[moderate_pairs[,2]],
        Observed = round(pair_obs[moderate_pairs], 3),
        Estimate = round(pair_est[moderate_pairs], 3)
      )
    } else {
      pair_df_mod <- data.frame(
        Term1 = rownames(pair_obs)[moderate_pairs[,1]],
        Term2 = colnames(pair_obs)[moderate_pairs[,2]],
        Observed = round(pair_obs[moderate_pairs], 3)
      )
    }
    pair_df_mod <- pair_df_mod[order(-pair_df_mod$Observed), ]
    print(pair_df_mod, row.names = FALSE)
    cat("\n")
  } else {
    cat("   ✓ No moderate concurvity pairs (", moderate_threshold, "-", threshold, ")\n\n")
  }

  # 2. Summary statistics
  cat("\n2. SUMMARY STATISTICS:\n\n")

  # Get upper triangle only (exclude diagonal)
  upper_tri_idx <- which(upper.tri(pair_obs), arr.ind = TRUE)
  upper_obs <- pair_obs[upper_tri_idx]

  cat("   Maximum pairwise observed concurvity:", round(max(upper_obs), 3), "\n")
  cat("   Mean pairwise observed concurvity:   ", round(mean(upper_obs), 3), "\n")
  cat("   Median pairwise observed concurvity: ", round(median(upper_obs), 3), "\n")
  cat("   Number of term pairs with obs >", threshold, ":", sum(upper_obs > threshold), "\n")
  cat("   Number of term pairs with obs >", moderate_threshold, ":", sum(upper_obs > moderate_threshold), "\n")

  # 3. Top 10 highest pairwise concurvities
  cat("\n\n3. TOP 10 PAIRWISE CONCURVITIES:\n\n")

  n_show <- min(10, length(upper_obs))
  top_idx <- upper_tri_idx[order(-upper_obs)[1:n_show], , drop = FALSE]

  if (!is.null(pair_est)) {
    top_df <- data.frame(
      Term1 = rownames(pair_obs)[top_idx[,1]],
      Term2 = colnames(pair_obs)[top_idx[,2]],
      Observed = round(pair_obs[top_idx], 3),
      Estimate = round(pair_est[top_idx], 3)
    )
  } else {
    top_df <- data.frame(
      Term1 = rownames(pair_obs)[top_idx[,1]],
      Term2 = colnames(pair_obs)[top_idx[,2]],
      Observed = round(pair_obs[top_idx], 3)
    )
  }
  print(top_df, row.names = FALSE)

  # 4. Recommendations
  cat("\n\n4. RECOMMENDATIONS:\n\n")

  if (nrow(high_pairs) > 0) {
    cat("   ⚠️  ACTION NEEDED:\n")
    cat("   High pairwise concurvity detected. Consider:\n")
    cat("   • Removing one variable from each highly collinear pair\n")
    cat("   • Using tensor product smooths (te/ti) for interacting variables\n")
    cat("   • Combining correlated predictors into a single smooth\n")
    cat("   • Using domain knowledge to select the most important predictor\n\n")
  } else if (nrow(moderate_pairs) > 0) {
    cat("   ⚡ MONITOR:\n")
    cat("   Some moderate concurvity present, but likely not problematic.\n")
    cat("   Check that coefficient estimates are stable and sensible.\n\n")
  } else {
    cat("   ✓ EXCELLENT:\n")
    cat("   No problematic concurvity detected. Model structure looks good!\n\n")
  }

  cat("   INTERPRETATION GUIDE:\n")
  cat("   • Values > 0.8: Problematic - terms are highly collinear\n")
  cat("   • Values 0.5-0.8: Moderate - investigate but may be acceptable\n")
  cat("   • Values < 0.5: Generally fine\n")

  # Return results invisibly
  invisible(list(
    pairwise_observed = pair_obs,
    pairwise_estimate = pair_est,
    high_pairs = if(exists("pair_df_high")) pair_df_high else NULL,
    moderate_pairs = if(exists("pair_df_mod")) pair_df_mod else NULL,
    top_pairs = top_df,
    summary = list(
      max = max(upper_obs),
      mean = mean(upper_obs),
      median = median(upper_obs),
      n_high = sum(upper_obs > threshold),
      n_moderate = sum(upper_obs > moderate_threshold)
    )
  ))
}


## -----------------------------------------------------------------------------
#### TURN INTO FUNCTION #######
# Manual VIF calculation (no packages needed)
model_to_test <- model_16_ar

# Get model matrix for parametric terms
X <- model.matrix(model_to_test)
param_cols <- !grepl("s\\(", colnames(X))
X_param <- X[, param_cols]

# Remove intercept if present
if("(Intercept)" %in% colnames(X_param)) {
  X_param <- X_param[, colnames(X_param) != "(Intercept)"]
}

# Calculate VIF manually
# VIF = 1 / (1 - R^2) where R^2 is from regressing each predictor on all others
vif_values <- numeric(ncol(X_param))
names(vif_values) <- colnames(X_param)

for(i in 1:ncol(X_param)) {
  # Regress predictor i on all other predictors
  y <- X_param[, i]
  X_others <- X_param[, -i, drop=FALSE]

  # Only calculate if we have variation
  if(sd(y) > 0 && ncol(X_others) > 0) {
    r_squared <- summary(lm(y ~ X_others))$r.squared
    vif_values[i] <- 1 / (1 - r_squared)
  } else {
    vif_values[i] <- NA
  }
}

cat("=== VARIANCE INFLATION FACTORS ===\n\n")
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = round(vif_values, 2),
  Interpretation = ifelse(vif_values < 5, "OK",
                   ifelse(vif_values < 10, "Moderate", "HIGH"))
)
print(vif_df)

cat("\n\nInterpretation:\n")
cat("  VIF < 5:  No multicollinearity concern\n")
cat("  VIF 5-10: Moderate multicollinearity\n")
cat("  VIF > 10: High multicollinearity (problematic)\n")


## -----------------------------------------------------------------------------

#code to run cross validation to check for overfitting

timeseries_for_cross_validation <- #put model name here
# Remove any existing fold column
if("fold" %in% names(timeseries_for_cross_validation)) {
  timeseries_for_cross_validation <- timeseries_for_cross_validation %>% select(-fold)
}

set.seed(123)
n_folds <- 10

# Get unique phrases
unique_phrases <- unique(timeseries_for_cross_validation$PHRASE_ID)
cat("Number of phrases:", length(unique_phrases), "\n")

# Create fold assignments
phrase_folds <- sample(rep(1:n_folds, length.out=length(unique_phrases)))

# Assign folds
fold_assignment <- data.frame(
  PHRASE_ID = unique_phrases,
  fold = phrase_folds
)

# Check fold distribution
cat("\nPhrases per fold:\n")
print(table(phrase_folds))

# Join to main data
timeseries_for_cross_validation <- timeseries_for_cross_validation %>%
  left_join(fold_assignment, by="PHRASE_ID")

# Verify
cat("\nObservations per fold:\n")
print(table(timeseries_for_cross_validation$fold))

# Run CV
rmse_cv <- numeric(n_folds)

for(i in 1:n_folds) {
  cat(sprintf("\nFold %d/%d...\n", i, n_folds))

  train_data <- timeseries_for_cross_validation[timeseries_for_cross_validation$fold != i, ]
  test_data <- timeseries_for_cross_validation[timeseries_for_cross_validation$fold == i, ]

  cat(sprintf("  Train: %d obs from %d phrases\n",
              nrow(train_data),
              length(unique(train_data$PHRASE_ID))))
  cat(sprintf("  Test:  %d obs from %d phrases\n",
              nrow(test_data),
              length(unique(test_data$PHRASE_ID))))

  model_cv <- bam(
  LOG_F0 ~ TONE * PREV_WORD_TONE + TONE * NEXT_WORD_TONE +
         VOWEL_HEIGHT + SEG_DURATION + SYLL_STRUCTURE +
         TONE * PREV_LARYNGEAL_CLASS + TONE * NEXT_LARYNGEAL_CLASS +
         RELATIVE_WORD_POSITION_IN_PHRASE * NUM_OF_WORDS_IN_PHRASE +
         s(NORM_TIME_STANDARD, by=TONE, k=6) +
         s(NORM_TIME_STANDARD, by=interaction(TONE, PREV_WORD_TONE), k=6) +
         s(NORM_TIME_STANDARD, by=interaction(TONE, NEXT_WORD_TONE), k=6) +
         s(NORM_TIME_STANDARD, by=PREV_LARYNGEAL_CLASS, k=6) +
         s(NORM_TIME_STANDARD, by=NEXT_LARYNGEAL_CLASS, k=6) +
         s(PHRASE_ID, bs="re"),
  data = train_data,
  discrete = TRUE,
  rho = 0.553,  # Use the correctly calculated rho
  AR.start = train_data$AR.start,
  method = "fREML"
)


  pred <- predict(model_cv, newdata=test_data)
  rmse_cv[i] <- sqrt(mean((test_data$LOG_F0 - pred)^2, na.rm=TRUE))

  cat(sprintf("  RMSE: %.6f\n", rmse_cv[i]))

    # Check overlap between train and test
  cat(sprintf("  Words - Train: %d unique, Test: %d unique, Overlap: %d\n",
              length(unique(train_data$WORD_ID)),
              length(unique(test_data$WORD_ID)),
              sum(unique(test_data$WORD_ID) %in% unique(train_data$WORD_ID))))

  cat(sprintf("  Phrases - Train: %d unique, Test: %d unique, Overlap: %d\n",
              length(unique(train_data$PHRASE_ID)),
              length(unique(test_data$PHRASE_ID)),
              sum(unique(test_data$PHRASE_ID) %in% unique(train_data$PHRASE_ID))))

  cat(sprintf("  Sentences - Train: %d unique, Test: %d unique, Overlap: %d\n",
              length(unique(train_data$SENTENCE_ID)),
              length(unique(test_data$SENTENCE_ID)),
              sum(unique(test_data$SENTENCE_ID) %in% unique(train_data$SENTENCE_ID))))

}

cat("\n=== CROSS-VALIDATION RESULTS ===\n")
cat(sprintf("CV RMSE: Mean = %.6f, SD = %.6f\n", mean(rmse_cv), sd(rmse_cv)))

summary_obj <- summary(model_cv)
in_sample_sd <- sqrt(summary_obj$scale)
cat(sprintf("In-sample SD: %.6f\n", in_sample_sd))
cat(sprintf("Ratio (CV/in-sample): %.3f\n", mean(rmse_cv) / in_sample_sd))

if(mean(rmse_cv) / in_sample_sd < 1.2) {
  cat("\n✓ Model generalizes well (ratio < 1.2)\n")
} else if(mean(rmse_cv) / in_sample_sd < 1.5) {
  cat("\n⚠ Some overfitting (ratio 1.2-1.5)\n")
} else {
  cat("\n✗ Substantial overfitting (ratio > 1.5)\n")
}


## -----------------------------------------------------------------------------
####### Visualizations #############


## -----------------------------------------------------------------------------
#plot main effect of model (e.g. H vs. L tone)
model_for_plot <- model_no_ar  # replace with your chosen model

# Get coefficients for parametric part
intercept <- coef(model_for_plot)["(Intercept)"]
tonel_coef <- coef(model_for_plot)["TONEL"]

# Call plot.gam() with seWithMean=TRUE to get standard errors
smooth_h <- plot(model_for_plot, select = 1, seWithMean = TRUE)
smooth_l <- plot(model_for_plot, select = 2, seWithMean = TRUE)

# Extract the data from the plot objects WITH standard errors
h_smooth_data <- data.frame(
  NORM_TIME = smooth_h[[1]]$x,
  smooth_effect = smooth_h[[1]]$fit,
  smooth_se = smooth_h[[1]]$se,
  TONE = "H"
)

l_smooth_data <- data.frame(
  NORM_TIME = smooth_l[[2]]$x,
  smooth_effect = smooth_l[[2]]$fit,
  smooth_se = smooth_l[[2]]$se,
  TONE = "L"
)

# Combine and add parametric + calculate confidence intervals
combined_data <- bind_rows(h_smooth_data, l_smooth_data) %>%
  mutate(
    parametric = ifelse(TONE == "H", intercept, intercept + tonel_coef),
    log_f0 = parametric + smooth_effect,
    log_f0_se = smooth_se,  # SE in log scale
    f0_hz = exp(log_f0),
    f0_lower = exp(log_f0 - 1.96 * log_f0_se),  # 95% CI
    f0_upper = exp(log_f0 + 1.96 * log_f0_se)
  )

# Calculate pitch range
f0_values <- exp(timeseries_no_NA_glott_collapse_df$LOG_F0)
f0_mean <- mean(f0_values, na.rm = TRUE)
f0_sd <- sd(f0_values, na.rm = TRUE)
#pitch_floor <- f0_mean - 2.5 * f0_sd
#pitch_ceiling <- f0_mean + 2.5 * f0_sd

pitch_floor <- 125
pitch_ceiling <- 150

# Plot with 95% CI ribbon
p_combined <- ggplot(combined_data, aes(x = NORM_TIME, y = f0_hz, color = TONE, fill = TONE)) +
  geom_ribbon(aes(ymin = f0_lower, ymax = f0_upper), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("H" = "orange", "L" = "purple")) +
  scale_fill_manual(values = c("H" = "orange", "L" = "purple")) +
  scale_y_continuous(limits = c(pitch_floor, pitch_ceiling)) +
  labs(
    title = "Parametric + by-TONE Smooths",
    subtitle = paste0("Y-axis: ", round(pitch_floor), "-", round(pitch_ceiling), " Hz (mean ± 2.5 SD) | Ribbons: 95% CI"),
    x = "Normalized Time",
    y = "F0 (Hz)"
  ) +
  theme_minimal()

print(p_combined)


## -----------------------------------------------------------------------------
#visualize effects of contextual factors

pitch_floor <- 125
pitch_ceiling <- 150
# ==============================================================================
# E.g. Extract parametric + by-TONE smooths + NEXT_WORD_TONE effects
# ==============================================================================

# Get coefficients
intercept <- coef(model_for_plot)["(Intercept)"]
tonel_coef <- coef(model_for_plot)["TONEL"]

# Get variance-covariance matrix for CI calculation
Vp <- vcov(model_for_plot)

# Get all NEXT_WORD_TONE levels
next_tones <- levels(df_for_plot$NEXT_WORD_TONE)
cat("NEXT_WORD_TONE levels:", next_tones, "\n\n")

# ==============================================================================
# For each TONE × NEXT_WORD_TONE combination, extract smooths
# ==============================================================================

all_data <- list()
reference_data <- list()  # For parametric + TONE smooth only

for (curr_tone in c("H", "L")) {

  # Get the by-TONE smooth (main effect) with SE
  if (curr_tone == "H") {
    tone_smooth <- plot(model_for_plot, select = 1, seWithMean = TRUE)
    tone_smooth_data <- data.frame(
      NORM_TIME = tone_smooth[[1]]$x,
      tone_smooth_effect = tone_smooth[[1]]$fit,
      tone_smooth_se = tone_smooth[[1]]$se
    )
  } else {
    tone_smooth <- plot(model_for_plot, select = 2, seWithMean = TRUE)
    tone_smooth_data <- data.frame(
      NORM_TIME = tone_smooth[[2]]$x,
      tone_smooth_effect = tone_smooth[[2]]$fit,
      tone_smooth_se = tone_smooth[[2]]$se
    )
  }

  # Create reference line (parametric + tone smooth only) with CI
  reference <- tone_smooth_data %>%
    mutate(
      TONE = curr_tone,
      parametric = ifelse(curr_tone == "H", intercept, intercept + tonel_coef),
      log_f0 = parametric + tone_smooth_effect,
      log_f0_se = tone_smooth_se,
      f0_hz = exp(log_f0),
      f0_lower = exp(log_f0 - 1.96 * log_f0_se),
      f0_upper = exp(log_f0 + 1.96 * log_f0_se)
    )
  reference_data[[curr_tone]] <- reference

  for (nt in next_tones) {

    # Find the index for the NEXT_WORD_TONE interaction smooth
    smooth_label <- paste0("s(NORM_TIME_STANDARD):interaction(TONE, NEXT_WORD_TONE)", curr_tone, ".", nt)

    # Find which smooth index this is
    smooth_idx <- NULL
    for (i in 1:length(model_for_plot$smooth)) {
      if (model_for_plot$smooth[[i]]$label == smooth_label) {
        smooth_idx <- i
        break
      }
    }

    if (!is.null(smooth_idx)) {
      # Extract the interaction smooth with SE
      next_smooth <- plot(model_for_plot, select = smooth_idx, seWithMean = TRUE)
      next_smooth_data <- data.frame(
        NORM_TIME = next_smooth[[smooth_idx]]$x,
        next_smooth_effect = next_smooth[[smooth_idx]]$fit,
        next_smooth_se = next_smooth[[smooth_idx]]$se
      )
    } else {
      # No smooth found for this combination
      next_smooth_data <- data.frame(
        NORM_TIME = tone_smooth_data$NORM_TIME,
        next_smooth_effect = 0,
        next_smooth_se = 0
      )
    }

    # Get parametric effects for NEXT_WORD_TONE
    next_param <- 0
    next_param_se <- 0
    if (nt != next_tones[1]) {
      param_name <- paste0("NEXT_WORD_TONE", nt)
      if (param_name %in% names(coef(model_for_plot))) {
        next_param <- coef(model_for_plot)[param_name]
        next_param_se <- sqrt(Vp[param_name, param_name])
      }
    }

    next_int <- 0
    next_int_se <- 0
    if (curr_tone == "L" && nt != next_tones[1]) {
      int_name <- paste0("TONEL:NEXT_WORD_TONE", nt)
      if (int_name %in% names(coef(model_for_plot))) {
        next_int <- coef(model_for_plot)[int_name]
        next_int_se <- sqrt(Vp[int_name, int_name])
      }
    }

    # Combine everything
    combined <- tone_smooth_data %>%
      left_join(next_smooth_data, by = "NORM_TIME") %>%
      mutate(
        TONE = curr_tone,
        next_context = nt,
        # Parametric baseline
        parametric = ifelse(curr_tone == "H", intercept, intercept + tonel_coef),
        # Add NEXT_WORD_TONE parametric effects
        parametric = parametric + next_param + next_int,
        # Total: parametric + tone smooth + next smooth
        log_f0 = parametric + tone_smooth_effect + next_smooth_effect,
        # Approximate SE (sum of squared SEs)
        log_f0_se = sqrt(tone_smooth_se^2 + next_smooth_se^2 + next_param_se^2 + next_int_se^2),
        # Convert to Hz with CI
        f0_hz = exp(log_f0),
        f0_lower = exp(log_f0 - 1.96 * log_f0_se),
        f0_upper = exp(log_f0 + 1.96 * log_f0_se)
      )

    all_data[[paste(curr_tone, nt, sep = "_")]] <- combined
  }
}

# Combine all data
plot_data <- bind_rows(all_data)
reference_lines <- bind_rows(reference_data)

# ==============================================================================
# VISUALIZATION
# ==============================================================================

# Define color palette
tone_colors <- c("H" = "orange", "L" = "purple", "N" = "grey50", "PAU" = "green3")

# Plot: H tone by following context
p_h <- ggplot(plot_data %>% filter(TONE == "H"),
              aes(x = NORM_TIME, y = f0_hz, color = next_context, fill = next_context)) +
  # Reference line CI ribbon for H (shaded orange)
  geom_ribbon(data = reference_lines %>% filter(TONE == "H"),
              aes(x = NORM_TIME, ymin = f0_lower, ymax = f0_upper),
              fill = "orange", alpha = 0.15, color = NA,
              inherit.aes = FALSE) +
  # Reference line CI ribbon for L (shaded purple)
  geom_ribbon(data = reference_lines %>% filter(TONE == "L"),
              aes(x = NORM_TIME, ymin = f0_lower, ymax = f0_upper),
              fill = "purple", alpha = 0.15, color = NA,
              inherit.aes = FALSE) +
  # Solid lines for NEXT_WORD_TONE contexts
  geom_line(linewidth = 1.5) +
  # Reference line mean for H (dashed orange) - on top
  geom_line(data = reference_lines %>% filter(TONE == "H"),
            aes(x = NORM_TIME, y = f0_hz),
            linetype = "dashed", color = "orange", linewidth = 1.2,
            inherit.aes = FALSE) +
  # Reference line mean for L (dashed purple) - on top
  geom_line(data = reference_lines %>% filter(TONE == "L"),
            aes(x = NORM_TIME, y = f0_hz),
            linetype = "dashed", color = "purple", linewidth = 1.2,
            inherit.aes = FALSE) +
  scale_color_manual(values = tone_colors) +
  scale_fill_manual(values = tone_colors) +
  scale_y_continuous(limits = c(pitch_floor, pitch_ceiling)) +
  labs(
    title = "H Tone Before Different Following Tones",
    subtitle = "Solid: Full model | Dashed: Parametric + TONE smooth | Shaded: 95% CI",
    x = "Normalized Time",
    y = "F0 (Hz)",
    color = "Followed by",
    fill = "Followed by"
  ) +
  theme_minimal()

print(p_h)

# Plot: L tone by following context
p_l <- ggplot(plot_data %>% filter(TONE == "L"),
              aes(x = NORM_TIME, y = f0_hz, color = next_context, fill = next_context)) +
  # Reference line CI ribbon for H (shaded orange)
  geom_ribbon(data = reference_lines %>% filter(TONE == "H"),
              aes(x = NORM_TIME, ymin = f0_lower, ymax = f0_upper),
              fill = "orange", alpha = 0.15, color = NA,
              inherit.aes = FALSE) +
  # Reference line CI ribbon for L (shaded purple)
  geom_ribbon(data = reference_lines %>% filter(TONE == "L"),
              aes(x = NORM_TIME, ymin = f0_lower, ymax = f0_upper),
              fill = "purple", alpha = 0.15, color = NA,
              inherit.aes = FALSE) +
  # Solid lines for NEXT_WORD_TONE contexts
  geom_line(linewidth = 1.5) +
  # Reference line mean for H (dashed orange) - on top
  geom_line(data = reference_lines %>% filter(TONE == "H"),
            aes(x = NORM_TIME, y = f0_hz),
            linetype = "dashed", color = "orange", linewidth = 1.2,
            inherit.aes = FALSE) +
  # Reference line mean for L (dashed purple) - on top
  geom_line(data = reference_lines %>% filter(TONE == "L"),
            aes(x = NORM_TIME, y = f0_hz),
            linetype = "dashed", color = "purple", linewidth = 1.2,
            inherit.aes = FALSE) +
  scale_color_manual(values = tone_colors) +
  scale_fill_manual(values = tone_colors) +
  scale_y_continuous(limits = c(pitch_floor, pitch_ceiling)) +
  labs(
    title = "L Tone Before Different Following Tones",
    subtitle = "Solid: Full model | Dashed: Parametric + TONE smooth | Shaded: 95% CI",
    x = "Normalized Time",
    y = "F0 (Hz)",
    color = "Followed by",
    fill = "Followed by"
  ) +
  theme_minimal()

print(p_l)

# Summary stats
cat("\n=== H TONE BY FOLLOWING CONTEXT ===\n")
plot_data %>%
  filter(TONE == "H") %>%
  group_by(next_context) %>%
  summarise(
    start = f0_hz[1],
    end = f0_hz[length(f0_hz)],
    slope = end - start,
    .groups = "drop"
  ) %>%
  print()

cat("\n=== L TONE BY FOLLOWING CONTEXT ===\n")
plot_data %>%
  filter(TONE == "L") %>%
  group_by(next_context) %>%
  summarise(
    start = f0_hz[1],
    end = f0_hz[length(f0_hz)],
    slope = end - start,
    .groups = "drop"
  ) %>%
  print()

