if (1 == 2) {
  install.packages("haven")
}
library(haven)
library(tidyverse)
setwd("~/GitHubs/Study-Data-v4/")
get_data <- function() {
  read_dta("Harvard_AI_Tutor_Study_Data.dta")
}
testthat::expect_silent(get_data())

#' These are the learners that did both tests
get_data_for_two_tests <- function() {
  get_data() |> filter(pre_score > 0) |> filter(post_score > 0)
}
testthat::expect_silent(get_data_for_two_tests())

#' These are the learners that did both tests
#' and did not fluke the second test
get_data_for_two_tests_second_done_well <- function(max_decrease = 1.99) {
  t <- get_data_for_two_tests()
  t$score_change <- t$post_score - t$pre_score
  u <- t |> dplyr::filter(score_change > -max_decrease)
  u$score_change <- NULL
  u
}
testthat::expect_silent(get_data_for_two_tests_second_done_well())

# Prescores are the same
get_test_data_with_same_prescore <- function() {
  t <- get_data()
  t$pre_score <- rnorm(n = nrow(t), mean = 2.0)
  t$post_score <- rnorm(n = nrow(t), mean = 3.0)
  t
}
testthat::expect_silent(get_test_data_with_same_prescore())


# Prescores different
get_test_data_with_different_prescore <- function() {
  t <- get_data()

  t$pre_score <- rnorm(n = nrow(t), mean = 2.0)
  t$post_score <- rnorm(n = nrow(t), mean = 3.0)

  ai_users_indices <- which(t$lect_AI == 1)
  t$pre_score[ai_users_indices] <- rnorm(n = length(ai_users_indices), mean = 3.0)
  t
}
testthat::expect_silent(get_test_data_with_different_prescore())

#' Put in data and it will produce a verdict if the distributions differ
are_prescores_the_same_between_users <- function(t) {
  t$lect_AI <- as.factor(t$lect_AI)
  results <- t.test(
    x = t |> filter(lect_AI == 1) |> select(pre_score) |> pull(),
    y = t |> filter(lect_AI == 0) |> select(pre_score) |> pull()
  )
  mean_yes <- mean(t |> filter(lect_AI == 1) |> select(pre_score) |> pull())
  mean_no <- mean(t |> filter(lect_AI == 0) |> select(pre_score) |> pull())
  if (results$p.value <= 0.05) {
    verdict <- "pre-scores are different distributions, according to a t-test"
    # Double-check my reasoning, these distributions are different
    testthat::expect_true(t.test(x = rnorm(n = nrow(t)/2), y = rnorm(n = nrow(t)/2, mean = 1.0))$p.value < 0.05)

  } else {
    verdict <- "pre-scores are same distributions, according to a t-test"
    # Double-check my reasoning, these distributions are the same
    testthat::expect_true(t.test(x = rnorm(n = nrow(t)/2), y = rnorm(n = nrow(t)/2))$p.value > 0.05)
  }
  list(
    verdict = verdict,
    p_value = results$p.value,
    mean_yes_users = mean_yes,
    mean_no_users = mean_no
  )
}
testthat::expect_true(
  stringr::str_detect(
    are_prescores_the_same_between_users(get_test_data_with_same_prescore())$verdict,
    "same distribution"
  )
)
testthat::expect_true(
  stringr::str_detect(
    are_prescores_the_same_between_users(get_test_data_with_different_prescore())$verdict,
    "different distribution"
  )
)


# Check: Indeed, all pretests comes from the same distribution
testthat::expect_true(
  stringr::str_detect(
    are_prescores_the_same_between_users(get_data())$verdict,
    "same distribution"
  )
)
# Check: Indeed, all pretests comes from the same distribution,
# even for only those learners that did two tests
testthat::expect_true(
  stringr::str_detect(
    are_prescores_the_same_between_users(get_data_for_two_tests())$verdict,
    "same distribution"
  )
)

compare_pre_scores <- function(t, title) {
  verdict <- are_prescores_the_same_between_users(t)
  t$lect_AI <- as.factor(t$lect_AI)
  ggplot(t, aes(x = lect_AI, y = pre_score)) +
    geom_violin() +
    labs(
      title = title,
      caption = paste0(
        verdict,
        "\np-value: ", round(verdict$p_value, digits = 3),
        "\npre-score no: ", round(verdict$mean_no_users, digits = 3), " ",
        "pre-score yes: ", round(verdict$mean_yes_users, digits = 3)

      )
    )
}
compare_pre_scores(get_data(), "All learners had the same pre-score")
ggsave("pre_scores_all.svg")
compare_pre_scores(get_data_for_two_tests(), "Learners that did both tests had the same pre-score")
ggsave("pre_scores_took_two_tests.svg", width = 5)



are_score_changes_the_same_between_users <- function(t) {
  t$score_change <- t$post_score - t$pre_score
  t$lect_AI <- as.factor(t$lect_AI)
  mean_yes <- mean(t |> filter(lect_AI == 1) |> select(score_change) |> pull())
  mean_no <- mean(t |> filter(lect_AI == 0) |> select(score_change) |> pull())

  results <- t.test(
    x = t |> filter(lect_AI == 1) |> select(score_change) |> pull(),
    y = t |> filter(lect_AI == 0) |> select(score_change) |> pull()
  )
  if (results$p.value <= 0.05) {
    verdict <- "score_change are different distributions, according to a t-test"
    # Double-check my reasoning, these distributions are different
    testthat::expect_true(t.test(x = rnorm(n = nrow(t)/2), y = rnorm(n = nrow(t)/2, mean = 1.0))$p.value < 0.05)

  } else {
    verdict <- "score_change are same distributions, according to a t-test"
    # Double-check my reasoning, these distributions are the same
    testthat::expect_true(t.test(x = rnorm(n = nrow(t)/2), y = rnorm(n = nrow(t)/2))$p.value > 0.05)
  }
  list(
    verdict = verdict,
    p_value = results$p.value,
    mean_yes_users = mean_yes,
    mean_no_users = mean_no
  )
}

# Measure increase between pre- and post score
compare_score_change <- function(t, title) {
  verdict <- are_score_changes_the_same_between_users(t)
  t$score_change <- t$post_score - t$pre_score
  t$lect_AI <- as.factor(t$lect_AI)

  ggplot(t, aes(x = lect_AI, y = score_change)) +
    geom_violin() +
    labs(
      title = title,
      caption = paste0(
        verdict$verdict,
        "\np-value: ", round(verdict$p_value, digits = 3),
        "\ndifference no: ", round(verdict$mean_no_user, digits = 3), " ",
        "difference yes: ", round(verdict$mean_yes_user, digits = 3)
      )
    )
}


# For all data, skill change is more in AI
compare_score_change(get_data(), title = "For all learners, AI tutorial increases scores 50%")
ggsave("skill_change_all.svg", width = 6)

# For all data, skill change is more in AI
compare_score_change(get_data_for_two_tests(), title = "For all learners that took two tests, AI tutorial increases scores 50%")
ggsave("skill_change_took_two_tests.svg", width = 7)

compare_score_change(get_data_for_two_tests_second_done_well(max_decrease = 1.99), title = "For all learners that took two tests and did not fluke the second test by 2 points,\nAI tutorial increases scores 30%")
# (1.515 - 1.148) / 1.148
ggsave("skill_change_took_two_tests_second_done_well.svg", width = 7)









## Postscores only

# Postscore are the same
get_test_data_with_same_postscore <- function() {
  t <- get_data()
  t$post_score <- rnorm(n = nrow(t), mean = 2.0)
  t$post_score <- rnorm(n = nrow(t), mean = 3.0)
  t
}
testthat::expect_silent(get_test_data_with_same_postscore())


# Postscore different
get_test_data_with_different_postscore <- function() {
  t <- get_data()

  t$post_score <- rnorm(n = nrow(t), mean = 2.0)

  ai_users_indices <- which(t$lect_AI == 1)
  t$post_score[ai_users_indices] <- rnorm(n = length(ai_users_indices), mean = 3.0)
  t
}
testthat::expect_silent(get_test_data_with_different_postscore())


#' Put in data and it will produce a verdict if the distributions differ
are_postscores_the_same_between_users <- function(t) {
  t$lect_AI <- as.factor(t$lect_AI)
  results <- t.test(
    x = t |> filter(lect_AI == 1) |> select(post_score) |> pull(),
    y = t |> filter(lect_AI == 0) |> select(post_score) |> pull()
  )
  mean_yes <- mean(t |> filter(lect_AI == 1) |> select(post_score) |> pull())
  mean_no <- mean(t |> filter(lect_AI == 0) |> select(post_score) |> pull())
  if (results$p.value <= 0.05) {
    verdict <- "pre-scores are different distributions, according to a t-test"
    # Double-check my reasoning, these distributions are different
    testthat::expect_true(t.test(x = rnorm(n = nrow(t)/2), y = rnorm(n = nrow(t)/2, mean = 1.0))$p.value < 0.05)

  } else {
    verdict <- "pre-scores are same distributions, according to a t-test"
    # Double-check my reasoning, these distributions are the same
    testthat::expect_true(t.test(x = rnorm(n = nrow(t)/2), y = rnorm(n = nrow(t)/2))$p.value > 0.05)
  }
  list(
    verdict = verdict,
    p_value = results$p.value,
    mean_yes_users = mean_yes,
    mean_no_users = mean_no
  )
}
testthat::expect_true(
  stringr::str_detect(
    are_postscores_the_same_between_users(get_test_data_with_same_postscore())$verdict,
    "same distribution"
  )
)
testthat::expect_true(
  stringr::str_detect(
    are_postscores_the_same_between_users(get_test_data_with_different_postscore())$verdict,
    "different distribution"
  )
)


# Check: Indeed, all posttests come from a different distribution
testthat::expect_true(
  stringr::str_detect(
    are_postscores_the_same_between_users(get_data())$verdict,
    "different distributions"
  )
)
# Check: Indeed, all posttests come from a different distribution,
# even for only those learners that did two tests
testthat::expect_true(
  stringr::str_detect(
    are_postscores_the_same_between_users(get_data_for_two_tests())$verdict,
    "different distributions"
  )
)

compare_post_scores <- function(t, title) {
  verdict <- are_postscores_the_same_between_users(t)
  t$lect_AI <- as.factor(t$lect_AI)
  ggplot(t, aes(x = lect_AI, y = post_score)) +
    geom_violin() +
    labs(
      title = title,
      caption = paste0(
        verdict,
        "\np-value: ", round(verdict$p_value, digits = 3),
        "\npost-score no: ", round(verdict$mean_no_users, digits = 3), " ",
        "post-score yes: ", round(verdict$mean_yes_users, digits = 3)

      )
    )
}
compare_post_scores(get_data(), "All learners a different post-score")
ggsave("post_scores_all.svg")
compare_post_scores(get_data_for_two_tests(), "Learners that did both tests had different post-scores")
ggsave("post_scores_took_two_tests.svg", width = 5)



#######################################################################
## Effect sizes
#######################################################################
#' Calculate pre-to-post effect sizes (score increase / standard deviation)
#' for both AI tutor users (lect_AI == 1) and non-AI users (lect_AI == 0).
get_effect_sizes <- function(t) {
  # Calculate score change per learner
  t$score_change <- t$post_score - t$pre_score

  # Separate data into AI users and non-AI users
  ai_users     <- t |> dplyr::filter(lect_AI == 1)
  non_ai_users <- t |> dplyr::filter(lect_AI == 0)

  # Internal function to calculate effect size for a given subset
  calc_group_stats <- function(df) {
    mean_change <- mean(df$score_change, na.rm = TRUE)
    sd_pre      <- sd(df$pre_score, na.rm = TRUE)
    sd_post     <- sd(df$post_score, na.rm = TRUE)
    sd_pooled   <- sqrt((sd_pre^2 + sd_post^2) / 2)

    list(
      mean_increase      = mean_change,
      sd_pre             = sd_pre,
      sd_pooled          = sd_pooled,
      es_pre_sd_baseline = mean_change / sd_pre,    # Effect size relative to pre-test variability
      es_pooled_sd       = mean_change / sd_pooled  # Effect size relative to pooled variability
    )
  }

  ai_stats     <- calc_group_stats(ai_users)
  non_ai_stats <- calc_group_stats(non_ai_users)

  results <- list(
    ai_tutor = list(
      effect_size_pre_sd    = round(ai_stats$es_pre_sd_baseline, 3),
      effect_size_pooled_sd = round(ai_stats$es_pooled_sd, 3),
      mean_increase         = round(ai_stats$mean_increase, 3),
      sd_pre                = round(ai_stats$sd_pre, 3)
    ),
    no_ai_tutor = list(
      effect_size_pre_sd    = round(non_ai_stats$es_pre_sd_baseline, 3),
      effect_size_pooled_sd = round(non_ai_stats$es_pooled_sd, 3),
      mean_increase         = round(non_ai_stats$mean_increase, 3),
      sd_pre                = round(non_ai_stats$sd_pre, 3)
    )
  )

  # Print formatted summary to console
  cat("=======================================================\n")
  cat(" PRE-TO-POST EFFECT SIZES (Standard Deviations Increase)\n")
  cat("=======================================================\n")
  cat("1. AI Tutor Group (lect_AI == 1):\n")
  cat("   - Mean Increase:        ", results$ai_tutor$mean_increase, "points\n")
  cat("   - Effect Size (Pre SD): ", results$ai_tutor$effect_size_pre_sd, "SDs\n")
  cat("   - Effect Size (Pooled): ", results$ai_tutor$effect_size_pooled_sd, "SDs\n\n")
  cat("2. No AI Tutor Group (lect_AI == 0):\n")
  cat("   - Mean Increase:        ", results$no_ai_tutor$mean_increase, "points\n")
  cat("   - Effect Size (Pre SD): ", results$no_ai_tutor$effect_size_pre_sd, "SDs\n")
  cat("   - Effect Size (Pooled): ", results$no_ai_tutor$effect_size_pooled_sd, "SDs\n")
  cat("=======================================================\n")

  invisible(results)
}

# ----------------------------------------------------------------------
# Example execution using your helper functions:
# ----------------------------------------------------------------------

# For learners that took both tests:
res_two_tests <- get_effect_sizes(get_data_for_two_tests())

# For learners that took both tests and did not fluke the second test:
res_filtered  <- get_effect_sizes(get_data_for_two_tests_second_done_well(max_decrease = 1.99))


# =======================================================
#   PRE-TO-POST EFFECT SIZES (Standard Deviations Increase)
# =======================================================
# 1. AI Tutor Group (lect_AI == 1):
# - Mean Increase:         1.515 points
# - Effect Size (Pre SD):  1.168 SDs
# - Effect Size (Pooled):  1.376 SDs
#
# 2. No AI Tutor Group (lect_AI == 0):
# - Mean Increase:         1.148 points
# - Effect Size (Pre SD):  0.97 SDs
# - Effect Size (Pooled):  0.96 SDs
# =======================================================
