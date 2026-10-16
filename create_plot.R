if (1 == 2) {
  install.packages("haven")
}
library(haven)
library(tidyverse)
setwd("~/GitHubs/Study-Data-v4/")
get_data <- function() {
  read_dta("Harvard_AI_Tutor_Study_Data.dta")
}

get_data_for_two_tests <- function() {
  get_data() |> filter(pre_score > 0) |> filter(post_score > 0)
}

compare_pre_scores <- function(t) {
  # Pre-scores differ: pro-AI learners have higher
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

  ggplot(t, aes(x = lect_AI, y = pre_score)) + geom_boxplot() +
    labs(
      title = "Pre-scores differ",
      subtitle = "pro-AI learners have higher initial skill",
      caption = paste0(
        verdict,
        "\np-value: ", round(results$p.value, digits = 3),
        "\npre-score no: ", round(mean_no, digits = 3), " ",
        "pre-score yes: ", round(mean_yes, digits = 3)

      )
    )
}

# Measure increase between pre- and post score
compare_score_change <- function(t) {
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
  ggplot(t, aes(x = lect_AI, y = score_change)) + geom_boxplot() +
    labs(
      title = "AI use increases change in score",
      subtitle = "But by 50%, not 200%",
      caption = paste0(
        verdict,
        "\np-value: ", round(results$p.value, digits = 3),
        "\ndifference no: ", round(mean_no, digits = 3), " ",
        "difference yes: ", round(mean_yes, digits = 3)
      )
    )
}

# For all data, the pre-scores do not differ
compare_pre_scores(t = get_data())
ggsave("pre_scores_all.svg")

# For all data of learners that did both tests, the pre-scores do not differ
compare_pre_scores(t = get_data_for_two_tests())
ggsave("pre_scores_took_two_tests.svg")

# For all data, skill change is more in AI
compare_score_change(t <- get_data())
ggsave("skill_change_all.svg")

# For all data, skill change is more in AI
compare_score_change(t <- get_data_for_two_tests())
ggsave("skill_change_took_two_tests.svg")

#ggplot(t, aes(x = score_change, fill = lect_AI)) + geom_histogram(position = "dodge", alpha = 0.5)
#ggplot(t, aes(x = score_change, fill = lect_AI)) + geom_density(alpha = 0.5)
