if (1 == 2) {
  install.packages("haven")
}
library(haven)
library(tidyverse)
get_data <- function() {
  read_dta("Harvard_AI_Tutor_Study_Data.dta")
}

# Pre-scores differ: pro-AI learners have higher
t <- get_data()
t$lect_AI <- as.factor(t$lect_AI)
ggplot(t, aes(x = lect_AI, y = pre_score)) + geom_boxplot() +
  ggtitle("Pre-scores differ: pro-AI learners have higher initial skill")
ggsave("pre_scores_differ.svg")
ggsave("pre_scores_differ.png")

# Measure increase between pre- and post score
t <- get_data()
t$score_change <- t$post_score - t$pre_score
t$lect_AI <- as.factor(t$lect_AI)
ggplot(t, aes(x = lect_AI, y = score_change)) + geom_boxplot() +
  ggtitle("Change change does not differ much")
ggsave("skill_change_does_not_differ_much.svg")
ggsave("skill_change_does_not_differ_much.png")





