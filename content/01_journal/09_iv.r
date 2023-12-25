library(estimatr)
library(ggdag)
library(ggplot2)
library(dplyr)

df <- readRDS("content/01_journal/data/rand_enc.rds")

iv_rand_enc <- dagify(
  Y ~ D,
  Y ~ U,
  D ~ U,
  D ~ Z,
  exposure = "D",
  latent = "U",
  outcome = "Y",
  coords = list(x = c(U = 1, D = 0, Y = 2, Z = -1),
                y = c(U = 1, D = 0, Y = 0, Z = 0)),
  labels = c("D" = "Used new feature", 
             "Y" = "Time spent on app", 
             "U" = "Unobserved characteristics",
             "Z" = "Random encouragement")
)
ggdag(iv_rand_enc, text = T) +
  guides(color = "none") +
  geom_dag_text(color = "white") +
  geom_dag_edges(edge_color = "white") +
  geom_dag_label_repel(aes(label = label))

naive_model <- lm(time_spent ~ used_ftr, data = df)
summary(naive_model)

cor(df)


first_stage <- lm(used_ftr ~ rand_enc, data = df)
summary(first_stage)

monotonicity_check <- df %>%
  mutate(observation_unit = case_when(
    (rand_enc == 1 & used_ftr == 1) ~ "Complier",
    (rand_enc == 1 & used_ftr == 0) ~ "Defier",
    (rand_enc == 0 & used_ftr == 0) ~ "Never taker",
    (rand_enc == 0 & used_ftr == 1) ~ "Always taker"
  ))


table(monotonicity_check$observation_unit)

model_iv <- iv_robust(time_spent ~ used_ftr | rand_enc, data = df)
summary(model_iv)