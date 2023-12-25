library(ggplot2)
library(dplyr)
library(tidyverse)
library(dagitty)
library(ggdag)
library(MatchIt)
library(modelsummary)

df <- readRDS(("content/01_journal/data/membership.rds"))
str(df)
summary(df)

dag_model <- 'dag {
bb="0,0,1,1"
Card [exposure,pos="0.075,0.4"]
Avg_purch [outcome,pos="0.4,0.4"]
Age [pos="0.1,0.2"]
Sex [pos="0.35,0.2"]
Pre_avg_purch [pos="0.2,0.6"]
Card -> Avg_purch
Pre_avg_purch -> Avg_purch
Pre_avg_purch -> Card
Age -> Card
Age -> Pre_avg_purch
Sex -> Card
Sex -> Pre_avg_purch
}
'
# draw DAG
ggdag_status(dag_model) +
  guides(fill = "none", color = "none") +  # Disable the legend
  geom_dag_edges(edge_color = "white")

#model <- lm(avg_purch ~ card + pre_avg_purch + age + sex, data = df)
model <- lm(avg_purch ~ card, data = df)
summary(model)

cem <- matchit(card ~ pre_avg_purch + age + sex,
               data = df, 
               method = 'cem', 
               estimand = 'ATE')
summary(cem)
df_cem = match.data(cem)
model_cem <- lm(avg_purch ~ card, data = df_cem, weights = weights)
summary(model_cem)

nn <- matchit(card ~ pre_avg_purch + age + sex,
              data = df,
              method = "nearest",
              distance = "mahalanobis",
              replace = T)
summary(nn)
df_nn <- match.data(nn)
model_nn <- lm(avg_purch ~ card, data = df_nn, weights = weights)
summary(model_nn)


model_prop <- glm(card ~ pre_avg_purch + age + sex,
                  data = df,
                  family = binomial(link = "logit"))
summary(model_prop)
df_aug <- df %>% mutate(propensity = predict(model_prop, type = "response"))

# Extend data by IPW scores
df_ipw <- df_aug %>% mutate(
  ipw = (card/propensity) + ((1-card) / (1-propensity)))

# Look at data with IPW scores
df_ipw %>% 
  select(card, pre_avg_purch, age, sex, propensity, ipw)

model_ipw <- lm(avg_purch ~ card,
                data = df_ipw, 
                weights = ipw)
summary(model_ipw)

model_ipw_trim <- lm(avg_purch ~ card,
                     data = df_ipw %>% filter(propensity %>% between(0.15, 0.85)),
                     weights = ipw)
summary(model_ipw_trim)

modelsummary::modelsummary(list("Naive" = model,
                                "CEM"  = model_cem,
                                "NN"    = model_nn,
                                "IPW1"  = model_ipw,
                                "IPW2"  = model_ipw_trim))