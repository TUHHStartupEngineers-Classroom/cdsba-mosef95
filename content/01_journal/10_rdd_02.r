library(estimatr)
library(ggdag)
library(ggplot2)
library(dplyr)
library(tidyverse)

df <- readRDS(("content/01_journal/data/shipping.rds"))
str(df)

ggplot(df, aes(x = purchase_amount)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Distribution of purchase amounts with cut-off at 30€",
       x = "Purchase amount (€)",
       y = "Frequency") +
  theme_minimal()