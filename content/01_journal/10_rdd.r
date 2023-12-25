library(estimatr)
library(ggdag)
library(ggplot2)
library(dplyr)
library(tidyverse)

df <- readRDS("content/01_journal/data/coupon.rds")

c0 <- 60
bw <- c0 + c(-10, 10)

# Subsets below and above threshold in specified bandwidth
df_bw_below <- df %>% filter(days_since_last >= bw[1] & days_since_last < c0)
df_bw_above <- df %>% filter(days_since_last >= c0 & days_since_last <= bw[2])

# Alternative way to define tables
# df_bw_below <- df %>% filter(days_since_last >= bw[1], days_since_last  < c0)
# df_bw_above <- df %>% filter(days_since_last >= c0, days_since_last <= bw[2])

model_bw_below <- lm(purchase_after ~ days_since_last, df_bw_below)
model_bw_above <- lm(purchase_after ~ days_since_last, df_bw_above)

y0 <- predict(model_bw_below, tibble(days_since_last = c0))
y1 <- predict(model_bw_above, tibble(days_since_last = c0))

late <- y1 - y0
sprintf("LATE: %.2f", late)

min_y <- min(df_bw$purchase_after)
max_y <- max(df_bw$purchase_after)

# Add lines for vertical distance and change limits of x-axis.
dep_var_bw <- 
  ggplot(df_bw, aes(x = days_since_last, y = purchase_after, color = coupon)) +
  geom_vline(xintercept = c0, color = "blue", linewidth = 2) +
  geom_point(alpha = 0.4, size = 1) +
  geom_smooth(data = df_bw_below, method = "lm", se = F, linewidth = 2) +
  geom_smooth(data = df_bw_above, method = "lm", se = F, linewidth = 2) +
  geom_segment(aes(x = c0, xend = bw[2], y = y0, yend = y0),
               linetype = "dotted", color = "red") +
  geom_segment(aes(x = bw[1], xend = c0, y = y1, yend = y1),
               linetype = "dotted", color = "red") +
  annotate("text", x = c0+2, y = mean(c(y1, y0)-2),
           label = sprintf("Difference: %.2f", (y1 - y0)),
           color = "green", fontface = 2) +
  scale_y_continuous(limits = c(min_y, max_y)) + 
  scale_color_discrete(labels = c("No coupon", "Coupon")) +
  xlab("Days since last purchase") +
  ylab("Purchase after coupon assignment") +
  theme(legend.title = element_blank())
dep_var_bw

lm_bw <- lm(purchase_after ~ days_since_last_centered + coupon, df_bw)
summary(lm_bw)