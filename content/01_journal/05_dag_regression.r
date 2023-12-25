df <- readRDS("content/01_journal/data/customer_sat.rds")
str(df)
df

model1 <- lm(satisfaction ~ follow_ups, data = df)
summary(model1)

model2 <- lm(satisfaction ~ follow_ups + subscription, data = df)
summary(model2)


satisfaction_not_cond <- ggplot(df, aes(x = follow_ups, y = satisfaction)) +
  geom_point() +
  stat_smooth(method = "lm", se = F)

# Conditioning on student  
satisfaction_cond <- ggplot(df, aes(x = follow_ups, y = satisfaction,
                                    color = subscription, 
                                    alpha = subscription)) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  scale_color_manual(values = c("Elite" = "red",
                                "Premium+" = "blue",
                                "Premium" = "green")) +
  scale_alpha_manual(values = c("Elite" = 1, "Premium+" = 0.6, "Premium" = 0.2)) +
  theme(legend.position = "right")


satisfaction_not_cond
satisfaction_cond