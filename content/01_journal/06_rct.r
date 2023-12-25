library(dplyr)

df <- readRDS(("content/01_journal/data/abtest_online.rds"))

str(df)

compare_device <- ggplot(df, aes(x = chatbot, y = after_stat(count), fill = mobile_device)) +
  stat_count(geom = "bar", position = "dodge") +
  labs(x = "Chatbot", y = "Number of users", title = "Number of users on mobile devices") +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
  theme_minimal()

compare_previous <- 
  ggplot(df, 
         aes(x = chatbot, 
             y = previous_visit, 
             color = as.factor(chatbot))) +
  stat_summary(geom = "errorbar", 
               width = .5,
               fun.data = "mean_se", 
               fun.args = list(mult=1.96),
               show.legend = F) +
  labs(x = NULL, y = "Previous visits", title = "Difference in previous visits")

compare_device
compare_previous

mean_previous_visits <- df %>%
  group_by(chatbot) %>%
  summarize(mean_previous_visits = mean(previous_visit))

print(mean_previous_visits)

model_purchase_amount <- lm(purchase_amount ~ chatbot, data = df)
summary(model_purchase_amount)

model_purchase_mobile <- lm(purchase ~ chatbot * mobile_device, data = df)
summary(model_purchase_mobile)

model_purchase <- glm(purchase ~ chatbot, family=binomial(link='logit'), data = df)
summary(model_purchase)
