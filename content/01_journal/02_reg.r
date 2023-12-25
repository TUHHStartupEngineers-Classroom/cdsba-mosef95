library(dplyr)

df <- readRDS("content/01_journal/data/car_prices.rds")
#df <- readRDS("data/car_prices.rds")
dim(df)
View(df)

summary(df)
str(df)

model <- lm(price ~., data = df)
summary(model)
min_rpm <- min(df$peakrpm)
max_rpm <- max(df$peakrpm)
print(min_rpm)
print(max_rpm)


df <- df %>%
  mutate(seat_heating = TRUE)

summary(df)

model <- lm(price ~., data = df)
summary(model)