df <- readRDS("content/01_journal/data/hospdd.rds")
str(df)
#print(df, n = 300)

#select the hospitals for which the new admission procedure will be applied
new_procedure_hospitals <- unique(df$hospital[df$procedure == 1])
new_procedure_hospitals

treated_data <- df %>% filter(hospital %in% new_procedure_hospitals)
mean_satisfaction_treated_before <- treated_data %>% filter(month == 3) %>% summarise(mean_satisfaction = mean(satis, na.rm = TRUE)) %>% pull(mean_satisfaction)
mean_satisfaction_treated_after <- treated_data %>% filter(month == 4) %>% summarise(mean_satisfaction = mean(satis, na.rm = TRUE)) %>% pull(mean_satisfaction)

control_data <- df %>% filter(!hospital %in% new_procedure_hospitals)
mean_satisfaction_control_before <- control_data %>% filter(month == 3) %>% summarise(mean_satisfaction = mean(satis, na.rm = TRUE)) %>% pull(mean_satisfaction)
mean_satisfaction_control_after <- control_data %>% filter(month == 4) %>% summarise(mean_satisfaction = mean(satis, na.rm = TRUE)) %>% pull(mean_satisfaction)

DiD_estimate_before <- mean_satisfaction_treated_before - mean_satisfaction_control_before
DiD_estimate_after <- mean_satisfaction_treated_after - mean_satisfaction_control_after

DiD <- DiD_estimate_after - DiD_estimate_before
cat("Estimated DiD: ", DiD)

model_01 <- lm(satis ~ procedure + month + hospital, data = df)
summary(model_01)

model_02 <- lm(satis ~ procedure + as.factor(month) + as.factor(hospital), data = df)
summary(model_02)