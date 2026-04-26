library(tidyverse)
source("scripts/02_prepare_data.R")


# Numeriska variabler för korrelation
numeric_data <- data_clean %>%
  select(where(is.numeric))

# Korrelationsmatris (hanterar NA med complete.obs)
cor_matrix <- cor(numeric_data, use = "complete.obs")

cor_matrix


# Fördelning av försäkringskostnader

viz_charges <- ggplot(data_clean, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "#2A9D8F", alpha = 0.8) +
  geom_vline(aes(xintercept = median(charges)), 
             linetype = "dashed", color = "black") +
  labs(
    title = "Fördelning av försäkringskostnader",
    x = "Kostnad",
    y = "Antal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  )

viz_charges


# Sammanfattning per rökstatus
smoker_summary <- data_clean %>%
  group_by(smoker) %>%
  summarise(
    avg_cost = mean(charges),
    median_cost = median(charges),
    count = n(),
    .groups = "drop"
  )

smoker_summary


# Boxplot - Kostnader per rökstatus
viz_smoker <- ggplot(data_clean, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("no" = "#2A9D8F", "yes" = "#7B6D8D")) +
  labs(
    title = "Kostnader för rökare vs icke-rökare",
    x = "Rökare",
    y = "Kostnad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

viz_smoker


# Sammanfattning per åldersgrupp
age_summary <- data_clean %>%
  group_by(age_group) %>%
  summarise(
    avg_cost = mean(charges),
    median_cost = median(charges),
    count = n(),
    .groups = "drop"
  )

age_summary


# Scatter - Kostnad vs ålder

viz_age_scat <- ggplot(data_clean, aes(x = age, y = charges)) +
  geom_point(color = "#2A9D8F", alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Samband mellan ålder och kostnad",
    x = "Ålder",
    y = "Kostnad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  )

viz_age_scat


# Boxplot - Kostnader per åldersgrupp

viz_age_box <- ggplot(data_clean, aes(x = age_group, y = charges, fill = age_group)) +
  geom_boxplot(alpha = 0.85, outlier.color = "black", outlier.alpha = 0.5) +
  scale_fill_manual(values = c(
    "young" = "#2A9D8F",
    "middle" = "#F4A6C8",
    "older" = "#7B6D8D"
  )) +
  labs(
    title = "Kostnader för olika åldersgrupper",
    x = "Åldersgrupp",
    y = "Kostnad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

viz_age_box


# Sammanfattning per risknivå
risk_summary <- data_clean %>%
  group_by(risk_level) %>%
  summarise(
    avg_cost = mean(charges),
    median_cost = median(charges),
    count = n(),
    .groups = "drop"
  )

risk_summary


# Boxplot - Kostnader per risknivå

viz_risk <- ggplot(data_clean, aes(x = risk_level, y = charges, fill = risk_level)) +
  geom_boxplot(alpha = 0.85, outlier.color = "black", outlier.alpha = 0.5) +
  scale_fill_manual(values = c(
    "low" = "#2A9D8F",
    "medium" = "#F4A6C8",
    "high" = "#7B6D8D"
  )) +
  labs(
    title = "Kostnader per risknivå",
    x = "Risknivå",
    y = "Kostnad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

viz_risk


# Sammanfattning per BMI-kategori
bmi_summary <- data_clean %>%
  group_by(bmi_category) %>%
  summarise(
    avg_cost = mean(charges),
    median_cost = median(charges),
    count = n(),
    .groups = "drop"
  )

bmi_summary


# Scatter - Kostnad vs BMI
viz_bmi_scat <- ggplot(data_clean, aes(x = bmi, y = charges)) +
  geom_point(color = "#2A9D8F", alpha = 0.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "Samband mellan BMI och kostnad",
    x = "BMI",
    y = "Kostnad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  )

viz_bmi_scat


# Boxplot - Kostnader per BMI-kategori
viz_bmi_box <- ggplot(data_clean, aes(x = bmi_category, y = charges, fill = bmi_category)) +
  geom_boxplot(alpha = 0.85, outlier.color = "black", outlier.alpha = 0.5) +
  scale_fill_manual(values = c(
    "underweight" = "#2A9D8F",
    "normal" = "#F4A6C8",
    "overweight" = "#7B6D8D",
    "obese" = "#5B8DB8"
  )) +
  labs(
    title = "Kostnader för olika BMI-grupper",
    x = "BMI-grupp",
    y = "Kostnad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11),
    legend.position = "none"
  )

viz_bmi_box
