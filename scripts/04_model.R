library(tidyverse)
source("scripts/02_prepare_data.R")

# -------------------------
# MODELLER
# -------------------------

# Basmodell: centrala variabler (ålder, BMI, rökning)
model1 <- lm(charges ~ age + bmi + smoker, data = data_clean)
summary(model1)


# Modell med kategoriserade variabler
model2 <- lm(charges ~ age_group + bmi_category + risk_level + history_score + 
               exercise_level + plan_type,
             data = data_clean)
summary(model2)


# Full modell med mer detaljerade variabler
model3 <- lm(charges ~ age + bmi + smoker + chronic_condition + prior_accidents 
             + prior_claims + exercise_level + plan_type,
      data = data_clean)
summary(model3)


# -------------------------
# MODELLJÄMFÖRELSE
# -------------------------

# Jämför R², justerat R² och residualstandardfel
model_comparison <- tibble(
  model = c(
    "Model 1: Basnivå",
    "Model 2: Kategoriserad & aggregerad",
    "Model 3: Rådata (full modell)"
  ),
  r_squared = c(
    summary(model1)$r.squared,
    summary(model2)$r.squared,
    summary(model3)$r.squared
  ),
  adjusted_r_squared = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared,
    summary(model3)$adj.r.squared
  ),
  residual = c(
    summary(model1)$sigma,
    summary(model2)$sigma,
    summary(model3)$sigma
  )
)

model_comparison


# -------------------------
# DIAGNOSTIK (MODELL 3)
# -------------------------

# Förbereder residualer och fitted values
model3_diagnostics <- data_clean %>%
  mutate(
    fitted_values = fitted(model3),
    residual = resid(model3)
  )


# Exempel på prediktion vs utfall
model3_diagnostics %>% 
  select(charges, fitted_values, residual) %>%
  slice_head(n = 10)


# Residualer vs predikterade värden
model3_residual_plot <- ggplot(model3_diagnostics, aes(x = fitted_values, y = residual)) +
  geom_point(color = "#2A9D8F", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed",linewidth = 0.8, color = "black") +
  labs(
    title = "Residualer mot predikterat värde",
    x = "Predikterat pris",
    y = "Residual"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  )

model3_residual_plot


# Faktiskt vs predikterat värde
model3_actual_vs_fitted_plot <- ggplot(model3_diagnostics, aes(x = fitted_values, y = charges)) +
  geom_point(color = "#2A9D8F", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",linewidth = 0.6, color = "black") +
  labs(
    title = "Faktiskt pris mot predikterat värde",
    x = "Predikterat pris",
    y = "Faktiskt pris"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  )

model3_actual_vs_fitted_plot


# -------------------------
# NORMALITETSTEST (QQ-PLOTS)
# -------------------------

# QQ-plot för alla modeller (visuell kontroll)
qqnorm(resid(model1))
qqline(resid(model1), col = "red")

qqnorm(resid(model2))
qqline(resid(model2), col = "red")

qqnorm(resid(model3))
qqline(resid(model3), col = "red")


# QQ-plot (modell 3 i ggplot-format för rapport)

model3_qq_plot <- ggplot(data.frame(resid = resid(model3)), aes(sample = resid)) +
  stat_qq(color = "#2A9D8F", alpha = 0.5, size = 2) +
  stat_qq_line(color = "black", linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "Q-Q-plot av residualer",
    x = "Teoretiska kvantiler",
    y = "Observerade kvantiler"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 11)
  )

model3_qq_plot
