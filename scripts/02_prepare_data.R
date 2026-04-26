library(tidyverse)
source("scripts/01_load_data.R")

# Översikt av datasetets struktur och innehåll
glimpse(data_raw)
summary(data_raw)
colSums(is.na(data_raw))

# Definierar kategoriska variabler för analys och städning
categorical_vars <- c(
  "sex", 
  "region", 
  "smoker", 
  "chronic_condition", 
  "exercise_level", 
  "plan_type"
)

# Undersöker fördelning i kategoriska variabler före städning
for (var in categorical_vars) {
  cat("\n----", var, "----\n")
  print(data_raw %>% count(.data[[var]]))
}
# Rensar textdata (tar bort whitespace och standardiserar till gemener)
data_clean <- data_raw %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(across(where(is.character), str_to_lower))

data_clean <- data_clean %>%
  mutate(across(all_of(categorical_vars), as.factor))

# Kontrollerar att städning lyckats och beräknar antal och andelar per kategori
glimpse(data_clean)


for (var in categorical_vars) {
  cat("\n----", var, "----\n")
  print(
    data_clean %>% 
      count(.data[[var]]) %>% 
      mutate(procent = n / sum(n) * 100)
  )
}

# Beräknar andel saknade värden per variabel
na_percent <- round((colSums(is.na(data_clean)) / nrow(data_clean)) * 100, 2)
na_percent[na_percent > 0]

# Ersätter saknade värden i numeriska variabler med median
data_clean <- data_clean %>%
  mutate(
    bmi = ifelse(is.na(bmi), median(bmi, na.rm = TRUE), bmi),
    annual_checkups = ifelse(is.na(annual_checkups), median(annual_checkups, na.rm = TRUE), annual_checkups)
  )

# Ersätter saknade kategoriska värden med "unknown"
data_clean <- data_clean %>%
  mutate(
    exercise_level = coalesce(exercise_level, "unknown"),
    exercise_level = factor(exercise_level,
                            levels = c("low", "medium", "high", "unknown"))
  )

# Kontrollerar att alla saknade värden har hanterats

colSums(is.na(data_clean))

# Skapar nya variablar
data_clean <- data_clean %>%
  mutate(
    # BMI-kategori
    bmi_category = case_when(
      bmi < 18.5 ~ "underweight",
      bmi < 25 ~ "normal",
      bmi < 30 ~ "overweight",
      TRUE ~ "obese"
    ),
    
    # Åldersgrupp
    age_group = case_when(
      age < 30 ~ "young",
      age < 50 ~ "middle",
      TRUE ~ "older"
    ),
    
    # Risknivå (kombinerar rökning + kronisk sjukdom)
    risk_level = case_when(
      smoker == "yes" & chronic_condition == "yes" ~ "high",
      smoker == "yes" | chronic_condition == "yes" ~ "medium",
      TRUE ~ "low"
    ),
    
    # Historikscore
    history_score = prior_accidents + prior_claims
    ) %>%
    
    # Gör om datatyp till fct
    mutate(
      age_group = factor(age_group, 
                         levels = c("young", "middle", "older")),
      bmi_category = factor(bmi_category, 
                            levels = c("underweight", "normal", "overweight", "obese")),
      risk_level = factor(risk_level, 
                          levels = c("low", "medium", "high"))
      )

glimpse(data_clean)
