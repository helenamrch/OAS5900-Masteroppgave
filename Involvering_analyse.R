# =============================================================================
# Involvering per kommune og navn
# =============================================================================
# Datasett-kolonner:
#   municipality  ??? kommunenavn
#   name          ??? personnavn
#   matched_text  ??? fritekst
#   involvement   ??? numerisk: 1 = vedtak, 2 = deltakelse, 3 = organisering, 4 = Annet
# =============================================================================

library(writexl)
library(readxl)
library(dplyr)
library(tidyverse) 

# -----------------------------------------------------------------------------
# 0. Les inn data  ???  juster filnavn/kolonnenavn etter behov
# -----------------------------------------------------------------------------

df <- name_context_matches_endelig

# -----------------------------------------------------------------------------
# 1. Kode involvement som factor med labels
# -----------------------------------------------------------------------------

df <- df |>
  mutate(
    involvement = factor(
      involvement,
      levels = 1:4,
      labels = c("1", "2", "3", "4")
    )
  )

# -----------------------------------------------------------------------------
# 2. Antall saker (rader) per kommune/navn
# -----------------------------------------------------------------------------

df_cases <- df |>
  group_by(municipality, name, year) |>
  summarise(total_involvment_cases_by_year = n(), .groups = "drop")

df_types <- df |>
  group_by(municipality, name, year, involvement) |>
  summarise(type_involvment_cases_by_year = n(), .groups = "drop")

# -----------------------------------------------------------------------------
# 3. merge
# -----------------------------------------------------------------------------

unike_navn_endelig <- unique(unike_navn_endelig)
unike_navn_endelig$year <- as.character(unike_navn_endelig$year)
df_cases$year <- as.character(df_cases$year)
df_analyse <- left_join(unike_navn_endelig, df_cases, by = c("municipality", "name", "year"))
kjonn_prosent_wide$year <- as.character(kjonn_prosent_wide$year)
df_analyse <- left_join(df_analyse, kjonn_prosent_wide, by = c("municipality", "year"))

write_xlsx(unike_navn_endelig,"unike_navn_endelig_3.xlsx")

df_types$year <- as.character(df_types$year)
df_types_1 <- df_types %>% filter(df_types$involvement=="1")
df_analyse_2 <- left_join(unike_navn_endelig, df_types_1, by = c("municipality", "name", "year"))
df_analyse_2 <- left_join(df_analyse_2, kjonn_prosent_wide, by = c("municipality", "year"))

df_types_2 <- df_types %>% filter(df_types$involvement=="2")
df_analyse_3 <- left_join(unike_navn_endelig, df_types_2, by = c("municipality", "name", "year"))
df_analyse_3 <- left_join(df_analyse_3, kjonn_prosent_wide, by = c("municipality", "year"))

df_types_3 <- df_types %>% filter(df_types$involvement=="3")
df_analyse_4 <- left_join(unike_navn_endelig, df_types_3, by = c("municipality", "name", "year"))
df_analyse_4 <- left_join(df_analyse_4, kjonn_prosent_wide, by = c("municipality", "year"))

df_types_4 <- df_types %>% filter(df_types$involvement=="4")
df_analyse_5 <- left_join(unike_navn_endelig, df_types_4, by = c("municipality", "name", "year"))
df_analyse_5 <- left_join(df_analyse_5, kjonn_prosent_wide, by = c("municipality", "year"))

# -----------------------------------------------------------------------------
# 4. N/A recode
# -----------------------------------------------------------------------------

df_analyse <- df_analyse |>
  mutate(total_involvment_cases_by_year = ifelse(is.na(total_involvment_cases_by_year), 0, total_involvment_cases_by_year))


df_analyse_2 <- df_analyse_2 |>
  mutate(type_involvment_cases_by_year = ifelse(is.na(type_involvment_cases_by_year), 0, type_involvment_cases_by_year))

df_analyse_3 <- df_analyse_3 |>
  mutate(type_involvment_cases_by_year = ifelse(is.na(type_involvment_cases_by_year), 0, type_involvment_cases_by_year))

df_analyse_4 <- df_analyse_4 |>
  mutate(type_involvment_cases_by_year = ifelse(is.na(type_involvment_cases_by_year), 0, type_involvment_cases_by_year))

df_analyse_5 <- df_analyse_5 |>
  mutate(type_involvment_cases_by_year = ifelse(is.na(type_involvment_cases_by_year), 0, type_involvment_cases_by_year))

# -----------------------------------------------------------------------------
# 5. Fixed effects regression model
# -----------------------------------------------------------------------------

library(fixest)

model1 = feols(total_involvment_cases_by_year ~ prosent_Jente + gender + prosent_Jente*gender | municipality + year, cluster = ~municipality + year, data = df_analyse)
print(model1)

model2 = feols(type_involvment_cases_by_year ~ prosent_Jente + gender + prosent_Jente*gender| municipality + year, cluster = ~municipality + year, data = df_analyse_2)
print(model2)

model3 = feols(type_involvment_cases_by_year ~ prosent_Jente + gender + prosent_Jente*gender| municipality + year, cluster = ~municipality + year, data = df_analyse_3)
print(model3)

model4 = feols(type_involvment_cases_by_year ~ prosent_Jente + gender + prosent_Jente*gender| municipality + year, cluster = ~municipality + year, data = df_analyse_4)
print(model4)

model5 = feols(type_involvment_cases_by_year ~ prosent_Jente + gender + prosent_Jente*gender| municipality + year, cluster = ~municipality + year, data = df_analyse_5)
print(model5)

# -----------------------------------------------------------------------------
# 6. Tabeller
# -----------------------------------------------------------------------------

result <- rbind(
  data.frame(Model = "Total Involvement Cases", model1$coeftable),
  data.frame(Model = "Involvement Type 1", model2$coeftable),
  data.frame(Model = "Involvement Type 2", model3$coeftable),
  data.frame(Model = "Involvement Type 3", model4$coeftable),
  data.frame(Model = "Involvement Type 4", model5$coeftable)
)

library(modelsummary)
library(tinytable)

names(coef(model1))

modelsummary(
  list("Hovedmodell" = model1),
  output = "tabell_hovedmodell.docx",
  estimate = "{estimate}{stars}",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 3,
  statistic = "({std.error})",
  coef_map = c(
    "prosent_Jente" = "Andel jenter",
    "genderJente" = "Jente",
    "prosent_Jente:genderJente" = "Andel jenter x gutt"
  ),
  coef_omit = "Intercept",
  gof_map = c("nobs", "r.squared.within"),
  add_rows = data.frame(
    term = c("Fixed effects", "Standardfeil"),
    "Hovedmodell" = c("Kommune og aar", "Cluster (kommune og aar)")
  )
)

install.packages("sjPlot")
library(sjPlot)

plot_model(model1, type = "int")
plot_model(model1, type = "est")

# -----------------------------------------------------------------------------
# 7. Eksporter
# -----------------------------------------------------------------------------

write_xlsx(unike_navn_endelig,"unike_navn_endelig_3.xlsx")
write.csv(result, "output_combined.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\ Lagret output_combined.csv ", nrow(result), "rader,", ncol(result), "kolonner\n")
