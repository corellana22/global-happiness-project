# analysis.R
# Global Happiness Project — quick EDA and figures
# ------------------------------------------------
# Usage (from repo root):
#   Rscript analysis.R
# Requirements:
#   - R packages: tidyverse
#   - Data file: whr-2023.csv (same columns as used in your report)

suppressPackageStartupMessages({
  library(tidyverse)
})

# ---- Setup ---------------------------------------------------------------
infile <- "whr-2023.csv"
outdir <- "figs"
if (!file.exists(infile)) {
  stop(paste0(
    "\nMissing data file '", infile, "'. ",
    "Place the World Happiness CSV in the repo root or update `infile`."
  ))
}
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ---- Load ----------------------------------------------------------------
# Expecting at least these columns:
# Country.name, year, Life.Ladder, Log.GDP.per.capita, Social.support,
# Healthy.life.expectancy.at.birth, Freedom.to.make.life.choices,
# Generosity, Perceptions.of.corruption, Positive.affect, Negative.affect
df <- readr::read_csv(infile, show_col_types = FALSE)

# ---- Basic summary -------------------------------------------------------
summary_tbl <- tibble(
  total_rows       = nrow(df),
  total_years      = df %>% distinct(year) %>% nrow(),
  total_countries  = df %>% distinct(Country.name) %>% nrow()
)
print(summary_tbl)

# ---- Region mapping (aligned with your QMD) ------------------------------
df <- df %>%
  mutate(
    Region = case_when(
      Country.name %in% c(
        "Austria","Belgium","Denmark","Finland","France","Germany","Iceland",
        "Ireland","Italy","Luxembourg","Malta","Netherlands","Norway",
        "Portugal","Spain","Sweden","Switzerland","United Kingdom","Greece","Cyprus"
      ) ~ "Western Europe",
      Country.name %in% c("Canada","United States","Australia","New Zealand") ~ "North America and ANZ",
      Country.name %in% c(
        "Argentina","Belize","Bolivia","Brazil","Chile","Colombia","Costa Rica",
        "Cuba","Dominican Republic","Ecuador","El Salvador","Guatemala","Haiti",
        "Honduras","Jamaica","Mexico","Nicaragua","Panama","Paraguay","Peru",
        "Suriname","Trinidad and Tobago","Uruguay","Venezuela","Guyana"
      ) ~ "Latin America and Caribbean",
      Country.name %in% c(
        "Albania","Bosnia and Herzegovina","Bulgaria","Croatia","Czechia","Estonia",
        "Hungary","Kosovo","Latvia","Lithuania","Montenegro","North Macedonia",
        "Poland","Romania","Serbia","Slovakia","Slovenia"
      ) ~ "Central and Eastern Europe",
      Country.name %in% c(
        "Armenia","Azerbaijan","Belarus","Kazakhstan","Kyrgyzstan","Moldova",
        "Russia","Tajikistan","Turkmenistan","Ukraine","Uzbekistan","Georgia"
      ) ~ "CIS",
      Country.name %in% c(
        "Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon",
        "Central African Republic","Chad","Congo (Brazzaville)","Congo (Kinshasa)",
        "Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Ivory Coast",
        "Kenya","Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania",
        "Mauritius","Mozambique","Namibia","Niger","Nigeria","Rwanda","Senegal",
        "Sierra Leone","Somalia","South Africa","South Sudan","Sudan","Togo",
        "Uganda","Tanzania","Zambia","Zimbabwe","Somaliland region","Comoros","Djibouti"
      ) ~ "Sub-Saharan Africa",
      Country.name %in% c(
        "Afghanistan","Bangladesh","Bhutan","India","Nepal","Pakistan","Sri Lanka","Maldives"
      ) ~ "South Asia",
      Country.name %in% c(
        "China","Hong Kong S.A.R. of China","Japan","Mongolia","South Korea","Taiwan Province of China"
      ) ~ "East Asia",
      Country.name %in% c(
        "Cambodia","Indonesia","Laos","Malaysia","Myanmar","Philippines","Singapore","Thailand","Vietnam"
      ) ~ "Southeast Asia",
      Country.name %in% c(
        "Algeria","Bahrain","Egypt","Iran","Iraq","Israel","Jordan","Kuwait","Lebanon","Libya",
        "Morocco","Oman","Qatar","Saudi Arabia","State of Palestine","Syria","Tunisia",
        "Turkiye","United Arab Emirates","Yemen"
      ) ~ "Middle East and North Africa",
      TRUE ~ "Other"
    )
  )

# ---- Global trend over time ----------------------------------------------
p_trend <- df %>%
  group_by(year) %>%
  summarise(mean_life_ladder = mean(Life.Ladder, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(year, mean_life_ladder)) +
  geom_line(linewidth = 1.1) +
  geom_point() +
  labs(
    title = "Global Average Life Ladder Over Time",
    x = "Year", y = "Average Life Ladder"
  ) +
  theme_minimal()
ggsave(file.path(outdir, "trend_life_ladder.png"), p_trend, width = 8, height = 5, dpi = 150)

# ---- GDP vs Life Ladder by region (facets) -------------------------------
df_facets <- df %>%
  mutate(
    Region_label = case_when(
      Region == "Latin America and Caribbean" ~ "LatAm & Caribbean",
      Region == "North America and ANZ" ~ "NA & ANZ",
      Region == "Middle East and North Africa" ~ "MENA",
      Region == "Central and Eastern Europe" ~ "Central & East Europe",
      TRUE ~ Region
    )
  )

p_gdp <- df_facets %>%
  drop_na(Life.Ladder, Log.GDP.per.capita, Region_label) %>%
  ggplot(aes(Log.GDP.per.capita, Life.Ladder)) +
  geom_point(alpha = 0.35, size = 1.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9, color = "tomato") +
  facet_wrap(~ Region_label, ncol = 2, scales = "free_y") +
  labs(
    title = "Happiness vs Log GDP per Capita by Region",
    x = "Log GDP per Capita", y = "Life Ladder"
  ) +
  theme_minimal()
ggsave(file.path(outdir, "gdp_vs_life_ladder_by_region.png"), p_gdp, width = 9, height = 10, dpi = 150)

# ---- Correlation matrix (numeric subset) ---------------------------------
num_cols <- c(
  "Life.Ladder","Log.GDP.per.capita","Social.support",
  "Healthy.life.expectancy.at.birth","Freedom.to.make.life.choices",
  "Generosity","Perceptions.of.corruption","Positive.affect","Negative.affect"
)

corr_tbl <- df %>%
  select(any_of(num_cols)) %>%
  drop_na() %>%
  cor() %>%
  round(3) %>%
  as.data.frame()

readr::write_csv(corr_tbl, file.path(outdir, "correlation_matrix.csv"))
print("Saved correlation matrix to figs/correlation_matrix.csv")

# ---- Simple distribution plots (GDP & Social support) --------------------
p_gdp_hist <- df %>%
  filter(is.finite(Log.GDP.per.capita)) %>%
  ggplot(aes(Log.GDP.per.capita)) +
  geom_histogram(binwidth = 0.2, color = "black", fill = "grey70") +
  labs(title = "Distribution of Log GDP per Capita", x = "Log GDP per Capita", y = "Count") +
  theme_minimal()
ggsave(file.path(outdir, "hist_log_gdp.png"), p_gdp_hist, width = 7, height = 4.5, dpi = 150)

p_support_hist <- df %>%
  filter(!is.na(Social.support)) %>%
  ggplot(aes(Social.support)) +
  geom_histogram(binwidth = 0.05, color = "black", fill = "grey70") +
  labs(title = "Distribution of Social Support", x = "Social Support (0–1)", y = "Count") +
  theme_minimal()
ggsave(file.path(outdir, "hist_social_support.png"), p_support_hist, width = 7, height = 4.5, dpi = 150)

# ---- Done ----------------------------------------------------------------
cat("\nAnalysis complete ✅\n")
cat("Figures saved to 'figs/' and summary printed above.\n")
sessionInfo()
