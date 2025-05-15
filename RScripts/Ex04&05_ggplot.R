# count sex within species
# boxplot+beeswarm weight vs. species
# boxplot+beeswarm weight vs. species AND sex

# scatterplot flipper length vs. body mass with regression line
# group by species and sex

# define your own colors for species (scale or name or rgb)

pacman::p_load(
  conflicted, tidyverse, wrappedtools,
  palmerpenguins, ggbeeswarm, hrbrthemes, plotly
)
# conflict_scout()
conflicts_prefer(
  dplyr::filter(),
  dplyr::lag()
)

# Table of sex counts within species
ex04Penguins <- palmerpenguins::penguins |>
  count(species, sex) |>
  tidyr::pivot_wider(names_from = sex, values_from = n, values_fill = 0)

View(ex04Penguins)

# Define custom species colors
species_colors <- c(
  "Adelie" = "#1f77b4", # Blue
  "Chinstrap" = "#ff7f0e", # Orange
  "Gentoo" = "#2ca02c" # Green
)

# Remove rows with missing data to avoid NA errors
ex04PenguinsCleaned <- palmerpenguins::penguins |>
  drop_na(body_mass_g, sex, species, flipper_length_mm)

# 1. Boxplot + beeswarm: body mass vs species
ggplot(ex04PenguinsCleaned, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  ggbeeswarm::geom_beeswarm(color = "black", size = 1.5) +
  scale_fill_manual(values = species_colors) +
  theme_minimal() +
  labs(
    title = "Body Mass by Species",
    x = "Species",
    y = "Body Mass (g)"
  )

# 2. Boxplot + beeswarm: body mass vs species and sex
ggplot(ex04PenguinsCleaned, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.75)) +
  ggbeeswarm::geom_beeswarm(aes(color = sex), dodge.width = 0.75, size = 1.5) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_color_manual(values = c("male" = "blue", "female" = "red")) +
  theme_minimal() +
  labs(
    title = "Body Mass by Species and Sex",
    x = "Species",
    y = "Body Mass (g)"
  )

# 3. Scatterplot: flipper length vs body mass with regression line, grouped by species and sex
# Assign sex symbols to a new column
ex04PenguinsInteractive <- ex04PenguinsCleaned |>
  mutate(
    sex = factor(sex, levels = c("female", "male")),
    species = factor(species, levels = c("Adelie", "Chinstrap", "Gentoo")),
    sex_symbol = recode(sex,
      "female" = "\u2640", # ♀
      "male" = "\u2642"
    ),
    group_combo = interaction(species, sex)
  )

# Build the plot
ex04PenguinsInteractive_ggplotly <- ggplot(ex04PenguinsInteractive, aes(
  x = flipper_length_mm,
  y = body_mass_g
)) +
  geom_text(aes(label = sex_symbol, color = species), size = 6, alpha = 0.7) +
  geom_smooth(aes(color = species, linetype = sex, group = group_combo),
    method = "lm", se = FALSE
  ) +
  scale_color_manual(name = "Species", values = species_colors) +
  scale_linetype_manual(name = "Sex", values = c("female" = "dashed", "male" = "solid")) +

  # Force legend order: color first, then linetype
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal() +
  labs(
    title = "Flipper Length vs Body Mass by Species and Sex",
    x = "Flipper Length (mm)",
    y = "Body Mass (g)"
  )

# Convert to interactive plot
ggplotly(ex04PenguinsInteractive_ggplotly)

str(ex04PenguinsInteractive)

# Ex 05
# Visualise bill depth vs. body mass, scatter only
# Add / compare linear/non-linear regression
# Facet by sex and species, use margins
# First clean the data
# Filter out rows with missing values to avoid errors
penguins_clean <- palmerpenguins::penguins |>
  drop_na(body_mass_g, bill_depth_mm, species, sex)

# Define custom colors for species
species_colors <- c(
  "Adelie" = "#1f77b4", # Blue
  "Chinstrap" = "#ff7f0e", # Orange
  "Gentoo" = "#2ca02c" # Green
)

# ============================================
# 📊 Scatterplot: Bill Depth vs. Body Mass
# ============================================

# Basic scatterplot
plot_basic <- ggplot(penguins_clean, aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point(aes(color = species), alpha = 0.7) +
  scale_color_manual(values = species_colors) +
  theme_minimal() +
  labs(
    title = "Bill Depth vs. Body Mass in Penguins",
    x = "Bill Depth (mm)",
    y = "Body Mass (g)",
    color = "Species"
  )

print(plot_basic)

# ============================================
# 📈 Add Linear and LOESS Regression
# ============================================

# Scatterplot with both linear and LOESS regression lines
plot_with_models <- ggplot(penguins_clean, aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point(aes(color = species), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(color = species), linetype = "solid") + # Linear
  geom_smooth(method = "loess", se = FALSE, aes(color = species), linetype = "dashed") + # Non-linear
  scale_color_manual(values = species_colors) +
  theme_minimal() +
  labs(
    title = "Linear and LOESS Fit: Bill Depth vs. Body Mass",
    subtitle = "Solid = Linear | Dashed = LOESS",
    x = "Bill Depth (mm)",
    y = "Body Mass (g)",
    color = "Species"
  )

print(plot_with_models)

# ============================================
# 🪟 Facet by Sex and Species with Margins
# ============================================

# Full faceted plot using facet_grid with margins
plot_faceted <- ggplot(penguins_clean, aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point(aes(color = species), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(color = species)) +
  scale_color_manual(values = species_colors) +
  facet_grid(sex ~ species, margins = TRUE) +
  theme_minimal() +
  labs(
    title = "Faceted: Bill Depth vs. Body Mass by Sex and Species",
    x = "Bill Depth (mm)",
    y = "Body Mass (g)",
    color = "Species"
  )

print(plot_faceted)
