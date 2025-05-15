pacman::p_load(
  conflicted, tidyverse, wrappedtools, usethis,
  palmerpenguins, ggbeeswarm, hrbrthemes, plotly
)
# conflict_scout()
conflicts_prefer(
  dplyr::filter(),
  dplyr::lag()
)

PenguinsData <- palmerpenguins::penguins

view(PenguinsData)
str(PenguinsData)

ks.test(
  x = PenguinsData$bill_length_mm,
  "pnorm",
  mean = mean(PenguinsData$bill_length_mm,
    na.rm = TRUE,
    sd = sd(PenguinsData$bill_length_mm, na.rm = TRUE)
  )
)

ksnormal(PenguinsData$bill_length_mm, lillie = FALSE)
shapiro.test(PenguinsData$bill_length_mm)

PenguinsData <- na.omit(palmerpenguins::penguins)

ggplot(PenguinsData, aes(x = bill_length_mm)) +
  geom_density(fill = "lightgreen")
ggplot(PenguinsData, aes(x = bill_length_mm, fill = sex)) +
  geom_density(alpha = 0.4, na.rm = TRUE) +
  labs(
    title = "Bill length of Penguins aggregated by Sex",
    x = "Bill length (mm)",
    y = "Density",
    fill = "Sex"
  ) +
  # facet_wrap(vars(species))+
  facet_grid(row = vars(species), col = vars(sex)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

#####################################
## Creating a facet grid in a loop ##
#####################################
# First select columns with numeric variables
# Extract the column names and place them in a vector
# This is because I want to run stats on numerical variables automatically
numvars <- PenguinsData |>
  select(where(is.numeric)) |>
  names()

# Step 2: Pre-fill results table
happy_penguins <- tibble(
  Variables = numvars,
  pKS = NA_real_,
  pSh = NA_real_
)

# Step 3: Run normality tests, but always plot body_mass_g
for (i in seq_along(numvars)) {
  varname <- numvars[i]

  # Drop NAs for statistical tests
  data_vec <- na.omit(PenguinsData[[varname]])

  # Run normality tests
  happy_penguins$pKS[i] <- ks.test(data_vec, "pnorm",
    mean = mean(data_vec),
    sd = sd(data_vec)
  )$p.value
  happy_penguins$pSh[i] <- shapiro.test(data_vec)$p.value

  # Generate density plot with fixed x-axis: body_mass_g
  p <- ggplot(PenguinsData, aes(x = body_mass_g, fill = sex)) +
    geom_density(alpha = 0.4, na.rm = TRUE) +
    labs(
      title = paste("Density of Body Mass (g), Testing:", varname),
      x = "Body Mass (g)",
      y = "Density",
      fill = "Sex",
      subtitle = paste0(
        "KS p (", varname, ") = ", signif(happy_penguins$pKS[i], 4),
        " | Shapiro p = ", signif(happy_penguins$pSh[i], 4)
      )
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    facet_grid(rows = vars(species), cols = vars(sex)) # Facet by species and sex

  print(p)
}

# Step 4: Format and print p-values
happy_penguins_new <- happy_penguins |>
  mutate(
    pKS = format.pval(pKS, digits = 5, eps = .00001),
    pSh = format.pval(pSh, digits = 5, eps = .00001)
  )

print(happy_penguins_new)

# Check out using the xkcd font
