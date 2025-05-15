##################################################################
### Safe to do this all the time except when already installed ###
### ##############################################################
# Sys.setenv(LANG = "en_EN.UTF-8") # to get errors/warnings in English
# if (!requireNamespace("pacman", quietly = TRUE)) {
#   install.packages("pacman")
# }
pacman::p_load(
  conflicted, # tests/solutions for name conflicts
  tidyverse, # metapackage
  wrappedtools # my own tools package
  # randomNames # used to create pseudo or fancy names
)
conflict_scout()
conflicts_prefer(
  dplyr::filter,
  stats::lag
)

# Define the number of elements (grains/flakes)
n_elements <- 5 * 10^3  # 5000

# Create a tibble "menage" representing the cruet stand
menage <- tibble(
  saltshaker = rep("salt", n_elements),
  peppercaster = rep("pepper", n_elements)
)

# Print the entire tibble (optional, may be large)
print(menage)

# ------------------------------
# Method 1: Base R style
# ------------------------------

# Print the full saltshaker column
print(menage$saltshaker)

# Print the first 5 salt grains
salt <- menage$saltshaker[1:5]
print(salt)

# Print the first 100 salt grains
salt_100 <- menage$saltshaker[1:100]
print(salt_100)

# ------------------------------
# Method 2: Tidyverse style
# ------------------------------

# Select the saltshaker column
salt_col <- menage %>% select(saltshaker)
print(salt_col)

# Use slice to get first 5 salt grains
salt_5 <- menage %>% slice(1:5) %>% pull(saltshaker)
print(salt_5)

# Use slice + pull to get first 100 salt grains
salt_100_dplyr <- menage %>% slice(1:100) %>% pull(saltshaker)
print(salt_100_dplyr)