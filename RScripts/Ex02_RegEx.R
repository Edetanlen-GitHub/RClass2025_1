pacman::p_load(tidyverse, lubridate)

### === Task 1: Detect Variants of "Meier" === ###
testset1 <- c("Meier", "Mayer", "Maier", "Meyer", "Mayr", "Maya", "Mayor")

# Regex pattern explanation:
# ^M         -> Starts with 'M'
# [ae]       -> Next letter is 'a' or 'e'
# [iy]       -> Then 'i' or 'y'
# e?         -> Optional 'e'
# r$         -> Ends with 'r'
pattern_meier <- "^M[ae][iy]e?r$"

# Visual pattern check
str_view(testset1, pattern_meier, match = TRUE)

# Detect using stringr
meier_matches_str <- testset1[str_detect(testset1, pattern_meier)]

# Base R alternative using subset()
meier_matches_subset <- subset(testset1, str_detect(testset1, pattern_meier))

cat("Task 1 - Meier variants (str_detect):\n")
print(meier_matches_str)

cat("Task 1 - Meier variants (subset):\n")
print(meier_matches_subset)


### === Task 2: Clean Labels and Format Units === ###
testset2 <- c("weight_mm", "height_cm", "age_yr", "temp_c")

# Regex to replace all underscores with spaces
labels_cleaned <- str_replace_all(testset2, "_", " ")

# Regex breakdown for unit formatting:
# _(\\w+)$  -> Match an underscore followed by a word at the end (captured as group 1)
# Replace with " (\\1)" to insert space and brackets
labels_with_units <- str_replace(testset2, "_(\\w+)$", " (\\1)") |> # Add unit in brackets
  str_replace_all("_", " ") # Clean remaining underscores

cat("\nTask 2 - Labels cleaned:\n")
print(labels_cleaned)

cat("Task 2 - Labels with units:\n")
print(labels_with_units)


### === Task 3: Standardize Dates to YYYY-MM-DD === ###
testset3 <- c("1980_12_30", "13.04.2005", "2005/04/25", "24121990")

# Use lubridate::parse_date_time with multiple regex-informed formats
# Supported formats:
# - Ymd (19801230)
# - dmy (24121990)
# - Y_m_d (1980_12_30)
# - d.m.Y (13.04.2005)
# - Y/m/d (2005/04/25)
parsed_dates <- parse_date_time(
  testset3,
  orders = c("Ymd", "dmy", "Y_m_d", "d.m.Y", "Y/m/d")
)

# Format to ISO 8601
standard_dates <- format(parsed_dates, "%Y-%m-%d")

cat("\nTask 3 - Standardized dates:\n")
print(standard_dates)


### === Task 4: Password Strength Check (Strict Regex-Based) === ###
testset4 <- c("pw2000", "That1sb3tt3r", "M@kesSense?", "NoDigits@this1")

# Regex breakdown for password strength:
# ^(?=.*[A-Z])         -> at least one uppercase
# (?=.*[a-z])          -> at least one lowercase
# (?=.*[0-9])          -> at least one digit
# (?=.*[^A-Za-z0-9])   -> at least one special character
# .{8,}$               -> at least 8 characters total
strength_pattern <- "^(?=.*[A-Z])(?=.*[a-z])(?=.*[0-9])(?=.*[^A-Za-z0-9]).{8,}$"

# Function to check using full regex
check_strength_regex <- function(pwd) {
  str_detect(pwd, strength_pattern)
}

pwd_strength <- sapply(testset4, check_strength_regex)

cat("\nTask 4 - Password strength (regex only):\n")
print(pwd_strength)
