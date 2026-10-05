library(tidyverse)
library(janitor)

# PREPARE WORLD BANK MORTALITY DATA #
prepare_mortality <- function(file, outcome_name) {

  dat <- read_csv(file, show_col_types = FALSE) |>
    clean_names()

  # World Bank files often use country_code.
  # Rename it to iso for easier merging later.
  if ("country_code" %in% names(dat)) {
    dat <- dat |>
      rename(iso = country_code)
  }

  dat_long <- dat |>
    pivot_longer(
      cols = matches("^x?[0-9]{4}$"),
      names_to = "year",
      values_to = outcome_name
    ) |>
    mutate(
      year = as.integer(str_extract(year, "[0-9]{4}")),
      iso = toupper(iso)
    ) |>
    filter(year >= 2000, year <= 2019) |>
    select(iso, year, all_of(outcome_name))

  return(dat_long)
}


# Apply the function to all four mortality data sets

maternal <- prepare_mortality(
  "raw/maternal_mortality.csv",
  "maternal_mortality"
)

infant <- prepare_mortality(
  "raw/infant_mortality.csv",
  "infant_mortality"
)

neonatal <- prepare_mortality(
  "raw/neonatal_mortality.csv",
  "neonatal_mortality"
)

under5 <- prepare_mortality(
  "raw/under5_mortality.csv",
  "under5_mortality"
)

# PREPARE DISASTER DATA #
disaster <- read_csv(
  "raw/disaster.csv",
  show_col_types = FALSE
) |>
  clean_names() |>
  filter(
    year >= 2000,
    year <= 2019,
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(
    year,
    iso,
    disaster_type
  ) |>
  mutate(
    iso = toupper(iso),
    earthquake = if_else(disaster_type == "Earthquake", 1L, 0L),
    drought = if_else(disaster_type == "Drought", 1L, 0L)
  ) |>
  group_by(
    iso,
    year
  ) |>
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  )

# PREPARE CONFLICT DATA #
conflict <- read_csv(
  "raw/conflict.csv",
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    iso = toupper(iso)
  ) |>
  group_by(
    iso,
    year
  ) |>
  summarise(
    battle_deaths = sum(best, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    armed_conflict = if_else(
      battle_deaths >= 25,
      1L,
      0L
    )
  ) |>
  group_by(iso) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    armed_conflict_lag1 = lag(armed_conflict)
  ) |>
  ungroup()

# PREPARE COVARIATES #
covariates <- read_csv(
  "raw/covariates.csv",
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    iso = toupper(iso)
  ) |>
  filter(
    year >= 2000,
    year <= 2019
  )

# CREATE COUNTRY-YEAR FRAME #
countries <- read.table(
  "raw/countries.txt",
  header = TRUE
) |>
  clean_names()


# If countries.txt contains country_code instead of iso
if ("country_code" %in% names(countries)) {
  countries <- countries |>
    rename(iso = country_code)
}


countries <- countries |>
  mutate(
    iso = toupper(iso)
  )


country_year <- crossing(
  iso = unique(countries$iso),
  year = 2000:2019
)

#  MERGE ALL DATA SETS #
final_data <- country_year |>
  left_join(
    maternal,
    by = c("iso", "year")
  ) |>
  left_join(
    infant,
    by = c("iso", "year")
  ) |>
  left_join(
    neonatal,
    by = c("iso", "year")
  ) |>
  left_join(
    under5,
    by = c("iso", "year")
  ) |>
  left_join(
    covariates,
    by = c("iso", "year")
  ) |>
  left_join(
    disaster,
    by = c("iso", "year")
  ) |>
  left_join(
    conflict |>
      select(
        iso,
        year,
        armed_conflict,
        armed_conflict_lag1
      ),
    by = c("iso", "year")
  ) |>
  mutate(
    earthquake = replace_na(earthquake, 0L),
    drought = replace_na(drought, 0L),
    armed_conflict = replace_na(armed_conflict, 0L)
  ) |>
  arrange(
    iso,
    year
  )

write_csv(
  final_data,
  "processed/final_data.csv"
)