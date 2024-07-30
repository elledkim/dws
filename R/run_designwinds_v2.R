args <- commandArgs(trailingOnly = TRUE)

risk_category <- as.integer(args[1])
build_year <- as.integer(args[2])
lifespan <- as.integer(args[3])
calc_method <- args[4]

# Get the current working directory
script_dir <- getwd()
cat("Current working directory: ", script_dir, "\n")  # Print to verify the working directory

# Ensure the generated_csvs directory exists inside the R directory
output_dir <- file.path(script_dir, "generated_csvs")
if (!dir.exists(output_dir)) {
  cat("Creating output directory: ", output_dir, "\n")
  dir.create(output_dir)
}

# Load required data and source files
tryCatch({
  load(file.path(script_dir, "sysdata_winds.Rdata"))
  load(file.path(script_dir, "sysdata.rda")) # fit_data contains all needed parameters
}, error = function(e) {
  cat("Error loading data files:", e$message, "\n")
  stop(e)
})

# library(urbnmapr)
library(scales)
library(ggplot2)

source_files <- c("nonstationary_return.R", "nonstationary_return_MRI.R", "nonstationary_return_AEP.R",
                  "design_wind_speed_v2024.R", "design_wind.R", "design_wind_AEP.R",
                  "design_wind_MRI.R", "xu.R", "LEP.R", "xu_LEP.R", "AEP.R")

tryCatch({
  lapply(source_files, function(f) source(file.path(script_dir, f)))
}, error = function(e) {
  cat("Error sourcing R files:", e$message, "\n")
  stop(e)
})

# Prepare the data frame with specified columns
nCounties <- length(fit_data$county_fips)
csv_cases <- data.frame(
  ID = integer(nCounties),
  X = numeric(nCounties),
  Y = numeric(nCounties),
  STATEFP = character(nCounties),
  COUNTYFP = character(nCounties),
  BUILDYEAR = integer(nCounties),
  LIFESPAN = integer(nCounties),
  RISKCAT = integer(nCounties),
  METHOD = character(nCounties),
  XD = numeric(nCounties),
  stringsAsFactors = FALSE
)

# Calculate the design wind speeds
tryCatch({
  for (c in 1:nCounties) {
    fips <- fit_data$county_fips[c]
    # Calculate the corresponding design wind speed
    csv_cases[c, "XD"] <- design_wind_speed_v2024(fips, lifespan, build_year, risk_category, calc_method)
    csv_cases[c, "ID"] <- fips
    csv_cases[c, "X"] <- fit_data$Longitude[c]
    csv_cases[c, "Y"] <- fit_data$Latitude[c]
    csv_cases[c, "STATEFP"] <- substr(toString(fips), 1, 2)
    csv_cases[c, "COUNTYFP"] <- substr(toString(fips), 3, 5)
    csv_cases[c, "BUILDYEAR"] <- build_year
    csv_cases[c, "LIFESPAN"] <- lifespan
    csv_cases[c, "RISKCAT"] <- risk_category
    csv_cases[c, "METHOD"] <- calc_method
  }
}, error = function(e) {
  cat("Error calculating design wind speeds:", e$message, "\n")
  stop(e)
})

# Write the CSV file to the correct directory inside the R folder
output_filename <- paste0("US_counties_Gori200_", build_year, "_", lifespan, "_", calc_method, risk_category, ".csv")

tryCatch({
  write.csv(csv_cases, file.path(output_dir, output_filename), row.names = FALSE)
  cat("CSV file generated successfully.\n")
  cat(output_filename, "\n")
}, error = function(e) {
  cat("Error writing CSV file:", e$message, "\n")
  stop(e)
})
