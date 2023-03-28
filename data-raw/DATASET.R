# Locations data
locations <- read.csv("data-raw/locations.csv")
locations$site <- factor(locations$site)
locations$code <- factor(locations$code)

usethis::use_data(locations, overwrite = TRUE)

# Samples data
samples <- read.csv("data-raw/samples.csv")
samples$date_collected <- as.Date(samples$date_collected, format = "%Y-%m-%d")
samples$date_collected <- as.character(samples$date_collected) # sqlite plays nicer w string dates...
samples$rain_yn <- ifelse(samples$rain_yn == 1, TRUE, FALSE)

usethis::use_data(samples, overwrite = TRUE)
