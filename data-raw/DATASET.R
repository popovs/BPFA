locations <- read.csv("data-raw/locations.csv")
locations$site <- factor(locations$site)

usethis::use_data(locations, overwrite = TRUE)
