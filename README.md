
# bpfa

<!-- badges: start -->
<!-- badges: end -->

Quick and easy data management for the ECCC-PESC coastal mud fatty acid analysis project.

## Installation

You can install `{bpfa}` from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("popovs/BPFA")
```

## Usage

When you first load the package, you will need to create a fresh copy of the database on your local machine before proceeding.

```r
# Load package and initialize db
library(bpfa)
initialize_bpfa() # Create a new copy of the database!
```

You can check the path where the database file was created with `bpfa_dir()`. On a Mac, it will be something like:

`~Library/Application Support/BPFA`

While on PC, it will be either:

`C:\Documents and Settings\<User>\Application Data\Local Settings\popovs\BPFA`

Or:

`C:\Documents and Settings\<User>\Application Data\popovs\BPFA`

```r
# Check the file path where db was created
bpfa_dir()
```

### Connecting to the database

Once you have created the database, you can connect to it to explore the data or add new data.

```r
# Connect to the database
bpfa <- connect_bpfa()
```

The database comes pre-loaded with location and sample info. However, the PESC data tables need to be updated with new data.

```r
DBI::dbListTables(bpfa)
DBI::dbGetQuery(bpfa, "select * from locations limit 10;") # this comes pre-loaded with data
DBI::dbGetQuery(bpfa, "select * from pesc_data limit 10;") # this will be empty
```

### Importing data

PESC Excel data files need to be cleaned and re-formatted first before they can be imported into the database.

```r
# Load and clean up some PESC data
lims1 <- read_lims("~/Documents/path/to/pesc/file/V22F085.xlsx")
# Inspect the processed data
View(lims1$batch)
View(lims1$`bench sheet`)
View(lims1$`ng data`)
```

The function catches mistakes and errors in the data and will warn you!

```r
lims2 <- read_lims("~/Documents/path/to/pesc/file/V22F085-BAD-DATA.xlsx") # will spit out warnings
```

Let's only import the good data. It's very easy.....

```r
import_bpfa(lims1)
```

And that's that!

### Extracting data

Now we can extract some data and plot it. Here we still use the `DBI` package, but convenience functions to extract data for you can be added in future.

```r
fatty_acid_dat <- DBI::dbGetQuery(bpfa, "select * from results;")

library(ggplot2)
ggplot(fatty_acid_dat[grep("PUFA", fatty_acid_dat$assay),],
       aes(x = assay, y = ng_per_dry, color = site)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  facet_wrap(~ site) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

### Wrapping up

Don't forget to disconnect when you are done!

```r
DBI::dbDisconnect(bpfa)
```

