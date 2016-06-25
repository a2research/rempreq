# raw_tables.R -------------------------
library(devtools)
TABLE_YEARS <- 1997:2014
TABLE_NROWS <- 206
LATEST_YEAR <- max(TABLE_YEARS)
# Read in raw BLS domestic and total real valued (2009 USD) Employment Requirements Matrix Files

# BLS sector numbers and names
sectors <- read.table('data-raw/sect313.csv', stringsAsFactors = FALSE, sep = ',', header = TRUE)
sectors <- na.omit(sectors)
sector_name <- function(sector_number) sectors[sector_number, 'Industry.Commodity.Description']
sector_number <- function(sector_name) which(sectors$Industry.Commodity.Description ==  sector_name)

add_to_table_list <- function(table_files, table_years) {
  all_years_empreq <- list(length(table_files))
  table_years <- as.character(table_years)
  i <- 1
  for(f in table_files) {
    all_years_empreq[[i]] <- read.table(f, stringsAsFactors = FALSE,
                                                sep = ',', header = TRUE, row.names = NULL,
                                                nrows = 206)
    i <- i + 1
  }
  names(all_years_empreq) <- table_years
  return(all_years_empreq)
}


# Single data frame for all years' tables, long format, stacked by row, with year field
rseries_all_years_empreq <- function(files) {
  rseries <- do.call(rbind, lapply(files, function(x) read.table(x, stringsAsFactors = FALSE, sep = ',',
                                                      header = TRUE, row.names = NULL,
                                                      nrows = 206)))
  rseries$year <- rep(TABLE_YEARS, each = TABLE_NROWS)
  return(rseries)
}

# Single data frame for all years tables, wide format, sectors in rows, one column each per year
cseries_all_years_empreq <- function(files) {
  cseries <- do.call(cbind, lapply(files, function(x) read.table(x, stringsAsFactors = FALSE, sep = ',', header = TRUE, row.names = NULL, nrows = 206)))
  # remove duplicate Industry columns
  cseries <- cseries[, -which(names(cseries) == 'Industry')[-1]]
  names(cseries)[-1] <- sapply('year', paste, rep(TABLE_YEARS, each = TABLE_NROWS),
                                                      sep='_')
  return(cseries)
}
use_data(TABLE_YEARS, TABLE_NROWS, LATEST_YEAR, sectors, overwrite = TRUE)
