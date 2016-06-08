# real_domrempreq_explore.R

library(dplyr)
BASE_DATA_DIR <- 'data/'
TABLE_YEARS <- 1997:2014
TABLE_NROWS <- 206
LATEST_YEAR <- max(TABLE_YEARS)
setwd('~')
setwd('~/SpiderOak Hive/SpiderOakBackup/Allan/EconDataCode/rempreq')

##### Sectors #####
sectors <- read.table(paste(BASE_DATA_DIR, 'sect313.csv', sep = ''), stringsAsFactors = FALSE, sep = ',', header = TRUE)
sectors <- na.omit(sectors)
sector_name <- function(sector_number) sectors[sector_number, 'Industry.Commodity.Description']
sector_number <- function(sector_name) which(sectors$Industry.Commodity.Description ==  sector_name)
##########

##### Domestic Only #####
domempreq <- read.table(paste(BASE_DATA_DIR, 'real_domempreq/REAL_DOMEMPREQ_2014.CSV', sep = ''),
                        stringsAsFactors = FALSE, sep = ',', header = TRUE, row.names = NULL, nrows = TABLE_NROWS)

files <-  list.files(paste(BASE_DATA_DIR, 'real_domempreq', sep = ''), full.names = TRUE)

add_to_table_list <- function(table_files, table_years) {
  all_years_real_domempreq <- list(length(table_files))
  table_years <- as.character(table_years)
  i <- 1
  for(f in table_files) {
    all_years_real_domempreq[[i]] <- read.table(f, stringsAsFactors = FALSE,
                                                sep = ',', header = TRUE, row.names = NULL,
                                                nrows = 206)
    i <- i + 1
  }
  names(all_years_real_domempreq) <- table_years
  return(all_years_real_domempreq)
}
##### Domestic Table List #####
dom_table_list <- add_to_table_list(files, TABLE_YEARS)

# Single data frame for all years' tables, stacked by row, with year field
rseries_all_years_real_domempreq <- do.call(rbind, lapply(files, function(x) read.table(x, stringsAsFactors = FALSE, sep = ',', header = TRUE, row.names = NULL, nrows = 206)))
rseries_all_years_real_domempreq$year <- rep(TABLE_YEARS, each = TABLE_NROWS)


cseries_all_years_real_domempreq <- do.call(cbind, lapply(files, function(x) read.table(x, stringsAsFactors = FALSE, sep = ',', header = TRUE, row.names = NULL, nrows = 206)))
# remove duplicate Industry columns
cseries_all_years_real_domempreq <- cseries_all_years_real_domempreq[, -which(names(cseries_all_years_real_domempreq) == 'Industry')[-1]]
names(cseries_all_years_real_domempreq)[-1] <- sapply('year', paste, rep(TABLE_YEARS, each = TABLE_NROWS),
                                                                     sep='_')

# Return the employment requirements table for the given year, current by default
table_by_year <- function(year = LATEST_YEAR, tablelist = dom_table_list) {
  year_list <- dom_tablelist[[as.character(year)]]
}

# Return the latest employment requirements table, convenience wrapper for table_year defaults
current_table <- function() {
  table_by_year()
}

# Return the employment requirements coefficient vector for a sector
series_by_sector <- function(series_table, sector_number) {
  as.vector(series_table[sector_number, ])[-1]
}

# Return the employment requirements coefficient vector for a sector and given year, latest year by default
series_by_sector_by_year <- function(sector_number, year = LATEST_YEAR, tablelist = table_list) {
  series_by_sector(table_by_year(year, tablelist), sector_number)
}

#series_by_sector <- function(sector_number, tablelist = table_list ) {
#  sector_list <- sapply(tablelist, function(x) x[sector_number, ])
#}

# Returns a vector of the number of jobs by all sectors for a given amount of production of a sector
sector_emp_impact <- function(sector_number, prod_output_mm, year = LATEST_YEAR, tablelist = table_list) {
  series_by_sector_by_year(sector_number) * prod_output_mm
}

y <- sector_emp_impact(27, 10)

# Returns the total number of jobs created in a given sector by a given amount of production of that sector
emp_impact <- function(sector_number, prod_output_mm, year = LATEST_YEAR,  tablelist = table_list) {
  sum(series_by_sector_by_year(sector_number, year))
}

####################
memp <- vector("numeric", length(TABLE_YEARS))

for(year in TABLE_YEARS) {
    v <<- vector()
  for(sector in sectors$Sector.Number) {
    v[length(v) + 1] <- sum(series_by_sector_by_year(sector, year))
  }
  memp <- cbind(memp, v)
}
memp <- memp[ , -1]
memp <- t(memp)

memp <- as.data.frame(cbind(TABLE_YEARS, memp))


library(tidyr)

memp_long <- memp %>%
  gather(sector_no, sector_emp, V2:V207)
tail(memp_long)

selected_memp <- memp_long %>%
  filter(sector_no %in% c('V2', 'V8', 'V16', 'V28', 'V29', 'V36', 'V78', 'V97', 'V137', 'V207'))

# time series to estimate trends
x <- ts(memp)



library(ggplot2)
p <- ggplot(selected_memp, aes(TABLE_YEARS, sector_emp, color = sector_no)) + geom_line(aes(group = sector_no))
# library(directlabels)

# direct.label(p, list(last.points, hjust = 0.7,
#                     vjust = 1), debug = T)


ggplot(memp_long, aes(TABLE_YEARS, log10(sector_emp))) + geom_line(aes(color = TABLE_YEARS, group = TABLE_YEARS))

p <- ggplot(memp_long, aes(x=TABLE_YEARS,y=sector_no))
p + geom_tile(aes(fill=sector_emp)) + scale_fill_gradient(low="white", high="darkblue") + xlab("") + ylab("")


####################

for(sector in sectors$Sector.Number) {
  v[length(v) + 1] <- sum(series_by_sector_by_year(sector, year))
}
# r <- outer(sectors$Sector.Number, TABLE_YEARS, FUN = "series_by_sector_by_year")


check <- series_by_sector_by_year(70, 1997)

x <- as.numeric(mapply(series_by_sector_by_year, rep(70, length(TABLE_YEARS)), TABLE_YEARS))
m <- matrix(x, length(TABLE_YEARS), TABLE_NROWS)



z <- rowSums(m)

y <- series_by_sector_by_year(70)
y <- t(y)

zdf <- data.frame(year = 1:TABLE_NROWS, employment_per_1MM = y[, 1], row.names = NULL)
ggplot(zdf, aes(year, employment_per_1MM)) + geom_point()

