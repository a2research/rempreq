# all_raw_tables.R -------------------------
library(devtools)

# Read in raw all (domestic and imports) employment requirements tables
files <-  list.files(paste('data-raw/real_empreq', sep = ''), full.names = TRUE)


# All Table List
all_table_list <- add_to_table_list(files, TABLE_YEARS)

rseries_all_years_real_allempreq <- rseries_all_years_empreq(files)
cseries_all_years_real_allempreq <- cseries_all_years_empreq(files)

use_data(all_table_list,
         rseries_all_years_real_allempreq,
         cseries_all_years_real_allempreq,
         overwrite = TRUE)

