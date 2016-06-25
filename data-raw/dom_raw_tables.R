# dom_raw_tables.R -------------------------
library(devtools)

# Read in raw domestic employment requirements tables
files <-  list.files(paste('data-raw/real_domempreq', sep = ''), full.names = TRUE)


# Domestic Table List
dom_table_list <- add_to_table_list(files, TABLE_YEARS)

rseries_all_years_real_domempreq <- rseries_all_years_empreq(files)
cseries_all_years_real_domempreq <- cseries_all_years_empreq(files)

use_data(dom_table_list,
         rseries_all_years_real_domempreq,
         cseries_all_years_real_domempreq,
         overwrite = TRUE )

