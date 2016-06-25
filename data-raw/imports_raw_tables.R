# imports_raw_tables -------------------
library(devtools)

# The imports empreq table is all production minus domestic production
imports_table_list <- mapply('-', all_table_list, dom_table_list, SIMPLIFY = FALSE)
imports_table_list <- lapply(imports_table_list, function(y) { y$Industry <- 1:length(y$Industry); return(y) } )

use_data(imports_table_list, overwrite = TRUE)
