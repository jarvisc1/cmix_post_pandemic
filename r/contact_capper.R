

# Load data ---------------------------------------------------------------
dt <- qs::qread('data/wrapup_part_cnts_nocap.qs')

chosen_cap<-300

# Fixing column with symptoms to be numbers
part_symp_cols <- grep("n_cnt", names(dt), value = TRUE)
for (col in part_symp_cols) {
  #set(dt, j = col, value = as.numeric(dt[[col]]))
  #for each column, set maximum value to 300
  dt[get(col) > chosen_cap, (col) := chosen_cap]
}
qs::qsave(dt,'data/wrapup_part_cnts_cap300.qs')