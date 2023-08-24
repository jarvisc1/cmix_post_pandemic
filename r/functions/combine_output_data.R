library(data.table)
library(stringr)

outputs_dir <- "outputs/uk"
outputs_files <- list.files(outputs_dir, recursive = T)


# Set filter to select data to combine at panel level (can filter later for run specific data)  
grep_pattern <- "panel_(ffc|eec)(.*)/(.*)data_filter_details.csv"
r0_estimate_files_ac <- grep(grep_pattern, outputs_files, value = TRUE)
r0_estimate_files_ac <- grep("england", r0_estimate_files_ac, value = TRUE)


# Set file name
fname_id <- "weighted"

r0_estimate_files <- c(r0_estimate_files_ac)
file_extract <- function(x) {
  # Get analysis details
  data_filter <- sub("\\/.*", "", x)
  data_details <- fread(file.path(outputs_dir, x))
  data_details[, data_filter := tolower(data_filter)]
  
    # Get r0 estimates
  data_dir <- sub("/data_filter_details.csv", "", x)
  r0_file_path <- file.path(outputs_dir, data_dir, "r0_estimates_panel.csv")
  if (file.exists(r0_file_path)){
    data_R0 <- fread(r0_file_path)
    data_R0[, data_filter := gsub("panel_", "", data_filter)]
    data <- merge(data_details, data_R0, by = "data_filter")
    data[, data_details := x]
    data[, wave_ids := gsub(".*wave_|\\/contact_matrices.*", "", x)]
    setnames(data, old = "variable", new = "baseline_matrices")
    
  } else {
    data <- data_details
  }
  
  
  data
}


r0_estimates <- lapply(r0_estimate_files, file_extract)

r0_estimates <- rbindlist(r0_estimates, fill = TRUE)

# Filter data
r0_estimates <- r0_estimates[nboots == "boots_100"]

setorder(r0_estimates, wave_ids)
# r0_estimates <- r0_estimates[nboots != "boots_1"]
# r0_estimates <- r0_estimates[grepl("trim_100", panel_details)]
# r0_estimates <- r0_estimates[filter_region == "ENGLAND" & baseline_matrices == "BBC Pandemic baseline"]


fpath <- file.path(outputs_dir, "combined", 
                   paste0("combined_R0_estimates_", fname_id, ".csv"))
fwrite(r0_estimates, fpath)
 
