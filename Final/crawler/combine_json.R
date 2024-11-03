library(jsonlite)
library(data.table)

# Function to load and convert a JSON file to a data table
load_json_as_dt <- function(file_path) {
  json_data <- fromJSON(file_path)
  return(data.table(json_data, keep.rownames = FALSE))
}

# Load all json files and convert to data tables
dt_list <- lapply(paste0("video_details_", 1:9, ".json"), load_json_as_dt)

# Combine all data tables into one
json_combined <- rbindlist(dt_list)

# Write the combined data to a new json file, with pretty printing
write_json(json_combined, "video_details.json", pretty = TRUE, auto_unbox = TRUE)
