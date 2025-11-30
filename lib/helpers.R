library(readxl)
library(openxlsx)
library(dplyr)

# Save sightings into a new workbook with only the DATABASE sheet
save_sightings <- function(updated_df, file_path) {
  
  # Create a fresh workbook
  wb <- createWorkbook()
  
  # Add DATABASE sheet
  addWorksheet(wb, "DATABASE")
  
  # Write updated data
  writeData(wb, "DATABASE", updated_df)
  
  # Add autofilter to header row
  addFilter(wb, sheet = "DATABASE", row = 1, cols = 1:ncol(updated_df))
  
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  
  # Save workbook
  saveWorkbook(wb, file_path, overwrite = FALSE)
}
