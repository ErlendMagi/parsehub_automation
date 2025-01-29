# Install required packages if not already installed
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("readr")) install.packages("readr")
if (!require("googlesheets4")) install.packages("googlesheets4")

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(googlesheets4)

# Set your ParseHub API key
api_key <- "tObtsPXQiYxs"  # Replace with your actual API key

# Define project and run tokens
project_tokens <- list(
  project1 = "tmone55bhgZS",  # First project token
  project2 = "tNuGeq31YEin"   # Second project token
)

run_tokens <- list(
  project1 = "tYnAGT2c8WWi",  # Run token for the first project
  project2 = "tj1PFBwp5iTT"   # Run token for the second project
)

# Define file paths
csv_file_path <- "C:/Users/erlen/Downloads/daily_summary_data.csv"  # Update with your preferred location
sheet_url <- "https://docs.google.com/spreadsheets/d/1V_5pWBAPcTxHSYkFSHoVv_UVoN9dHcUcxlWRRsPCqEQ/edit#gid=0"  # Your actual Google Sheet link

library(jsonlite)
library(googlesheets4)

# Detect if running inside GitHub Actions
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  # Use service account authentication in GitHub Actions
  service_account_json <- Sys.getenv("GOOGLE_SHEETS_CREDENTIALS")
  writeLines(service_account_json, "gs4_auth.json")
  gs4_auth(path = "gs4_auth.json")
} else {
  # Use normal interactive authentication for local testing
  gs4_auth()
}



# Helper function to handle API responses
handle_response <- function(response) {
  if (status_code(response) == 200) {
    return(fromJSON(content(response, "text"), flatten = TRUE))
  } else {
    stop(paste("API Request failed with status:", status_code(response)))
  }
}

# Get Data for a Specific Run
get_run_data <- function(api_key, run_token) {
  url <- paste0("https://www.parsehub.com/api/v2/runs/", run_token, "/data?api_key=", api_key, "&format=json")
  response <- GET(url)
  handle_response(response)
}

### **PROCESS PROJECT 1 DATA** ###
process_project1_data <- function(data) {
  df <- data.frame(
    Currency = names(data),
    Value = unlist(data),
    stringsAsFactors = FALSE
  )
  
  df <- df %>%
    mutate(
      NumericValue = str_replace_all(Value, "[^0-9,]", ""),
      NumericValue = str_replace_all(NumericValue, " ", ""),
      NumericValue = str_replace(NumericValue, ",", "."),
      NumericValue = as.numeric(NumericValue),
      Unit = "NOK"
    ) %>%
    select(Currency, NumericValue, Unit)
  
  return(df)
}

### **PROCESS PROJECT 2 DATA** ###
process_project2_data <- function(data_list) {
  valid_data_frames <- data_list[!sapply(data_list, is.null)]
  
  combined_df <- bind_rows(valid_data_frames, .id = "Data_Set")
  
  combined_df <- combined_df %>%
    mutate(
      Currency = str_extract(omsetning, "[A-Z]{3}"),        
      Omsetning_Numeric = str_replace_all(omsetning, "[^0-9,]", ""),
      Omsetning_Numeric = str_replace_all(Omsetning_Numeric, " ", ""),
      Omsetning_Numeric = str_replace(Omsetning_Numeric, ",", "."),
      Omsetning_Numeric = as.numeric(Omsetning_Numeric),
      gearing = as.numeric(str_replace(gearing, ",", "."))
    ) %>%
    select(Data_Set, retning, gearing, Omsetning_Numeric, Currency)
  
  return(combined_df)
}

# Fetch and Process Project 2 Data
fetch_and_process_project2 <- function(raw_data) {
  data_list <- raw_data$list1$data
  cleaned_data <- process_project2_data(data_list)
  return(cleaned_data)
}

# Fetch and Process Both Projects
fetch_and_process_projects <- function() {
  project1_data_raw <- get_run_data(api_key, run_tokens$project1)
  project1_data <- process_project1_data(project1_data_raw)
  
  project2_data_raw <- get_run_data(api_key, run_tokens$project2)
  project2_data <- fetch_and_process_project2(project2_data_raw)
  
  list(project1 = project1_data, project2 = project2_data)
}

# Fetch and process the projects
projects_data <- fetch_and_process_projects()

# Assign to separate variables
project1_data <- projects_data$project1
project2_data <- projects_data$project2

### **JOIN DATA, CONVERT TO NOK, AND APPLY GEARING MULTIPLIER** ###
project2_data <- project2_data %>%
  left_join(project1_data, by = "Currency") %>%
  mutate(
    NumericValue = ifelse(is.na(NumericValue), 1, NumericValue),
    Omsetning_NOK = Omsetning_Numeric * NumericValue,
    Leveraged_Turnover_NOK = Omsetning_NOK * gearing
  ) %>%
  filter(Omsetning_Numeric > 0) %>%
  select(Data_Set, retning, gearing, Omsetning_Numeric, Currency, Omsetning_NOK, Leveraged_Turnover_NOK)

### **SUMMARIZE LONG & SHORT LEVERAGED TURNOVER WITH DATE & DAY OF WEEK** ###
current_date <- Sys.Date()
day_of_week <- weekdays(current_date)  # Get the day name (e.g., "Monday")

summary_data <- project2_data %>%
  group_by(retning) %>%
  summarise(Total_Leveraged_NOK = sum(Leveraged_Turnover_NOK, na.rm = TRUE)) %>%
  pivot_wider(names_from = retning, values_from = Total_Leveraged_NOK, values_fill = 0) %>%
  mutate(
    Net_Exposure_NOK = Long - Short,
    Date = current_date,
    Day_of_Week = day_of_week
  ) %>%
  select(Date, Day_of_Week, Long, Short, Net_Exposure_NOK) %>%
  rename(Total_Long_NOK = Long, Total_Short_NOK = Short)

### **APPEND TO CSV (KEEPING PREVIOUS DATA)** ###
if (file.exists(csv_file_path)) {
  existing_data <- read_csv(csv_file_path, show_col_types = FALSE)
  updated_data <- bind_rows(existing_data, summary_data)
  write_csv(updated_data, csv_file_path)
} else {
  write_csv(summary_data, csv_file_path)
}

### **UPLOAD TO GOOGLE SHEETS (APPEND MODE)** ###
if (gs4_has_token()) {
  existing_data <- tryCatch(read_sheet(sheet_url, sheet = "Summary"), error = function(e) NULL)
  updated_data <- if (!is.null(existing_data)) bind_rows(existing_data, summary_data) else summary_data
  sheet_write(updated_data, sheet_url, sheet = "Summary")
  message("✅ `summary_data` uploaded to Google Sheets!")
} else {
  message("❌ Google authentication failed. Run `gs4_auth()` again.")
}

# Display data
print(summary_data)
View(summary_data)
