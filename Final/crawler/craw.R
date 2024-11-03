# Load necessary libraries
library(httr)
library(jsonlite)

# Set your API key and search query
api_key <- "AIzaSyD4mso98FK1TUJjlOM2uYAcf1DqCJi2MIw"
search_query <- "R Programming"

# Create the API request URL
base_url <- "https://www.googleapis.com/youtube/v3/search"
request_url <- paste(base_url, "?part=snippet&maxResults=25&q=", URLencode(search_query), "&key=", api_key, sep="")

# Make the API request
response <- GET(request_url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON response
  content_data <- content(response, "text")
  parsed_data <- fromJSON(content_data)
  
  # Extract video information
  if ("items" %in% names(parsed_data) && nrow(parsed_data$items) > 0) {
    video_titles <- parsed_data$items$snippet$title
    print(video_titles)
  } else {
    print("No items found or items are not in the expected format.")
  }
} else {
  print(paste("Error in request: ", status_code(response)))
}
