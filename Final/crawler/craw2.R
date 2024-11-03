# Load necessary libraries
library(httr)
library(jsonlite)

# Set your API key and search query
api_key <- ""
search_query <- "R Programming"

# Create the API request URL
base_url <- "https://www.googleapis.com/youtube/v3/search"
request_url <- paste(base_url, "?part=snippet&maxResults=1&q=", URLencode(search_query), "&key=", api_key, sep="")

# Make the API request
response <- GET(request_url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON response
  content_data <- content(response, "text")
  parsed_data <- fromJSON(content_data)
  
  # Extract video IDs - directly accessing the column
  video_ids <- parsed_data$items$id$videoId
  
  # Initialize vector to store like counts
  like_counts <- numeric(length(video_ids))
  
  # Loop through video IDs and get like counts
  for (i in seq_along(video_ids)) {
    if (!is.na(video_ids[i])) {
      video_details_url <- paste("https://www.googleapis.com/youtube/v3/videos?part=statistics&id=", video_ids[i], "&key=", api_key, sep="")
      video_details_response <- GET(video_details_url)
      if (status_code(video_details_response) == 200) {
        video_details_content <- content(video_details_response, "text")
        video_details <- fromJSON(video_details_content)
        
        # Debug: Print the structure of video_details
        print(str(video_details))
        
        # Extract like counts based on the correct structure
        like_counts[i] <- video_details$items$statistics$likeCount
      }
    }
  }
  
  # Combine titles and like counts
  video_titles <- parsed_data$items$snippet$title
  video_info <- data.frame(Title = video_titles, Likes = like_counts, stringsAsFactors = FALSE)
  print(video_info)
} else {
  print(paste("Error in request: ", status_code(response)))
  }