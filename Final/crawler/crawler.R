# Load necessary libraries
library(httr)
library(jsonlite)

# Load the CSV file
csv_file <- "topSongsLyrics1960_2005.csv"
songs_data <- read.csv(csv_file)

# Extract the song names
song_names <- songs_data$song

# Set your API key and search query
api_key <- "YOUR_API_KEY"

# Create the API request URL
youtube_api <- "https://www.googleapis.com/youtube/v3/"
base_url <- paste(youtube_api, "search", sep="")

# Initialize an empty list to store video details
all_video_details <- list()

# Loop through each song name
for (song_name in song_names) {
  search_query <- song_name
  request_url <- paste(base_url, "?part=snippet&maxResults=1&q=", URLencode(search_query), "&key=", api_key, sep="")
  
  # Make the API request
  response <- GET(request_url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response
    content_data <- content(response, "text")
    parsed_data <- fromJSON(content_data)
    
    # Extract video IDs
    video_ids <- parsed_data$items$id$videoId
    
    # Loop through video IDs and get like counts
    for (video_id in video_ids) {
      if (!is.na(video_id)) {
        video_details_url <- paste(youtube_api, "videos?part=statistics&id=", video_id, "&key=", api_key, sep="")
        video_details_response <- GET(video_details_url)
        if (status_code(video_details_response) == 200) {
          video_details_content <- content(video_details_response, "text")
          video_details <- fromJSON(video_details_content)
          
          # Add the search query and video ID to each video detail
          video_details$search_query <- search_query
          video_details$video_id <- video_id
          
          # Add the video details to the list
          all_video_details[[length(all_video_details) + 1]] <- video_details
        }
      }
    }
  }
}

# Save the entire list of video details into a single JSON file
write_json(all_video_details, "all_video_details.json")