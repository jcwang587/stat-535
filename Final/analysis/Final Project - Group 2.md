# Final Project - Group 2

[Top US Songs from 1950 to 2019, w. lyrics](https://www.kaggle.com/datasets/stefancomanita/top-us-songs-from-1950-to-2019-w-lyrics)

Top 10 songs from the US for each year starting 1950 until 2019. The dataset includes the year the song made the chart, it's rank in the chart, the artist(s), the song name and full lyrics.

## Data Collection

The file is in CSV format, it contains 5 columns and 700 rows (10 top songs per year, for 70 years), the columns are:

- the first column is the year the song was included in the chart
- the second column is the rank the song got (this is nor normalized)
- the third column is the name of the artist(s) performing the song (this is not normalized)
- the forth column is the name/title of the song
- the fifth and last column are the lyrics of the song, a pipe character (|) denotes a new line

In addition to the provided data, we will collect `like` and `view` counts from YouTube using the web-scraping method.

## Visualization of Datasets, Analysis
- Utilize R visualization functions to present dataset variables, such as `like` and `view` numbers, for each year.
- Employ relevant course material, including packages, functions, and numerical/statistical schemes for optimization, sampling, and fitting, as necessary. 
- For example, we can identify some common words in the lyrics and designate them as keywords. Our objective will be to categorize songs based on these keywords derived from the lyrics. Furthermore, we will establish fitting models to analyze the variables previously visualized.
- Our analysis will also incorporate methods with Monte Carlo and bootstrapping.
