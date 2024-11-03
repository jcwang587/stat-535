library(dplyr)
library(stringr)
data <- read.csv("topSongsLyrics1960_2005.csv")
#this is a csv file containing frequency of words in a extremely large linguistic corpus; the corpus of contemporary american english
COCA <- read.csv("freqs_coca.csv",col.names = c("key","freq_corpus"))
#some lyris column's lyrics contain weird string remove them
data$lyrics <- str_replace_all(data$lyrics,"You might also like2","")
data$lyrics <- str_replace_all(data$lyrics,"You might also like","")
#create a column that is free of line segmentation
data$clean_seg_lyrics <- str_replace_all(data$lyrics,"\\|"," ")
#remove punctuation so as to find unique words ever occur
data$no_punct <- str_replace_all(data$clean_seg_lyrics,"[,)()?.—!;]","") %>% str_replace_all("\\[","") %>% str_replace_all("\\]","") %>% str_replace_all("\\{","") %>% str_replace_all("\\}","") %>% str_replace_all("\\*","_")
#split the whole lyrics cell into words based on white space
split_lyrics <- str_split(data$no_punct," ")
#found another weird cell of a song, remove it manually
data <- data[-3,]
#redo lyrics-split after removing that row
split_lyrics <- str_split(data$no_punct," ")
#create a empty dictionary to store every word
dictionary <- as.character(vector())
#iterate through every song and store every word
for(i in 1:length(split_lyrics)){
  #the original dataset is not perfectly clean
  #this line is to remove werid numbers in the very end of each lyric cell
  split_lyrics[[i]][length(split_lyrics[[i]])] <- str_replace_all(split_lyrics[[i]][length(split_lyrics[[i]])],"[0123456789]","")
  #we don't need to store repeated words
  dictionary <- c(dictionary,unique(split_lyrics[[i]]))
}
#we don't care about upper vs lower cases
dictionary_lower <- unique(tolower(dictionary))
data$clean_seg_lyrics_lower <- tolower(data$clean_seg_lyrics)
#create a data.frame that would have each unique word as an entry and later on add how many songs they have appeared in
DICT <- data.frame(key=dictionary_lower,times=as.numeric(NA),percentage=as.numeric(NA))
#iterate through every unique word
for(i in 1:nrow(DICT)){
  #check for each song whether word_i appears using grepl, sum the true false for all 448 songs
  #the word can appear in any position in the song
  DICT$times[i] <- sum(grepl(paste0(" ",DICT$key[i], " "),data$clean_seg_lyrics_lower)|grepl(paste0("^",DICT$key[i], " "),data$clean_seg_lyrics_lower)|grepl(paste0(" ",DICT$key[i], "$"),data$clean_seg_lyrics_lower))
  #how many percentage of all the 448 songs does this word appear in
  DICT$perentage[i] <- DICT$times[i]/448
  #this process takes about 15 mins, so just print out the progress
  if(i%in%seq(1000,10000,1000)){
    print(i)
  }
}
DICT <- left_join(DICT,COCA) %>% arrange(desc(percentage))
plot(DICT$percentage,log(DICT$freq_corpus), xlab="percentage of songs the word appear in", ylab="the word's base frequency (obtained from COCA corpus)")
#high frequent words occur in more songs, which is not by itself interesting

head(DICT,10) #for instance, the top 5 common words in the 448 songs are 
#the, and, to, I, you

remove_veryfreq_words<-DICT[log(DICT$freq_corpus)<=11,] %>% arrange(desc(percentage))
head(remove_veryfreq_words,10)
#now it's more interesting, we see words like
#love, feel, yeah, give, baby being most common
saveRDS(DICT,"dictionary_for_topsongs.rds")
