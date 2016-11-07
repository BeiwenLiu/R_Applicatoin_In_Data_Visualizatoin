library(ggplot2)
library(stringr)
library(tm)
library(GGally)
library(reshape)
load("movies_merged")
# dimention of the data
dim(movies_merged)

# Question 1

colnames(movies_merged)
table(movies_merged$Type)
# we can see there are 64 game, 725 series and 40000 movies
movies_merged = subset(movies_merged, Type == "movie")
dim(movies_merged) # after subset, there are only 40000 movies left


# Question 2

class(movies_merged$Runtime) # character
head(sort(unique(movies_merged$Runtime)))#""1 h"        "1 h 1 min"  "1 h 10 min" "1 h 11 min" "1 h 12 min" "1 h 14 min""
tail(sort(unique(movies_merged$Runtime))) #"95 min" "96 min" "97 min" "98 min" "99 min" "N/A"   

# there are 4 type of Runtime, 1)"N/A", 2) "XX h xx min", 3) "xx min" 4)"xx h"

Runtime_num <- c()
for(i in seq_along(movies_merged$Runtime)){
  
  x <- strsplit(movies_merged$Runtime[i], ' ')[[1]]
  if (length(x) ==  2){ 
    if(x[2] == "min") #3) "xx min"
    {Runtime_num <- c(Runtime_num, suppressWarnings(as.numeric(x[1])))}
    # 4)"xx h"
    else{Runtime_num <- c(Runtime_num, suppressWarnings(as.numeric(x[1])) * 60)}
  }
  # 2) "XX h xx min"
  else if( length((x) == 4)) {
    Runtime_num <- c(Runtime_num, suppressWarnings(as.numeric(x[1])) * 60  + suppressWarnings(as.numeric(x[3])))
  }
  # 1)"N/A"
  else Runtime_num <- c(Runtime_num, NA)
}

# combine the row of Runtime with Year to a new dataframe
Runtime_Year_merged <- data.frame(Runtime = Runtime_num , movies_merged$Year)
colnames(Runtime_Year_merged) <- c("Runtime", "Year")

# remove all the NA columns of Runtime_Year_merged dataframe
Runtime_Year_merged <- Runtime_Year_merged[complete.cases(Runtime_Year_merged),]

ggplot(Runtime_Year_merged, aes(Runtime)) + geom_histogram() + ggtitle("distribution of Runtime of Movies") + coord_cartesian(xlim = c(0, 300))
ggplot(data = Runtime_Year_merged, aes(Year, Runtime)) + geom_point() + ggtitle("Runtime versus Years")

# combine the row of Runtime with Budget to a new dataframe
Runtime_Budget_merged <- data.frame(Runtime = Runtime_num , movies_merged$Budget)
colnames(Runtime_Budget_merged) <- c("Runtime", "Budget")

Runtime_Budget_merged <- Runtime_Budget_merged[complete.cases(Runtime_Budget_merged),]

ggplot(data = Runtime_Budget_merged, aes(Runtime, Budget)) + geom_point() + ggtitle("Runtime versus Budget")


# Question 3

# only work on the Genre and Gross columns of movies_merged 

Genre_Gross_merged <- movies_merged[, c("Genre", "Gross")]

sum((Genre_Gross_merged$Genre == "N/A")) ## there are 986 rows is na for Genre
sum(is.na(Genre_Gross_merged$Gross)) # but no NA rows 

## here we can see some rows is "N/A"
##we need to change to NA and then remove this rows
# convert Genre column's "N/A" to NA 
Genre_Gross_merged$Genre <- ifelse(Genre_Gross_merged$Genre == "N/A", NA, Genre_Gross_merged$Genre)
#Test to see if it is converted successfully 
sum(is.na(Genre_Gross_merged$Genre))


## remove the NA rows of genre column 
Genre_Gross_merged <- Genre_Gross_merged[complete.cases(Genre_Gross_merged[, 1]),]
dim(Genre_Gross_merged) # 39014 X 2

# replace "," with " " in Genre column to prepare string for dictionary construction
Genre_Gross_merged$Genre <- gsub(",", " ", Genre_Gross_merged$Genre)
# use tm library to convert Genre columns to binary 
corp <- Corpus(VectorSource(Genre_Gross_merged$Genre))


dtm <- DocumentTermMatrix(corp)
Genre_df <- data.frame(as.matrix(dtm))
# add Gross column to the new dataframe
Genre_df$Gross <- Genre_Gross_merged$Gross 
head(Genre_df)
# get the top 10 Genres
sorted_genre <- sort(colSums(Genre_df[, 1:28]), decreasing = TRUE)[1:10]
name_list <- names(sorted_genre)
# only subset the top 10 Genres and complete.cases
sub_Genre_df <- Genre_df[,c(names(sorted_genre), "Gross")]
dim(sub_Genre_df) ## 39014 X 11

sub_Genre_df <- sub_Genre_df[complete.cases(sub_Genre_df$Gross),] #4555 X 11

# nowe we can create a data.frame with one column for the top 10 Genres and another column for corresponding Gross revenue 
Genre_df_long <- data.frame(genre = character(), gross = integer())

for(i in 1:10) {
  sub <- sub_Genre_df[sub_Genre_df[, i] == 1, c(i, 11)]
  #print(dim(sub))
  sub[, 1] <- name_list[i]
  Genre_df_long <- rbind(as.matrix(Genre_df_long), as.matrix(sub))
}

dim(Genre_df_long) # dim =  8469 X 2
Genre_df_long <- as.data.frame(Genre_df_long)
Genre_df_long$gross <- as.numeric(as.character(Genre_df_long$gross))
ggplot(Genre_df_long, aes(factor(genre), log10(gross))) + geom_boxplot() + ggtitle("Gross Revenue For Top 10 Genres")  + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Question 4
# first investigate the data
head(movies_merged[, c("Released", "Year", "Gross")])
sum(is.na(movies_merged$Released)) ## there are 4949 NA in Released column   
sum(is.na(movies_merged$Year)) ## 0 NA in Year column
sum(is.na(movies_merged$Gross)) ## there are about 7/8 of NA in Gross column

# check if there are any mismatch between Year and Released
sub_movies <- movies_merged[,c("Released", "Year", "Gross")]
# extract the year information from Released column
sub_movies$Release_year <- as.numeric(substr(sub_movies$Released, 1, 4))
# then convert all the NA column of Release_year to the same value to avoid remove more rows
sub_movies$Release_year <- ifelse(is.na(sub_movies$Release_year), sub_movies$Year, sub_movies$Release_year)

# first look the first 10 rows by eye
head(sub_movies, 10) # we can see row #7 has different values
# then count how many rows have different released year and Year value
matchedIndex <- sub_movies$Year == sub_movies$Release_year
sum(matchedIndex) # 5727
# further keep the rows Gross is not na
matchedIndex <- ifelse(!is.na(sub_movies$Gross), TRUE, matchedIndex)
sum(matchedIndex) # 35043
sub_movies_merged <- movies_merged[matchedIndex, ]

# Question 5

sub_movies <- movies_merged[, c("Released", "Genre", "Gross")]

sum(is.na(movies_merged$Gross)) #there are 35442 NAs for Gross column
sub_movies <- sub_movies[complete.cases(sub_movies),]
dim(sub_movies) # only 4513 rows left
# extract the month from released date
sub_movies$Month <- as.factor(substr(sub_movies$Released, 6, 7))
#Then further remove the NA rows for genre column 

sub_movies$Genre <- ifelse(sub_movies$Genre == "N/A", NA, sub_movies$Genre)

sub_movies <- sub_movies[complete.cases(sub_movies),]
dim(sub_movies) # further remove 3 rows

sub_movies$Genre <- gsub(",", " ", sub_movies$Genre)
corp <- Corpus(VectorSource(sub_movies$Genre))
dtm <- DocumentTermMatrix(corp)
Genre_matrix <- as.matrix(dtm)

sub_movies_with_genre_one_hot <- data.frame(cbind(sub_movies,Genre_matrix))
dim(sub_movies_with_genre_one_hot)
p <- ggplot(sub_movies_with_genre_one_hot, aes(Month, log10(Gross)))
p + geom_boxplot() + ggtitle(" Gross revanue VS. Different Month ")

#Find the top 10 genres
name_list <- names(sort(colSums(sub_movies_with_genre_one_hot[, 5:29]), decreasing = TRUE)[1:10])

# only subset the corresponding columns
sub_movies_with_genre_one_hot <- sub_movies_with_genre_one_hot[, c(name_list, "Month", "Gross")]
# for each top 10 genre, graph its gross revenue for each month
for(i in 1:10) {
  sub <- sub_movies_with_genre_one_hot[sub_movies_with_genre_one_hot[, i] == 1, c(i, 11, 12)]
  ttl <- paste("Gross Revenue vesus month For:", name_list[i], sep = " ")
  p <- ggplot(sub, aes(Month,log10(Gross))) 
  print(p + geom_boxplot() + ggtitle(ttl))
}


# Question 6

cor(movies_merged[,c(16:17, 20, 22:25, 27:29)], use = "complete.obs")
rowIndex <-  complete.cases(movies_merged[,c(16:17, 20, 22, 27:28, 38)])
sub_rating <- movies_merged[rowIndex, c(16:17, 20, 22, 27:28, 38)]
# sample the data
set.seed(1)
sampleIndex <- sample(seq_along(sub_rating$Gross), size = 400, replace = FALSE)
sub_rating_sample <- sub_rating[sampleIndex,]
ggpairs(sub_rating_sample, columns = c(1, 4, 6 ,7),
        columnLabels = c("imdbR", "tomatoR", "tomatoUserR", "Gross"),
        title = "Correlation Between Ratings and Gross Revenue")

ggpairs(sub_rating_sample, columns = c(2, 3, 5 ,7),
        columnLabels = c("imdbV", "tomatoM", "tomatoUserM", "Gross"),
        title = "Correlation Between Votes, Meters and Gross Revenue")


# Question 7

awards_normination <- c()

for( i in seq_along(movies_merged$Awards)){
  if(movies_merged$Awards[i] == "N/A"){
    awards_normination <- c(awards_normination, 0)
  }else{
    x <- movies_merged$Awards[i]
    temp <- gregexpr("[0-9]+", x) # find the numbers with any number of digits
    # extract the awards and normination and sum them  
    total_awards_normination  <- sum(as.numeric(unlist(regmatches(x, temp))))
    
    awards_normination <- c(awards_normination, total_awards_normination)
  }
}

length(awards_normination[awards_normination > 6])
# by a simple test we can split the movie with awards and normination to 1:5 ratio 
movies_merged$zero_award <- 0
movies_merged$some_award <- 0
movies_merged$many_award <- 0
for( i in seq_along(awards_normination)){
  if(awards_normination[i] > 6){
    movies_merged$many_award[i] <- 1
  }else if(awards_normination[i] > 0 ){
    movies_merged$some_award[i] <- 1
  }else{
    movies_merged$zero_award[i] <- 1
  }
}

sub_award <- movies_merged[,c("many_award", "some_award", "zero_award", "Gross")] 
sub_award <- sub_award[complete.cases(sub_award),]
dim(sub_award)
sub_award$award <- NA 
for( i in seq_along(sub_award$many_award)){
  if(sub_award$many_award[i] == 1){
    sub_award$award[i] <- "many_award"
  }else if(sub_award$some_award[i] == 1){
    sub_award$award[i] <- "some_award"
  }else{
    sub_award$award[i] <- "zero_award"
  }
}
ggplot(sub_award, aes(factor(award), log10(Gross))) + 
  geom_boxplot() +
  ggtitle("Gross Revenue vesus Awards and Nominations")

# Question 8

# insight 1
gross_budget <- movies_merged[,c("Budget", "Gross")]
gross_budget <- gross_budget[complete.cases(gross_budget),]
ggplot(gross_budget, aes(log10(Budget), log10(Gross))) + geom_point() + ggtitle("Gross Vesus Budget")

# insight 2

gross_domestic_gross <- movies_merged[,c("Domestic_Gross", "Gross")]
gross_domestic_gross <-gross_domestic_gross[complete.cases(gross_domestic_gross),]
ggplot(gross_domestic_gross, aes(log10(Domestic_Gross), log10(Gross))) + geom_point() + ggtitle("Domestic Gross Vesus Gross")

# new insight 

Genre_df <- Genre_df[complete.cases(Genre_df),]

Genre_df$total_Genre <-rowSums(Genre_df[, 1:28])
ggplot(Genre_df, aes(factor(total_Genre), log10(Gross))) + geom_boxplot() + ggtitle("Total Genres vesus Gross Revenue") 




