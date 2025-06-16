getwd()
setwd("C:/Users/bianc/Downloads/murakami dataset/data")
#run twice on vscode for it to change the directory / retrieve the directory before inputting commmands

# LOADING THE DATA
data1 <- read.csv("c:/Users/bianc/Downloads/murakami dataset/data/rawmurakami.csv", stringsAsFactors = FALSE)
data2 <- read.csv("c:/Users/bianc/Downloads/murakami dataset/data/ratingsreviewsmur.csv")

# EXPLORING DATA da
# called data1 & data2 in the terminal

# ADDING (APPENDING) RATINGS & REVIEWS FROM GOODREADS
# these were manually entered by me onto a separete csv file
# the ratings and reviews are in the same order as what is presented in the raw dataset
data3 <- cbind(data1, data2)
df1 <- data.frame(data3)

# REMOVE DUPLICATE VALUES (DISREGARDING BNB IDS, ISBNS AND PUBLISHERS)
df2 <- df1[!duplicated(df1$book), ]
# View(df2) - Check if any duplicates were missed & manually removed
#2, 18, 48, 55, 59, 76, 88, 91, 93
df3 <- df2[!df2$book %in% c(
    "1Q84", 
    "Wind/Pinball : two early novels", 
    "Blind willow, sleeping woman : twenty-four stories",
    "Two novels",
    "After the quake : stories",
    "The sputnik sweetheart : a novel",
    "Dance dance dance : a novel",
    "Hard-boiled wonderland and the end of the world : a novel",
    "The hard-boiled wonderland and the end of the world"), ]

# FIXING MISSPELLING
df3$book[df3$book == "1Q84 : [a novel"] <- "1Q84."


# CREATING A NEW CSV FOR VISUALISATION
write.csv(df3, "altmurakami.csv", row.names = FALSE)
