library(tidyverse)

# Part 1 ======================================================================

# Read the input file
lines <- read_lines("./day-01/input/1.txt")

# Parse the input file
# Each line (row) contains two columns of data separated by whitespaces
parsed_lines <- str_split(lines, pattern = "\\s+", simplify = T)

# Convert the parsed values to numbers
parsed_lines <- apply(parsed_lines, 2, FUN = as.numeric)

# Extract each column and sort it
col1 <- parsed_lines[,1] |> sort()
col2 <- parsed_lines[,2] |> sort()

# Get the distance between the numbers
distance <- abs(col1 - col2)

# Get the sum
sum(distance)

# Part 2 ======================================================================

#' Get the frequency of a number in a vector
#' 
get_number_frequency <- function(number, vector) {
    logical_vector <- number == vector
    sum <- sum(logical_vector)
    return(sum)
}

frequencies <- map_int(col1, ~ get_number_frequency(.x, col2))

similarity_score <- (frequencies * col1) |> sum()
similarity_score
