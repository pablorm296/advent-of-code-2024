library(tidyverse)

# Part 1 ======================================================================

# Read the input file
lines <- read_lines("./day-02/input/1.txt")

# Parse the input file
# Each line should be converted to a vector of umbers. Each number is separated by
# a white space
parsed_lines <- str_split(lines, "\\s+")

#' Function to get the distance between the numbers of a vector
#' 
get_distances <- function(vector) {
    distances <- imap_dbl(
        vector,
        function(x, idx) {
            distance <- abs(x - vector[idx + 1])
            return(distance)
        }
    )
    return(distances)
}

#' Function to check if a report is safe
#' 
check_report <- function(vector) {
    # Coerce to number
    vector <- as.numeric(vector)

    ordered_asc <- sort(vector)
    ordered_des <- sort(vector, decreasing = TRUE)

    is_inc_or_dec <- all(vector == ordered_asc) || all(vector == ordered_des)

    if (!is_inc_or_dec) {
        return("unsafe")
    }

    # Get the distances
    distances <- get_distances(vector)

    # Check if any distance is smaller than 0 or bigger than 3
    invalid_distances <- (distances < 1) | (distances > 3)

    if (any(invalid_distances, na.rm = TRUE)) {
        return("unsafe")
    }

    return("safe")

}

map_chr(parsed_lines, check_report) |>
    table()
