#' Function for reading lines of a text file
#' 
read_lines <- function(path, n = -1) {
    con <- file(path, "r")
    lines <- readLines(con, n)
    close(con)

    return(lines)
}
