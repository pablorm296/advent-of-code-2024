library(tidyverse)

# Part 1 ======================================================================

# Read the input file
lines <- read_lines("./day-04/input/1.txt")

# Parse the input file as a matrix
letters_grid <- lines |> str_split("", simplify = TRUE)

log_hit <- function(row, col, direction) {
    message <- str_glue("Found in {row},{col}, going {direction}")
    print(message)
}

is_xmas <- function(vector) {
    as_string <- str_c(vector, collapse = "")
    return(as_string == "XMAS")
}

find_xmas <- function(row, col, matrix) {
    # If current position is not an x, just skip
    if (matrix[row, col] != "X") {
        return(0)
    }

    # Init a counter
    xmas_from_position <- 0

    can_go_up <- (row - 3) >= 1
    can_go_right <- (col + 3) <= ncol(matrix)
    can_go_down <- (row + 3) <= nrow(matrix)
    can_go_left <- (col - 3) >= 1

    # Up
    if (can_go_up) {
        vector <- matrix[row:(row - 3), col]
        if (is_xmas(vector)) {
            log_hit(row, col, "up")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    # Diagonal up right
    if (can_go_up && can_go_right) {
        matrix_subset <- matrix[(row - 3):row, col:(col + 3)]
        flipped_subset <- matrix_subset[nrow(matrix_subset):1, ]
        vector <- diag(flipped_subset)
        if (is_xmas(vector)) {
            log_hit(row, col, "up-right")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    # Right
    if (can_go_right) {
        vector <- matrix[row, col:(col + 3)]
        if (is_xmas(vector)) {
            log_hit(row, col, "right")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    # Diagonal down right
    if (can_go_down && can_go_right) {
        matrix_subset <- matrix[row:(row + 3), col:(col + 3)]
        vector <- diag(matrix_subset)
        if (is_xmas(vector)) {
            log_hit(row, col, "down right")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    # Down
    if (can_go_down) {
        vector <- matrix[row:(row + 3), col]
        if (is_xmas(vector)) {
            log_hit(row, col, "down")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    # Diagonal down left
    if (can_go_down && can_go_left) {
        matrix_subset <- matrix[row:(row + 3), (col - 3):col]
        flipped_subset <- matrix_subset[, ncol(matrix_subset):1]
        vector <- diag(flipped_subset)
        if (is_xmas(vector)) {
            log_hit(row, col, "down left")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    # Left
    if (can_go_left) {
        vector <- matrix[row, col:(col - 3)]
        if (is_xmas(vector)) {
            log_hit(row, col, "left")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    # Diagonal up left
    if (can_go_up && can_go_left) {
        matrix_subset <- matrix[(row - 3):row, (col - 3):col]
        vector <- diag(matrix_subset)
        vector <- vector[length(vector):1]
        if (is_xmas(vector)) {
            log_hit(row, col, "up left")
            xmas_from_position <- xmas_from_position + 1
        }
    }

    return(xmas_from_position)
}

hits <- 0

for (i in 1:nrow(letters_grid)) {
    for (j in 1:ncol(letters_grid)) {
        hits <- hits + find_xmas(i, j, letters_grid)
    }
}

# Part 2 ======================================================================

is_mas <- function(vector) {
    string <- str_c(vector, collapse = "")
    return(string == "MAS" || string == "SAM")
}

find_x_mas <- function(row, col, matrix) {
    # If current position is not an m, just skip
    if (!(matrix[row, col] %in% c("M", "S"))) {
        return(FALSE)
    }

    can_go_right <- (col + 2) <= ncol(matrix)
    can_go_down <- (row + 2) <= nrow(matrix)

    if (!can_go_down || !can_go_right) {
        return(FALSE)
    }

    # From the current position, get a 3*3 matrix
    new_matrix <- matrix[row:(row + 2), col:(col + 2)]

    # Get main diagonal
    main_diag <- diag(new_matrix)

    # Get the other diagonal. For this, we need to flip the matrix
    flipped_matrix <- new_matrix[,ncol(new_matrix):1]
    other_diag <- diag(flipped_matrix)

    is_x_mas <- is_mas(main_diag) && is_mas(other_diag)

    # Log hit
    if (is_x_mas) {
        log_hit(row, col, "down-right")
    }

    return(is_x_mas)

}

hits <- 0

for (i in 1:nrow(letters_grid)) {
    for (j in 1:ncol(letters_grid)) {
        hits <- hits + find_x_mas(i, j, letters_grid)
    }
}
