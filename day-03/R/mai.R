library(tidyverse)

# Part 1 ======================================================================

# Read the input file
lines <- read_lines("./day-03/input/1.txt")

# Concatenate the lines into a single character string
corrupted_memory <- str_c(lines, collapse = "")

mul_instructions <- str_extract_all(corrupted_memory, "mul\\(\\d{1,3},\\d{1,3}\\)", simplify = TRUE) |>
    as.character()

eval_mul_instruction <- function(string) {
    is_valid <- str_detect(string, "mul\\(\\d{1,3},\\d{1,3}\\)")
    if (!is_valid) stop("Invalid mul instruction")

    numbers <- str_extract_all(string, "\\d{1,3}", simplify = TRUE) |> as.numeric()

    result <- prod(numbers)

    return(result)
}

mul_results <- map_int(mul_instructions, eval_mul_instruction)

sum(mul_results)
