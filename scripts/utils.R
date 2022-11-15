
theme_mls <- function(){
  my_theme <- theme_light()  %+%
    theme(
      text = element_text(size=10, family="LM Roman 10")
    )

  my_colors <- c("#242c44", "#e1287e", "#081693", "#f34f85", "#745399",
            "#848ca4")

  return(
    list(
      my_theme,
      scale_color_manual(values = my_colors),
      scale_fill_manual(values = my_colors)
    )
  )
}


library(stringr)
library(tidyverse)

RmdWords <- function(file) {

  # Creates a string of text
  file_string <- file %>%
    readLines() %>%
    paste0(collapse = " ") %>%
    # Remove YAML header
    str_replace_all("^<--- .*?--- ", "") %>%
    str_replace_all("^--- .*?--- ", "") %>%
    # Remove code
    str_replace_all("```.*?```", "") %>%
    str_replace_all("`.*?`", "") %>%
    # Remove LaTeX
    str_replace_all("[^\\\\]\\$\\$.*?[^\\\\]\\$\\$", "") %>%
    str_replace_all("[^\\\\]\\$.*?[^\\\\]\\$", "") %>%
    # Deletes text between tags
    str_replace_all("TC:ignore.*?TC:endignore", "") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("  ", "") %>%
    str_replace_all("<", "") %>%
    str_replace_all(">", "")

  # Save several different results
  word_count <- str_count(file_string, "\\S+")
  char_count <- str_replace_all(string = file_string, " ", "") %>% str_count()

  return(list(num_words = word_count, num_char = char_count, word_list = file_string))
}


