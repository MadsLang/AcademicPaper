
theme_mls <- function(){
  my_theme <- theme_light() +
    theme(
      text = element_text(size=10, family="LM Roman 10"),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
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

fixl <- function(x){
  return(gsub("-", "\u00ad", x, fixed=TRUE))
}

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

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


icc <- function(model){
  # A small function to calculate ICC from a lme4 model.
  # Dependencies: broom.mixed, dplyr, lme4

  icc <- data.frame(broom.mixed::tidy(model, effects=c("ran_pars"))) %>%
    mutate(variance = estimate^2) %>%
    mutate(icc = variance / sum(variance)) %>%
    filter(term == "sd__(Intercept)")
  return(icc$icc)
}


std <- function(x) {
  # Merlin's function to z-standardize

  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

min_max_normalize <- function(m){
  (m - min(m))/(max(m)-min(m))
}

