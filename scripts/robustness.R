




### Cook's D


cooksD_plot <- function(m){
  cooksD <- data.frame(cooks.distance(m1)) %>%
    mutate(rowname = row_number()) %>%
    arrange(cooks.distance.m1.)

  p <- ggplot(cooksD, aes(x = rowname, y = cooks.distance.m1.)) +
    geom_point(alpha=0.5, color=single_color) +
    geom_hline(yintercept = 4/nrow(df)) +
    scale_y_log10() +
    theme_mls()
  return(p)
}



### DFBETAS


dfbetas_plot <- function(m, plot_label){
  # Calculate the DFBETAs and turn into tibble
  DFBETAs <- dfbetas(m) %>%
    data.frame() %>%
    rownames_to_column() %>%
    as_tibble() %>%
    select(
      rowname,
      timeliness, unexpectedness, proximity_geo,
      ner_ORG_MAIN_dummy, ner_PER_MAIN_dummy,
      negativity_topic, negativity_pred, positivity_pred,
      breaking, spreadability, hard_news_final,
      words100
    ) %>%
    pivot_longer(
      cols = c(
        "timeliness",
        "unexpectedness",
        "proximity_geo",
        "ner_ORG_MAIN_dummy",
        "ner_PER_MAIN_dummy",
        "negativity_topic",
        "negativity_pred",
        "positivity_pred",
        "breaking",
        "spreadability",
        "hard_news_final",
        "words100",
      ), names_to = "variable"
    ) %>%
    mutate(variable = case_when(
      variable == "timeliness" ~ "Timeliness",
      variable == "unexpectedness" ~ "Unexpectedness",
      variable == "proximity_geo" ~ "Geographical proximity",
      variable =="ner_ORG_MAIN_dummy" ~ "Cultural proximity",
      variable == "ner_PER_MAIN_dummy" ~ "Personalization",
      variable == "negativity_topic" ~ "Negativity (topic)",
      variable == "negativity_pred" ~ "Negativity (predicted)",
      variable == "positivity_pred" ~ "Positivity (predicted)",
      variable == "breaking" ~ "Impact",
      variable == "spreadability" ~ "Spreadability",
      variable == "hard_news_final" ~ "Hard News (topic)",
      variable == "words100" ~ "Hard News (style)"
    )
    )


  y_labels = c(
    "Timeliness",
    "Unexpectedness",
    "Geographical proximity",
    "Cultural proximity",
    "Personalization",
    "Negativity (topic)",
    "Negativity (predicted)",
    "Positivity (predicted)",
    "Impact",
    "Spreadability",
    "Hard News (topic)",
    "Hard News (style)"
  )

  # Plot the results
  p <- ggplot(data = DFBETAs,
              aes(
                y = factor(variable, levels = rev(y_labels)),
                x = value)
  ) +
    geom_violin(fill = "gray", color="gray", alpha=0.3) +
    geom_point(position=position_jitter(width=0,height=0.1), size=1, color="black", alpha=0.5) +
    geom_vline(xintercept = 2/sqrt(nrow(df)), color = "#901A1E", linetype='dashed', alpha=0.5) +
    geom_vline(xintercept = -2/sqrt(nrow(df)), color = "#901A1E", linetype='dashed', alpha=0.5) +
    theme_mls() +
    labs(x = paste0("DFBETAs, ", plot_label), y = "Predictors")
  return(p)
}


dfbetas_plot(m1, "Number of reactions")



### Negative Binomial Regression with IRR

# m1_log <- mfx::negbinirr(
#   link_clicks ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
#     ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
#     positivity_pred + breaking + spreadability + hard_news_final + words100
#   # control vars
#   + created_time_PM + created_time_weekend + premium + new_strategy + election
#   ,
#   data = df
# )




