

outcome_var_plot <- function(){
  temp <- nilt %>%
    mutate(rsex = as.character(rsex)) %>%
    group_by(rsex) %>%
    mutate(
      m = mean(persinc2, na.rm=T),
      med = median(persinc2, na.rm=T)
    ) %>%
    ungroup()

  p <- ggplot(temp, aes(x = rsex, y = persinc2, color=rsex)) +
    geom_jitter(width = 0.2, alpha=0.7, show.legend = F) +
    geom_errorbar(aes(ymax=m, ymin=m, fill = "Mean"), width= 0.5, color="#000000", show.legend = T) +
    geom_errorbar(aes(ymax=med, ymin=med, fill = "Median"), width= 0.5, color="#000000", linetype='dashed', show.legend = T) +
    guides(color = "none", fill = guide_legend(override.aes = list(linetype = c("solid", "dashed")))) +
    labs(
      fill='',
      x='Sex: Female (ref.: Male)',
      y="Annual Personal Income (GBP)",
      caption="*Note*: <br> The points are jittered on the x-axis for the purpose of visualization. <br> There are only two categories for this question."
    ) +
    theme_mls() + theme(
      legend.position = "bottom",
      plot.caption = element_markdown(hjust = 0)
    )
  p
}



sampling_table_categorical <- function(){
  subset <- nilt %>% mutate(dataset = "Cleaned Sample")
  full <- nilt_full %>% mutate(dataset = "Full Sample")
  temp <- bind_rows(subset, full)

  # Categorial cariables
  out_table <- sumtable(
    temp,
    vars = c("rsex","religcat","orient","uninatid","tunionsa","rsuper"),
    labels = c(
      'Sex',
      'Religion',
      'Sexual Orientation',
      'Constitutional View',
      'Trade union membership',
      'Supervisor: No'
    ),
    group="dataset",
    group.test = TRUE,
    out = 'return'
  )
  out_table <- out_table[-1,]
  row.names(out_table) <- NULL

  knitr::kable(
    out_table,
    format = "latex",
    booktabs = TRUE,
    caption = "Descriptive Statistics for Cleaned and Full Sample (Categorical Variables)"
  ) %>%
    add_header_above(.,c(' '=1,'Cleaned Sample'=2,'Full Sample'=2,' '=1),escape = FALSE) %>%
    kableExtra::kable_styling(position = "center",latex_options = "HOLD_position") %>%
    footnote(general = "Statistical significance markers: * p<0.1; ** p<0.05; *** p<0.01")
}


sampling_table_numerical <- function(){
  subset <- nilt %>% mutate(dataset = "Cleaned Sample")
  full <- nilt_full %>% mutate(dataset = "Full Sample")
  temp <- bind_rows(subset, full)

  # Numerical variables
  out_table <- sumtable(
    temp,
    vars = c("persinc2","rage"),
    labels = c(
      'Annual Personal Income (GBP)',
      'Age'
    ),
    group="dataset",
    group.test = TRUE,
    out = 'return'
  )
  out_table <- out_table[-1,]
  row.names(out_table) <- NULL

  knitr::kable(
    out_table,
    format = "latex",
    booktabs = TRUE,
    caption = "Descriptive Statistics for Cleaned and Full Sample (Numerical Variables)"
  ) %>%
    add_header_above(.,c(' '=1,'Cleaned Sample'=3,'Full Sample'=3,' '=1),escape = FALSE) %>%
    kableExtra::kable_styling(position = "center",latex_options = "HOLD_position") %>%
    footnote(general = "Statistical significance markers: * p<0.1; ** p<0.05; *** p<0.01")
}