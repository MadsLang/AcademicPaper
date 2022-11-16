

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


hetero_plot <- function(){
  p <- ggplot() +
    geom_jitter(aes(x = model$fitted.values, y = model$residuals), width = 0.2, alpha=0.7, color="#242c44") +
    geom_hline(yintercept = 0, color = 'black', linetype='dashed') +
    xlab("Fitted Values") +
    ylab("Residuals") +
    theme_mls()

  temp <- broom::tidy(lmtest::bptest(model, studentize=FALSE)) %>%
    mutate(across(where(is.numeric), round, 3)) %>%
    relocate(Method = method) %>%
    rename(
      Statistic = statistic,
      `P-value` = p.value,
      Parameter = parameter
    )

  tt <- ttheme_minimal(
    core=list(fg_params=list(fontfamily="LM Roman 10", fontsize=10, hjust=0, x=0.1)),
    colhead=list(fg_params=list(fontfamily="LM Roman 10", fontsize=10, hjust=0, x=0.1)),
    rowhead=list(fg_params=list(fontfamily="LM Roman 10", fontsize=10, hjust=0))
  )
  tbl <- tableGrob(temp, rows=NULL, theme=tt)

  grid.arrange(p, tbl,
               nrow = 2, heights = c(2, 0.3))
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

control_vars_plot <- function(variable, label){
  p <- ggplot(nilt, aes(x = variable, fill=variable)) +
    geom_bar() +
    theme_mls() +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(expand=c(0,0), limits=c(0,max(table(variable)) + 50)) +
    xlab(label) +
    ylab("Count")
  p
}


numeric_vars_plot <- function(variable, label){
  p <- ggplot(nilt, aes(x = variable, fill='1')) +
    geom_density() +
    geom_segment(aes(x=mean(variable), y=0, xend=mean(variable), yend=Inf), color="#000000", linetype='dashed') +
    annotate(geom='label', x = mean(variable), y = Inf, hjust = 0, vjust = 1, label.size = NA, fill=NA, label='Mean', family = "LM Roman 10", size=3) +
    theme_mls() +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      plot.caption = element_markdown(hjust = 0)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, .05)))
    labs(
      x=label,
      y="Density",
      caption="*Note*: <br> To estimate density, there is used kernel density estimation (KDE)."
    )
  p
}




