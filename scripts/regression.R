# run model



m1 <- lm(
  link_clicks ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)

m2 <- lm(
  total_reactions ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)


m3a <- lm(
  shares ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)

m3b <- lm(
  share_mention ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)

m4a <- lm(
  n_comments ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)

m4b <- lm(
  share_replies ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)

m5a <- lm(
  attack_share ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)

m5b <- lm(
  recognition_share ~ timeliness + unexpectedness + proximity_geo + ner_ORG_MAIN_dummy +
    ner_PER_MAIN_dummy + negativity_topic + negativity_pred +
    positivity_pred + breaking + spreadability + hard_news_final
  # control vars
  + created_time_PM + created_time_weekend + premium + new_strategy + election
  ,
  data = df
)




# Set name of variables
cov_labels <-c(
  "Intercept",
  "Timeliness",
  "Unexpectedness",
  "Geographical proximity",
  "Cultural proximity",
  "Personalization",
  "Negativity (topic, ref. neutral)",
  "Negativity (style, ref. neutral)",
  "Positivity (style, ref. neutral)",
  "Impact",
  "Spreadability",
  "Hard News (ref. soft news)",
  "Posted time PM (ref. AM)",
  "Posted time, weekend, (ref. weekday)",
  "Pay-wall article",
  "Strategy Change",
  "Election Period"
)

# # Print result
# stargazer(
#   m1,
#   m2,
#   m3,
#   m4,
#   m5,
#   m6,
#   m7,
#   se = estimatr::starprep(m1,m2,m3,m4,m5,m6,m7),
#   dep.var.labels = c(
#     "Information Seeking",
#     "Emotionality",
#     "Sharing, Self-expressive",
#     "Sharing, phatic",
#     "Reciprocity",
#     "(Dis)respect, offensive language",
#     "Respect, recognizing language"
#   ),
#   model.numbers=FALSE,
#   header=FALSE,
#   summary=TRUE,
#   title="Regression results",
#   covariate.labels = cov_labels,
#   font.size = "small",
#   no.space = TRUE,
#   column.sep.width = "3pt",
#   dep.var.caption  = "Dependent Variable",
#   table.placement = "H"
# )


# reg1 <- function(){
#   return(
#       stargazer(
#       m1,
#       se = estimatr::starprep(m1),
#       dep.var.labels = c(
#         "Link clicks"
#       ),
#       model.numbers=FALSE,
#       header=FALSE,
#       summary=TRUE,
#       title="Regression results",
#       covariate.labels = cov_labels,
#       digits = 0,
#       font.size = "footnotesize",
#       single.row = T,
#       dep.var.caption  = "Dependent Variable",
#       table.placement = "H"
#     )
#   )
# }


round_first_col <- function(latex_output){
  re_match_coef <- "\\d+\\.\\d+(?= \\\\;.+\\d+\\.\\d+ \\\\;)"
  #re_match_stderr <- "\\(\\d+\\.\\d+\\)(?=.+\\d+\\.\\d+ \\\\;)"

  for (match in str_match_all(latex_output, re_match_coef)[[1]]){
    latex_output <- str_replace(latex_output, match, as.character(round(as.numeric(match), 0)))
  }

  return(cat(latex_output))
}



reg1 <- function(){
  m1_out <- texreg::extract(m1)

  return(
    texreg(
      list(m1),
      include.ci = FALSE,
      custom.coef.names = cov_labels,
      custom.model.names = c(
        "Link clicks"
      ),
      digits = 0,
      font.size = "footnotesize",
      caption = "Linear regression model",
      single.row = TRUE,
      booktabs = TRUE,
      use.packages=FALSE,
      dcolumn = TRUE,
      groups = c(
        "\\textbf{Intercept}" = list(1:1),
        "\\textbf{Predictors}" = list(2:12),
        "\\textbf{Control variables}" = list(13:17)
      ),
      include.nobs = FALSE,
      include.rsquared = FALSE,
      include.adjrs = FALSE,
      custom.gof.rows = list(
        "Num. obs" = paste0(round(m1_out@gof[3], 0), ""),
        "R$^2$" = paste0(round(m1_out@gof[1], 2), ""),
        "Adj. R$^2$" = paste0(round(m1_out@gof[2], 2), "")
      ),
      custom.note = "***p < 0.001; **p < 0.01; *p < 0.05",
      table.placement = "H",
      float.pos = "H"
    )
  )
}



reg2 <- function(){
  m2_out <- texreg::extract(m2)

  return(
    texreg(
      list(m2),
      include.ci = FALSE,
      custom.coef.names = cov_labels,
      custom.model.names = c(
        "Number of reactions"
      ),
      digits = 0,
      font.size = "footnotesize",
      caption = "Linear regression model",
      single.row = TRUE,
      booktabs = TRUE,
      use.packages=FALSE,
      dcolumn = TRUE,
      groups = c(
        "\\textbf{Intercept}" = list(1:1),
        "\\textbf{Predictors}" = list(2:12),
        "\\textbf{Control variables}" = list(13:17)
      ),
      include.nobs = FALSE,
      include.rsquared = FALSE,
      include.adjrs = FALSE,
      custom.gof.rows = list(
        "Num. obs" = paste0(round(m2_out@gof[3], 0), ""),
        "R$^2$" = paste0(round(m2_out@gof[1], 2), ""),
        "Adj. R$^2$" = paste0(round(m2_out@gof[2], 2), "")
      ),
      custom.note = "***p < 0.001; **p < 0.01; *p < 0.05",
      table.placement = "H",
      float.pos = "H"
    )
  )
}






reg3 <- function(){
  m3a_out <- texreg::extract(m3a)
  m3b_out <- texreg::extract(m3b)

  return(
    round_first_col(
        texreg(
        list(m3a, m3b),
        include.ci = FALSE,
        custom.coef.names = cov_labels,
        custom.model.names = c(
          "Number of shares",
          "Prop. of mention comments"
        ),
        digits = 3,
        font.size = "footnotesize",
        caption = "Linear regression models",
        single.row = TRUE,
        booktabs = TRUE,
        use.packages=FALSE,
        dcolumn = TRUE,
        groups = c(
          "\\textbf{Intercept}" = list(1:1),
          "\\textbf{Predictors}" = list(2:12),
          "\\textbf{Control variables}" = list(13:17)
        ),
        include.nobs = FALSE,
        include.rsquared = FALSE,
        include.adjrs = FALSE,
        custom.gof.rows = list(
          "Num. obs" = c(
            paste0(round(m3a_out@gof[3], 0), ""),
            paste0(round(m3b_out@gof[3], 0), "")
          ),
          "R$^2$" = c(
            paste0(round(m3a_out@gof[1], 2), ""),
            paste0(round(m3b_out@gof[1], 2), "")
          ),
          "Adj. R$^2$" = c(
            paste0(round(m3a_out@gof[2], 2), ""),
            paste0(round(m3b_out@gof[2], 2), "")
          )
        ),
        custom.note = "***p < 0.001; **p < 0.01; *p < 0.05",
        table.placement = "H",
        float.pos = "H"
      )
    )
  )
}

reg4 <- function(){
  m4a_out <- texreg::extract(m4a)
  m4b_out <- texreg::extract(m4b)

  return(
    round_first_col(
      texreg(
        list(m4a, m4b),
        include.ci = FALSE,
        custom.coef.names = cov_labels,
        custom.model.names = c(
          "Number of comments",
          "Prop. of reply comments"
        ),
        digits = 3,
        font.size = "footnotesize",
        caption = "Linear regression models",
        single.row = TRUE,
        booktabs = TRUE,
        use.packages=FALSE,
        dcolumn = TRUE,
        groups = c(
          "\\textbf{Intercept}" = list(1:1),
          "\\textbf{Predictors}" = list(2:12),
          "\\textbf{Control variables}" = list(13:17)
        ),
        include.nobs = FALSE,
        include.rsquared = FALSE,
        include.adjrs = FALSE,
        custom.gof.rows = list(
          "Num. obs" = c(
            paste0(round(m4a_out@gof[3], 0), ""),
            paste0(round(m4b_out@gof[3], 0), "")
          ),
          "R$^2$" = c(
            paste0(round(m4a_out@gof[1], 2), ""),
            paste0(round(m4b_out@gof[1], 2), "")
          ),
          "Adj. R$^2$" = c(
            paste0(round(m4a_out@gof[2], 2), ""),
            paste0(round(m4b_out@gof[2], 2), "")
          )
        ),
        custom.note = "***p < 0.001; **p < 0.01; *p < 0.05",
        table.placement = "H",
        float.pos = "H"
      )
    )
  )
}


reg5 <- function(){
  m5a_out <- texreg::extract(m5a)
  m5b_out <- texreg::extract(m5b)

  return(
    texreg(
      list(m5a, m5b),
      include.ci = FALSE,
      custom.coef.names = cov_labels,
      custom.model.names = c(
        "Prop. of offensive comments",
        "Prop. of recognizing comments"
      ),
      digits = 3,
      font.size = "footnotesize",
      caption = "Linear regression models",
      single.row = TRUE,
      booktabs = TRUE,
      use.packages=FALSE,
      dcolumn = TRUE,
      groups = c(
        "\\textbf{Intercept}" = list(1:1),
        "\\textbf{Predictors}" = list(2:12),
        "\\textbf{Control variables}" = list(13:17)
      ),
      include.nobs = FALSE,
      include.rsquared = FALSE,
      include.adjrs = FALSE,
      custom.gof.rows = list(
        "Num. obs" = c(
          paste0(round(m5a_out@gof[3], 0), ""),
          paste0(round(m5b_out@gof[3], 0), "")
        ),
        "R$^2$" = c(
          paste0(round(m5a_out@gof[1], 2), ""),
          paste0(round(m5b_out@gof[1], 2), "")
        ),
        "Adj. R$^2$" = c(
          paste0(round(m5a_out@gof[2], 2), ""),
          paste0(round(m5b_out@gof[2], 2), "")
        )
      ),
      custom.note = "***p < 0.001; **p < 0.01; *p < 0.05",
      table.placement = "H",
      float.pos = "H"
    )
  )
}





### Plot functions

# make_coefficient_df <- function(model){
#   tidy_model_df <- broom::tidy(eval(model)) %>%
#     filter(term != '(Intercept)') %>%
#     mutate(labels = cov_labels) %>%
#     mutate(var = names(eval(model)$model)[1]) %>%
#     mutate(zero = 0.0)
#   return(tidy_model_df)
# }
#
# coefficient_plot_collected <- function(){
#   model_list <- list(m1,m2,m3,m4)
#
#   plot_list <- lapply(
#     model_list,
#     function(m){make_coefficient_df(m)}
#   )
#
#   temp <- bind_rows(plot_list)
#
#   p <- ggplot(temp, aes(x=forcats::fct_rev(forcats::fct_inorder(labels)), y=estimate)) +
#     geom_hline(aes(yintercept=zero), color=single_color, alpha=0.5) +
#     geom_errorbar(
#       aes(ymin=estimate-std.error, ymax=estimate+std.error),
#       width=0.3,
#       position=position_dodge(.9),
#       alpha=0.5
#     ) +
#     geom_point() +
#     scale_y_continuous(limits = c(-0.2, 0.2)) +
#     labs(
#       x = "",
#       y = ""
#     ) +
#     coord_flip() +
#     theme_mls() +
#     theme(
#       strip.background = element_rect(fill="transparent"),
#       strip.text = element_text(colour = 'black'),
#       panel.spacing = unit(0.5, "cm")
#     ) +
#     facet_wrap(vars(var),strip.position="top", ncol = 4)
#
#   return(
#     annotate_figure(
#       p,
#       bottom = text_grob(
#         "Note: For all models, the intercept is excluded from the visualization. \n Error bars represents the standard error of the estimate. ",
#         hjust = 1, x = 1, size=8, family = "LM Roman 10")
#     )
#   )
# }



