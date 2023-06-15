

# factor_loadings_table <- function(loadings_df){
#
#   rownames(loadings_df) <- c(
#     "Number of shares",
#     "Number of comments",
#     "Link clicks",
#     "Number of Likes",
#     "Reactions (Love)",
#     "Reactions (Wow)",
#     "Reactions (Haha)",
#     "Reactions (Anger)",
#     "Reactions (Sorry)",
#     "Share of mention comments",
#     "Share of replies",
#     "Share of comment with multiple replies",
#     "Share of comments with question",
#     "Share of comments with media link",
#     "Mean tokens of comments",
#     "Mean LIX of comments",
#     "Mean likes of comments",
#     "Share of hidden comments",
#     "Share of comments with offensive language",
#     "Share of comments with hateful language",
#     "Share of comments with recognition"
#   )
#
#   knitr::kable(
#     loadings_df,
#     format = "latex",
#     booktabs = TRUE,
#     caption = "Factor Loadings for PCA",
#     linesep = ""
#   ) %>%
#     # add_header_above(.,c(' '=1,'Cleaned Sample'=2,'Full Sample'=2,' '=1),escape = FALSE) %>%
#     kableExtra::kable_styling(position = "center",latex_options = "HOLD_position")
# }





describe <- function(temp, label_list){
  out_table <- sumtable(
    temp,
    labels=label_list,
    numformat=c('comma'),
    out = 'return'
  )

  knitr::kable(
    out_table,
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "Descriptive statistics"
  ) %>%
    #add_header_above(.,c(' '=1,'All articles'=1,'Articles posted on FB'=1,' '=1),escape = FALSE) %>%
    kableExtra::kable_styling(
      position = "center",
      latex_options = "HOLD_position"
    )
}



shared_comparison_table <- function(){
  subset <- df %>%
    mutate(sample = "Posted on Facebook")
  table_df <- full_df %>%
    mutate(sample = "All") %>%
    bind_rows(subset) %>%
    select(
      timeliness, unexpectedness, proximity_geo, ner_ORG_MAIN_dummy,
      ner_PER_MAIN_dummy, hard_news_final, negativity_topic, negativity_pred,
      positivity_pred, breaking, sample
    ) %>%
    mutate_all(factor)


  out_table <- sumtable(
    table_df,
    labels = c(
      "Timeliness",
      "Unexpectedness",
      "Geographical proximity",
      "Cultural proximity",
      "Personalization",
      "Hard News",
      "Negativity (topic)",
      "Negativity (style)",
      "Positivity (style)",
      "Impact"
    ),
    group="sample",
    factor.counts = FALSE,
    group.test = TRUE,
    out = 'return'
  )

  out_table <- out_table[-1,]
  row.names(out_table) <- NULL

  out_table <- bind_cols(out_table[,1],out_table[,3],out_table[,5],out_table[,6])
  names(out_table) <- c(
    "Variable",
    "Article published on website",
    "Articles shared on Facebook",
    "X2-test"
  )

  knitr::kable(
    out_table,
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = "Descriptive statistics for all articles and only articles shared on Facebook"
  ) %>%
    #add_header_above(.,c('Variable'=1,'All articles'=1,'Articles posted on FB'=1,'Test'=1),escape = FALSE) %>%
    kableExtra::column_spec(2:3, width="3cm") %>%
    kableExtra::kable_styling(position = "center",latex_options = "HOLD_position") %>%
    footnote(
      general = "Statistical significance markers: * p<0.1; ** p<0.05; *** p<0.01",
      general_title = ""
    )
}



news_value_table <- function(){
  news_content <- readxl::read_xlsx("data/news_content_overblik.xlsx", sheet = "news_values_overview", n_max = 8) %>%
    replace(is.na(.), "") %>%
    select(-Year)
  return(
    knitr::kable(
      news_content[,1:5],
      format = "latex",
      booktabs = TRUE,
      linesep = "",
      caption = "Litterature overview of news value taxonomies"
    ) %>%
      kableExtra::kable_styling(
        position = "center",
        font_size = 7,
        latex_options = "scale_down"
      ),
    knitr::kable(
      bind_cols(news_content[,1], news_content[,6:10]),
      format = "latex",
      booktabs = TRUE,
      linesep = "",
      caption = "Litterature overview of news value taxonomies"
    ) %>%
      kableExtra::kable_styling(
        position = "center",
        font_size = 7,
        latex_options = "scale_down"
      ),
    knitr::kable(
      bind_cols(news_content[,1], news_content[,11:16]),
      format = "latex",
      booktabs = TRUE,
      linesep = "",
      caption = "Litterature overview of news value taxonomies"
    ) %>%
      kableExtra::kable_styling(
        position = "center",
        font_size = 7,
        latex_options = "scale_down"
      )
  )
}


