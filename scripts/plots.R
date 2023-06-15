
#
# engagement_per_hour <- function(df){
#   temp1 <- df %>%
#     mutate(created_time_hour = format(as.POSIXct(created_time), format = "%H")) %>%
#     group_by(created_time_hour) %>%
#     summarise(
#       count = sum(post_impressions) / n()
#     ) %>%
#     mutate(label = "Impressions per post")
#
#   temp2 <- df %>%
#     mutate(created_time_hour = format(as.POSIXct(created_time), format = "%H")) %>%
#     group_by(created_time_hour) %>%
#     summarise(
#       count = sum(link_clicks) / n()
#     ) %>%
#     mutate(label = "Link Clicks per post")
#
#   temp3 <- df %>%
#     mutate(created_time_hour = format(as.POSIXct(created_time), format = "%H")) %>%
#     group_by(created_time_hour) %>%
#     summarise(
#       count = sum(n_comments) / n()
#     ) %>%
#     mutate(label = "Number of comments per post")
#
#   temp <- bind_rows(temp1, temp2, temp3)
#
#   temp4 <- df %>%
#     mutate(created_time_hour = format(as.POSIXct(created_time), format = "%H")) %>%
#     group_by(created_time_hour) %>%
#     summarise(
#       count =  n()
#     ) %>%
#     mutate(label = "Number of posts")
#
#   temp <- bind_rows(temp1, temp2, temp3, temp4)
#
#
#   p <- ggplot(temp, aes(x=created_time_hour, y=count, group=label)) +
#     geom_line() +
#     ylab("") +
#     xlab("") +
#     theme_mls() +
#     theme(
#       strip.background = element_rect(fill="transparent"),
#       strip.text = element_text(colour = 'black')
#     ) +
#     facet_wrap(vars(label), scales = "free", strip.position="top")
#   return(p)
# }
#
#
# engagement_per_weekday <- function(df){
#   temp1 <- df %>%
#     mutate(created_time_day = format(as.POSIXct(created_time), format = "%a")) %>%
#     group_by(created_time_day) %>%
#     summarise(
#       count = n()
#     ) %>%
#     mutate(label = "Number of posts on day")
#
#   temp2 <- df %>%
#     mutate(created_time_day = format(as.POSIXct(created_time), format = "%a")) %>%
#     group_by(created_time_day) %>%
#     summarise(
#       count = sum(post_impressions) / n()
#     ) %>%
#     mutate(label = "Impressions per posts on day")
#
#   temp <- bind_rows(temp1, temp2)
#
#   p <- ggplot(temp, aes(x=factor(created_time_day, levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")), y=count, group=label)) +
#     geom_line(stat = "identity") +
#     ylab("") +
#     xlab("") +
#     scale_y_continuous(expand = c(1,1)) +
#     theme_mls() +
#     theme(
#       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#       strip.background = element_rect(fill="transparent"),
#       strip.text = element_text(colour = 'black')
#     ) +
#     facet_wrap(vars(label), scales = "free", strip.position="top")
#   return(p)
# }


# parallel_analysis_plot <- function(){
#   plotdf <- data.frame(
#     result = result_pca$sdev,
#     par_analysis = paral_analysis$`0.95`
#   ) %>%
#     mutate(component = row_number()) %>%
#     gather(key, eigenvalue, -component) %>%
#     mutate(key = ifelse(key == "result", "PCA", "PA"))
#
#   p <- ggplot(plotdf, aes(x=component, y=eigenvalue, color=key)) +
#     geom_point() +
#     geom_line() +
#     xlab("Component") +
#     ylab("Eigenvalue") +
#     labs(color="") +
#     scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10), limits=c(1,10)) +
#     #scale_y_continuous(limits=c(0,3), expand=c(0,0)) +
#     theme_mls()
#   return(p)
# }



# single_component_jitterplot <- function(predictor, component, temp_df){
#   p <- ggplot(temp_df, aes(x = factor(eval(parse(text = predictor))), y = eval(parse(text = component)), color=factor(eval(parse(text = predictor))))) +
#     geom_jitter(width = 0.2, alpha=0.7, show.legend = F) +
#     geom_errorbar(aes(ymax=eval(parse(text = paste0(component,"_m"))), ymin=eval(parse(text = paste0(component,"_m"))), fill = "Mean"), width= 0.5, color="#000000", show.legend = T) +
#     guides(color = "none", fill = guide_legend(override.aes = list(linetype = c("dashed")))) +
#     labs(
#       fill='',
#       x=predictor,
#       y=component,
#     ) +
#     theme_mls()
#   return(p)
# }



# component_jitterplot <- function(predictor){
#   temp <- df %>%
#     group_by(factor(eval(parse(text = predictor)))) %>%
#     mutate(
#       PC1_m = mean(PC1, na.rm=T),
#       PC2_m = mean(PC2, na.rm=T),
#       PC3_m = mean(PC3, na.rm=T),
#       PC4_m = mean(PC4, na.rm=T)
#     ) %>%
#     ungroup()
#
#   var_list = c("PC1", "PC2", "PC3", "PC4")
#
#   plot_list <- lapply(
#     var_list,
#     function(var){single_component_jitterplot(predictor, var, temp)}
#   )
#
#   fig <- ggarrange(
#     plotlist = plot_list,
#     ncol = 2,
#     nrow = 2,
#     common.legend = TRUE,
#     legend="bottom"
#   )
#
#   return(
#       annotate_figure(
#       fig,
#       bottom = text_grob(
#         "Note: The points are jittered on the x-axis for the purpose of visualization.",
#         hjust = 1, x = 1, size=10, family = "LM Roman 10")
#     )
#   )
# }



# single_component_week_variance_plot <- function(component, temp){
#   p <- ggplot(df, aes(x = factor(created_time_week, levels=temp$created_time_week), y = eval(parse(text = component)))) +
#     geom_jitter(color="grey", width = 0.2, alpha=0.2) +
#     geom_point(
#       data=temp,
#       aes(
#         x=created_time_week,
#         y=eval(parse(text = paste0(component,"_m")))
#       ),
#       color = single_color
#     ) +
#     geom_linerange(
#       data=temp,
#       aes(
#         x = created_time_week,
#         y=eval(parse(text = paste0(component,"_m"))),
#         ymin=eval(parse(text = paste0(component,"_m"))) - eval(parse(text = paste0(component,"_s"))),
#         ymax=eval(parse(text = paste0(component,"_m"))) + eval(parse(text = paste0(component,"_s")))
#       ),
#       color = single_color
#     ) +
#     scale_y_continuous(limits = c(0,1)) +
#     labs(
#       y = component,
#       x = "Week of year",
#     ) +
#     theme_mls() +
#     theme(
#       axis.text.x=element_text(
#         color=rep(c("black", rep("transparent", each = 5-1)), length(temp$created_time_week) / 5),
#         angle = 90, vjust = 0.5, hjust=1
#       )
#     )
#   return(p)
# }


#
# created_time_week_variance_plot <- function(){
#
#   temp <- df %>%
#     group_by(created_time_week) %>%
#     summarise(
#       PC1_m = mean(PC1),
#       PC1_s = sd(PC1),
#       PC2_m = mean(PC2),
#       PC2_s = sd(PC2),
#       PC3_m = mean(PC3),
#       PC3_s = sd(PC3),
#       PC4_m = mean(PC4),
#       PC4_s = sd(PC4),
#     ) %>%
#     arrange(created_time_week)
#
#   var_list = c("PC1", "PC2", "PC3", "PC4")
#
#   plot_list <- lapply(
#     var_list,
#     function(var){single_component_week_variance_plot(var, temp)}
#   )
#
#   fig <- ggarrange(
#     plotlist = plot_list,
#     ncol = 2,
#     nrow = 2,
#     common.legend = TRUE,
#     legend="bottom"
#   )
#
#   return(
#     annotate_figure(
#       fig,
#       bottom = text_grob(
#         "Note: The points are jittered on the x-axis for the purpose of visualization.",
#         hjust = 1, x = 1, size=10, family = "LM Roman 10")
#     )
#   )
# }


n_posts_per_week_plot <- function(){
  major_fnc <- every_nth(n = 5)


  temp <- df %>%
    group_by(created_time_week) %>%
    summarise(n = n()) %>%
    mutate(major = ifelse(created_time_week %in% major_fnc(created_time_week), T, F))

  minor <- temp %>%
    filter(major == F)

  p <- ggplot(temp, aes(x = fixl(created_time_week), y = n, group = 1)) +
    geom_vline(
      mapping=NULL,
      xintercept=fixl(minor$created_time_week),
      colour='grey80',
      alpha=0.2
    ) +
    geom_step(color=single_color, size=1.5) +
    labs(
      x = "Year and week",
      y = "Number of posts"
    ) +
    scale_y_continuous(limits = c(0,320), expand=c(0,0)) +
    scale_x_discrete(breaks=every_nth(n = 5)) +
    theme_mls() +
    theme(
      axis.text.x=element_text(
        #color=rep(c("black", rep("transparent", each = 5-1)), length(temp$created_time_week) / 5),
        angle = 90, vjust = 0.8, hjust=1
      ),
      panel.grid.major.x = element_line(color = "grey80"),
    )
  return(p)
}




sidney_lee_plot <- function(){
  p <- ggplot(df, aes(x = link_clicks, y = facebook_pageview_n)) +
    geom_point(color=single_color, alpha=0.7) +
    scale_x_continuous(breaks=c(0,100000,200000,300000,400000,500000)) +
    scale_y_continuous(breaks=c(0,100000,200000,300000,400000,500000)) +
    labs(
      x = "Clicks on link on EB's Facebook post",
      y = fixl("Pageviews from Facebook based on referer url")
    ) +
    theme_mls() +
    theme(aspect.ratio = 1)
  return(p)
}




#
# # Get lower triangle of the correlation matrix
# get_lower_tri<-function(cormat){
#   cormat[upper.tri(cormat)] <- NA
#   return(cormat)
# }
# # Get upper triangle of the correlation matrix
# get_upper_tri <- function(cormat){
#   cormat[lower.tri(cormat)]<- NA
#   return(cormat)
# }
#
# correlation_matrix <- function(df){
#   # colnames(df) <- c(
#   #   "Information Seeking",
#   #   "Emotionality",
#   #   "Sharing, Self-expressive",
#   #   "Sharing, phatic",
#   #   "Reciprocity",
#   #   "(Dis)respect, offensive language",
#   #   "Respect, recognizing language"
#   # )
#
#   cormat <- round(cor(df),2)
#
#   upper_tri <- get_upper_tri(cormat)
#
#   melted_cormat <- melt(upper_tri)
#
#   ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
#     geom_tile(color = "white")+
#     scale_fill_gradient2(low = "blue", high = "red", mid = "white",
#                          midpoint = 0, limit = c(-1,1), space = "Lab",
#                          na.value = 'transparent',
#                          name="Pearson\nCorrelation") +
#     theme_minimal() + # minimal theme
#     theme(
#         text = element_text(size=10, family="LM Roman 10"),
#         axis.text.x = element_text(
#           angle = 45, vjust = 1,
#           size = 12, hjust = 1)
#       ) +
#     coord_fixed()
#
#   ggheatmap +
#     geom_text(aes(Var2, Var1, label = fixl(value)), color = "black", family="LM Roman 10", size = 4) +
#     theme(
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       panel.grid.major = element_blank(),
#       panel.border = element_blank(),
#       panel.background = element_blank(),
#       axis.ticks = element_blank(),
#       legend.justification = c(1, 0),
#       legend.position = c(0.6, 0.7),
#       legend.direction = "horizontal")+
#     guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
#                                  title.position = "top", title.hjust = 0.5))
# }


make_coef_df <- function(m, rounding, m_name){
  temp <- data.frame(summary(m)$coefficients) %>%
    mutate(Estimate_label = ifelse(Pr...t.. < 0.05, round(Estimate, rounding), NA)) %>%
    mutate(Estimate = std(Estimate)) %>%
    mutate(Estimate = ifelse(Pr...t.. < 0.05, Estimate, NA)) %>%
    select(Estimate, Estimate_label) %>%
    rownames_to_column(var="Predictor") %>%
    filter(Predictor %in% c(
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
      "hard_news_final"
    )) %>%
    gather(key="Model", value="Coefficient", -Predictor) %>%
    spread(Model, Coefficient) %>%
    mutate(m_name = m_name) %>%
    mutate(Predictor = case_when(
      Predictor == "timeliness" ~ "Timeliness",
      Predictor == "unexpectedness" ~ "Unexpectedness",
      Predictor == "proximity_geo" ~ "Geographical proximity",
      Predictor =="ner_ORG_MAIN_dummy" ~ "Cultural proximity",
      Predictor == "ner_PER_MAIN_dummy" ~ "Personalization",
      Predictor == "negativity_topic" ~ "Negativity (topic)",
      Predictor == "negativity_pred" ~ "Negativity (style)",
      Predictor == "positivity_pred" ~ "Positivity (style)",
      Predictor == "breaking" ~ "Impact",
      Predictor == "spreadability" ~ "Spreadability",
      Predictor == "hard_news_final" ~ "Hard News"
    )
    )
  return(temp)
}

coefficient_matrix_plot <- function(){
  y_labels = c(
    "Timeliness",
    "Unexpectedness",
    "Geographical proximity",
    "Cultural proximity",
    "Personalization",
    "Negativity (topic)",
    "Negativity (style)",
    "Positivity (style)",
    "Impact",
    "Spreadability",
    "Hard News"
  )

  x_labels = c(
    "Link clicks",
    "Number of \nreactions",
    "Number of \nshares",
    "Prop. of \nmention \ncomments",
    "Number of \ncomments",
    "Prop. of \nreply \ncomments",
    "Prop. of \noffensive \ncomments",
    "Prop. of \nrecognizing \ncomments"
  )

  coefs <- bind_rows(
    make_coef_df(m1, rounding = 0, "Link clicks"),
    make_coef_df(m2, rounding = 0, "Number of \nreactions"),
    make_coef_df(m3a, rounding = 0, "Number of \nshares"),
    make_coef_df(m3b, rounding = 2, "Prop. of \nmention \ncomments"),
    make_coef_df(m4a, rounding = 0, "Number of \ncomments"),
    make_coef_df(m4b, rounding = 2, "Prop. of \nreply \ncomments"),
    make_coef_df(m5a, rounding = 2, "Prop. of \noffensive \ncomments"),
    make_coef_df(m5b, rounding = 2, "Prop. of \nrecognizing \ncomments"),
  )

  p <- ggplot(coefs, aes(
    factor(m_name, levels=x_labels),
    factor(Predictor, levels = rev(y_labels)),
    fill = Estimate,
    label=fixl(Estimate_label),
    ))+
    geom_tile(color = "white", size=1) +
    geom_text(family="LM Roman 10", size=3) +
    #coord_fixed(0.3) +
    scale_fill_gradient2(
      low = "#1034A6",
      high = "#F62D2D",
      mid = "white",
      midpoint = 0,
      #space = "Lab",
      na.value = 'transparent',
      limits=c(-3,3),
      breaks=c(-3,-2,-1,0,1,2,3),
      labels=fixl(c("-3","-2","-1", "0", "1", "2", "3")),
      name="Standard Deviation of Estimate \n",
    ) +
    theme_minimal() + # minimal theme
    theme(
      text = element_text(size=10, family="LM Roman 10"),
      legend.position = "bottom",
      legend.title.align=0.5,
      legend.margin=margin(0,0,0,0, 'cm'),
      plot.margin = margin(0,0,0,0, "cm")
    ) +
    labs(
      y = "",
      x = ""
    )

  return(p)
}



# scree_plot <- function(){
#   fviz_eig(result_pca) +
#     scale_x_discrete(limits = c("1","2","3","4","5")) +
#     scale_y_continuous(limits = c(0,40), expand=c(0,0)) +
#     theme_mls()
# }
















### OLD
#
# outcome_var_plot <- function(){
#   temp <- nilt %>%
#     mutate(rsex = as.character(rsex)) %>%
#     group_by(rsex) %>%
#     mutate(
#       m = mean(persinc2, na.rm=T),
#       med = median(persinc2, na.rm=T)
#     ) %>%
#     ungroup()
#
#   p <- ggplot(temp, aes(x = rsex, y = persinc2, color=rsex)) +
#     geom_jitter(width = 0.2, alpha=0.7, show.legend = F) +
#     geom_errorbar(aes(ymax=m, ymin=m, fill = "Mean"), width= 0.5, color="#000000", show.legend = T) +
#     geom_errorbar(aes(ymax=med, ymin=med, fill = "Median"), width= 0.5, color="#000000", linetype='dashed', show.legend = T) +
#     guides(color = "none", fill = guide_legend(override.aes = list(linetype = c("solid", "dashed")))) +
#     labs(
#       fill='',
#       x='Gender: Female (ref.: Male)',
#       y="Annual Personal Income (GBP)",
#       caption="*Note*: <br> The points are jittered on the x-axis for the purpose of visualization. <br> There are only two categories for this question."
#     ) +
#     theme_mls() + theme(
#       legend.position = "bottom",
#       plot.caption = element_markdown(hjust = 0)
#     )
#   p
# }
#
#
# rsuper_plot <- function(){
#   p <- ggplot(nilt, aes(x = rsex, y = persinc2, group=rsuper, color=rsuper)) +
#     geom_point(position = "jitter") +
#     geom_smooth(method="lm") +
#     theme_mls() +
#     labs(
#       color='Supervisor',
#       x='Gender: Female (ref.: Male)',
#       y="Annual Personal Income (GBP)",
#       caption="*Note*: <br> The points are jittered on the x-axis for the purpose of visualization. <br> There are only two categories for this question."
#     ) +
#     theme_mls() + theme(
#       legend.position = "bottom",
#       plot.caption = element_markdown(hjust = 0)
#     )
#   p
# }
#
#
# hetero_plot <- function(){
#   p <- ggplot() +
#     geom_jitter(aes(x = model$fitted.values, y = model$residuals), width = 0.2, alpha=0.7, color="#242c44") +
#     geom_hline(yintercept = 0, color = 'black', linetype='dashed') +
#     xlab("Fitted Values") +
#     ylab("Residuals") +
#     theme_mls()
#
#   temp <- broom::tidy(lmtest::bptest(model, studentize=FALSE)) %>%
#     mutate(across(where(is.numeric), round, 3)) %>%
#     relocate(Method = method) %>%
#     rename(
#       Statistic = statistic,
#       `P-value` = p.value,
#       Parameter = parameter
#     )
#
#   tt <- ttheme_minimal(
#     core=list(fg_params=list(fontfamily="LM Roman 10", fontsize=10, hjust=0, x=0.1)),
#     colhead=list(fg_params=list(fontfamily="LM Roman 10", fontsize=10, hjust=0, x=0.1)),
#     rowhead=list(fg_params=list(fontfamily="LM Roman 10", fontsize=10, hjust=0))
#   )
#   tbl <- tableGrob(temp, rows=NULL, theme=tt)
#
#   grid.arrange(p, tbl,
#                nrow = 2, heights = c(2, 0.3))
# }
#
#
# sampling_table_categorical <- function(pref_format){
#   subset <- nilt %>% mutate(dataset = "Cleaned Sample")
#   full <- nilt_full %>% mutate(dataset = "Full Sample")
#   temp <- bind_rows(subset, full)
#
#   # Categorial cariables
#   out_table <- sumtable(
#     temp,
#     vars = c("rsex","religcat","orient","uninatid","tunionsa","rsuper"),
#     labels = c(
#       'Gender',
#       'Religion',
#       'Sexual Orientation',
#       'Constitutional View',
#       'Trade union membership',
#       'Supervisor: No'
#     ),
#     group="dataset",
#     group.test = TRUE,
#     out = 'return'
#   )
#   out_table <- out_table[-1,]
#   row.names(out_table) <- NULL
#
#   knitr::kable(
#     out_table,
#     format = pref_format,
#     booktabs = TRUE,
#     caption = "Descriptive Statistics for Cleaned and Full Sample (Categorical Variables)"
#   ) %>%
#     add_header_above(.,c(' '=1,'Cleaned Sample'=2,'Full Sample'=2,' '=1),escape = FALSE) %>%
#     kableExtra::kable_styling(position = "center",latex_options = "HOLD_position") %>%
#     footnote(general = "Statistical significance markers: * p<0.1; ** p<0.05; *** p<0.01")
# }
#
#
# sampling_table_numerical <- function(pref_format){
#   subset <- nilt %>% mutate(dataset = "Cleaned Sample")
#   full <- nilt_full %>% mutate(dataset = "Full Sample")
#   temp <- bind_rows(subset, full)
#
#   # Numerical variables
#   out_table <- sumtable(
#     temp,
#     vars = c("persinc2","rage"),
#     labels = c(
#       'Annual Personal Income (GBP)',
#       'Age'
#     ),
#     group="dataset",
#     group.test = TRUE,
#     out = 'return'
#   )
#   out_table <- out_table[-1,]
#   row.names(out_table) <- NULL
#
#   knitr::kable(
#     out_table,
#     format = pref_format,
#     booktabs = TRUE,
#     caption = "Descriptive Statistics for Cleaned and Full Sample (Numerical Variables)"
#   ) %>%
#     add_header_above(.,c(' '=1,'Cleaned Sample'=3,'Full Sample'=3,' '=1),escape = FALSE) %>%
#     kableExtra::kable_styling(position = "center",latex_options = "HOLD_position") %>%
#     footnote(general = "Statistical significance markers: * p<0.1; ** p<0.05; *** p<0.01")
# }
#
# control_vars_plot <- function(variable, label){
#   p <- ggplot(nilt, aes(x = variable, fill=variable)) +
#     geom_bar() +
#     theme_mls() +
#     theme(
#       legend.position = "none",
#       axis.ticks.y = element_blank(),
#       axis.ticks.x = element_blank()
#     ) +
#     scale_y_continuous(expand=c(0,0), limits=c(0,max(table(variable)) + 50)) +
#     xlab(label) +
#     ylab("Count")
#   p
# }
#
#
# numeric_vars_plot <- function(variable, label){
#   p <- ggplot(nilt, aes(x = variable, fill='1')) +
#     geom_density() +
#     geom_segment(aes(x=mean(variable), y=0, xend=mean(variable), yend=Inf), color="#000000", linetype='dashed') +
#     annotate(geom='label', x = mean(variable), y = Inf, hjust = 0, vjust = 1, label.size = NA, fill=NA, label='Mean', family = "LM Roman 10", size=3) +
#     theme_mls() +
#     theme(
#       legend.position = "none",
#       axis.ticks.y = element_blank(),
#       axis.ticks.x = element_blank(),
#       plot.caption = element_markdown(hjust = 0)
#     ) +
#     scale_y_continuous(expand = expansion(mult = c(0, .05)))
#     labs(
#       x=label,
#       y="Density",
#       caption="*Note*: <br> To estimate density, there is used kernel density estimation (KDE)."
#     )
#   p
# }
#
#
#

