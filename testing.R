
rm(list=ls())
source("scripts/config.R")





### Bortfaldsanalyse

subset <- df %>%
  mutate(sample = "Posted on Facebook")
table_df <- full_df %>%
  mutate(sample = "All") %>%
  bind_rows(subset)


out_table <- sumtable(
  table_df,
  vars = c(
    'breaking',
    'topic_Begivenhed',
    'topic_Bolig',
    'topic_Dyr',
    'topic_Erhverv',
    'topic_Katastrofe',
    'topic_Kendt',
    'topic_Konflikt_og_krig',
    'topic_Kriminalitet',
    'topic_Kultur',
    'topic_Politik',
    'topic_Samfund',
    'topic_Sport',
    'topic_Sundhed',
    'topic_Teknologi',
    'topic_Transportmiddel',
    'topic_Uddannelse',
    'topic_Underholdning',
    'topic_Vejr',
    'topic_Videnskab',
    'topic_Økonomi'
  ),
  group="sample",
  group.test = TRUE,
  out = 'return'
)




### Udforskende plots

p <- ggplot(df, aes(x=pageview_n, y=facebook_pageview_n, color=factor(shared_fb))) +
  geom_point(alpha=0.5) +
  theme_mls()
p



p <- ggplot(df, aes(x=link_clicks, y=facebook_pageview_n)) +
  geom_point(alpha=0.5, color=single_color) +
  theme_mls()
p




p <- ggplot(temp, aes(x=comment_count, y=facebook_pageview_n, color=question_message)) +
  geom_point(alpha=0.8) +
  geom_smooth(method='lm') +
  theme_mls()
p

p <- ggplot(temp, aes(x=shares, y=facebook_pageview_n)) +
  geom_point(alpha=0.8, color=single_color) +
  theme_mls()
p


## Correlation matrix

temp <- df %>%
  dplyr::select(
  pageview_n, facebook_pageview_n, twitter_pageview_n,
  characters, breaking, shares, comment_count, post_impressions,
  link_clicks, like_count, reactions_love, reactions_wow,
  reactions_haha, reactions_anger, question_message)
correlation_matrix(temp)



# PCA

pca_df <- df %>%
  dplyr::select(
    facebook_pageview_n, shares, comment_count,
    post_impressions, link_clicks, like_count,
    reactions_love, reactions_wow,
    reactions_haha, reactions_anger
  ) %>% replace(is.na(.), 0)

sq_pca_df <- sqrt(pca_df)

result_pca <- prcomp(sq_pca_df, center = TRUE, scale = TRUE)
fviz_eig(result_pca)

paral_analysis <- hornpa(k = 10, size = 19341, reps = 500, seed = 1111)


plotdf <- data.frame(
  result = result_pca$sdev,
  par_analysis = paral_analysis$`0.95`
) %>%
  mutate(component = row_number()) %>%
  gather(key, eigenvalue, -component)

p <- ggplot(plotdf, aes(x=component, y=eigenvalue, color=key)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) +
  theme_mls()
p





fviz_pca_var(result_pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



