
rm(list=ls())
source("scripts/initialize.R")





### Bortfaldsanalyse

subset <- df %>%
  mutate(sample = "Posted on Facebook")
table_df <- full_df %>%
  mutate(sample = "All") %>%
  bind_rows(subset)


out_table <- sumtable(
  table_df,
  vars = c(
    "hard_news_final"
  ),
  group="sample",
  group.test = TRUE,
  out = 'return'
)






### Udforskende plots














## Correlation matrix

temp <- df %>%
  dplyr::select(
  pageview_n, facebook_pageview_n, twitter_pageview_n,
  characters, breaking, shares, comment_count, post_impressions,
  link_clicks, like_count, reactions_love, reactions_wow,
  reactions_haha, reactions_anger,
  n_mention, n_replies, n_comments, n_multiple_replies,
  n_tokens, n_chars, mean_lix, n_likes, n_hidden, attack_count,
  hate_count, recognition_count, n_media_link, n_misinfo_link) %>%
  replace(is.na(.), 0)
correlation_matrix(temp)


temp <- df %>%
  dplyr::select(starts_with("topic_")) %>%
  mutate_if(is.factor, as.numeric)
correlation_matrix(temp)

p <- ggplot(df, aes(x = hard_news, y = soft_news)) +
  geom_jitter(alpha=0.5) +
  theme_mls()
p

View(df %>% filter(hard_news > 1) %>% filter(soft_news > 1) %>%
       dplyr::select(starts_with("topic_") | starts_with("article") | starts_with("nice_")))







View(df %>% dplyr::select(article_id, title, PC1, PC2, PC3, PC4))










