# set knitr options
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
knitr::opts_chunk$set(
  fig.width = 6,
  #fig.asp = 0.8,
  fig.pos= "H",
  fig.align='center',
  table.pos = "H",
  out.width = "80%",
  dev.args = list(png = list(type = "cairo"))
)

options(scipen=999)

# load packages

pacman::p_load(
  tidyverse, reticulate, extrafont, rmarkdown, tinytex,
  stargazer, sandwich, ggplot2, ggsci, vtable, knitr, ggtext,
  lmtest, gridExtra, grid, ggpubr, moderndive, reshape2,
  bestNormalize, psych, remotes, stringr,
  scales, timetk, ggrepel, texreg
)
# remotes::install_version("rela", version="4.1")
library("rela")



# import fonts
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import(pattern = "lmroman*")

# load utils: theme_mls()
source("scripts/utils.R")
source("scripts/plots.R")
source("scripts/tables.R")
theme_set(theme_mls())
single_color <- "#e1287e"

# Read data
full_df <- read_csv("../../data/analysis_df.csv") %>%
  mutate(shared_fb = ifelse(is.na(post_id), FALSE, TRUE)) %>%
  mutate(
    topic_Begivenhed = factor(replace_na(topic_Begivenhed, 0)),
    topic_Bolig = factor(replace_na(topic_Bolig, 0)),
    topic_Dyr = factor(replace_na(topic_Dyr, 0)),
    topic_Erhverv = factor(replace_na(topic_Erhverv, 0)),
    topic_Katastrofe = factor(replace_na(topic_Katastrofe, 0)),
    topic_Kendt = factor(replace_na(topic_Kendt, 0)),
    topic_Konflikt_og_krig = factor(replace_na(topic_Konflikt_og_krig, 0)),
    topic_Kriminalitet = factor(replace_na(topic_Kriminalitet, 0)),
    topic_Kultur = factor(replace_na(topic_Kultur, 0)),
    topic_Politik = factor(replace_na(topic_Politik, 0)),
    topic_Samfund = factor(replace_na(topic_Samfund, 0)),
    topic_Sport = factor(replace_na(topic_Sport, 0)),
    topic_Sundhed = factor(replace_na(topic_Sundhed, 0)),
    topic_Teknologi = factor(replace_na(topic_Teknologi, 0)),
    topic_Transportmiddel = factor(replace_na(topic_Transportmiddel, 0)),
    topic_Uddannelse = factor(replace_na(topic_Uddannelse, 0)),
    topic_Underholdning = factor(replace_na(topic_Underholdning, 0)),
    topic_Vejr = factor(replace_na(topic_Vejr, 0)),
    topic_Videnskab = factor(replace_na(topic_Videnskab, 0)),
    topic_Økonomi = factor(replace_na(topic_Økonomi, 0)),
    ner_PER_MAIN_dummy = as.integer(replace_na(ner_PER_MAIN_dummy, 0)),
    ner_ORG_MAIN_dummy = as.integer(replace_na(ner_ORG_MAIN_dummy, 0)),
    negativity_topic = as.integer(replace_na(negativity_topic, 0)),
    breaking = as.integer(replace_na(breaking, 0)),
    premium = as.integer(replace_na(premium, 0)),
    spreadability = as.integer(replace_na(spreadability, 0))
    ) %>%
  mutate(
    total_reactions = like_count + reactions_love + reactions_wow + reactions_haha + reactions_anger + reactions_sorry,
    share_multiple_replies = n_multiple_replies / n_comments,
    share_media_link = n_media_link / n_comments,
    created_time_week = strftime(created_time, format = "%Y-%V"),
    created_time_month = strftime(created_time, format = "%Y-%m"),
    proximity_geo = validated_location
  ) %>%
  filter(words > 10) %>%
  mutate(
    lix = replace_na(lix, 0),
    words = words + 1,
    words100 = words / 100
  ) %>%
  mutate(
    other_pageview_facebook = facebook_pageview_n - link_clicks,
    not_clicked_share = (post_impressions - link_clicks) / post_impressions,
    reactions_share = (reactions_love + reactions_wow + reactions_haha + reactions_anger + reactions_sorry) / total_reactions
  ) %>%
  mutate(created_time_month = ifelse(created_time_month == "2021-02", "2021-03", created_time_month)) %>%
  mutate(new_strategy = ifelse(created_time_month >= "2022-05", 1, 0)) %>%
  mutate(election = case_when(
    created_time %>% between_time('2022-10-05 00:00:00','2022-11-02 00:00:00') ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(created_time_month = factor(created_time_month)) %>%
  distinct(article_id, .keep_all = T) %>%
  mutate_if(is.numeric, ~replace_na(., 0))


df <- full_df %>%
  filter(shared_fb == TRUE) %>%
  mutate(
    lix_norm = min_max_normalize(lix),
    words_norm = min_max_normalize(words),
  ) %>%
  mutate(article_complexity2 = lix_norm + words_norm) %>%
  mutate(article_complexity2 = min_max_normalize(article_complexity2))






######################### PCA #############################

# Make and transform the df for PCA


# pca_df <- df %>%
#   dplyr::select(
#     shares, comment_count,
#     link_clicks, total_reactions, like_count_share,
#     reactions_love_share, reactions_anger_share,
#     share_replies, share_multiple_replies, share_question,
#     mean_tokens, mean_likes,
#     attack_share, hate_share,
#   ) %>%
#   replace(is.na(.), 0) # %>%
#   # mutate_all(std)
#   #mutate_all(function(x){x + 1})
#
# # removed_cols_but_important <- c(recognition_share, share_mention,)
# # removed_cols_for_good <- c(
# #     share_hidden, mean_lix,
# #     share_media_link, reactions_sorry_share, reactions_haha_share, reactions_wow_share)
#
#
#
# # transformed_pca_df <- pca_df %>%
# #   dplyr::mutate_all(function(x){predict(boxcox(x, standardize = TRUE))})
#
#
# ### First, we test the assumptions for a PCA
#
#
# transformed_pca_df_Cor <- cor(pca_df)
# transformed_pca_df_Cov <- cov(pca_df)
#
# # # Kaiser-Meyer-Olkin (KMO) value
# assumptions <- paf(as.matrix(pca_df), eigcrit = 1, convcrit = .001)
# print(assumptions$KMO)
#
# # Bartlett’s Test for Sphericity
# bartlettTest <- cortest.bartlett(
#   transformed_pca_df_Cor,
#   n = dim(pca_df)[1]
# )
# bartlettTest
#
# # Positivity of the correlation matrix
# det(cor(pca_df))
#
#
#
# ### Then, we actually run the PCA
#
# result_pca <- prcomp(pca_df, center = TRUE, scale = TRUE)
# # fviz_eig(result_pca)
#
#
#
# paral_analysis <- hornpa(k = dim(pca_df)[2], size = dim(pca_df)[1], reps = 500, seed = 1111)
#
# # add n components to retain here
# retained_n <- 4
#
#
#
#
#
#
# oblique_rotation <- promax(result_pca$rotation[,1:retained_n])
# result_pca$rotation <- oblique_rotation$loadings
#
#
#
# # make loadings table
# attr(result_pca$rotation, "class") <- "matrix"
# loadings_df <- data.frame(result_pca$rotation) %>%
#   mutate_all(function(x){ifelse(x > 0.3 | x < -0.3, round(x, 2), " ")})
#
#
# # Add components to dataframe
# rotated_PCs <- pca_df %>%
#   factor.scores(x = ., f = result_pca$rotation)
#
# df[, c("PC1","PC2","PC3","PC4")] <- rotated_PCs$scores
#
# df <- df %>%
#   mutate(
#     PC1 = min_max_normalize(PC1),
#     PC2 = min_max_normalize(PC2),
#     PC3 = min_max_normalize(PC3),
#     PC4 = min_max_normalize(PC4),
#   )
#
