# set knitr options
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = 0.8,
  fig.pos= "H",
  fig.align='center',
  table.pos = "H",
  out.width = "80%"
)

options(scipen=999)

# load packages
pacman::p_load(
  tidyverse, reticulate, extrafont, rmarkdown, tinytex,
  stargazer, sandwich, ggplot2, ggsci, vtable, knitr, ggtext,
  lmtest, gridExtra, grid, ggpubr, moderndive, reshape2,
  factoextra, hornpa
)

# import fonts
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import(pattern = "lmroman*")

# load utils: theme_mls()
source("scripts/utils.R")
source("scripts/plots.R")
theme_set(theme_mls())
single_color <- "#e1287e"

# Read data
full_df <- read_csv("../../data/analysis_df.csv") %>%
  filter(published_utc > as.POSIXct("2021-02-07 00:00:00", tz="UTC")) %>%
  filter(published_utc < as.POSIXct("2023-01-01 00:00:00", tz="UTC")) %>%
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
    topic_Økonomi = factor(replace_na(topic_Økonomi, 0))
  )

df <- full_df %>%
  filter(shared_fb == TRUE) %>%
  mutate(question_message = str_detect(message, fixed("?")))
