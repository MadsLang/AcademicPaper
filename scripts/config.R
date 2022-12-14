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
  lmtest, gridExtra, grid, ggpubr, moderndive
)

# import fonts
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import(pattern = "lmroman*")

# load utils: theme_mls()
source("scripts/utils.R")
source("scripts/plots.R")
theme_set(theme_mls())

# Read data
nilt <- readRDS("data/fullnilt_2012.rds") %>%
  select(persinc2, rsex, religcat, orient, uninatid, tunionsa, rsuper, rage) %>%
  mutate(
    tunionsa = relevel(tunionsa, ref = 2),
    rsuper = relevel(rsuper, ref = 2)
  ) %>%
  drop_na()

nilt_full <- readRDS("data/fullnilt_2012.rds") %>%
  select(persinc2, rsex, religcat, orient, uninatid, tunionsa, rsuper, rage) %>%
  mutate(
    tunionsa = relevel(tunionsa, ref = 2),
    rsuper = relevel(rsuper, ref = 2)
  )
