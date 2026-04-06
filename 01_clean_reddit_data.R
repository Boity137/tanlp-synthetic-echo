# =============================================================================
# 01_clean_reddit_data.R
# The Synthetic Echo: Reddit Data Cleaning & Bot-Flagging Pipeline
#
# Author:  Lutendo Boitumelo Mulea
# Course:  Text Analysis and Natural Language Processing
#          Constructor University | Spring 2026
#
# Purpose: Load, clean, label, and bot-flag r/MiddleEastNews data
#          (25,648 posts + 6,506 comments, Pushshift archive 2009-2018)
#          for downstream diversity and topic modelling analysis.
#
# Output:  data/reddit_clean.rds  — cleaned posts + comments
#          data/reddit_flagged.rds — with bot flags and conflict labels
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

# Install missing packages on first run
packages_needed <- c("tidyverse", "lubridate", "quanteda", "tidytext",
                     "stringr", "janitor", "here")

installed <- rownames(installed.packages())
to_install <- packages_needed[!packages_needed %in% installed]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(lubridate)
library(quanteda)
library(tidytext)
library(stringr)
library(janitor)
library(here)

# Reproducibility
set.seed(2026)
