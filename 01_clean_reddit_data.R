# =============================================================================
# PROJECT : The Synthetic Echo — Bot Stereotypes & Geopolitical Discourse
# AUTHOR  : Lutendo Boitumelo Mulea
# FILE    : 01_clean_reddit_data.R
# PURPOSE : Load, clean, and prepare Reddit posts & comments from
#           r/MiddleEastNews for NLP analysis of Iran-USA and Gaza-Israel
#           conflict discourse.
# TOOL    : Positron (R)
# =============================================================================


# ── 0. PACKAGES ───────────────────────────────────────────────────────────────
# Install any missing packages before loading
packages <- c(
  "tidyverse",   # data wrangling
  "jsonlite",    # read JSONL
  "lubridate",   # datetime handling
  "stringr",     # string cleaning
  "textclean",   # text normalisation
  "hunspell",    # spell/language detection
  "janitor",     # clean column names
  "writexl"      # optional: export to Excel
)

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0) install.packages(to_install)

lapply(packages, library, character.only = TRUE)


# ── 1. LOAD DATA ──────────────────────────────────────────────────────────────
message("Loading posts ...")
posts_raw <- read_csv("r_MiddleEastNews_posts.csv", show_col_types = FALSE)

message("Loading comments ...")
comments_raw <- read_csv("MiddleEastNews_Comments.csv", show_col_types = FALSE)


# ── 2. STANDARDISE COLUMN NAMES ───────────────────────────────────────────────
posts_raw    <- janitor::clean_names(posts_raw)
comments_raw <- janitor::clean_names(comments_raw)

message(glue::glue("Raw posts: {nrow(posts_raw)} | Raw comments: {nrow(comments_raw)}"))


# ── 3. SELECT RELEVANT COLUMNS ────────────────────────────────────────────────
# Posts: keep text, metadata, and engagement signals
posts <- posts_raw %>%
  select(
    id,
    author,
    title,
    selftext,
    created_utc,
    score,
    upvote_ratio,
    num_comments,
    url,
    domain,
    is_self,
    subreddit,
    permalink,
    distinguished,
    edited,
    over_18,
    gilded
  )

# Comments: keep text, metadata, and thread linkage
comments <- comments_raw %>%
  select(
    id,
    author,
    body,
    created_utc,
    score,
    controversiality,
    link_id,      # ties comment back to a post
    parent_id,
    subreddit,
    permalink,
    distinguished,
    edited,
    gilded
  )


# ── 4. DATETIME CONVERSION ────────────────────────────────────────────────────
posts <- posts %>%
  mutate(
    created_dt  = as_datetime(created_utc, tz = "UTC"),
    created_date = as_date(created_dt),
    year  = year(created_dt),
    month = month(created_dt),
    hour  = hour(created_dt)
  )

comments <- comments %>%
  mutate(
    created_dt   = as_datetime(created_utc, tz = "UTC"),
    created_date = as_date(created_dt),
    year  = year(created_dt),
    month = month(created_dt),
    hour  = hour(created_dt)
  )


# ── 5. REMOVE DELETED / REMOVED / EMPTY TEXT ──────────────────────────────────
# Reddit replaces removed content with these sentinel strings
deleted_tokens <- c("[deleted]", "[removed]", "", NA_character_)

posts <- posts %>%
  mutate(
    # Combine title + selftext as full post text
    text_raw = case_when(
      is_self ~ paste(title, selftext, sep = " "),
      TRUE    ~ title                         # link posts: title only
    )
  ) %>%
  filter(!text_raw %in% deleted_tokens,
         !is.na(text_raw),
         str_length(str_trim(text_raw)) > 5)

comments <- comments %>%
  filter(!body %in% deleted_tokens,
         !is.na(body),
         str_length(str_trim(body)) > 5) %>%
  rename(text_raw = body)


# ── 6. DEDUPLICATE ────────────────────────────────────────────────────────────
# Exact-duplicate detection (same author + same text = likely bot repost)
posts <- posts %>%
  mutate(text_hash = digest::digest(text_raw, algo = "md5")) %>%     # one hash per row
  # For the full dedup across rows we use group_by + slice:
  group_by(text_hash) %>%
  slice_min(order_by = created_utc, n = 1, with_ties = FALSE) %>%
  ungroup()

comments <- comments %>%
  mutate(text_hash = mapply(digest::digest, text_raw, MoreArgs = list(algo = "md5"))) %>%
  group_by(text_hash) %>%
  slice_min(order_by = created_utc, n = 1, with_ties = FALSE) %>%
  ungroup()

# NOTE: digest package — install if needed:
# install.packages("digest")
library(digest)

# Re-run the hash step after loading digest:
posts <- posts %>%
  mutate(text_hash = sapply(text_raw, digest, algo = "md5")) %>%
  group_by(text_hash) %>%
  slice_min(order_by = created_utc, n = 1, with_ties = FALSE) %>%
  ungroup()

comments <- comments %>%
  mutate(text_hash = sapply(text_raw, digest, algo = "md5")) %>%
  group_by(text_hash) %>%
  slice_min(order_by = created_utc, n = 1, with_ties = FALSE) %>%
  ungroup()


# ── 7. BOT / AUTOMATED ACCOUNT FLAGGING ───────────────────────────────────────
# Flag accounts that match common bot-naming patterns OR show
# high-frequency posting behaviour (a key indicator of automation).

## 7a. Username pattern flag
bot_name_patterns <- c(
  "bot$", "^bot",          # starts or ends with 'bot'
  "_bot_", "-bot-",
  "auto",                  # AutoModerator, AutoNewsBot, etc.
  "news_?feed", "rss",
  "crawler", "spider",
  "breaking_?news",
  "alert_?bot",
  "^\\[deleted\\]$"
)

flag_bot_name <- function(author) {
  str_detect(
    str_to_lower(author),
    paste(bot_name_patterns, collapse = "|")
  )
}

posts <- posts %>%
  mutate(is_bot_name = flag_bot_name(author))

comments <- comments %>%
  mutate(is_bot_name = flag_bot_name(author))

## 7b. High-frequency posting flag
# Authors posting more than 2 SD above mean = automated-like volume
posts_freq <- posts %>%
  count(author, name = "post_count") %>%
  mutate(
    mean_posts = mean(post_count),
    sd_posts   = sd(post_count),
    is_high_freq_poster = post_count > (mean_posts + 2 * sd_posts)
  )

comments_freq <- comments %>%
  count(author, name = "comment_count") %>%
  mutate(
    mean_comments = mean(comment_count),
    sd_comments   = sd(comment_count),
    is_high_freq_commenter = comment_count > (mean_comments + 2 * sd_comments)
  )

posts <- posts %>%
  left_join(posts_freq %>% select(author, post_count, is_high_freq_poster),
            by = "author")

comments <- comments %>%
  left_join(comments_freq %>% select(author, comment_count, is_high_freq_commenter),
            by = "author")

## 7c. Composite bot flag
posts <- posts %>%
  mutate(is_suspected_bot = is_bot_name | is_high_freq_poster)

comments <- comments %>%
  mutate(is_suspected_bot = is_bot_name | is_high_freq_commenter)

message(glue::glue(
  "Suspected bots — Posts: {sum(posts$is_suspected_bot, na.rm=TRUE)} | ",
  "Comments: {sum(comments$is_suspected_bot, na.rm=TRUE)}"
))


# ── 8. CONFLICT LABELLING ─────────────────────────────────────────────────────
# Classify each post/comment into one or both conflict categories.

iran_usa_keywords <- c(
  "iran", "iranian", "tehran", "irgc", "rouhani", "khamenei",
  "khomeini", "nuclear deal", "jcpoa", "sanctions", "persian",
  "usa.?iran", "iran.?usa", "trump.?iran", "biden.?iran",
  "zarif", "pompeo", "gulf.?tension"
)

gaza_israel_keywords <- c(
  "gaza", "israel", "israeli", "palestine", "palestinian",
  "hamas", "idf", "netanyahu", "west bank", "occupation",
  "intifada", "settler", "ceasefire", "rocket", "airstrike",
  "blockade", "two.?state", "oslo", "zion", "nakba",
  "al.?aqsa", "jerusalem", "tel aviv", "ramallah"
)

label_conflict <- function(text) {
  t <- str_to_lower(text)
  iran  <- str_detect(t, paste(iran_usa_keywords,    collapse = "|"))
  gaza  <- str_detect(t, paste(gaza_israel_keywords, collapse = "|"))
  case_when(
    iran & gaza ~ "both",
    iran        ~ "iran_usa",
    gaza        ~ "gaza_israel",
    TRUE        ~ "other"
  )
}

posts <- posts %>%
  mutate(conflict_label = label_conflict(text_raw))

comments <- comments %>%
  mutate(conflict_label = label_conflict(text_raw))

message("Conflict label distribution — Posts:")
print(table(posts$conflict_label))
message("Conflict label distribution — Comments:")
print(table(comments$conflict_label))


# ── 9. TEXT CLEANING ──────────────────────────────────────────────────────────
clean_text <- function(text) {
  text %>%
    # Remove URLs
    str_remove_all("https?://\\S+|www\\.\\S+") %>%
    # Remove Reddit-specific markup (e.g. **bold**, *italic*, >quote, ^super)
    str_remove_all("\\*{1,3}|_{1,3}|\\^|>\\s") %>%
    # Remove HTML entities
    textclean::replace_html() %>%
    # Remove non-ASCII characters (emojis, special Unicode)
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
    # Normalize whitespace
    str_squish() %>%
    # Lower case
    str_to_lower() %>%
    # Remove leading/trailing spaces
    str_trim()
}

posts <- posts %>%
  mutate(text_clean = clean_text(text_raw))

comments <- comments %>%
  mutate(text_clean = clean_text(text_raw))


# ── 10. LANGUAGE FILTER (English only) ────────────────────────────────────────
# Uses hunspell to check if the majority of unique words are English.
# This is approximate — for stricter detection use the 'cld3' package.

is_english <- function(text, threshold = 0.35) {
  words <- str_split(text, "\\s+")[[1]]
  words <- words[str_length(words) > 2]
  if (length(words) == 0) return(FALSE)
  checked <- hunspell::hunspell_check(words, dict = "en_US")
  mean(checked) >= threshold
}

message("Filtering non-English posts (this may take a moment) ...")
posts <- posts %>%
  mutate(is_english = sapply(text_clean, is_english)) %>%
  filter(is_english)

message("Filtering non-English comments ...")
comments <- comments %>%
  mutate(is_english = sapply(text_clean, is_english)) %>%
  filter(is_english)


# ── 11. ADD DISCOURSE DIVERSITY FEATURES ──────────────────────────────────────
# Pre-compute metrics needed for the synthetic echo analysis.

## Type-Token Ratio (TTR): lexical diversity per post/comment
ttr <- function(text) {
  tokens <- str_split(str_to_lower(text), "\\s+")[[1]]
  tokens <- tokens[str_length(tokens) > 0]
  if (length(tokens) == 0) return(NA_real_)
  length(unique(tokens)) / length(tokens)
}

## Token count
token_count <- function(text) {
  tokens <- str_split(str_to_lower(str_trim(text)), "\\s+")[[1]]
  sum(str_length(tokens) > 0)
}

message("Computing lexical diversity metrics ...")
posts <- posts %>%
  mutate(
    token_n = sapply(text_clean, token_count),
    ttr     = sapply(text_clean, ttr)
  )

comments <- comments %>%
  mutate(
    token_n = sapply(text_clean, token_count),
    ttr     = sapply(text_clean, ttr)
  )


# ── 12. STEREOTYPE KEYWORD SCORING (Seed Lexicon) ────────────────────────────
# Simple presence/count of stereotype-associated terms.
# Expand this lexicon based on Lutendo's theoretical framework.

stereotype_terms <- c(
  # Iran-USA stereotypes
  "terrorist", "terrorism", "evil", "radical", "extremist",
  "mullahs", "regime", "rogue state", "axis of evil",
  "great satan", "death to america", "nuclear threat",
  "warmonger", "aggressor",
  # Gaza-Israel stereotypes
  "genocide", "apartheid", "colonizer", "settler", "occupier",
  "baby killer", "human shield", "propaganda",
  "antisemit", "jihadist", "martyr",
  "self-defense", "terror tunnel", "barbaric"
)

count_stereotypes <- function(text) {
  t <- str_to_lower(text)
  sum(str_count(t, paste(stereotype_terms, collapse = "|")))
}

flag_stereotype <- function(text) {
  count_stereotypes(text) > 0
}

posts <- posts %>%
  mutate(
    stereotype_count = sapply(text_clean, count_stereotypes),
    has_stereotype   = stereotype_count > 0
  )

comments <- comments %>%
  mutate(
    stereotype_count = sapply(text_clean, count_stereotypes),
    has_stereotype   = stereotype_count > 0
  )


# ── 13. FINAL CLEAN DATASETS ──────────────────────────────────────────────────
# Separate into conflict-specific subsets for analysis

posts_iran_usa    <- posts    %>% filter(conflict_label %in% c("iran_usa",    "both"))
posts_gaza_israel <- posts    %>% filter(conflict_label %in% c("gaza_israel", "both"))
comments_iran_usa    <- comments %>% filter(conflict_label %in% c("iran_usa",    "both"))
comments_gaza_israel <- comments %>% filter(conflict_label %in% c("gaza_israel", "both"))

# Combined dataset (all conflicts, labelled)
all_posts    <- posts
all_comments <- comments

message("\n── FINAL COUNTS ──────────────────────────────────────────────")
message(glue::glue("Posts total:              {nrow(all_posts)}"))
message(glue::glue("Posts (Iran-USA):          {nrow(posts_iran_usa)}"))
message(glue::glue("Posts (Gaza-Israel):       {nrow(posts_gaza_israel)}"))
message(glue::glue("Comments total:           {nrow(all_comments)}"))
message(glue::glue("Comments (Iran-USA):       {nrow(comments_iran_usa)}"))
message(glue::glue("Comments (Gaza-Israel):    {nrow(comments_gaza_israel)}"))
message(glue::glue("Suspected bot posts:       {sum(all_posts$is_suspected_bot,    na.rm=TRUE)}"))
message(glue::glue("Suspected bot comments:    {sum(all_comments$is_suspected_bot, na.rm=TRUE)}"))


# ── 14. EXPORT ────────────────────────────────────────────────────────────────
# Adjust output paths as needed

output_dir <- "cleaned_data"
dir.create(output_dir, showWarnings = FALSE)

write_csv(all_posts,             file.path(output_dir, "posts_all_clean.csv"))
write_csv(all_comments,          file.path(output_dir, "comments_all_clean.csv"))
write_csv(posts_iran_usa,        file.path(output_dir, "posts_iran_usa.csv"))
write_csv(posts_gaza_israel,     file.path(output_dir, "posts_gaza_israel.csv"))
write_csv(comments_iran_usa,     file.path(output_dir, "comments_iran_usa.csv"))
write_csv(comments_gaza_israel,  file.path(output_dir, "comments_gaza_israel.csv"))

message("\n✓ All cleaned files saved to /cleaned_data/")


# ── 15. QUICK SUMMARY TABLE ───────────────────────────────────────────────────
summary_tbl <- bind_rows(
  all_posts    %>% mutate(type = "post"),
  all_comments %>% mutate(type = "comment")
) %>%
  group_by(type, conflict_label, is_suspected_bot) %>%
  summarise(
    n             = n(),
    mean_ttr      = round(mean(ttr, na.rm = TRUE), 3),
    mean_tokens   = round(mean(token_n, na.rm = TRUE), 1),
    pct_stereo    = round(mean(has_stereotype, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

print(summary_tbl)
write_csv(summary_tbl, file.path(output_dir, "summary_by_conflict_bot.csv"))

message("\n✓ Summary table saved.")

# =============================================================================
# NEXT STEPS (02_analyse_discourse.R):
#   1. TF-IDF per author group (bot vs human) per conflict
#   2. Shannon entropy of token distributions → discourse diversity index
#   3. Time-series of TTR: do bot posting spikes correlate with diversity dips?
#   4. Topic modelling (LDA / STM) to detect stereotype cluster dominance
#   5. Statistical tests: Wilcoxon / permutation on TTR bot vs human
# =============================================================================
