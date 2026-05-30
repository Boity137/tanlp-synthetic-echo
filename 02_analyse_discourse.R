# =============================================================================
# 02_analyse_discourse.R
# The Synthetic Echo: Bot Stereotypes & Human Discourse Diversity
# Lutendo Boitumelo Mulea | TANLP Spring 2026 | Constructor University
# =============================================================================
# Hypotheses tested:
#   H1: Bots use stereotype terms at significantly higher rates than humans
#   H2: Higher bot activity correlates with lower lexical diversity (TTR, entropy)
#   H3: LDA topic distributions in human comments narrow during high-bot periods
# =============================================================================

# ── 0. Setup ──────────────────────────────────────────────────────────────────
setwd("C:/Users/user/Downloads/NLP Project LB Mulea")

packages <- c(
  "tidyverse", "tidytext", "quanteda", "quanteda.textstats",
  "topicmodels", "ldatuning", "ggplot2", "ggpubr",
  "scales", "lubridate", "coin", "rstatix", "entropy",
  "reshape2", "knitr", "writexl"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

cat("✓ Packages loaded\n")

# ── 1. Load Cleaned Data ──────────────────────────────────────────────────────
comments <- read_csv("cleaned_data/comments_clean.csv", show_col_types = FALSE)
posts    <- read_csv("cleaned_data/posts_clean.csv",    show_col_types = FALSE)

cat(sprintf("✓ Loaded %d comments | %d posts\n", nrow(comments), nrow(posts)))

# Ensure required columns exist
stopifnot(
  all(c("author", "text_clean", "is_bot", "conflict", "ttr",
        "token_count", "stereotype_hits", "created_date") %in% names(comments))
)

# ── 2. Feature Engineering ────────────────────────────────────────────────────
comments <- comments %>%
  mutate(
    created_date  = as.Date(created_date),
    week          = floor_date(created_date, "week"),
    is_bot        = as.logical(is_bot),
    account_type  = if_else(is_bot, "Bot", "Human"),
    stereotype_rate = stereotype_hits / pmax(token_count, 1),
    conflict_label  = case_when(
      conflict == "iran_usa"     ~ "Iran–USA",
      conflict == "gaza_israel"  ~ "Gaza–Israel",
      conflict == "both"         ~ "Both",
      TRUE                       ~ "Other"
    )
  ) %>%
  filter(conflict %in% c("iran_usa", "gaza_israel", "both"))

cat(sprintf("✓ Analysis corpus: %d comments\n", nrow(comments)))

# ── 3. Descriptive Summary ────────────────────────────────────────────────────
desc_summary <- comments %>%
  group_by(conflict_label, account_type) %>%
  summarise(
    n               = n(),
    mean_ttr        = round(mean(ttr, na.rm = TRUE), 3),
    median_ttr      = round(median(ttr, na.rm = TRUE), 3),
    mean_tokens     = round(mean(token_count, na.rm = TRUE), 1),
    mean_stereo_rate = round(mean(stereotype_rate, na.rm = TRUE), 4),
    .groups = "drop"
  )

print(desc_summary)
write_csv(desc_summary, "cleaned_data/descriptive_summary.csv")
cat("✓ Descriptive summary saved\n")

# ── 4. H1: Stereotype Rate — Bots vs Humans ───────────────────────────────────
cat("\n── H1: Wilcoxon test — stereotype rate (bot vs human) ──\n")

h1_results <- comments %>%
  group_by(conflict_label) %>%
  summarise(
    w_stat  = wilcox.test(stereotype_rate ~ is_bot)$statistic,
    p_value = wilcox.test(stereotype_rate ~ is_bot)$p.value,
    bot_median   = median(stereotype_rate[is_bot == TRUE],  na.rm = TRUE),
    human_median = median(stereotype_rate[is_bot == FALSE], na.rm = TRUE),
    effect_direction = if_else(bot_median > human_median, "Bots > Humans", "Humans > Bots"),
    .groups = "drop"
  ) %>%
  mutate(
    significant = p_value < 0.05,
    p_label     = case_when(
      p_value < 0.001 ~ "p < 0.001",
      p_value < 0.01  ~ "p < 0.01",
      p_value < 0.05  ~ "p < 0.05",
      TRUE            ~ paste0("p = ", round(p_value, 3))
    )
  )

print(h1_results)
write_csv(h1_results, "cleaned_data/H1_wilcoxon_results.csv")

# H1 Plot — Boxplot of stereotype rate by account type & conflict
p_h1 <- comments %>%
  filter(!is.na(stereotype_rate)) %>%
  ggplot(aes(x = account_type, y = stereotype_rate, fill = account_type)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.8) +
  facet_wrap(~ conflict_label) +
  scale_fill_manual(values = c("Bot" = "#E63946", "Human" = "#457B9D")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = "H1: Stereotype Rate — Bots vs Humans",
    subtitle = "Proportion of tokens matching stereotype lexicon per comment",
    x = NULL, y = "Stereotype Rate",
    fill = "Account Type",
    caption = "Wilcoxon rank-sum test; * p < 0.05"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave("cleaned_data/H1_stereotype_rate_boxplot.png",
       p_h1, width = 8, height = 5, dpi = 300)
cat("✓ H1 plot saved\n")

# ── 5. Weekly Bot Activity & Diversity Metrics ────────────────────────────────
weekly <- comments %>%
  group_by(week, conflict_label) %>%
  summarise(
    total_comments  = n(),
    n_bot           = sum(is_bot, na.rm = TRUE),
    n_human         = sum(!is_bot, na.rm = TRUE),
    bot_share       = n_bot / total_comments,
    mean_ttr_all    = mean(ttr, na.rm = TRUE),
    mean_ttr_human  = mean(ttr[!is_bot], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_comments >= 3)   # drop weeks with too few comments

# Shannon entropy per week (human comments only)
human_comments <- comments %>% filter(!is_bot, !is.na(text_clean), text_clean != "")

weekly_entropy <- human_comments %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) > 2) %>%
  group_by(week, conflict_label) %>%
  count(word) %>%
  group_by(week, conflict_label) %>%
  summarise(
    shannon_entropy = entropy::entropy(n, unit = "log2"),
    unique_words    = n(),
    .groups = "drop"
  )

weekly <- weekly %>%
  left_join(weekly_entropy, by = c("week", "conflict_label"))

write_csv(weekly, "cleaned_data/weekly_metrics.csv")
cat("✓ Weekly metrics saved\n")

# ── 6. H2: Bot Activity vs Lexical Diversity ──────────────────────────────────
cat("\n── H2: Spearman correlation — bot share vs TTR/entropy ──\n")

h2_results <- weekly %>%
  filter(!is.na(mean_ttr_human), !is.na(bot_share)) %>%
  group_by(conflict_label) %>%
  summarise(
    # TTR correlation
    cor_ttr   = cor(bot_share, mean_ttr_human, method = "spearman", use = "complete.obs"),
    p_ttr     = cor.test(bot_share, mean_ttr_human, method = "spearman")$p.value,
    # Entropy correlation
    cor_ent   = cor(bot_share, shannon_entropy, method = "spearman", use = "complete.obs"),
    p_ent     = cor.test(bot_share, shannon_entropy, method = "spearman")$p.value,
    n_weeks   = n(),
    .groups   = "drop"
  ) %>%
  mutate(
    sig_ttr = p_ttr < 0.05,
    sig_ent = p_ent < 0.05
  )

print(h2_results)
write_csv(h2_results, "cleaned_data/H2_spearman_results.csv")

# H2 Plot — Scatter: bot share vs mean human TTR over time
p_h2 <- weekly %>%
  filter(!is.na(mean_ttr_human)) %>%
  ggplot(aes(x = bot_share, y = mean_ttr_human, colour = conflict_label)) +
  geom_point(aes(size = total_comments), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("Iran–USA" = "#2A9D8F", "Gaza–Israel" = "#E9C46A",
                                  "Both" = "#F4A261")) +
  labs(
    title    = "H2: Weekly Bot Share vs Human Lexical Diversity (TTR)",
    subtitle = "Each point = one week; size = total comments",
    x = "Bot Share of Weekly Comments",
    y = "Mean TTR (Human Comments Only)",
    colour = "Conflict", size = "N Comments",
    caption = "Spearman ρ; dashed line = OLS trend"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave("cleaned_data/H2_botshare_vs_ttr_scatter.png",
       p_h2, width = 8, height = 5, dpi = 300)
cat("✓ H2 plot saved\n")

# H2 Entropy over time
p_h2b <- weekly %>%
  filter(!is.na(shannon_entropy)) %>%
  ggplot(aes(x = week, y = shannon_entropy, colour = conflict_label)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(aes(size = bot_share), alpha = 0.5) +
  scale_size_continuous(labels = percent_format(accuracy = 1), range = c(1, 5)) +
  scale_colour_manual(values = c("Iran–USA" = "#2A9D8F", "Gaza–Israel" = "#E9C46A",
                                  "Both" = "#F4A261")) +
  labs(
    title    = "Shannon Entropy of Human Discourse Over Time",
    subtitle = "Point size = bot share that week",
    x = NULL, y = "Shannon Entropy (bits)",
    colour = "Conflict", size = "Bot Share"
  ) +
  theme_minimal(base_size = 12)

ggsave("cleaned_data/H2_entropy_over_time.png",
       p_h2b, width = 9, height = 4, dpi = 300)
cat("✓ H2 entropy timeline saved\n")

# ── 7. High vs Low Bot Periods ────────────────────────────────────────────────
bot_threshold <- median(weekly$bot_share, na.rm = TRUE)

weekly <- weekly %>%
  mutate(bot_period = if_else(bot_share >= bot_threshold, "High Bot", "Low Bot"))

comments <- comments %>%
  left_join(weekly %>% select(week, conflict_label, bot_period),
            by = c("week", "conflict_label"))

# Wilcoxon: human TTR in high vs low bot weeks
cat("\n── H2b: Wilcoxon — human TTR in high vs low bot weeks ──\n")

h2b_results <- comments %>%
  filter(!is_bot, !is.na(bot_period), !is.na(ttr)) %>%
  group_by(conflict_label) %>%
  summarise(
    w_stat       = wilcox.test(ttr ~ bot_period)$statistic,
    p_value      = wilcox.test(ttr ~ bot_period)$p.value,
    high_bot_ttr = median(ttr[bot_period == "High Bot"], na.rm = TRUE),
    low_bot_ttr  = median(ttr[bot_period == "Low Bot"],  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(significant = p_value < 0.05)

print(h2b_results)
write_csv(h2b_results, "cleaned_data/H2b_wilcoxon_highlow_bot.csv")

# ── 8. H3: LDA Topic Modelling ────────────────────────────────────────────────
cat("\n── H3: LDA topic modelling on human comments ──\n")

# Prepare DTM
lda_corpus <- comments %>%
  filter(!is_bot, !is.na(text_clean), text_clean != "", !is.na(bot_period)) %>%
  mutate(doc_id = paste0("doc_", row_number()))

# Tokenise and build DTM
dtm_data <- lda_corpus %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words, by = "word") %>%
  filter(
    nchar(word) > 2,
    !str_detect(word, "^[0-9]+$")
  ) %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

# Remove empty docs
dtm_data <- dtm_data[rowSums(as.matrix(dtm_data)) > 0, ]
cat(sprintf("✓ DTM: %d documents × %d terms\n", nrow(dtm_data), ncol(dtm_data)))

# Fit LDA with k=5 topics
set.seed(42)
k <- 5
lda_model <- LDA(dtm_data, k = k, control = list(seed = 42))

# Top terms per topic
top_terms <- tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot top terms
p_h3_terms <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ paste("Topic", topic), scales = "free") +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "H3: LDA Top Terms per Topic (Human Comments)",
    subtitle = paste0("k = ", k, " topics; all human comments across both conflicts"),
    x = "β (term-topic probability)", y = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave("cleaned_data/H3_lda_top_terms.png",
       p_h3_terms, width = 10, height = 7, dpi = 300)
cat("✓ H3 LDA terms plot saved\n")

# Document-topic distribution
doc_topics <- tidy(lda_model, matrix = "gamma") %>%
  rename(doc_id = document)

# Join back to metadata
doc_meta <- lda_corpus %>% select(doc_id, bot_period, conflict_label)

doc_topics_meta <- doc_topics %>%
  left_join(doc_meta, by = "doc_id") %>%
  filter(!is.na(bot_period))

# Topic concentration per period (entropy of topic distribution)
topic_entropy <- doc_topics_meta %>%
  group_by(bot_period, conflict_label, doc_id) %>%
  summarise(
    doc_entropy = entropy::entropy(gamma, unit = "log2"),
    .groups = "drop"
  ) %>%
  group_by(bot_period, conflict_label) %>%
  summarise(
    mean_topic_entropy = mean(doc_entropy, na.rm = TRUE),
    sd_topic_entropy   = sd(doc_entropy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

cat("\n── H3: Topic entropy in high vs low bot periods ──\n")
print(topic_entropy)
write_csv(topic_entropy, "cleaned_data/H3_topic_entropy.csv")

# Wilcoxon on document topic entropy
h3_results <- doc_topics_meta %>%
  group_by(bot_period, conflict_label, doc_id) %>%
  summarise(doc_entropy = entropy::entropy(gamma, unit = "log2"), .groups = "drop") %>%
  group_by(conflict_label) %>%
  summarise(
    w_stat      = wilcox.test(doc_entropy ~ bot_period)$statistic,
    p_value     = wilcox.test(doc_entropy ~ bot_period)$p.value,
    high_median = median(doc_entropy[bot_period == "High Bot"], na.rm = TRUE),
    low_median  = median(doc_entropy[bot_period == "Low Bot"],  na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    significant      = p_value < 0.05,
    narrowing_effect = high_median < low_median
  )

print(h3_results)
write_csv(h3_results, "cleaned_data/H3_wilcoxon_topic_entropy.csv")

# H3 Plot — Topic entropy by bot period
p_h3_entropy <- doc_topics_meta %>%
  group_by(bot_period, conflict_label, doc_id) %>%
  summarise(doc_entropy = entropy::entropy(gamma, unit = "log2"), .groups = "drop") %>%
  ggplot(aes(x = bot_period, y = doc_entropy, fill = bot_period)) +
  geom_boxplot(width = 0.5, outlier.alpha = 0.3) +
  facet_wrap(~ conflict_label) +
  scale_fill_manual(values = c("High Bot" = "#E63946", "Low Bot" = "#2A9D8F")) +
  labs(
    title    = "H3: Topic Diversity of Human Comments by Bot Activity Period",
    subtitle = "Higher entropy = more diverse topic usage per comment",
    x = NULL, y = "Document Topic Entropy (bits)",
    fill = "Period",
    caption = "Wilcoxon rank-sum test on document-level topic entropy"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("cleaned_data/H3_topic_entropy_boxplot.png",
       p_h3_entropy, width = 8, height = 5, dpi = 300)
cat("✓ H3 topic entropy plot saved\n")

# ── 9. TF-IDF: Bot vs Human Signature Terms ───────────────────────────────────
cat("\n── TF-IDF: Distinguishing bot vs human vocabulary ──\n")

tfidf_data <- comments %>%
  filter(!is.na(text_clean), text_clean != "") %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words, by = "word") %>%
  filter(nchar(word) > 2, !str_detect(word, "^[0-9]+$")) %>%
  count(account_type, word) %>%
  bind_tf_idf(word, account_type, n)

top_tfidf <- tfidf_data %>%
  group_by(account_type) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()

p_tfidf <- top_tfidf %>%
  mutate(word = reorder_within(word, tf_idf, account_type)) %>%
  ggplot(aes(tf_idf, word, fill = account_type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ account_type, scales = "free") +
  scale_y_reordered() +
  scale_fill_manual(values = c("Bot" = "#E63946", "Human" = "#457B9D")) +
  labs(
    title    = "TF-IDF: Most Distinctive Terms — Bots vs Humans",
    subtitle = "Terms with highest TF-IDF weight for each account type",
    x = "TF-IDF", y = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave("cleaned_data/tfidf_bot_vs_human.png",
       p_tfidf, width = 9, height = 5, dpi = 300)
cat("✓ TF-IDF plot saved\n")

# ── 10. Final Results Summary ─────────────────────────────────────────────────
cat("\n═══════════════════════════════════════════════════════════\n")
cat("  FINAL RESULTS SUMMARY\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("H1 — Stereotype Rate (Bot vs Human):\n")
h1_results %>%
  select(conflict_label, bot_median, human_median, p_label, significant) %>%
  print()

cat("\nH2 — Bot Share vs TTR (Spearman ρ):\n")
h2_results %>%
  select(conflict_label, cor_ttr, p_ttr, cor_ent, p_ent) %>%
  print()

cat("\nH2b — TTR in High vs Low Bot Weeks (Wilcoxon):\n")
h2b_results %>%
  select(conflict_label, high_bot_ttr, low_bot_ttr, p_value, significant) %>%
  print()

cat("\nH3 — Topic Entropy in High vs Low Bot Periods:\n")
h3_results %>%
  select(conflict_label, high_median, low_median, p_value, significant, narrowing_effect) %>%
  print()

# Save full results
all_results <- list(
  H1 = h1_results,
  H2 = h2_results,
  H2b = h2b_results,
  H3 = h3_results
)
write_xlsx(all_results, "cleaned_data/all_hypothesis_results.xlsx")

cat("\n✓ All results saved to cleaned_data/\n")
cat("✓ Plots saved:\n")
cat("   - H1_stereotype_rate_boxplot.png\n")
cat("   - H2_botshare_vs_ttr_scatter.png\n")
cat("   - H2_entropy_over_time.png\n")
cat("   - H3_lda_top_terms.png\n")
cat("   - H3_topic_entropy_boxplot.png\n")
cat("   - tfidf_bot_vs_human.png\n")
cat("   - all_hypothesis_results.xlsx\n")
cat("\n🎓 Analysis complete for Lutendo Boitumelo Mulea — TANLP 2026\n")
