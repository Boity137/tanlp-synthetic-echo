# The Synthetic Echo
## How Automated Bots Use Stereotypes to Crowd Out Human Perspectives in Geopolitical Conflict

**Author:** Lutendo Boitumelo Mulea  
**Course:** Text Analysis and Natural Language Processing (TANLP)  
**Institution:** Constructor University | Spring 2026  

---

## Research Question
Does the high-volume repetition of stereotypical narratives by automated bots 
on Reddit create a 'synthetic echo' that statistically reduces the diversity of 
human discourse regarding the Iran–USA and Gaza–Israel conflicts?

## Repository Contents
| File | Description |
|------|-------------|
| `01_clean_reddit_data.R` | Data cleaning pipeline — deduplication, bot flagging, conflict labelling, TTR |
| `02_analyse_discourse.R` | Hypothesis testing — Wilcoxon, Spearman, LDA topic modelling |

## Data
- Source: r/MiddleEastNews (Pushshift archive, 2009–2018)
- 25,648 posts + 6,506 comments
- Analysis corpus: 1,427 comments (432 bot, 995 human)

## Requirements
R 4.5.3 with packages: tidyverse, tidytext, topicmodels, ggplot2, entropy, writexl
Sunstein, C. R. (2017). #Republic: Divided Democracy in the Age of Social Media. Princeton University Press.
Varol, O., et al. (2017). Online human-bot interactions: Detection, estimation, and characterization. Proceedings of ICWSM.
