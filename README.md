# tanlp-synthetic-echo
The Synthetic Echo: How Automated Bots Use Stereotypes to Crowd Out Human Perspectives in Geopolitical Conflict
Lutendo Boitumelo Mulea
Text Analysis and Natural Language Processing 

Research Question
Does the heavy repetition of identical stereotyped stories by bots on Reddit generate a kind of synthetic echo, statistically decreasing the variety of human discourse on the Iran-USA and Gaza-Israel conflicts?

Hypotheses
#	Hypothesis
H1: Bot accounts have much higher usage of stereotype-associated terms than human accounts.
H2	There is a negative correlation between periods of high bot activity and lexical diversity (Type-Token Ratio, Shannon entropy) in human comments.
H3	Topic models of human commenting on high-bot times indicate that the distribution of topic is narrower compared to low-bot times.
Why This Matters
The use of automated bots is being recorded in highly sensitive political online areas. At high volume, when fixed stereotypical frames are amplified by bots, e.g. by "terrorist state" or "occupier" they can create the statistical signature of an echo chamber even in cases where the individual human users are not predisposed to agree.

This project goes beyond bot detection to quantifying bot effects on discourse diversity - a question that has direct implications to platform moderation, media literacy education, and conflict communication research.

Dataset
Property	Detail
It is published in Source/MiddleEastNews (Pushshift archive).
Period	2009–2018
Posts	25,648
Comments	6,506
Conflicts discussed	Iran-USA, Gaza-Israel.
Status	Collected, cleaned, conflict-labelled, and bot-flagged.
Methods
Bot detection: Pattern of username + high-frequency posting threshold (±2 SD)
Diversity measures: Type-Token Ratio (per comment), Shannon entropy (weekly corpus), LDA topic breadth.
Statistical tests: Wilcoxon rank-sum, Spearman correlation, permutation testing
Tools & Environment
Language: R 4.5.1
IDE: Positron
Important packages: quanteda, tidytext, topicmodels, ggplot2.R # Bot-flagging and data cleaning pipeline.R -TTR and Shannon entropy calculations.R LDA topic modelling.R # permutation, Spearman, and Wilcoxon tests.└── data/ # (not tracked — see .gitignore)
Repository Structure
tanlp-synthetic-echo/
├── README.md
├── 01_clean_reddit_data.R       # Data cleaning and bot-flagging pipeline
├── 02_diversity_metrics.R       # TTR and Shannon entropy calculations
├── 03_topic_models.R            # LDA topic modelling
├── 04_statistical_tests.R       # Wilcoxon, Spearman, permutation tests
└── data/                        # (not tracked — see .gitignore)
Expected Findings
There will be a statistically significant greater stereotype term frequency of bot accounts compared to human accounts in both conflict situations.
The diversity of human comments (TTR and Shannon entropy) will be statistically reduced in those weeks in which the bot activity is the most active.

Research Question
Does the heavy repetition of identical stereotyped stories by bots on Reddit generate a kind of synthetic echo, statistically decreasing the variety of human discourse on the Iran-USA and Gaza-Israel conflicts?

Hypotheses
#	Hypothesis
H1: Bot accounts have much higher usage of stereotype-associated terms than human accounts.
H2	There is a negative correlation between periods of high bot activity and lexical diversity (Type-Token Ratio, Shannon entropy) in human comments.
H3	Topic models of human commenting on high-bot times indicate that the distribution of topic is narrower compared to low-bot times.
Why This Matters
The use of automated bots is being recorded in highly sensitive political online areas. At high volume, when fixed stereotypical frames are amplified by bots, e.g. by "terrorist state" or "occupier" they can create the statistical signature of an echo chamber even in cases where the individual human users are not predisposed to agree.

This project goes beyond bot detection to quantifying bot effects on discourse diversity - a question that has direct implications to platform moderation, media literacy education, and conflict communication research.

Dataset
Property	Detail
It is published in Source/MiddleEastNews (Pushshift archive).
Period	2009–2018
Posts	25,648
Comments	6,506
Conflicts discussed	Iran-USA, Gaza-Israel.
Status	Collected, cleaned, conflict-labelled, and bot-flagged.
Methods
Bot detection: Pattern of username + high-frequency posting threshold (±2 SD)
Diversity measures: Type-Token Ratio (per comment), Shannon entropy (weekly corpus), LDA topic breadth.
Statistical tests: Wilcoxon rank-sum, Spearman correlation, permutation testing
Tools & Environment
Language: R 4.5.1
IDE: Positron
Important packages: quanteda, tidytext, topicmodels, ggplot2.R # Bot-flagging and data cleaning pipeline.R -TTR and Shannon entropy calculations.R LDA topic modelling.R # permutation, Spearman, and Wilcoxon tests.└── data/ # (not tracked — see .gitignore)
Repository Structure
tanlp-synthetic-echo/
├── README.md
├── 01_clean_reddit_data.R       # Data cleaning and bot-flagging pipeline
├── 02_diversity_metrics.R       # TTR and Shannon entropy calculations
├── 03_topic_models.R            # LDA topic modelling
├── 04_statistical_tests.R       # Wilcoxon, Spearman, permutation tests
└── data/                        # (not tracked — see .gitignore)
Expected Findings
There will be a statistically significant greater stereotype term frequency of bot accounts compared to human accounts in both conflict situations.
The diversity of human comments (TTR and Shannon entropy) will be statistically reduced in those weeks in which the bot activity is the most active.
The topic distributions at high-bot periods will be narrowed as LDA topic models will show.
Provided it is confirmed, it will be the first empirical finding that bots decrease discourse diversity when it comes to geopolitical conflicts.
The topic distributions at high-bot periods will be narrowed as LDA topic models will show.
Provided it is confirmed, it will be the first empirical finding that bots decrease discourse diversity when it comes to geopolitical conflicts.

Key References
Bail, C. A., et al. (2018). Exposure to opposing views on social media can increase political polarization. PNAS, 115(37), 9216–9221.
Entman, R. M. (1993). Framing: Toward clarification of a fractured paradigm. Journal of Communication, 43(4), 51–58.
Ferrara, E., et al. (2016). The rise of social bots. Communications of the ACM, 59(7), 96–104.
Sunstein, C. R. (2017). #Republic: Divided Democracy in the Age of Social Media. Princeton University Press.
Varol, O., et al. (2017). Online human-bot interactions: Detection, estimation, and characterization. Proceedings of ICWSM.
