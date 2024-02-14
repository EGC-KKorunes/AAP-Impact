# AAP-Impact
This repository contains scripts for analyzing the impact of the Applicant Assistance Program (AAP). 

## Datasets
- For this study, we retrieved publicly available award outcomes data from the SBIR Award Data database (SBIR.gov) between 2010 and 2022. These data also included demographic information. If a company received an award under majority ownership (at the time of award) by 1) a person who identifies as a woman or 2) a member of an underrepresented minority, the awardee is assigned to be WOSB or SDB, respectively. These data were subset to include only Phase I SBIR grant awards distributed by the NIH. This yielded a dataset of n = 9,458 unique Phase I awards. These data were then subset to include only those awards given by institutes and centers (ICs) that have participated in the NIH AAP (n = 11 ICs), resulting in a final dataset of n = 4,530 unique Phase I awards given by ICs that have participated in the AAP. Enrichment of awards and funding distributed to WOSB and SDB in the AAP was calculated as the mean percent change between within-group percentages of WOSBs and SDBs for funding years 2020 - 2022 (grouped within year; n = 3) between AAP awardees and the general awardee population.
- We additionally leveraged an internal database (Eva Garland Consulting, LLC) that consisted of participant-reported survey data collected from AAP participants (surveys distributed through Survey Monkey) to understand better the impact of the AAP on improving participant knowledge of the SBIR application process. Surveys were designed to assess participant knowledge related to the process of preparing and submitting an NIH Phase I SBIR grant proposal, and included 16 questions with 6 possible responses to each question: “Not applicable”, “Not sure”, “Not at all”, “Very little”, “Some”, and “A great deal”. Surveys were distributed both prior to the participant beginning the program (pre-completion), and after the program’s completion (post-completion; only given to participants who completed at least 5 weeks of the AAP program). This database consisted of a total of n = 716 returned surveys (n = 358 pre- and n = 358 post-completion surveys) spanning a total of n = 6 AAP cohorts between 2021 and 2022. To understand the impact of the AAP program on the knowledge gain of participants who have already undertaken the process of submitting an NIH Phase I SBIR proposal, these survey data were subset to include only those AAP participants who were submitting a ‘resubmission’ of a previous NIH Phase I SBIR proposal, resulting in a total of n = 88 surveys included in the analysis of the resubmission subset. Surveys were separately subset to only WOSBs and SDBs (n = 500 surveys). Surveys were grouped by AAP cohort (n = 6) and analyzed for the change in the percent of participants responding with “A great deal” between pre- and post-participation surveys for each question (knowledge gain). To compare knowledge gain against zero for the resubmission subset, one-sample t-tests were used (normality of the distribution of residuals was assessed using Shapiro-Wilk tests).
- Finally, we curated a novel database of follow-on funding (including NIH Phase II SBIR awards, publicly-available Venture-Capital funding, and other Phase I SBIR awards) received by previous AAP participants to explore the potential impact of the AAP on future funding success. The data were further subset to include only those AAP participants who have received Phase I SBIR awards following their participation in the AAP (n = 101 participants), or to only those who received their NIH Phase I SBIR awards during or before the year 2021, to visualize funding pathways following post-AAP Phase I success. All data analyses and visualization were conducted using the R language (ver. 4.3.2) within RStudio (ver. 2023.09.1, Build 494 “Desert Sunflower”).
