---
title: "TaskSwitching_README"
format: html
editor: visual
---

## 

## Language-Use Context and Task Switching: Evidence from Local and Global Switch Costs in Multilinguals

### Overview

This repository contains trial-level data and R analysis scripts for a study examining associations between contextual language use and task switching performance in a multilingual sample. This study examined local and global switch costs in two tasks:

- Number-Letter task: A univalent task

- Colour-Shape task: A bivalent task

Contextual language use is represented by three weighted scores: Single Language Context (SLC), Dual Language Context (DLC), and Dense Code-switching Context (DCS). these scores were analysed as continuous predictors.

The analyses examine whether the contextual language use scores are associated with local switch costs (Switch vs. Repeat trials within the Mixed block) and global switch costs (Repeat trials in the Mixed block vs. Repeat trials in the Pure block).

### Repository Contents

The repository contains the following:

| File | Description |
|----|----|
| `NumberLetter_Data.csv` | Trial-level master dataset for Number-Letter task. Includes participant-level contextual language use scores. |
| `ColourShape_Data.csv` | Trial-level master dataset for Colour-Shape task. Includes participant-level contextual language use scores. |
| `NumberLetter_Analysis.R` | R script for data preparation, local and global switch cost analyses, follow-up estimates, plots and model diagnostics for Number-Letter task. |
| `ColourShape_Analysis.R` | R script for data preparation, local and global switch cost analyses, follow-up estimates, plots and model diagnostics for Colour-Shape task. |
| `README.md` | Repository overview and instructions. |
| `Codebook.md` | Definitions of dataset variables and their coding. |

### Participants and data structure

The datasets contain trial-level data from 61 participants. Each dataset contains 7320 rows, with participant identifiers and task-related variables recorded for each trial. The three weighted contextual language use scores are included in the master datasets as participant-level variables. Each participant has one score for each context, and the three scores sum to 100. Because these scores are joined to trial-level data, the same participant-level scores are repeated across that participant's trial records.

The datasets contain records before the analysis-specific filtering steps. They should not be interpreted as containing only the trials included in the statistical models.

### Data preparation and analysis

The R scripts perform the following general steps:

1.  Import the corresponding master CSV file.

2.  Retain correct trials with reaction times (RT) between 200 and 2500 milliseconds for RT analyses.

3.  Convert relevant variables to factors and calculate log-transformed RT.

4.  Fit linear mixed-effects models with participant random intercepts.

5.  Examine local and global switch costs and their interactions with each of the three continuous language use scores.

6.  Generate follow-up estimates and plots for selected interactions.

7.  Inspect residual plots, normal Q-Q plots, model singularity, and random-effects variance.

#### Local switch cost

Local switch cost is examined by comparing Switch and Repeat trials within the Mixed block.

#### Global switch cost

Global switch cost is examined by comparing Repeat trials in the Mixed and Pure blocks.

The analyses use log-transformed RT as the dependent variable. The three language use scores are centered for the corresponding analyses. The model specifications and analysis details are provided in the R scripts.

### Requirements

The analysis scripts require R and the following scripts:

- `dplyr`

- `ggplot2`

- `lme4`

- `lmerTest`

- `emmeans`

### How to reproduce the analyses

1.  Download or clone the repository
2.  Open the project directory in RStudio or set it as the working directory in R.
3.  Ensure that the two CSV files and their corresponding R scripts are in the same directory.
4.  Run `NumberLetter_Analysis.R` to reproduce the Number-Letter analyses.
5.  Run `ColourShape_Analysis.R` to reproduce the Colour-Shape analyses.

The scripts are designed to read the corresponding CSV files from the working directory. If the files are stored elsewhere, update the import paths accordingly.

### Interpretation

The analyses assess associations between continuous contextual language use scores and task switching performance. The observational design does not establish that contextual language use causes differences in task switching performance.

### Citation and availability

**Data and code:** \[\_ \_\]

**Related publication:** \[\_ \_\]

### Contact

For any queries about the data or analysis scripts, contact the corresponding author listed in the associated publication.
