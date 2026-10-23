# Number–Letter Task: Consolidated Analysis Script

## 1. Load packages
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)

## 2. Import the master dataset and prepare the data
# Import the master dataset
NumberLetter_Master <- read.csv("NumberLetter_data.csv")

# Check the imported data
nrow(NumberLetter_Master)
length(unique(NumberLetter_Master$Subject))
names(NumberLetter_Master)

# Check the context totals
summary(NumberLetter_Master[, c("Single_Total_W", "Dual_Total_W", "Dense_Total_W")])

# Retain correct trials with RTs between 200 and 2500 ms
NumberLetter_Clean <- NumberLetter_Master %>% filter(Accuracy == 1, RT >= 200, RT <= 2500)

# Check the cleaned dataset
nrow(NumberLetter_Clean)
length(unique(NumberLetter_Clean$Subject))
summary(NumberLetter_Clean$RT)

# Convert relevant variables to factors
NumberLetter_Clean <- NumberLetter_Clean %>%
  mutate(
    Subject = factor(Subject),
    Task = factor(Task),
    Block = factor(Block),
    TrialType = factor(TrialType),
    Subtask = factor(Subtask)
  )

# Check factor levels
levels(NumberLetter_Clean$Block)
levels(NumberLetter_Clean$TrialType)

# Create log RT
NumberLetter_Clean <- NumberLetter_Clean %>%
  mutate(logRT = log(RT))
summary(NumberLetter_Clean$logRT)
sum(!is.finite(NumberLetter_Clean$logRT))

## 3. Local switch cost
##Local switch cost is examined using correct trials from the mixed block, comparing switch and repeat trials.
# Select mixed-block trials
NumberLetter_Local <- NumberLetter_Clean %>% filter(Block == "Mixed")

# Check the data
nrow(NumberLetter_Local)
length(unique(NumberLetter_Local$Subject))
table(NumberLetter_Local$TrialType)

# Centre the three context scores
NumberLetter_Local <- NumberLetter_Local %>%
  mutate(
    Single_Total_W_c = Single_Total_W - mean(Single_Total_W),
    Dual_Total_W_c = Dual_Total_W - mean(Dual_Total_W),
    Dense_Total_W_c = Dense_Total_W - mean(Dense_Total_W)
  )

# Check centred predictors
summary(NumberLetter_Local[, c("Single_Total_W_c", "Dual_Total_W_c", "Dense_Total_W_c")])

### 3.1 Single Language Context (SLC)
# Fit the centred model
model_Local_Single <- lmer(logRT ~ TrialType * Single_Total_W_c + (1 | Subject),
  data = NumberLetter_Local)
summary(model_Local_Single)
coef(summary(model_Local_Single))

# Estimate trends by trial type
NL_trends_local_single <- emtrends(model_Local_Single, ~ TrialType, var = "Single_Total_W_c")
NL_trends_local_single
contrast(NL_trends_local_single, method = "pairwise")

### 3.2 Dual Language Context (DLC)
# Fit the centred model
model_Local_Dual <- lmer(logRT ~ TrialType * Dual_Total_W_c + (1 | Subject),
  data = NumberLetter_Local)
summary(model_Local_Dual)
coef(summary(model_Local_Dual))

# Define low, average, and high DLC scores
mean_Dual <- mean(NumberLetter_Local$Dual_Total_W)
sd_Dual <- sd(NumberLetter_Local$Dual_Total_W)

Dual_values <- c(
  Low = mean_Dual - sd_Dual,
  Average = mean_Dual,
  High = mean_Dual + sd_Dual
)
Dual_values

# Estimate means at low, average, and high DLC scores
NL_emm_local_dual <- emmeans(model_Local_Dual, ~ TrialType | Dual_Total_W_c,
  at = list(Dual_Total_W_c = c(-sd_Dual, 0, sd_Dual)))
NL_emm_local_dual

# Estimate trends by trial type
NL_trends_local_dual <- emtrends(model_Local_Dual, ~ TrialType, var = "Dual_Total_W_c")
NL_trends_local_dual
contrast(NL_trends_local_dual, method = "pairwise")

# Back-transform estimated means and confidence intervals
NL_emm_local_dual_RT <- as.data.frame(NL_emm_local_dual) %>%
  mutate(
    RT = exp(emmean),
    RT_lower = exp(lower.CL),
    RT_upper = exp(upper.CL)
  )
NL_emm_local_dual_RT

# Plot the interaction
ggplot(
  NL_emm_local_dual_RT,
  aes(
    x = Dual_Total_W_c,
    y = RT,
    colour = TrialType,
    group = TrialType
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = RT_lower, ymax = RT_upper),
    width = sd_Dual * 0.08
  ) +
  scale_x_continuous(
    breaks = c(-sd_Dual, 0, sd_Dual),
    labels = c("Low", "Average", "High")
  ) +
  labs(
    title = "Number–Letter Task: Local Switch Cost by Dual Context",
    x = "Dual-language context score",
    y = "Predicted RT (ms)",
    colour = "Trial type"
  ) +
  theme_minimal()

### 3.3 Dense Code-switching Context (DCS)
# Fit the centred model
model_Local_Dense <- lmer(logRT ~ TrialType * Dense_Total_W_c + (1 | Subject),
  data = NumberLetter_Local)
summary(model_Local_Dense)
coef(summary(model_Local_Dense))

# 4. Global switch cost
##Global switch cost is examined using correct repeat trials from the pure and mixed blocks. The two pure blocks are combined into a single `Pure` condition.
# Select repeat trials from pure and mixed blocks
NumberLetter_Global <- NumberLetter_Clean %>%
  filter(
    Block %in% c("Pure Number", "Pure Letter", "Mixed"),
    TrialType == "Repeat"
  ) %>%
  mutate(
    GlobalCondition = ifelse(Block == "Mixed", "Mixed", "Pure"),
    GlobalCondition = factor(
      GlobalCondition,
      levels = c("Pure", "Mixed")
    )
  )

# Check the data
nrow(NumberLetter_Global)
length(unique(NumberLetter_Global$Subject))
table(NumberLetter_Global$GlobalCondition)

# Centre the three context scores
NumberLetter_Global <- NumberLetter_Global %>%
  mutate(
    Single_Total_W_c = Single_Total_W - mean(Single_Total_W),
    Dual_Total_W_c = Dual_Total_W - mean(Dual_Total_W),
    Dense_Total_W_c = Dense_Total_W - mean(Dense_Total_W)
  )

# Check centred predictors
summary(NumberLetter_Global[, c("Single_Total_W_c", "Dual_Total_W_c", "Dense_Total_W_c")])

### 4.1 Single Language Context (SLC)
model_Global_Single <- lmer(logRT ~ GlobalCondition * Single_Total_W_c + (1 | Subject),
  data = NumberLetter_Global)
summary(model_Global_Single)
coef(summary(model_Global_Single))

### 4.2 Dual Language Context (DLC)
model_Global_Dual <- lmer(logRT ~ GlobalCondition * Dual_Total_W_c + (1 | Subject),
  data = NumberLetter_Global)
summary(model_Global_Dual)
coef(summary(model_Global_Dual))

### 4.3 Dense Code-switching Context (DCS)
model_Global_Dense <- lmer(logRT ~ GlobalCondition * Dense_Total_W_c + (1 | Subject),
  data = NumberLetter_Global)
summary(model_Global_Dense)
coef(summary(model_Global_Dense))

## 5. Model diagnostics
##The following function reproduces the diagnostic checks used in the original analyses.
check_model_diagnostics <- function(model) {

# Residuals versus fitted values
plot(model)
  
# Normal Q-Q plot of residuals
qqnorm(resid(model))
qqline(resid(model))
  
# Check singularity and random-effects variance
print(isSingular(model))
print(VarCorr(model))}

##Run the diagnostics for each of the six models:
# Local switch cost
check_model_diagnostics(model_Local_Single)
check_model_diagnostics(model_Local_Dual)
check_model_diagnostics(model_Local_Dense)

# Global switch cost
check_model_diagnostics(model_Global_Single)
check_model_diagnostics(model_Global_Dual)
check_model_diagnostics(model_Global_Dense)

###END###