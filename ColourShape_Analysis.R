# Colour–Shape Task: Consolidated Analysis Script

## 1. Load packages
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)

## 2. Import and prepare the master dataset
# Import the master dataset
ColourShape_Master <- read.csv("ColourShape_data.csv")

# Check the imported data
nrow(ColourShape_Master)
length(unique(ColourShape_Master$Subject))
names(ColourShape_Master)

# Check context totals
summary(ColourShape_Master[, c(
  "Single_Total_W",
  "Dual_Total_W",
  "Dense_Total_W"
)])

# Retain correct trials with RTs between 200 and 2500 ms
ColourShape_Clean <- ColourShape_Master %>%
  filter(Accuracy == 1, RT >= 200, RT <= 2500)

# Check the cleaned dataset
nrow(ColourShape_Clean)
length(unique(ColourShape_Clean$Subject))
summary(ColourShape_Clean$RT)

# Convert relevant variables to factors
ColourShape_Clean <- ColourShape_Clean %>%
  mutate(
    Subject = factor(Subject),
    Task = factor(Task),
    Block = factor(Block),
    TrialType = factor(TrialType),
    Subtask = factor(Subtask)
  )

# Check factor levels
levels(ColourShape_Clean$Block)
levels(ColourShape_Clean$TrialType)

# Create log RT
ColourShape_Clean <- ColourShape_Clean %>%
  mutate(logRT = log(RT))
summary(ColourShape_Clean$logRT)
sum(!is.finite(ColourShape_Clean$logRT))

## 3. Local switch cost
##Local switch cost is examined using correct trials from the Mixed block, comparing Repeat and Switch trials.
# Select Mixed-block trials
ColourShape_Local <- ColourShape_Clean %>%
  filter(Block == "Mixed")

# Check the data
nrow(ColourShape_Local)
length(unique(ColourShape_Local$Subject))
table(ColourShape_Local$TrialType)

# Centre the three context scores
ColourShape_Local <- ColourShape_Local %>%
  mutate(
    Single_Total_W_c = Single_Total_W - mean(Single_Total_W),
    Dual_Total_W_c = Dual_Total_W - mean(Dual_Total_W),
    Dense_Total_W_c = Dense_Total_W - mean(Dense_Total_W)
  )

# Check centred predictors
summary(ColourShape_Local[, c(
  "Single_Total_W_c",
  "Dual_Total_W_c",
  "Dense_Total_W_c"
)])

### 3.1 Single Language Context (SLC)
# Fit the centred model
model_Local_Single <- lmer(logRT ~ TrialType * Single_Total_W_c + (1 | Subject), 
  data = ColourShape_Local)
summary(model_Local_Single)
coef(summary(model_Local_Single))

# Estimate trends by trial type
CS_trends_local_single <- emtrends(model_Local_Single, ~ TrialType, var = "Single_Total_W_c")
CS_trends_local_single
contrast(CS_trends_local_single, method = "pairwise")

### 3.2 Dual Language Context (DLC)
# Fit the centred model
model_Local_Dual <- lmer(logRT ~ TrialType * Dual_Total_W_c + (1 | Subject),
  data = ColourShape_Local)
summary(model_Local_Dual)
coef(summary(model_Local_Dual))

### 3.3 Dense Code-switching Context (DCS)
# Fit the centred model
model_Local_Dense <- lmer(logRT ~ TrialType * Dense_Total_W_c + (1 | Subject),
  data = ColourShape_Local)
summary(model_Local_Dense)
coef(summary(model_Local_Dense))

## 4. Global switch cost
##Global switch cost is examined using correct Repeat trials from the Pure and Mixed blocks.
# Select Repeat trials from Pure and Mixed blocks
ColourShape_Global <- ColourShape_Clean %>%
  filter(
    Block %in% c("Pure", "Mixed"),
    TrialType == "Repeat"
  ) %>%
  mutate(
    Block = factor(Block, levels = c("Pure", "Mixed"))
  )

# Check the data
nrow(ColourShape_Global)
length(unique(ColourShape_Global$Subject))
table(ColourShape_Global$Block)

# Centre the three context scores
ColourShape_Global <- ColourShape_Global %>%
  mutate(
    Single_Total_W_c = Single_Total_W - mean(Single_Total_W),
    Dual_Total_W_c = Dual_Total_W - mean(Dual_Total_W),
    Dense_Total_W_c = Dense_Total_W - mean(Dense_Total_W)
  )

# Check centred predictors
summary(ColourShape_Global[, c(
  "Single_Total_W_c",
  "Dual_Total_W_c",
  "Dense_Total_W_c"
)])

### 4.1 Single Language Context (SLC)
# Fit the centred model
model_Global_Single <- lmer(logRT ~ Block * Single_Total_W_c + (1 | Subject),
  data = ColourShape_Global)
summary(model_Global_Single)
coef(summary(model_Global_Single))

# Estimate trends by block
CS_trends_global_single <- emtrends(model_Global_Single, ~ Block, var = "Single_Total_W_c")
CS_trends_global_single

# Compare the trends between blocks
contrast(CS_trends_global_single, method = "pairwise")

# Define low, average, and high SLC scores
mean_Single <- mean(ColourShape_Global$Single_Total_W)
sd_Single <- sd(ColourShape_Global$Single_Total_W)
Single_values_c <- c(-sd_Single, 0, sd_Single)

# Estimate means at low, average, and high SLC scores
CS_emm_single <- emmeans(model_Global_Single, ~ Block | Single_Total_W_c,
  at = list(Single_Total_W_c = Single_values_c))
CS_emm_single

names(as.data.frame(CS_emm_single))

# Back-transform estimated means and confidence intervals
CS_emm_single_RT <- as.data.frame(CS_emm_single) %>%
  mutate(
    RT = exp(emmean),
    RT_lower = exp(asymp.LCL),
    RT_upper = exp(asymp.UCL)
  )
CS_emm_single_RT

# Plot the interaction
ggplot(
  CS_emm_single_RT,
  aes(
    x = Single_Total_W_c,
    y = RT,
    colour = Block,
    group = Block
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = RT_lower, ymax = RT_upper),
    width = 1.5
  ) +
  scale_x_continuous(
    breaks = Single_values_c,
    labels = c("Low", "Average", "High")
  ) +
  labs(
    title = "Colour–Shape Task: Global Switch Cost by Single Context",
    x = "Single-language context score",
    y = "Predicted RT (ms)",
    colour = "Block"
  ) +
  theme_minimal()

### 4.2 Dual Language Context (DLC)
# Fit the centred model
model_Global_Dual <- lmer(logRT ~ Block * Dual_Total_W_c + (1 | Subject),
  data = ColourShape_Global)
summary(model_Global_Dual)
coef(summary(model_Global_Dual))

### 4.3 Dense Code-switching Context (DCS)
# Fit the centred model
model_Global_Dense <- lmer(logRT ~ Block * Dense_Total_W_c + (1 | Subject),
  data = ColourShape_Global)
summary(model_Global_Dense)
coef(summary(model_Global_Dense))

# Estimate trends by block
CS_trends_global_dense <- emtrends(model_Global_Dense, ~ Block, var = "Dense_Total_W_c")
CS_trends_global_dense

# Compare the trends between blocks
contrast(CS_trends_global_dense, method = "pairwise")

# Define low, average, and high DCS scores
mean_Dense <- mean(ColourShape_Global$Dense_Total_W)
sd_Dense <- sd(ColourShape_Global$Dense_Total_W)
Dense_values_c <- c(-sd_Dense, 0, sd_Dense)
Dense_values_c

# Estimate means at low, average, and high DCS scores
CS_emm_dense <- emmeans(model_Global_Dense, ~ Block | Dense_Total_W_c,
  at = list(Dense_Total_W_c = Dense_values_c))
CS_emm_dense

# Back-transform estimated means and confidence intervals
CS_emm_dense_RT <- as.data.frame(CS_emm_dense) %>%
  mutate(
    RT = exp(emmean),
    RT_lower = exp(asymp.LCL),
    RT_upper = exp(asymp.UCL)
  )
CS_emm_dense_RT

# Plot the interaction
ggplot(
  CS_emm_dense_RT,
  aes(
    x = Dense_Total_W_c,
    y = RT,
    colour = Block,
    group = Block
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = RT_lower, ymax = RT_upper),
    width = 1.5
  ) +
  scale_x_continuous(
    breaks = Dense_values_c,
    labels = c("Low", "Average", "High")
  ) +
  labs(
    title = "Colour–Shape Task: Global Switch Cost by Dense Context",
    x = "Dense-language context score",
    y = "Predicted RT (ms)",
    colour = "Block"
  ) +
  theme_minimal()

## 5. Model diagnostics
# Function for residual and random-effects diagnostics
check_model_diagnostics <- function(model) {
# Residuals versus fitted values
plot(model)
  
# Normal Q-Q plot of residuals
qqnorm(resid(model))
qqline(resid(model))

# Singularity and random-effects variance
print(isSingular(model))
print(VarCorr(model))
}

# Local  switch-cost models
check_model_diagnostics(model_Local_Single)
check_model_diagnostics(model_Local_Dual)
check_model_diagnostics(model_Local_Dense)

# Global switch-cost models
check_model_diagnostics(model_Global_Single)
check_model_diagnostics(model_Global_Dual)
check_model_diagnostics(model_Global_Dense)

###END###