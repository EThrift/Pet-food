#created on 22/08/24
# By Emily Thrift
# script to  assess the levels of polymer levels in foods

#load libraries
library(tidyverse)
library(ggplot2)
library(emmeans)
library(car)

#add in working location

#load dataframe 
df_polymer_main <- read.csv("Polymer use model.csv")
df_polymer_type <- read.csv("Polymer_type.csv")
df_polymer_food <- read.csv("Polymer_food.csv")
df_polymer_f <- read.csv("Polymer food.csv") 
df_polymer_t <- read.csv("Polymer t.csv") 
df_polymer_price <- read.csv("Polymer price.csv")
# Remove spaces from column names
colnames(df_polymer_f) <- make.names(colnames(df_polymer_f))
colnames(df_polymer_t) <- make.names(colnames(df_polymer_t))
colnames(df_polymer_type) <- make.names(colnames(df_polymer_type))
colnames(df_polymer_main) <- make.names(colnames(df_polymer_main))

colnames(df_polymer_food) <- make.names(colnames(df_polymer_food))
colnames(df_polymer_price) <- make.names(colnames(df_polymer_price))


df_price <- read.csv ("Price polymer.csv")
colnames(df_price) <- make.names(colnames(df_price))

df_polymer <- read.csv("Polymer_combined.csv")
colnames(df_polymer) <- make.names(colnames(df_polymer))


df_SDI_richness <- df_polymer %>%
  group_by(`Target.animal`, Type, `Price.category`) %>%
  summarise(
    Richness = n_distinct(Polymer),               # unique polymers
    SDI = 1 - sum((n / sum(n))^2),                # Simpson diversity index
    TotalCount = sum(n)                            # total polymers counted
  ) %>%
  ungroup()

library(MASS)  # for glm.nb()

# Model 0: intercept-only (no predictors)
glm_richness_nb <- glm.nb(Richness ~ 1, data = df_SDI_richness)
summary(glm_richness_nb)

# Model 1: with Type
glm_richness1_nb <- glm.nb(Richness ~ Type, data = df_SDI_richness)
summary(glm_richness1_nb)

# Model comparison:
anova(glm_richness_nb, glm_richness1_nb)
#model glm_richness1_nb

# Model 2: Type + Target.animal
glm_richness2_nb <- glm.nb(Richness ~ Target.animal, data = df_SDI_richness)
summary(glm_richness2_nb)

anova(glm_richness_nb, glm_richness2_nb)

# Model 3: Type + Target.animal + Price.category
glm_richness3_nb <- glm.nb(Richness ~ Price.category, data = df_SDI_richness)
summary(glm_richness3_nb)

anova(glm_richness_nb, glm_richness3_nb)


# Load the DHARMa package
library(DHARMa)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = glm_richness_nb)

# Plot residual diagnostics (QQ plot, residual vs predicted, etc)
plot(sim_res)

# Test for overdispersion
testDispersion(sim_res)

gauss_sdi_null <- lm(SDI ~ 1, data = df_SDI_richness)
gauss_sdi_type <- lm(SDI ~ Type, data = df_SDI_richness)
anova(gauss_sdi_null, gauss_sdi_type)

gauss_sdi2 <- lm(SDI ~  Target.animal, data = df_SDI_richness)
summary(gauss_sdi2)

anova(gauss_sdi, gauss_sdi2)

#p = 0.18

# Model with Type + Target.animal + Price.category
gauss_sdi3 <- lm(SDI ~  Target.animal + Price.category, data = df_SDI_richness)
summary(gauss_sdi3)

anova(gauss_sdi2, gauss_sdi3)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = gauss_sdi2)

# Plot residual diagnostics (QQ plot, residual vs predicted, etc)
plot(sim_res)

# Test for overdispersion
testDispersion(sim_res)

vif(gauss_sdi3)
library(emmeans)
#pairwise comparisons 

emmeans_target <- emmeans(gauss_sdi3, ~ Target.animal)

# Perform pairwise comparisons
pairwise_comparisons <- contrast(emmeans_target, method = "pairwise")

# View the results of the pairwise comparisons
summary(pairwise_comparisons)

#p =0.97

df_use <- read.csv("Polymer use long.csv")

colnames(df_use) <- make.names(colnames(df_use)) 

library(binom)
library(patchwork)

library(dplyr)

colnames(df_polymer_f)

wilson_ci <- binom.confint(df_polymer_main$n, 
                           df_polymer_main$Total, 
                           method = "wilson")

df_polymer_main <- df_polymer_main %>%
  mutate(Lower = wilson_ci$lower * 100,  # Convert to percentage
         Upper = wilson_ci$upper * 100,
         label_y = Upper + 3.5)  # Position labels above error bars

P <- ggplot(df_polymer_main, aes(x = Polymer, y = Percentage, fill = Target.animal)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 6, position = position_dodge(width = 0.9), fontface = "plain", vjust = -0.5) +
  labs(x = "Target Animal", y = "% of Total") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Cat" = "grey", "Dog" = "white", "Hedgehog" = "grey30"))

P


# Summarize data: mean, sd, se, total, and label positions
df_counts_summary <- df_polymer_f %>%
  group_by(Use, Food.type) %>%
  summarise(
    mean_count = mean(n),             # mean count per sample
    sd_count = sd(n),                 # SD of counts
    n_samples = n(),                  # number of samples
    se_count = sd_count / sqrt(n_samples),  # SE of mean
    total = sum(n),                   # total counts (optional for labels)
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_count - se_count,
    upper = mean_count + se_count,
    label_y = upper + 0.2,             # position label just above error bar
    label_text = round(mean_count, 2)  # rounded mean count for label
  ) %>%
  # Optional: filter out groups with too few samples
  filter(n_samples >= 3)

# Set order of factor levels for plotting
df_counts_summary$Use <- factor(df_counts_summary$Use, levels = c("Additives", "Industrial", "Packaging", "Textiles"))

# Position dodge for side-by-side bars
dodge <- position_dodge(width = 0.6)

# Plot
ggplot(df_counts_summary, aes(x = Use, y = mean_count, pattern = Food.type)) +
  geom_col_pattern(
    position = dodge,
    width = 0.6,
    fill = "white",
    color = "black",
    pattern_fill = "black",
    pattern_angle = 45,
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    pattern_key_scale_factor = 0.6
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = dodge,
    width = 0.2
  ) +
  geom_text(
    aes(y = label_y, label = label_text),
    position = dodge,
    size = 4.5,
    color = "black"
  ) +
  scale_pattern_manual(values = c(Dry = "none", Wet = "stripe")) +
  labs(
    x = "Polymer Use",
    y = "Mean Number of Pieces per Sample",
    pattern = "Food Type"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 3), expand = expansion(mult = c(0, 0.1))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.line = element_line(color = "black", size = 0.5),
    panel.border = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_blank()
  )

ggsave("plots/polymer_use_type.pdf", P, width = 12, height = 6)

# Summarize data: mean, sd, se, total, and label positions
df_counts_summary <- df_polymer_t %>%
  group_by(Use, Target.animal) %>%
  summarise(
    mean_count = mean(n),             # mean count per sample
    sd_count = sd(n),                 # SD of counts
    n_samples = n(),                  # number of samples
    se_count = sd_count / sqrt(n_samples),  # SE of mean
    total = sum(n),                   # total counts (optional for labels)
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_count - se_count,
    upper = mean_count + se_count,
    label_y = upper + 0.2,             # position label just above error bar
    label_text = round(mean_count, 2)  # rounded mean count for label
  ) %>%
  # Optional: filter out groups with too few samples
  filter(n_samples >= 3)

# Set order of factor levels for plotting
df_counts_summary$Use <- factor(df_counts_summary$Use, levels = c("Additives", "Industrial", "Packaging", "Textiles"))

# Position dodge for side-by-side bars
dodge <- position_dodge(width = 0.6)

# Plot
ggplot(df_counts_summary, aes(x = Use, y = mean_count, fill = Target.animal)) +
  geom_col(
    position = dodge,
    width = 0.6,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = dodge,
    width = 0.2
  ) +
  geom_text(
    aes(y = label_y, label = label_text),
    position = dodge,
    size = 4.5,
    color = "black"
  ) +
  scale_fill_manual(values = c(
    Cat = "grey90",
    Dog = "grey50",
    Hedgehog = "grey10"
  )) +
  labs(
    x = "Polymer Use",
    y = "Mean Number of Pieces per Sample",
    fill = "Target Animal"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 3), expand = expansion(mult = c(0, 0.1))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.line = element_line(color = "black", size = 0.5),
    panel.border = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_blank()
  )
ggsave("plots/polymer_use_target.pdf", P, width = 11, height = 6)


#count not percentage 

# Filter out Additives and ensure factor levels
df_plot <- df_polymer_main %>%
  filter(Use != "Additives") %>%
  mutate(
    Use = factor(Use, levels = c("Industrial", "Packaging", "Textiles")),
    Price.category = factor(Price.category, levels = c("Value", "Mid range", "Premium"))
  )

# Define dodge position for grouped jitter
dodge <- position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)

ggplot(df_plot, aes(x = Use, y = n, color = Price.category)) +
  geom_jitter(position = dodge, size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Value" = "lightblue",
                                "Mid range" = "blue",
                                "Premium" = "darkblue")) +
  labs(
    x = "Polymer Use",
    y = "Number of particles per sample",
    color = "Price Category"
  ) +
  expand_limits(y = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),          # remove all grids
    axis.line = element_line(color = "black", size = 0.8),  # black x and y axis
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  )


# Set factor levels if needed
df_counts_summary$Use <- factor(df_counts_summary$Use, levels = c("Additives", "Industrial", "Packaging", "Textiles"))
df_counts_summary$Price.category <- factor(df_counts_summary$Price.category, levels = c("Value", "Mid range", "Premium"))

dodge <- position_dodge(width = 0.6)

ggplot(df_counts_summary, aes(x = Use, y = total_count, fill = Price.category)) +
  geom_col(position = dodge, width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.2) +
  geom_text(aes(y = label_y, label = label_text), position = dodge, size = 4.5, color = "black") +
  scale_fill_manual(values = c(
    "Value" = "lightblue",
    "Mid range" = "blue",
    "Premium" = "darkblue"
  )) +
  labs(
    x = "Polymer Use",
    y = "Total Count",
    fill = "Price Category"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.line = element_line(color = "black", size = 0.5),
    panel.border = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_blank()
  )

df_counts_summary <- df_price %>%
  group_by(Use, Price.category) %>%
  summarise(
    total_count = sum(n),       # total counts per group
    n_rows = n(),               # number of rows in group (for info)
    .groups = "drop"
  ) %>%
  mutate(
    poisson_error = sqrt(total_count),
    lower = total_count - poisson_error,
    upper = total_count + poisson_error,
    label_y = upper + 0.2,
    label_text = round(total_count, 0)
  )

# Set factor levels if needed
df_counts_summary$Use <- factor(df_counts_summary$Use, levels = c("Additives", "Industrial", "Packaging", "Textiles"))
df_counts_summary$Price.category <- factor(df_counts_summary$Price.category, levels = c("Value", "Mid range", "Premium"))

dodge <- position_dodge(width = 0.6)

ggplot(df_counts_summary, aes(x = Use, y = total_count, fill = Price.category)) +
  geom_col(position = dodge, width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.2) +
  geom_text(aes(y = label_y, label = label_text), position = dodge, size = 4.5, color = "black") +
  scale_fill_manual(values = c(
    "Value" = "lightblue",
    "Mid range" = "blue",
    "Premium" = "darkblue"
  )) +
  labs(
    x = "Polymer Use",
    y = "Total Count",
    fill = "Price Category"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.line = element_line(color = "black", size = 0.5),
    panel.border = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_blank()
  )

df_counts_summary <- df_price %>%
  group_by(Use, Price.category) %>%
  summarise(
    total_count = sum(n),
    n_rows = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sqrt(total_count),                # Poisson SE on total count
    lower = total_count - 1.96 * se,
    upper = total_count + 1.96 * se,
    lower = ifelse(lower < 0, 0, lower),  # no negative lower bound
    label_y = upper + 0.2,
    label_text = round(total_count, 0)
  )

df_price %>%
  filter(Use == "Textiles", Price.category == "Value") %>%
  summarise(
    n_samples = n(),
    mean_count = mean(n),
    sd_count = sd(n),
    min_count = min(n),
    max_count = max(n)
  )

df_price %>%
  filter(Use == "Textiles", Price.category == "Mid range") %>%
  summarise(
    n_samples = n(),
    mean_count = mean(n),
    sd_count = sd(n),
    min_count = min(n),
    max_count = max(n)
  )

# Set factor levels if needed
df_counts_summary$Use <- factor(df_counts_summary$Use, levels = c("Additives", "Industrial", "Packaging", "Textiles"))
df_counts_summary$Price.category <- factor(df_counts_summary$Price.category, levels = c("Value", "Mid range", "Premium"))

dodge <- position_dodge(width = 0.6)

ggplot(df_counts_summary, aes(x = Use, y = total_count, fill = Price.category)) +
  geom_col(position = dodge, width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = dodge, width = 0.2) +
  geom_text(aes(y = label_y, label = label_text), position = dodge, size = 4.5, color = "black") +
  scale_fill_manual(values = c(
    "Value" = "lightblue",
    "Mid range" = "blue",
    "Premium" = "darkblue"
  )) +
  labs(
    x = "Polymer Use",
    y = "Total Count",
    fill = "Price Category"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.line = element_line(color = "black", size = 0.5),
    panel.border = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_blank()
  )
