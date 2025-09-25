#created on 23/09/24
# By Emily Thrift
# script for a glm to assess differences in shape of plastics between different foods, animals and prices 


rm(list = ls())

library(tidyverse)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(emmeans)
library(DHARMa)

#add in working location

setwd('C:/Users/Emily Thrift/Desktop/Supplementary feed potential mps')
list.files()

#load dataframe 

df_shape <- read.csv("Fragment.csv")


# Remove spaces from column names
colnames(df_shape) <- make.names(colnames(df_shape))


#check for perfect separation 
table(df_shape$Target.animal,df_shape$Fragment)

Fragmentnull <- glm(Fragment ~ 1, 
                    data = df_shape, 
                    family = binomial (link = "logit"))
summary(Fragmentnull)

Fragment1 <- glm(Fragment
                        ~ Food.type, 
                        data = df_shape, 
                        family = binomial (link = "logit"))
summary(Fragment1)

anova(Fragmentnull, Fragment1)
Fragment2 <- glm(Fragment
                          ~  Target.animal, 
                          data = df_shape, 
                          family = binomial (link = "logit"))

summary(Fragment2)

anova(Fragmentnull, Fragment2)

Fragment3 <- glm(Fragment
                 ~  Price.category, 
                 data = df_shape, 
                 family = binomial (link = "logit"))
summary(Fragment3)

anova(Fragmentnull, Fragment3)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = Fragment1)

# Plot diagnostics
plot(sim_res)
#QQplot shows bad fit deviations detected

# Overdispersion test using DHARMa
testDispersion(sim_res)

df_shape <- read.csv("fibre.csv")

# Remove spaces from column names
colnames(df_shape) <- make.names(colnames(df_shape))

Fibrenull <- glm(Fibre
                  ~ 1, 
                  data = df_shape, 
                  family = binomial (link = "logit"))
summary(Fibrenull)


Fibre1 <- glm(Fibre
                     ~ Food.type, 
                     data = df_shape, 
                     family = binomial (link = "logit"))
summary(Fibre1)

anova(Fibrenull, Fibre1)


Fibre2 <- glm(Fibre
                     ~ Target.animal, 
                     data = df_shape, 
                     family = binomial (link = "logit"))

summary(Fibre2)

anova(Fibrenull, Fibre2)

Fibre3 <- glm(Fibre
              ~   Price.category, 
              data = df_shape, 
              family = binomial (link = "logit"))

summary(Fibre3)

anova(Fibrenull, Fibre3)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = Fibre2)

# Plot diagnostics
plot(sim_res)
#QQplot shows bad fit deviations detected

# Overdispersion test using DHARMa
testDispersion(sim_res)


df_type <- read_csv("Food type shape plot.csv")


# Fix column names
colnames(df_type) <- make.names(colnames(df_type))

library(binom)

# Calculate Wilson confidence intervals
wilson_ci2 <- binom.confint(x = df_type$n, n = df_type$Total, method = "wilson")

# Add confidence intervals to the data frame
df_type <- df_type %>%
  mutate(lower = wilson_ci2$lower,
         upper = wilson_ci2$upper)

# Calculate label positions just above error bars
df_type <- df_type %>%
  mutate(label_y = upper * 100 + 2.0) 
# Summarize the total for each soil.group and add it as a separate column
# Summarize totals for each soil group
totals <- df_type %>%
  group_by(Type) %>%
  summarize(Total = unique(Total), .groups = 'drop')

# Join totals back to the original data frame
df_type <- df_type %>%
  left_join(totals, by = "Type")

P <- ggplot(df_type, aes(x = Type, y = Percentage, fill = Shape)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), 
                width = 0.3, position = position_dodge(width = 0.8)) +
  geom_text(aes(y = label_y, label = n), 
            size = 4.5, position = position_dodge(width = 0.8), vjust = -0.5, fontface = "plain") +
  geom_text(data = totals, aes(x = Type, y = -5, label = paste("N =", Total)), 
            vjust = 1.5, size = 4, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Fibre" = "grey80", "Fragment" = "white")) +
  labs(x = "Food type", y = "% Presence") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(limits = c(0, 100))

ggsave("plots/shapetype.pdf", P, width = 10, height = 6)

P

df_type <- read_csv("Target animal shape plot.csv")


# Fix column names
colnames(df_type) <- make.names(colnames(df_type))

# Calculate Wilson confidence intervals
wilson_ci2 <- binom.confint(x = df_type$n, n = df_type$Total, method = "wilson")

# Add confidence intervals to the data frame
df_type <- df_type %>%
  mutate(lower = wilson_ci2$lower,
         upper = wilson_ci2$upper)

# Calculate label positions just above error bars
df_type <- df_type %>%
  mutate(label_y = upper * 100 + 2.0) 
# Summarize the total for each soil.group and add it as a separate column
# Summarize totals for each soil group
totals <- df_type %>%
  group_by(Target.animal) %>%
  summarize(Total = unique(Total), .groups = 'drop')

# Join totals back to the original data frame
df_type <- df_type %>%
  left_join(totals, by = "Target.animal")

P <- ggplot(df_type, aes(x = Target.animal, y = Percentage, fill = Shape)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), 
                width = 0.3, position = position_dodge(width = 0.8)) +
  geom_text(aes(y = label_y, label = n), 
            size = 4.5, position = position_dodge(width = 0.8), vjust = -0.5, fontface = "plain") +
  geom_text(data = totals, aes(x = Target.animal, y = -5, label = paste("N =", Total)), 
            vjust = 1.5, size = 4.5, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Fibre" = "grey80", "Fragment" = "white")) +
  labs(x = "Target animal", y = "% Presence") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(limits = c(0, 100))
ggsave("plots/shapetarget.pdf", P, width = 10, height = 6)
P

df_type <- read_csv("Price shape plot.csv")


# Fix column names
colnames(df_type) <- make.names(colnames(df_type))

# Calculate Wilson confidence intervals
wilson_ci2 <- binom.confint(x = df_type$n, n = df_type$Total, method = "wilson")

# Add confidence intervals to the data frame
df_type <- df_type %>%
  mutate(lower = wilson_ci2$lower,
         upper = wilson_ci2$upper)

# Calculate label positions just above error bars
df_type <- df_type %>%
  mutate(label_y = upper * 100 + 2.0) 
# Summarize the total for each soil.group and add it as a separate column
# Summarize totals for each soil group
totals <- df_type %>%
  group_by(Price.category) %>%
  summarize(Total = unique(Total), .groups = 'drop')

# Join totals back to the original data frame
df_type <- df_type %>%
  left_join(totals, by = "Price.category")

P <- ggplot(df_type, aes(x = Price.category, y = Percentage, fill = Shape)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), 
                width = 0.3, position = position_dodge(width = 0.8)) +
  geom_text(aes(y = label_y, label = n), 
            size = 4.5, position = position_dodge(width = 0.8), vjust = -0.5, fontface = "plain") +
  geom_text(data = totals, aes(x = Price.category, y = -5, label = paste("N =", Total)), 
            vjust = 1.5, size = 4, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Fibre" = "grey80", "Fragment" = "white")) +
  labs(x = "Price category", y = "% Presence") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(limits = c(0, 100))

ggsave("plots/shapeprice.pdf", P, width = 10, height = 6)

P
