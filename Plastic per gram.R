#created on 20/08/24
# By Emily Thrift
# script for a glm to assess the levels of plastic presence in the brands of supplementary feed

#load libraries
library(tidyverse)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(emmeans)

#add in working location



library(ggplot2)
library(reshape2)

df_presence <- read_csv("Plastic per gram.csv")


# Remove spaces from column names
colnames(df_presence) <- make.names(colnames(df_presence))

# Proceed with GLMM analysis on the filtered data
modelnull <- glm(n ~ 1, 
                    data = df_presence, 
                    family = quasipoisson)
# Proceed with GLMM analysis on the filtered data
model1animal <- glm(n ~ Type, 
                    data = df_presence, 
                    family = quasipoisson)
summary(model1animal)

anova(modelnull, model1animal)

library(performance)
check_overdispersion(model1animal)
check_model(model1animal)


model2animal <- glm(n ~ Type + Target.animal, 
                    data = df_presence, 
                    family = quasipoisson)
summary(model2animal)

anova(model1animal, model2animal)

model3animal <- glm(n ~ Type +  Price.category, 
                    data = df_presence, 
                    family = quasipoisson)
summary(model3animal)

anova(model1animal, model3animal)

library(dplyr)
library(ggplot2)
library(scales)

# Calculate mean and SD per Animal group
df_summary <- df_presence %>%
  group_by(Target.animal) %>%
  summarise(size_mean = mean(n),
            size_SD = sd(n))

library(ggplot2)
library(scales)  # for alpha()

P <- ggplot(df_presence, aes(x = Target.animal, y = n)) +
  geom_violin(fill = alpha("grey80", 0.4), color = "black") +
  geom_point(data = df_summary, aes(x = Target.animal, y = size_mean), 
             color = "blue", size = 4.5) +
  geom_errorbar(data = df_summary, aes(x = Target.animal, y = size_mean, 
                                       ymin = pmax(size_mean - size_SD, 1),  # clip at 1
                                       ymax = size_mean + size_SD),
                width = 0.2, color = "blue")+
  theme_minimal() +
  labs(x = "Target animal", y = "Plastic per gram") +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  coord_cartesian(ylim = c(1, 4))  # keep if your data fits here well

P  # to display the plot

ggsave("plots/gramtarget.pdf", P, width = 10, height = 6)


# Calculate mean and SD per type
df_summary <- df_presence %>%
  group_by(Type) %>%
  summarise(size_mean = mean(n),
            size_SD = sd(n))


P <- ggplot(df_presence, aes(x = Type, y = n)) +
  geom_violin(fill = alpha("grey80", 0.4), color = "black") +
  geom_point(data = df_summary, aes(x = Type, y = size_mean), 
             color = "blue", size = 4.5) +
  geom_errorbar(data = df_summary, aes(x = Type, y = size_mean, 
                                       ymin = pmax(size_mean - size_SD, 1),
                                       ymax = size_mean + size_SD),
                width = 0.2, color = "blue") +
  labs(x = "Food type", y = "Plastic per gram") +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black")
  ) +
  coord_cartesian(ylim = c(1, 4))

P  # to display the plot

ggsave("plots/gramtype.pdf", P, width = 10, height = 6)

# Relevel Price.category to desired order
df_presence$Price.category <- factor(df_presence$Price.category,
                                     levels = c("Value", "Mid range", "Premium"))
# Calculate mean and SD per type
df_summary <- df_presence %>%
  group_by(Price.category) %>%
  summarise(size_mean = mean(n),
            size_SD = sd(n))


P <- ggplot(df_presence, aes(x = Price.category, y = n)) +
  geom_violin(fill = alpha("grey80", 0.4), color = "black") +
  geom_point(data = df_summary, aes(x = Price.category, y = size_mean), 
             color = "blue", size = 4.5) +
  geom_errorbar(data = df_summary, aes(x = Price.category, y = size_mean, 
                                       ymin = pmax(size_mean - size_SD, 1),  # clip at 1
                                       ymax = size_mean + size_SD),
                width = 0.2, color = "blue") +
  theme_minimal() +
  labs(x = "Price category", y = "Plastic per gram") +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  coord_cartesian(ylim = c(1, 4))  # keep if your data fits here well

P  # to display the plot

ggsave("plots/gramprice.pdf", P, width = 10, height = 6)
