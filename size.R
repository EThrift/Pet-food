# Load necessary libraries
library(dplyr)
library(lme4)
library(tidyverse)
library(ggplot2)
library(glmmTMB)
library(lme4) 


df_sizes <- read_csv("size.csv")


# Remove spaces from column names
colnames(df_sizes) <- make.names(colnames(df_sizes))

#model relationship between taxonomic group and size

modelnull <- glm(Size ~ 1 ,  data = df_sizes)
summary(modelnull)

model1 <- glm(Size ~  Type ,  data = df_sizes)
summary(model1)

anova(modelnull, model1)

model2 <- glm(Size ~  Animal ,  data = df_sizes)
summary(model2)

anova(modelnull,model2)

model3 <- glm(Size ~  Price.category ,  data = df_sizes)
summary(model3)

anova(modelnull,model3)
#model 1 df 2 F stat 0.74, p =  0.47

# Load the DHARMa package
library(DHARMa)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = modelnull)

# Plot diagnostics
plot(sim_res)
#QQplot shows good fit

# Overdispersion test using DHARMa
testDispersion(sim_res)

# Create a box plot of Size by brand
library(ggplot2)

#have a function which means you can just call this when you want to calculate standard error 
std_err <- function(x) {
  SE <- sd(x)/sqrt(length(x))
  return(SE)
}


library(dplyr)
library(ggplot2)
library(scales)

# Calculate mean and SD per Animal group
df_summary <- df_sizes %>%
  group_by(Animal) %>%
  summarise(size_mean = mean(Size),
            size_SD = sd(Size))


P <- ggplot(df_sizes, aes(x = Animal, y = Size)) +
  geom_violin(fill = alpha("grey80", 0.4), color = "black") +
  geom_point(data = df_summary, aes(x = Animal, y = size_mean), 
             color = "blue", size = 4.5) +
  geom_errorbar(data = df_summary, aes(x = Animal, y = size_mean, 
                                       ymin = pmax(size_mean - size_SD, 0),  # prevents ymin < 0
                                       ymax = size_mean + size_SD),
                width = 0.2, color = "blue") +
  theme_minimal() +
  labs(x = "Target animal", y = "Size (mm)") +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  coord_cartesian(ylim = c(0, 4))  # keep if your data fits here well

P  # to display the plot

ggsave("plots/sizetarget.pdf", P, width = 10, height = 6)

# Calculate mean and SD per Animal group
df_summary <- df_sizes %>%
  group_by(Type) %>%
  summarise(size_mean = mean(Size),
            size_SD = sd(Size))

P <- ggplot(df_sizes, aes(x = Type, y = Size)) +
  geom_violin(fill = alpha("grey80", 0.4), color = "black") +
  geom_point(data = df_summary, aes(x = Type, y = size_mean), 
             color = "blue", size = 4.5) +
  geom_errorbar(data = df_summary, aes(x = Type, y = size_mean, 
                                       ymin = pmax(size_mean - size_SD, 0),  # prevent ymin < 0
                                       ymax = size_mean + size_SD),
                width = 0.2, color = "blue") +
  theme_minimal() +
  labs(x = "Food type", y = "Size (mm)") +   # Adjust x-axis label if needed
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  coord_cartesian(ylim = c(0, 4))
print(P)

ggsave("plots/sizetype.pdf", P, width = 10, height = 6)


# Calculate mean and SD per Animal group
df_summary <- df_sizes %>%
  group_by(Price.category) %>%
  summarise(size_mean = mean(Size),
            size_SD = sd(Size))

df_sizes$Price.category <- factor(df_sizes$Price.category, levels = c("Value", "Mid range", "Premium"))
P <- ggplot(df_sizes, aes(x = Price.category, y = Size)) +
  geom_violin(fill = alpha("grey80", 0.4), color = "black") +
  geom_point(data = df_summary, aes(x = Price.category, y = size_mean), 
             color = "blue", size = 4.5) +
  geom_errorbar(data = df_summary, aes(x = Price.category, y = size_mean, 
                                       ymin = pmax(size_mean - size_SD, 0),  # prevent ymin < 0
                                       ymax = size_mean + size_SD),
                width = 0.2, color = "blue") +
  theme_minimal() +
  labs(x = "Price category", y = "Size (mm)") +   # Adjust x-axis label if needed
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  coord_cartesian(ylim = c(0, 4))
print(P)

ggsave("plots/sizeprice.pdf", P, width = 10, height = 6)

