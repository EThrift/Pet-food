#created on 20/08/24
# By Emily Thrift
# script for a glm to assess the levels of plastic presence in the brands of supplementary feed

#load libraries
library(tidyverse)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(emmeans)
library(car)

#add in working location

setwd('C:/Users/Emily Thrift/Desktop
/Inverts')
list.files()

#load dataframe 

df_presence <- read_csv("Overall plastic presence All.csv")


# Remove spaces from column names
colnames(df_presence) <- make.names(colnames(df_presence))

df_presence_p <- read_csv("Overall plastic presence product.csv")


# Remove spaces from column names
colnames(df_presence_p) <- make.names(colnames(df_presence_p))

# Cross-tabulate Brand by Target.animal
brand_animal_table <- table(df_presence$Brand, df_presence$Target.animal)

# View the cross-tabulation
print(brand_animal_table)

library(ggplot2)
library(reshape2)

# Melt the table for ggplot
brand_animal_melt <- melt(brand_animal_table)

# Create heatmap with custom color scale
ggplot(brand_animal_melt, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "grey", "black"),  # Customize colors if needed
    values = scales::rescale(c(0, 6, 12)),  # Scale 0, 6, and 12 to 0-1 range
    breaks = c(0, 6, 12),  # Set the breaks for the color scale
    limits = c(0, 12)      # Set the range of the color scale
  ) +
  labs(x = "Target Animal", y = "Brand", fill = "Count") +
  theme_minimal()

#test multicollinearity 
modelall <- glm(Plastic.present ~ Price.category + Brand  + Food.type, 
             family = binomial, data = df_presence)

# Check Variance Inflation Factor
vif(modelall)

modelprice <- glm(Plastic.present ~ Price.category + Food.type, 
                family = binomial, data = df_presence)

# Check Variance Inflation Factor
vif(modelprice)

#mmodel brand and price separately

# GLM analysis

model_null <- glm(Plastic.present ~ 1, data = df_presence, family = binomial)

model1animal <- glm(Plastic.present ~ Food.type, 
                    data = df_presence, 
                    family = binomial(link = "logit"))
summary(model1animal)
anova(model_null, model1animal)

model2animal <- glm(Plastic.present ~  Target.animal, 
                    data = df_presence, 
                    family = binomial(link = "logit"))
summary(model2animal)

anova(model_null, model2animal)

model3animal <- glm(Plastic.present ~  Price.category, 
                    data = df_presence, 
                    family = binomial(link = "logit"))
summary(model3animal)

anova(model_null, model3animal)

# Load the DHARMa package
library(DHARMa)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = model3animal)


# Plot diagnostics
plot(sim_res)
#QQplot shows good fit

# Overdispersion test using DHARMa
testDispersion(sim_res)
#no evidence of overdispersion p = 0.84

library(emmeans)
#pairwise comparisons 
# Calculate the estimated marginal means for Taxonomic.group
emmeans_price <- emmeans(model3animal, ~ Price.category)

# Perform pairwise comparisons
pairwise_comparisons <- contrast(emmeans_price, method = "pairwise")

# View the results of the pairwise comparisons
summary(pairwise_comparisons)

#sample level 
table(df_presence$Plastic.present)
chisq.test(table(df_presence$Plastic.present))

#product level
table(df_presence_p$Plastic.present)
chisq.test(table(df_presence_p$Plastic.present))


# Hedgehog-only dataframe
df_hedgehog <- df_presence %>%
  filter(Target.animal == "Hedgehog")

# Cats and dogs dataframe
df_cats_dogs <- df_presence %>%
  filter(Target.animal %in% c("Cat", "Dog"))

# Proceed with GLMM analysis on the filtered data
modelnull <- glm(Plastic.present ~ 1, 
                       data = df_cats_dogs, 
                       family = binomial(link = "logit"))
summary(modelnull)

model1catanddog <- glm(Plastic.present ~ Food.type, 
                     data = df_cats_dogs, 
                     family = binomial(link = "logit"))
summary(model1catanddog)


anova(modelnull, model1catanddog)

model2catanddog <- glm(Plastic.present ~ Target.animal, 
                       data = df_cats_dogs, 
                       family = binomial(link = "logit"))
summary(model2catanddog)


anova(modelnull, model2catanddog)

model3catanddog <- glm(Plastic.present ~ Brand, 
                     data = df_cats_dogs, 
                     family = binomial(link = "logit"))
summary(model3catanddog)

anova(modelnull, model3catanddog)



#model 2 better p = <0.0001

# Load the DHARMa package
library(DHARMa)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = model3catanddog)


# Plot diagnostics
plot(sim_res)
#QQplot shows good fit

# Overdispersion test using DHARMa
testDispersion(sim_res)
#no evidence of overdispersion p = 0.86



library(emmeans)
#pairwise comparisons 
# Calculate the estimated marginal means for Taxonomic.group
emmeans_brand <- emmeans(model3catanddog, ~ Brand)

# Perform pairwise comparisons
pairwise_comparisons <- contrast(emmeans_brand, method = "pairwise")

# View the results of the pairwise comparisons
summary(pairwise_comparisons)

#Iams - Whiskas	p = 0.08


# Cross-tabulate the 'brand' and 'microplastic_presence' variables
contingency_table <- table(df_hedgehog$Brand, df_hedgehog$Plastic.present)

# Run the Chi-square test
chi_square_result <- chisq.test(contingency_table)

# Output the results
chi_square_result

#Chi square 7.57, df = 7, p = 0.37

df_presence_plot <- read_csv("Overall plastic presence target animal plot.csv")


# 0Remove spaces from column names
colnames(df_presence_plot) <- make.names(colnames(df_presence_plot))

library(binom)
# Calculate Wilson confidence intervals correctly
wilson_ci <- binom.confint(df_presence_plot$n, 
                           df_presence_plot$Total, 
                           method = "wilson")

df_presence_plot <- df_presence_plot %>%
  mutate(Lower = wilson_ci$lower * 100,  # Convert to percentage
         Upper = wilson_ci$upper * 100,
         label_y = Upper + 3.5)  # Position labels above error bars

P <- ggplot(df_presence_plot, aes(x = Target.animal, y = Percentage)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", aes(fill = Target.animal)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 6, position = position_dodge(width = 0.9), fontface = "plain", vjust = -0.5) +
  labs(x = "Target Animal", y = "% Plastic Prevalence") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"   # ✅ removes legend
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Cat" = "grey", "Dog" = "white", "Hedgehog" = "grey30"))

ggsave("plots/target.pdf", P, width = 9, height = 6)

P

df_presence_plot <- read_csv("Overall plastic presence target animal plot 2.csv")


# 0Remove spaces from column names
colnames(df_presence_plot) <- make.names(colnames(df_presence_plot))

library(binom)
# Calculate Wilson confidence intervals correctly
wilson_ci <- binom.confint(df_presence_plot$n, 
                           df_presence_plot$Total, 
                           method = "wilson")

df_presence_plot <- df_presence_plot %>%
  mutate(Lower = wilson_ci$lower * 100,  # Convert to percentage
         Upper = wilson_ci$upper * 100,
         label_y = Upper + 3.5)  # Position labels above error bars

P <- ggplot(df_presence_plot, aes(x = Target.animal, y = Percentage)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", aes(fill = Target.animal)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 6, position = position_dodge(width = 0.9), fontface = "plain", vjust = -0.5) +
  labs(x = "Target Animal", y = "% Plastic Prevalence") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none"   # ✅ removes legend
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Cat" = "grey", "Dog" = "white", "Hedgehog" = "grey30"))

P

ggsave("plots/target2.pdf", P, width = 9, height = 6)

# Read in the CSV
df_presence_plot <- read_csv("Overall plastic presence plot.csv")

# Clean column names
colnames(df_presence_plot) <- make.names(colnames(df_presence_plot))

# Calculate Wilson confidence intervals
wilson_ci <- binom.confint(df_presence_plot$n, 
                           df_presence_plot$Total, 
                           method = "wilson")

# Add lower/upper bounds and label position to the data
df_presence_plot <- df_presence_plot %>%
  mutate(Lower = wilson_ci$lower * 100,  # Convert to %
         Upper = wilson_ci$upper * 100,
         label_y = Upper + 3.5)  # Offset labels above error bars

P <- ggplot(df_presence_plot, aes(x = Food.type, y = Percentage, fill = Food.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)), 
            size = 6, position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Food type", y = "% Plastic Prevalence") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Set to "right" if you want the legend back
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Wet" = "grey", "Dry" = "white"))  # ✅ Set Wet/Dry colors

ggsave("plots/food.pdf", P, width = 9, height = 6)

# Read in the CSV
df_presence_plot <- read_csv("Overall plastic presence plot 2.csv")

# Clean column names
colnames(df_presence_plot) <- make.names(colnames(df_presence_plot))

# Calculate Wilson confidence intervals
wilson_ci <- binom.confint(df_presence_plot$n, 
                           df_presence_plot$Total, 
                           method = "wilson")

# Add lower/upper bounds and label position to the data
df_presence_plot <- df_presence_plot %>%
  mutate(Lower = wilson_ci$lower * 100,  # Convert to %
         Upper = wilson_ci$upper * 100,
         label_y = Upper + 3.5)  # Offset labels above error bars

P <- ggplot(df_presence_plot, aes(x = Food.type, y = Percentage, fill = Food.type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)), 
            size = 6, position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Food type", y = "% Plastic Prevalence") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Set to "right" if you want the legend back
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Wet" = "grey", "Dry" = "white"))  # ✅ Set Wet/Dry colors

ggsave("plots/food2.pdf", P, width = 9, height = 6)

df_presence_plot_price <- read_csv("Price presence plot.csv")


# 0Remove spaces from column names
colnames(df_presence_plot_price) <- make.names(colnames(df_presence_plot_price))

library(binom)
# Calculate Wilson confidence intervals correctly
wilson_ci <- binom.confint(df_presence_plot_price$n, 
                           df_presence_plot_price$Total, 
                           method = "wilson")

df_presence_plot_price <- df_presence_plot_price %>%
  mutate(Lower = wilson_ci$lower * 100,  # Convert to percentage
         Upper = wilson_ci$upper * 100,
         label_y = Upper + 3.5)  # Position labels above error bars

df_presence_plot_price$Price.per.kg <- factor(df_presence_plot_price$Price.per.kg,
                                              levels = c("Value", "Mid range", "Premium"))


P <- ggplot(df_presence_plot_price, aes(x = Price.per.kg, y = Percentage)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", aes(fill = Price.per.kg)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 6, position = position_dodge(width = 0.9), fontface = "plain", vjust = -0.5) +
  labs(x = "Price category", y = "% Plastic Prevalence") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "none",   # ✅ removes legend
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5)  # 🔧 Adjust left margin here
      ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Value" = "grey", "Mid range" = "white", "Premium" = "grey30"))

ggsave("plots/price.pdf", P, width = 9, height = 6)


df_presence_plot_price <- read_csv("Price presence plot 2.csv")


# 0Remove spaces from column names
colnames(df_presence_plot_price) <- make.names(colnames(df_presence_plot_price))

library(binom)
# Calculate Wilson confidence intervals correctly
wilson_ci <- binom.confint(df_presence_plot_price$n, 
                           df_presence_plot_price$Total, 
                           method = "wilson")

df_presence_plot_price <- df_presence_plot_price %>%
  mutate(Lower = wilson_ci$lower * 100,  # Convert to percentage
         Upper = wilson_ci$upper * 100,
         label_y = Upper + 3.5)  # Position labels above error bars

df_presence_plot_price$Price.per.kg <- factor(df_presence_plot_price$Price.per.kg,
                                              levels = c("Value", "Mid range", "Premium"))

P <- ggplot(df_presence_plot_price, aes(x = Price.per.kg, y = Percentage)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black", aes(fill = Price.per.kg)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 6, position = position_dodge(width = 0.9), fontface = "plain", vjust = -0.5) +
  labs(x = "Price category", y = "% Plastic Prevalence") +
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
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 25),  # ⬅️ breaks at 0, 25, 50, 75, 100
    expand = expansion(mult = c(0, 0.1))
  )+
  scale_fill_manual(values = c("Value" = "grey", "Mid range" = "white", "Premium" = "grey30"))

ggsave("plots/price2.pdf", P, width = 9, height = 6)

# Read both CSVs
df_samples <- read_csv("Overall plastic presence target animal plot.csv") %>%
  mutate(Type = "Sample")
df_products <- read_csv("Overall plastic presence target animal plot 2.csv") %>%
  mutate(Type = "Product")

# Combine and clean
df_combined <- bind_rows(df_samples, df_products)
colnames(df_combined) <- make.names(colnames(df_combined))

# Calculate Wilson confidence intervals
wilson_ci <- binom.confint(df_combined$n, df_combined$Total, method = "wilson")

df_combined <- df_combined %>%
  mutate(
    Lower = wilson_ci$lower * 100,
    Upper = wilson_ci$upper * 100,
    label_y = Upper + 3.5
  )

# Plot
P <- ggplot(df_combined, aes(x = Target.animal, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 5.5, position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Target Animal", y = "% Plastic Prevalence", fill = "Type") +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Sample" = "grey", "Product" = "white")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.position = "right"  # ✅ legend moved to the right
  )
P
ggsave("plots/target_combined.pdf", P, width = 9, height = 6)

#combined food type plot
# Read both datasets
df_samples <- read_csv("Overall plastic presence plot.csv") %>%
  mutate(Type = "Sample")
df_products <- read_csv("Overall plastic presence plot 2.csv") %>%
  mutate(Type = "Product")

# Combine and clean
df_combined <- bind_rows(df_samples, df_products)
colnames(df_combined) <- make.names(colnames(df_combined))

# Calculate Wilson confidence intervals
wilson_ci <- binom.confint(df_combined$n, df_combined$Total, method = "wilson")

df_combined <- df_combined %>%
  mutate(
    Lower = wilson_ci$lower * 100,
    Upper = wilson_ci$upper * 100,
    label_y = Upper + 3.5
  )

# Plot
P <- ggplot(df_combined, aes(x = Food.type, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 5.5, position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Food Type", y = "% Plastic Prevalence", fill = "Type") +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("Sample" = "grey", "Product" = "white")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.text = element_text(size = 16),
    legend.title  = element_text(size = 18),
    legend.position = "right"  # ✅ legend on the right
  )
P
ggsave("plots/food_combined.pdf", P, width = 9, height = 6)

#combined price plot

# Read both CSVs for price category
df_samples_price <- read_csv("Price presence plot.csv") %>%
  mutate(Type = "Sample")
df_products_price <- read_csv("Price presence plot 2.csv") %>%
  mutate(Type = "Product")

# Combine datasets
df_combined_price <- bind_rows(df_samples_price, df_products_price)

# Clean column names
colnames(df_combined_price) <- make.names(colnames(df_combined_price))

# Ensure Price.per.kg is a factor with the right order
df_combined_price$Price.per.kg <- factor(df_combined_price$Price.per.kg,
                                         levels = c("Value", "Mid range", "Premium"))

# Calculate Wilson confidence intervals
wilson_ci <- binom.confint(df_combined_price$n, 
                           df_combined_price$Total, 
                           method = "wilson")

df_combined_price <- df_combined_price %>%
  mutate(
    Lower = wilson_ci$lower * 100,  # Convert to percentage
    Upper = wilson_ci$upper * 100,
    label_y = Upper + 3.5  # Label position above error bars
  )

P <- ggplot(df_combined_price, aes(x = Price.per.kg, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.4, position = position_dodge(width = 0.9)) +
  geom_text(aes(y = label_y, label = paste(n, "/", Total)),
            size = 6, position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(x = "Price category", y = "% Plastic Prevalence", fill = "Type") +
  scale_y_continuous(
    limits = c(0, 105), 
    breaks = seq(0, 100, by = 25),     # <--- breaks in increments of 10
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = c("Sample" = "grey", "Product" = "white")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 5)
  )

P
ggsave("plots/price_combined.pdf", P, width = 9, height = 6)
