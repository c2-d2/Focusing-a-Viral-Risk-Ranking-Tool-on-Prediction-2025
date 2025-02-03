# Set working directory
setwd("INSERT WORKING DIRECTORY")

# Load libraries
library(tidyverse)
library(dplyr)

# Load data
library("readxl")
spillover <- read_excel("structured_spillover_ranking_weighted.xlsx")

# Label the risk factors 
non_spillover_dependent_factors <- c("Host plasticity - No. of species - Weighted Score",	
           "Host plasticity - No. of orders - Weighted Score",
           "Geography of the host(s) - Weighted Score",
           "Number of primary high-risk disease transmission interfaces where the virus has been detected - Weighted Score",	
           "Genetic relatedness between the host species and humans - Weighted Score",	
           "Land use in host ecosystem - Weighted Score",
           "Livestock density in host ecosystem - Weighted Score",	
           "Human population density in host ecosystem - Weighted Score",	
           "Deforestation in host ecosystem - Weighted Score",	
           "Urbanization in host ecosystem - Weighted Score",	
           "Agricultural system change in host ecosystem - Weighted Score",	
           "Genome classification of the virus - Weighted Score",
           "Envelope status of the virus - Weighted Score",	
           "Viral genome segmentation - Weighted Score",	
           "Virus species infectivity in terrestrial mammals (excluding humans) - Weighted Score",	
           "Proportion of virus species known to infect terrestrial mammals (excluding humans) in the viral family - Weighted Score",	
           "Proportion of viruses within a viral family that are known to infect more than 1 host species - Weighted Score",	
           "Geography of the virus in animals - Weighted Score",	
           "Transmission mode of the virus - Weighted Score",	
           "Frequency of interaction between domestic animals and humans in the host ecosystem - Weighted Score",	
           "Intimacy of interaction between domestic animals and humans in the host ecosystem - Weighted Score",
           "Frequency of interaction between wild animals and humans in the host ecosystem - Weighted Score",
           "Intimacy of interaction between wild animals and humans in the host ecosystem - Weighted Score")
spillover_dependent_factors <- c("Virus species infectivity in humans - Weighted Score",
         "Proportion of virus species known to infect humans in the viral family - Weighted Score",	
         "Epidemicity of the virus species - Weighted Score",
         "Animal to human transmission - Weighted Score",	
         "Human to human transmission - Weighted Score",	
         "Duration of virus species infection in humans - Weighted Score",	
         "Pandemic virus - Weighted Score",
         "Proportion of known human pathogens in the viral family - Weighted Score")

# Calculate original scores (sum of all columns labeled spillover_dependent_factors and non_spillover_factors)
original_scores <- rowSums(spillover[, c(spillover_dependent_factors, non_spillover_dependent_factors)], na.rm = TRUE)

# Calculate adjusted scores (sum of non_spillover_factors columns)
adjusted_scores <- rowSums(spillover[, c(non_spillover_dependent_factors)], na.rm = TRUE)

# Normalize the scores to the highest scoring virus
normalized_original_scores <- original_scores / max(original_scores, na.rm = TRUE)
normalized_adjusted_scores <- adjusted_scores / max(adjusted_scores, na.rm = TRUE)

# Rank the scores (descending order)
original_ranks <- rank(-original_scores)
adjusted_ranks <- rank(-adjusted_scores)

# Create a data frame for plotting
plot_spillover <- data.frame(
  VirusName = spillover$`Virus Name`,
  VirusSpecies = spillover$`Virus Species`,
  VirusFamily = spillover$`Virus Family`,
  VirusGenus = spillover$`Virus Genus`,	
  VirusDistribution = spillover$`Virus Distribution`,
  HostDistribution= spillover$`Host Distribution`,
  HostPlasticity_Species= spillover$`Host Plasticity (Species)`,
  HostPlasticity_Order= spillover$`Host Plasticity (Order)`,
  HumanVirus = spillover$`Human Virus?`,
  ZoonoticVirus = spillover$`Zoonotic Virus?`,
  HumanTransmission = spillover$`Human Transmission?`,
  RiskLevels = spillover$`Risk Levels`,
  DataAvailability = spillover$`Data Availability`,
  NormalizedOriginalScores = normalized_original_scores,
  NormalizedAdjustedScores = normalized_adjusted_scores,
  OriginalRanks = original_ranks,
  AdjustedRanks = adjusted_ranks,
  OriginalScores = original_scores,
  AdjustedScores = adjusted_scores
)

# Identify the top ten highest and lowest original scores
top_10_original <- plot_spillover %>% arrange(desc(NormalizedOriginalScores)) %>% head(10)
bottom_10_original <- plot_spillover %>% arrange(NormalizedOriginalScores) %>% head(10)

# Identify the top ten highest and lowest adjusted scores
top_10_adjusted <- plot_spillover %>% arrange(desc(NormalizedAdjustedScores)) %>% head(10)
bottom_10_adjusted <- plot_spillover %>% arrange(NormalizedAdjustedScores) %>% head(10)

# Shortened names for plotting
short_factor_name <- c(
  "Host plasticity - No. of species - Weighted Score" = "Host plasticity - No. of species",
  "Host plasticity - No. of orders - Weighted Score" = "Host plasticity - No. of orders",
  "Geography of the host(s) - Weighted Score" = "Geography of the host(s)",
  "Number of primary high-risk disease transmission interfaces where the virus has been detected - Weighted Score" = "Number of primary high-risk disease transmission interfaces where the virus has been detected",	
  "Genetic relatedness between the host species and humans - Weighted Score" = "Genetic relatedness between the host species and humans",	
  "Land use in host ecosystem - Weighted Score" = "Land use in host ecosystem",
  "Livestock density in host ecosystem - Weighted Score" = "Livestock density in host ecosystem",	
  "Human population density in host ecosystem - Weighted Score" = "Human population density in host ecosystem",	
  "Deforestation in host ecosystem - Weighted Score" = "Deforestation in host ecosystem",	
  "Urbanization in host ecosystem - Weighted Score" = "Urbanization in host ecosystem",	
  "Agricultural system change in host ecosystem - Weighted Score" = "Agricultural system change in host ecosystem",	
  "Genome classification of the virus - Weighted Score" = "Genome classification of the virus",
  "Envelope status of the virus - Weighted Score" = "Envelope status of the virus",	
  "Viral genome segmentation - Weighted Score" = "Viral genome segmentation",	
  "Virus species infectivity in terrestrial mammals (excluding humans) - Weighted Score" = "Virus species infectivity in terrestrial mammals (excluding humans)",	
  "Proportion of virus species known to infect terrestrial mammals (excluding humans) in the viral family - Weighted Score" = "Proportion of virus species known to infect terrestrial mammals (excluding humans) in the viral family",	
  "Proportion of viruses within a viral family that are known to infect more than 1 host species - Weighted Score" = "Proportion of viruses within a viral family that are known to infect more than 1 host species",	
  "Geography of the virus in animals - Weighted Score" = "Geography of the virus in animals",	
  "Transmission mode of the virus - Weighted Score" = "Transmission mode of the virus",	
  "Frequency of interaction between domestic animals and humans in the host ecosystem - Weighted Score" = "Frequency of interaction between domestic animals and humans in the host ecosystem",	
  "Intimacy of interaction between domestic animals and humans in the host ecosystem - Weighted Score" = "Intimacy of interaction between domestic animals and humans in the host ecosystem",
  "Frequency of interaction between wild animals and humans in the host ecosystem - Weighted Score" = "Frequency of interaction between wild animals and humans in the host ecosystem",
  "Intimacy of interaction between wild animals and humans in the host ecosystem - Weighted Score" = "Intimacy of interaction between wild animals and humans in the host ecosystem",
  "Virus species infectivity in humans - Weighted Score" = "Virus species infectivity in humans",
  "Proportion of virus species known to infect humans in the viral family - Weighted Score" = "Proportion of virus species known to infect humans in the viral family",	
  "Epidemicity of the virus species - Weighted Score" = "Epidemicity of the virus species",
  "Animal to human transmission - Weighted Score" = "Animal to human transmission",	
  "Human to human transmission - Weighted Score" = "Human to human transmission",	
  "Duration of virus species infection in humans - Weighted Score" = "Duration of virus species infection in humans",	
  "Pandemic virus - Weighted Score" = "Pandemic virus",
  "Proportion of known human pathogens in the viral family - Weighted Score" = "Proportion of known human pathogens in the viral family")

# Load fonts for plotting
library(extrafont)
font_import()
fonts()  

### FIGURE 1. Normalized Original vs. Adjusted Risk Scores by Virus Family and Human Virus Classification

# Load library
library(ggplot2)

# Define custom shapes for viral families
custom_shapes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)

# Plot with shapes by VirusFamily and colors by HumanVirus
ggplot(plot_spillover, aes(x = NormalizedOriginalScores, y = NormalizedAdjustedScores, shape = VirusFamily, color = HumanVirus)) +
  geom_point(size = 3) +
  scale_shape_manual(values = custom_shapes) +
  scale_color_manual(values = c("Yes" = "steelblue", "No" = "darkorange")) +
  labs(
    title = "Normalized Original vs. Adjusted Risk Scores",
    x = "Normalized Original Risk Scores",
    y = "Normalized Adjusted Risk Scores",
    color = "Human Virus Classification",
    shape = "Viral Family") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"),
    legend.position = "right", 
    legend.text = element_text(size = 8), 
    legend.key.height = unit(0.4, "cm"), 
    legend.key.width = unit(0.4, "cm")) +
  coord_equal()  

### ROC Curve for "Human Virus?" Factor

# Load library
library(pROC)

# Create a binary outcome (0 or 1) for the "Human Virus?" factor
    ## The "Human Virus?" factor is designated in the SpillOver: Viral Risk Ranking online tool
plot_spillover$HumanVirusBinary <- ifelse(plot_spillover$HumanVirus == "Yes", 1, 0)

# Extract the normalized scores and "Human Virus?" column
scores_data <- plot_spillover %>%
  select(HumanVirusBinary, NormalizedOriginalScores, NormalizedAdjustedScores)

# Set font
par(family = "serif", font = 2)

# Plot ROC for normalized original scores
roc_original <- roc(scores_data$HumanVirusBinary, scores_data$NormalizedOriginalScores, 
                    plot = TRUE, 
                    col = "lightgreen", 
                    main = "ROC Curves for Normalized Original and Adjusted Risk Scores by Human Virus Classification",
                    cex.main = 1)

# Add ROC plot for normalized adjusted scores
roc_adjusted <- roc(scores_data$HumanVirusBinary, scores_data$NormalizedAdjustedScores, 
                    plot = TRUE, 
                    add = TRUE, 
                    col = "orange")

# Add legend
legend("bottomright", legend = c("Original Risk Scores", "Adjusted Risk Scores"), col = c("lightgreen", "orange"), lwd = 2)

# Compare AUC values
auc_original <- auc(roc_original)
auc_adjusted <- auc(roc_adjusted)

print(paste("AUC for Original Risk Scores: ", round(auc_original, 2)))
print(paste("AUC for Adjusted Risk Scores: ", round(auc_adjusted, 2)))

# ### ROC Curve for "Zoonotic Virus?" Factor
# 
# # Load library
# library(pROC)
# 
# # Create a binary outcome (0 or 1) for "Zoonotic Virus?" factor
# ## The "Zoonotic Virus?" factor is designated in the SpillOver: Viral Risk Ranking online tool
# plot_spillover$ZoonoticVirusBinary <- ifelse(plot_spillover$ZoonoticVirus == "Yes", 1, 0)
# 
# # Extract the normalized scores and "Human Virus?" column
# scores_data <- plot_spillover %>%
#   select(ZoonoticVirusBinary, NormalizedOriginalScores, NormalizedAdjustedScores)
# 
# # Change font
# par(family = "serif", font = 2)
# 
# # Plot ROC for normalized original scores
# roc_original <- roc(scores_data$ZoonoticVirusBinary, scores_data$NormalizedOriginalScores, 
#                     plot = TRUE, 
#                     col = "lightblue", 
#                     main = "ROC Curves for Normalized Original and Adjusted Risk Scores by Zoonotic Virus Classification",
#                     cex.main = 1)
# 
# # Add ROC plot for normalized adjusted scores
# roc_adjusted <- roc(scores_data$ZoonoticVirusBinary, scores_data$NormalizedAdjustedScores, 
#                     plot = TRUE, 
#                     add = TRUE, 
#                     col = "red")
# 
# # Add legend
# legend("bottomright", legend = c("Original Risk Scores", "Adjusted Risk Scores"), col = c("lightblue", "red"), lwd = 2)
# 
# # Compare AUC values
# auc_original <- auc(roc_original)
# auc_adjusted <- auc(roc_adjusted)
# 
# print(paste("AUC for Original Risk Scores: ", round(auc_original, 2)))
# print(paste("AUC for Adjusted Risk Scores: ", round(auc_adjusted, 2)))

# ### ROC Curve for "Human Transmission?" Factor
# 
# # Load library
# library(pROC)
# 
# # Create a binary outcome (0 or 1) for "Human Transmission?" factor
# ## The "Human Transmission?" factor is designated in the SpillOver: Viral Risk Ranking online tool
# plot_spillover$HumanTransmissionBinary <- ifelse(plot_spillover$HumanTransmission == "Yes", 1, 0)
# 
# # Extract the normalized scores and "Human Transmission?" column
# scores_data <- plot_spillover %>%
#   select(HumanTransmissionBinary, NormalizedOriginalScores, NormalizedAdjustedScores)
# 
# # Change font
# par(family = "serif", font = 2)
# 
# # Plot ROC for normalized original scores
# roc_original <- roc(scores_data$HumanTransmissionBinary, scores_data$NormalizedOriginalScores, 
#                     plot = TRUE, 
#                     col = "purple", 
#                     main = "ROC Curves for Normalized Original and Adjusted Risk Scores by Human Transmission Classification",
#                     cex.main = 1)
# 
# # Add ROC plot for normalized adjusted scores
# roc_adjusted <- roc(scores_data$HumanTransmissionBinary, scores_data$NormalizedAdjustedScores, 
#                     plot = TRUE, 
#                     add = TRUE, 
#                     col = "blue")
# 
# # Add legend
# legend("bottomright", legend = c("Original Risk Scores", "Adjusted Risk Scores"), col = c("purple", "blue"), lwd = 2)
# 
# # Compare AUC values
# auc_original <- auc(roc_original)
# auc_adjusted <- auc(roc_adjusted)
# 
# print(paste("AUC for Original Risk Scores: ", round(auc_original, 2)))
# print(paste("AUC for Adjusted Risk Scores: ", round(auc_adjusted, 2)))


### FIGURE 2. Comparing Human and Non-Human Viruses Across Risk Factors

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Function to apply shortened names
apply_short_names <- function(long_names) {
  return(as.character(short_factor_name[long_names]))}

# Calculate the mean and SD of aggregated non-spillover dependent factors
non_spillover_data <- spillover %>%
  select(all_of(non_spillover_dependent_factors), HumanVirus = `Human Virus?`) %>%
  pivot_longer(cols = -HumanVirus, names_to = "Variable", values_to = "Score")

mean_non_spillover <- non_spillover_data %>%
  group_by(HumanVirus) %>%
  summarize(
    Variable = "All Non-Spillover Dependent Risk Factors",
    Mean = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    .groups = 'drop')

# Calculate the mean and SD of aggregated spillover dependent factors
spillover_data <- spillover %>%
  select(all_of(spillover_dependent_factors), HumanVirus = `Human Virus?`) %>%
  pivot_longer(cols = -HumanVirus, names_to = "Variable", values_to = "Score")

mean_spillover <- spillover_data %>%
  group_by(HumanVirus) %>%
  summarize(
    Variable = "All Spillover Dependent Risk Factors",
    Mean = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    .groups = 'drop')

# Calculate means and SDs for individual spillover dependent factors
spillover_data <- spillover %>%
  select(all_of(spillover_dependent_factors), HumanVirus = `Human Virus?`) %>%
  pivot_longer(cols = -HumanVirus, names_to = "Variable", values_to = "Score") %>%
  group_by(Variable, HumanVirus) %>%
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    .groups = 'drop')

# Calculate means and SDs for individual non-spillover dependent factors
non_spillover_data <- spillover %>%
  select(all_of(non_spillover_dependent_factors), HumanVirus = `Human Virus?`) %>%
  pivot_longer(cols = -HumanVirus, names_to = "Variable", values_to = "Score") %>%
  group_by(Variable, HumanVirus) %>%
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    .groups = 'drop')

# # Calculate difference in means for non-spillover dependent factors
# mean_differences <- spillover %>%
#   select(all_of(non_spillover_dependent_factors), HumanVirus = `Human Virus?`) %>%
#   pivot_longer(cols = -HumanVirus, names_to = "Variable", values_to = "Score") %>%
#   group_by(Variable, HumanVirus) %>%
#   summarize(
#     Mean = mean(Score, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   pivot_wider(names_from = HumanVirus, values_from = Mean, names_prefix = "Mean_Human_") %>%
#   mutate(
#     Difference = Mean_Human_Yes - Mean_Human_No)  # Calculate the difference in means
# 
# print(mean_differences)

# Extract specific non-spillover factors
individual_factors <- spillover %>%
  select(`Urbanization in host ecosystem - Weighted Score`,
         `Host plasticity - No. of species - Weighted Score`,
         `Geography of the virus in animals - Weighted Score`,
         HumanVirus = `Human Virus?`) %>%
  pivot_longer(cols = -HumanVirus, names_to = "Variable", values_to = "Score") %>%
  group_by(Variable, HumanVirus) %>%
  summarize(
    Mean = mean(Score, na.rm = TRUE),
    SD = sd(Score, na.rm = TRUE),
    .groups = 'drop')

print(individual_factors)

# Apply short names to all variables
individual_factors$Variable <- apply_short_names(individual_factors$Variable)
spillover_data$Variable <- apply_short_names(spillover_data$Variable)

# Combine all datasets
combined_data <- bind_rows(mean_spillover, mean_non_spillover, spillover_data, individual_factors)
combined_data

non_spillover_factors_select <- c(
  "Urbanization in host ecosystem",
  "Host plasticity - No. of species",
  "Geography of the virus in animals")

spillover_dependent_factors <- c("Virus species infectivity in humans",
                                 "Proportion of virus species known to infect humans in the viral family",	
                                 "Epidemicity of the virus species",
                                 "Animal to human transmission",	
                                 "Human to human transmission",	
                                 "Duration of virus species infection in humans",	
                                 "Pandemic virus",
                                 "Proportion of known human pathogens in the viral family")

# Match FactorType to 'non-spillover' or 'spillover' dependent categories 
combined_data <- combined_data %>%
  mutate(FactorType = factor(case_when(
    Variable == "All Spillover Dependent Risk Factors" ~ "All Spillover Dependent Risk Factors",
    Variable == "All Non-Spillover Dependent Risk Factors" ~ "All Non-Spillover Dependent Risk Factors",
    Variable %in% spillover_dependent_factors ~ "Spillover Dependent Factors",
    Variable %in% non_spillover_factors_select ~ "Non-Spillover Dependent Factors"), 
    levels = c(
    "All Spillover Dependent Risk Factors",
    "All Non-Spillover Dependent Risk Factors",
    "Spillover Dependent Factors",
    "Non-Spillover Dependent Factors")))

# Verify categorization
summary(combined_data$FactorType)

# Modify combined_data to add a new Category label for 'non-spillover' or 'spillover' dependent categories
combined_data <- combined_data %>%
  mutate(FactorCategory = case_when(
    FactorType %in% c("All Spillover Dependent Risk Factors", "Spillover Dependent Factors") ~ "Spillover Dependent",
    FactorType %in% c("All Non-Spillover Dependent Risk Factors", "Non-Spillover Dependent Factors") ~ "Non-Spillover Dependent"
  ))

# Wrap risk factor names for plotting
combined_data$Variable <- str_wrap(combined_data$Variable, width = 50)

# Plot
ggplot(combined_data, aes(x = Variable, y = Mean, color = HumanVirus, shape = FactorCategory)) +
  geom_point(position = position_dodge(width = 0.8), size = 4) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, position = position_dodge(width = 0.8)) +
  coord_flip() +
  facet_grid(FactorType ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "Comparing Human and Non-Human Viruses Across Risk Factors",
    x = "Risk Factors",
    y = "Weighted Risk Score Mean & Standard Deviation",
    color = "Human Virus",
    shape = "Factor Category") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", face = "bold"), 
    axis.text.y = element_text(size = 10, face = "bold", hjust = 1, vjust = 0.5), 
    axis.ticks.y = element_line(size = 0.5, color = "black"), 
    legend.position = "bottom",
    strip.text = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.spacing = unit(2, "lines"),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)) +
  scale_shape_manual(
    values = c("Spillover Dependent" = 16, "Non-Spillover Dependent" = 17),
    name = "Risk Factor") +
  scale_color_manual(values = c("Yes" = "steelblue", "No" = "darkorange"))
