file_path<- "/Users/cwuyang/Downloads/Football_Players_Full_Dataset2.csv"

player_data <- read.csv(file_path) # Reading the data
dim(player_data) # dimensions of the dataset


any(is.na(player_data)) # check for missing values in the entire dataset
player_data[is.na(player_data)] <- 0   # Replacing missinig values with NA
colSums(is.na(player_data)) # check for missing values in each column in the dataset 


duplicate_rows <- player_data[duplicated(player_data), ]

# Display duplicate rows (if any)
if (nrow(duplicate_rows) > 0) {
  print("Duplicate Rows:")
  print(duplicate_rows)
  
  # Remove duplicate rows
  player_data <- unique(player_data)
  print("Duplicate rows removed.")
} else {
  print("No duplicate rows found.")
}



# Replacing names for defense
replace_for_defense <- c("DFFW", "DFMF")
defense_name <- "DF"
player_data$Pos<- gsub(paste(replace_for_defense, collapse = "|"), defense_name , player_data$Pos)

#Replacing names for forward
replace_for_forward <- c("FWDF", "FWMF")
forward_name <- "FW"
player_data$Pos<- gsub(paste(replace_for_forward , collapse = "|"), forward_name , player_data$Pos)

#Replacing names for midfield
replace_for_midfield <- c("MFDF", "MFFW")
midfield_name <- "MF"
player_data$Pos<- gsub(paste(replace_for_midfield  , collapse = "|"), midfield_name , player_data$Pos)

# Printing the categories in the position column to check if they are changed 
unique_categories <- unique(player_data$Pos)
print(unique_categories)

summary(player_data) # summary of the dataset

# Categorizing countries by continent
categorize_continents <- function(nation) {
  africa <- c("MAR", "GHA", "CIV", "NGA", "CMR", "ZAM", "GAM", "MLI", "TUN", "BFA", "COD", "GUI", "ROU", "GAB", "CPV", "ALG", "GAM", "COM", "CGO", "EGY", "MAD", "TUN", "GUI", "CMR", "HAI", "MOZ", "BDI", "GEO", "GUF", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE", "GUF", "MOZ", "BDI", "GEO", "ZIM", "SLE")
  asia <- c("ARM", "JPN", "KVX", "IRN", "TUR", "GEO", "UZB", "PHI", "ISR", "UZB")
  europe <- c("FRA", "DEN", "ENG", "ITA", "SCO", "GER", "SUI", "ESP", "AUT", "NOR", "NED", "POR", "ALB", "WAL", "SRB", "BUL", "CRO", "IRL", "LUX", "MNE", "RUS", "HUN", "SVK", "LTU", "LAT", "EST", "MKD", "SVN", "BIH", "POL", "SVK", "LTU", "LVA", "HUN", "RUS", "UKR", "LTU", "LVA", "EST", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE", "GRE", "CYP", "CTA", "UKR", "LVA", "GEO", "MNE")
  north_america <- c("USA", "CAN", "MEX")
  south_america <- c("BRA", "ARG", "CHI", "PAR", "URU", "COL", "PER", "ECU", "VEN")
  
  if (nation %in% africa) {
    return("Africa")
  } else if (nation %in% asia) {
    return("Asia")
  } else if (nation %in% europe) {
    return("Europe")
  } else if (nation %in% north_america) {
    return("North America")
  } else if (nation %in% south_america) {
    return("South America")
  } else {
    return("Other")
  }
}

library(ggplot2)

player_data$Continent <- sapply(player_data$Nation, categorize_continents)

ggplot(player_data, aes(x = Continent, fill = Nation)) +
  geom_bar() +
  labs(title = " Number of players by continent", x = "Continent", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)

# A bar plot for the number of players from the top 10 nationalities

nationality_counts <- table(player_data$Nation)

top_n <- 10

top_nationalities <- names(head(sort(nationality_counts, decreasing = TRUE), n = top_n))

top_n_df <- data.frame(Nationality = factor(top_nationalities, levels = top_nationalities),
                       Frequency = as.numeric(nationality_counts[top_nationalities]))
ggplot(top_n_df, aes(x = Nationality, y = Frequency, fill = Nationality)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5, color = "black", size = 3) +  
  labs(title = paste("Top", top_n, "Nationalities"), x = "Nationality", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(player_data, aes(x = Pos, fill = Pos)) +
  geom_bar(color = "black", show.legend = FALSE) +
  labs(title = "Distribution of Players by Position",
       x = "Position",
       y = "Number of Players") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none") +
  scale_fill_viridis_d()  

library(dplyr)


ggplot(player_data, aes(y = Age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Box Plot of Player Ages",
       x = "",
       y = "Age") +
  theme_minimal()


boxplot.stats(player_data$Age)$out


# check for outliers using boxplot for the Matches Played Column 
ggplot(player_data, aes(y = MP)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Matches Played",
       x = "",
       y = "Matches Played") +
  theme_minimal()

boxplot.stats(player_data$MP)$out   # Shows there are no outliers

ggplot(player_data, aes(y = Goals)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Box Plot of Goals Scored",
       x = "",
       y = "Goals Scored") +
  theme_minimal()

boxplot.stats(player_data$Goals)$out

sum(is.na(player_data$Min))
player_data$Min <- as.numeric(player_data$Min)
library(ggplot2)
ggplot(player_data, aes(x = Pos, y = Min, fill = Pos)) +
  geom_boxplot() +
  labs(title = "Comparison of Minutes Played Across Positions",
       x = "Position",
       y = "Minutes Played") +
  theme_minimal()

library(ggplot2)

# Assuming 'player_data' is your dataset
ggplot(player_data, aes(x = Comp, fill = Comp)) +
  geom_bar() +
  labs(title = "Count of Players in Each League",
       x = "League",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# ANOVA TEST
model <- aov(Goals ~ Pos, data = player_data)
summary(model)

tukey_result <- TukeyHSD(model)
print(tukey_result)



ggplot(player_data, aes(x = Pos, y = Goals, fill = Pos)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Positions vs. Goals Scored",
       x = "Position",
       y = "Goals Scored") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Assuming 'player_data' is your dataset
average_stats <- player_data %>%
  group_by(Pos) %>%
  summarise(
    avg_goals = mean(Goals),
    avg_assists = mean(Assists),
    avg_touches = mean(Touches)
  )

library(ggplot2)
library(viridis)  # For a better color palette


# Assuming 'player_data' is your dataset
average_stats <- player_data %>%
  group_by(Pos) %>%
  summarise(
    avg_goals = mean(Goals),
    avg_assists = mean(Assists),
    avg_touches = mean(Touches)
  )

# Reshape data for plotting
average_stats_long <- tidyr::gather(average_stats, key = "Variable", value = "Average", -Pos)

# Plotting with improvements
ggplot(average_stats_long, aes(x = Pos, y = Average, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Goals, Assists, and Touches by Player Position",
       x = "Player Position",
       y = "Average Value",
       fill = "Variable") +
  scale_fill_viridis(discrete = TRUE) +  # Using a better color palette
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(average_stats_long, aes(x = Pos, y = Average, color = Variable, group = Variable)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Goals, Assists, and Touches by Player Position",
       x = "Player Position",
       y = "Average Value",
       color = "Variable") +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))


position_counts <- player_data %>%
  group_by(Pos) %>%
  summarise(count = n())
average_stats_long <- tidyr::gather(average_stats, key = "Variable", value = "Average", -Pos)

cat("Counts by Position:\n")
print(position_counts)
cat("\nAverage Stats by Position:\n")
print(average_stats)

library(dplyr)

# Compute mean, median, and standard deviation for each variable and player position
summary_stats <- player_data %>%
  group_by(Pos) %>%
  summarise(
    mean_goals = mean(Goals),
    median_goals = median(Goals),
    sd_goals = sd(Goals),
    mean_assists = mean(Assists),
    median_assists = median(Assists),
    sd_assists = sd(Assists),
    mean_touches = mean(Touches),
    median_touches = median(Touches),
    sd_touches = sd(Touches)
  )


print("Summary Statistics by Player Position:")
print(summary_stats)


 # analysis of position vs assist
ggplot(player_data, aes(x = Pos, y = Assists, fill = Pos)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Positions vs. Assists ",
       x = "Position",
       y = "Assists") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# analysis of postion vs assist
model_1 <- aov(Assists~ Pos, data = player_data)
summary(model_1)
tukey_result_1 <- TukeyHSD(model)
print(tukey_result_1)


# Correlation between goals scored and assist
correlation_coef <- cor(player_data$Goals, player_data$Assists)
print(correlation_coef) # check for linear relationship between the two columns

# Scatter plot of goals scored vs assists
library(ggplot2)
ggplot(player_data, aes(x = Goals, y = Assists, color = Pos)) +
  geom_point() +
  labs(title = "Scatter Plot: Goals Scored vs. Assists by Position",
       x = "Goals Scored",
       y = "Assists") +
  scale_color_discrete(name = "Position")



# chisq test
chisq_result <- chisq.test(table(player_data$Nation, player_data$Pos))
print(chisq_result)

# Since the p-value is less than 0.05, you would reject the null hypothesis and conclude that there is a statistically significant association between the "Nation" and "Pos" variables in your dataset.
# Select relevant columns for correlation analysis
selected_columns <- c("Goals", "Assists", "MP", "Touches")
subset_data <- player_data[, selected_columns]

# Calculate the correlation matrix for the selected columns
correlation_matrix_subset <- cor(subset_data)

# View the correlation matrix for the selected columns
print(correlation_matrix_subset)

player_data$Age_Group <- cut(player_data$Age, breaks = c(0, 25, 32, 35, Inf),
                             labels = c("Under 25", "25-32", "32-35", "35+"))

# Print the count of players in each age group
age_group_counts <- table(player_data$Age_Group)
print(age_group_counts)





# Maximum Forward values
max_goals <-  max(player_data$Goals, na.rm = TRUE)
max_assists <-  max(player_data$Assists, na.rm = TRUE)
max_shots <- max(player_data$Shots, na.rm = TRUE)
max_SoT <- max(player_data$SoT, na.rm = TRUE)
max_SCA <- max(player_data$SCA, na.rm = TRUE)
max_GCA <- max(player_data$GCA, na.rm = TRUE)


#Midfield maximum values

max_Tkl <- max(player_data$Tkl, na.rm= TRUE)
max_Int <- max(player_data$Int, na.rm= TRUE)
max_PasTotCmp <- max(player_data$PasTotCmp, na.rm= TRUE)
max_PasAss <- max(player_data$PasAss, na.rm= TRUE)
max_PPA <- max(player_data$PPA, na.rm= TRUE)

# Defender maximum values
max_Clr <- max(player_data$Clr, na.rm= TRUE)
max_AerWon <- max(player_data$AerWon, na.rm=TRUE)
max_Blocks<- max(player_data$Blocks, na.rm=TRUE)
max_TklDef3rd<- max(player_data$TklDef3rd, na.rm=TRUE)

player_data$ScaledPerformance <- rep(NA, nrow(player_data))

calculate_forward_metric <- function(x) {
  # Assigning weights
  w_goals = 0.3
  w_assists = 0.2
  w_shots = 0.1
  w_sot = 0.2
  w_sca = 0.1
  w_gca = 0.1
  
  # Calculating weighted values
  goals_value <- (as.numeric(x$Goals) / max_goals) * 100 * w_goals
  assists_value <- (as.numeric(x$Assists) / max_assists) * 100 * w_assists
  shots_value <- (as.numeric(x$Shots) / max_shots) * 100 * w_shots
  sot_value <- (as.numeric(x$SoT) / max_SoT) * 100 * w_sot
  sca_value <- (as.numeric(x$SCA) / max_SCA) * 100 * w_sca
  gca_value <- (as.numeric(x$GCA) / max_GCA) * 100 * w_gca
  
  # Summing weighted values for final performance metric
  pm <- goals_value + assists_value + shots_value + sot_value + sca_value + gca_value
  return(pm)
}

calculate_midfielder_metric <- function(x) {
  # Assigning weights
  w_tackles = 0.15
  w_interceptions = 0.15
  w_passes_total_cmp = 0.2
  w_passes_assisted = 0.2
  w_ppa = 0.15
  w_sca = 0.15
  
  # Calculating weighted values
  tackles_value <- (as.numeric(x$Tkl) / max_Tkl) * 100 * w_tackles
  interceptions_value <- (as.numeric(x$Int) / max_Int) * 100 * w_interceptions
  passes_total_cmp_value <- (as.numeric(x$PasTotCmp) / max_PasTotCmp) * 100 * w_passes_total_cmp
  passes_assisted_value <- (as.numeric(x$PasAss) / max_PasAss) * 100 * w_passes_assisted
  ppa_value <- (as.numeric(x$PPA) / max_PPA) * 100 * w_ppa
  sca_value <- (as.numeric(x$SCA) / max_SCA) * 100 * w_sca
  
  # Summing weighted values for final performance metric
  pm <- tackles_value + interceptions_value + passes_total_cmp_value + passes_assisted_value + ppa_value + sca_value
  return(pm)
}


calculate_defender_metric <- function(x) {
  # Assigning weights
  w_clearances = 0.2
  w_aerial_duels_won = 0.2
  w_blocks = 0.2
  w_tackles_def_3rd = 0.2
  w_tackles = 0.1
  w_interceptions = 0.1
  
  # Calculating weighted values
  clearances_value <- (as.numeric(x$Clr) / max_Clr) * 100 * w_clearances
  aerial_duels_won_value <- (as.numeric(x$AerWon) / max_AerWon) * 100 * w_aerial_duels_won
  blocks_value <- (as.numeric(x$Blocks) / max_Blocks) * 100 * w_blocks
  tackles_def_3rd_value <- (as.numeric(x$TklDef3rd) / max_TklDef3rd) * 100 * w_tackles_def_3rd
  tackles_value <- (as.numeric(x$Tkl) / max_Tkl) * 100 * w_tackles
  interceptions_value <- (as.numeric(x$Int) / max_Int) * 100 * w_interceptions
  
  # Summing weighted values for final performance metric
  pm <- clearances_value + aerial_duels_won_value + blocks_value + tackles_def_3rd_value + tackles_value + interceptions_value
  return(pm)
}


for (i in 1:nrow(player_data)) {
  x <- player_data[i, ]
  
  if (x$Pos == 'FW') {
    player_data$PerformanceMetric[i] <- calculate_forward_metric(x)
  } else if (x$Pos == 'MF') {
    player_data$PerformanceMetric[i] <- calculate_midfielder_metric(x)
  } else if (x$Pos == 'DF') {
    player_data$PerformanceMetric[i] <- calculate_defender_metric(x)
  }
  
  # Print the performance metric for each player (except for goalkeepers)
  if (x$Pos %in% c('FW', 'MF', 'DF')) {
    cat("Player:", x$Player, "| Position:", x$Pos, "| Performance Metric:", player_data$PerformanceMetric[i], "\n")
  }
}



# Assuming 'Position' is the column that contains player positions
fw_players <- player_data[player_data$Pos == 'FW', ]
linear_model_fw <- lm(PerformanceMetric ~ Age, data = fw_players)
summary(linear_model_fw)

correlation <- cor(fw_players$Age, fw_players$PerformanceMetric, use = "complete.obs")
print(paste("Pearson correlation coefficient:", correlation))

# Create a scatter plot with regression line for FW players only
ggplot(fw_players, aes(x = Age, y = PerformanceMetric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Age vs Performance Metric for  Players",
       x = "Age",
       y = "Performance Metric") +
  theme_minimal()


library(ggplot2)

# Calculate the Pearson correlation coefficient
correlation <- cor(fw_players$MP, fw_players$PerformanceMetric, use = "complete.obs")
print(paste("Pearson correlation coefficient:", correlation))

# Perform a linear regression analysis
linear_model <- lm(PerformanceMetric ~ MP, data = fw_players)
summary(linear_model)

# You can also plot the linear regression line on top of the scatter plot
library(ggplot2)
ggplot(fw_players, aes(x = MP, y = PerformanceMetric)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot of Matches Played vs Performance Metric with Regression Line",
       x = "Matches Played",
       y = "Performance Metric") +
  theme_minimal()




mf_players <- player_data[player_data$Pos == 'MF', ]

linear_model_mf_age <- lm(PerformanceMetric ~ Age, data = mf_players)
summary(linear_model_mf_age)
linear_model_mf_mp <- lm(PerformanceMetric ~ MP, data = mf_players)
summary(linear_model_mf_mp)

library(ggplot2)
ggplot(mf_players, aes(x = Age, y = PerformanceMetric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Age vs Performance Metric for MF Players",
       x = "Age",
       y = "Performance Metric") +
  theme_minimal()

ggplot(mf_players, aes(x = MP, y = PerformanceMetric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Matches Played vs Performance Metric for MF Players",
       x = "Matches Played",
       y = "Performance Metric") +
  theme_minimal()

correlation_mf <- cor(mf_players$MP, mf_players$PerformanceMetric, use = "complete.obs")
print(paste("Pearson correlation coefficient for MF players:", correlation_mf))


# Filter for DF players based on the 'Pos' column
df_players <- player_data[player_data$Pos == 'DF', ]

# Fit a linear model for PerformanceMetric as a function of Age for DF players
linear_model_df_age <- lm(PerformanceMetric ~ Age, data = df_players)
summary(linear_model_df_age)

# Fit a linear model for PerformanceMetric as a function of Matches Played (MP) for DF players
linear_model_df_mp <- lm(PerformanceMetric ~ MP, data = df_players)
summary(linear_model_df_mp)

# Create a scatter plot with regression line for Age vs PerformanceMetric for DF players
ggplot(df_players, aes(x = Age, y = PerformanceMetric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Age vs Performance Metric for DF Players",
       x = "Age",
       y = "Performance Metric") +
  theme_minimal()

# Create a scatter plot with regression line for MP vs PerformanceMetric for DF players
ggplot(df_players, aes(x = MP, y = PerformanceMetric)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Matches Played vs Performance Metric for DF Players",
       x = "Matches Played",
       y = "Performance Metric") +
  theme_minimal()

# Calculate the Pearson correlation coefficient for MP and PerformanceMetric for DF players
correlation_df <- cor(df_players$MP, df_players$PerformanceMetric, use = "complete.obs")
print(paste("Pearson correlation coefficient for DF players:", correlation_df))

position_stats <- player_data %>%
  group_by(Pos) %>%
  summarise(
    MeanPerformance = mean(PerformanceMetric, na.rm = TRUE),
    SdPerformance = sd(PerformanceMetric, na.rm = TRUE)
  )



library(randomForest)
library(dplyr)
library(caret)
library(FactoMineR) # for PCA

# Read and preprocess the dataset
file_path <- "/Users/cwuyang/Downloads/Football_Players_Full_Dataset2.csv"
player_data_2 <- read.csv(file_path, stringsAsFactors = TRUE)


player_data_2 <- na.omit(player_data_2)

# Select numeric columns for PCA
numeric_cols <- sapply(player_data_2, is.numeric)
player_data_numeric <- player_data_2[, numeric_cols]

# Perform PCA on numeric columns
pca_result <- PCA(player_data_numeric, ncp = 30, graph = FALSE)

# Select the first 20 principal components
pca_data <- data.frame(pca_result$ind$coord)

# Combine PCA components with the original categorical data
# Exclude categorical columns with too many categories
categorical_cols <- sapply(player_data_2, is.factor)
categorical_data <- player_data_2[, categorical_cols]
categorical_data <- categorical_data[,sapply(categorical_data, nlevels) <= 53] # Exclude high cardinality factors

# Final dataset for random forest model
final_data <- cbind(pca_data, categorical_data)

# Splitting the dataset
set.seed(123)
train_index <- createDataPartition(final_data$Pos, p = 0.8, list = FALSE)
train_data <- final_data[train_index, ]
test_data <- final_data[-train_index, ]

# Building and evaluating the random forest model
model <- randomForest(Pos ~ ., data = train_data, importance = TRUE)
predictions <- predict(model, test_data)
confusionMatrix(predictions, test_data$Pos)

# View variable importance
importance(model)
varImpPlot(model)

library(e1071)  # for SVM

# Assuming that final_data contains the PCA-reduced data and the 'Pos' column

# Splitting the dataset into training and testing sets
set.seed(123)
train_index <- createDataPartition(final_data$Pos, p = 0.8, list = FALSE)
train_data <- final_data[train_index, ]
test_data <- final_data[-train_index, ]

# Building the SVM model
svm_model <- svm(Pos ~ ., data = train_data)

# Predicting on the test set
svm_predictions <- predict(svm_model, test_data)

# Evaluate the model
confusionMatrix(svm_predictions, test_data$Pos)

