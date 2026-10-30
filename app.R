
setwd("D:/python/OneDrive/Desktop/R project")



# ==========================================================
# PROJECT: FITNESS AND HEALTH DATA ANALYZER
# Platform: RStudio
# Type: CLI / Console Based Data Analysis Project
# Dataset: fitness_health_dataset.csv
# ==========================================================

# 1. INSTALL AND LOAD REQUIRED PACKAGES

packages <- c("ggplot2", "dplyr", "tidyr")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(ggplot2)
library(dplyr)
library(tidyr)

# 2. LOAD DATASET

cat("\n==========================================\n")
cat("     FITNESS AND HEALTH DATA ANALYZER\n")
cat("==========================================\n")

file_name <- "fitness_health_dataset.csv"

if (!file.exists(file_name)) {
  stop("CSV file not found! Keep the CSV in the same project folder.")
}

fitness <- read.csv(file_name)

cat("\nDataset loaded successfully!\n")

# 3. DATASET INFORMATION

cat("\n---------- DATASET INFORMATION ----------\n")

cat("\nNumber of Records:", nrow(fitness))
cat("\nNumber of Columns:", ncol(fitness))

cat("\n\nDataset Structure:\n")
str(fitness)

cat("\n\nFirst 6 Records:\n")
print(head(fitness))

# 4. CHECK MISSING VALUES

cat("\n---------- MISSING VALUES ----------\n")
print(colSums(is.na(fitness)))

# 5. DATA CLEANING

fitness <- fitness %>%
  drop_na()

# 6. CALCULATE BMI

fitness <- fitness %>%
  mutate(
    Height_m = Height_cm / 100,
    BMI = Weight_kg / (Height_m ^ 2),
    BMI_Category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI < 25 ~ "Normal",
      BMI < 30 ~ "Overweight",
      TRUE ~ "Obesity Range"
    )
  )

cat("\n---------- BMI ANALYSIS ----------\n")
cat("\nAverage BMI:", round(mean(fitness$BMI), 2))
cat("\n\nBMI Categories:\n")
print(table(fitness$BMI_Category))

# 7. GENERAL HEALTH STATISTICS

cat("\n---------- HEALTH STATISTICS ----------\n")

cat("\nAverage Age:",
    round(mean(fitness$Age), 2))

cat("\nAverage Weight:",
    round(mean(fitness$Weight_kg), 2), "kg")

cat("\nAverage Calories Burned:",
    round(mean(fitness$Calories_Burned), 2))

cat("\nAverage Steps:",
    round(mean(fitness$Steps), 0))

cat("\nAverage Water Intake:",
    round(mean(fitness$Water_Liters), 2), "litres")

cat("\nAverage Sleep:",
    round(mean(fitness$Sleep_Hours), 2), "hours")

# 8. ACTIVITY ANALYSIS

cat("\n---------- ACTIVITY ANALYSIS ----------\n")

activity_summary <- fitness %>%
  group_by(Activity) %>%
  summarise(
    Total_Calories = sum(Calories_Burned),
    Average_Calories = mean(Calories_Burned),
    Average_Duration = mean(Duration_min),
    Total_Steps = sum(Steps),
    .groups = "drop"
  )

print(activity_summary)

# 9. GRAPH 1: CALORIES BURNED BY ACTIVITY

graph1 <- ggplot(activity_summary,
                 aes(x = Activity, y = Total_Calories,
                     fill = Activity)) +
  geom_col() +
  labs(
    title = "Total Calories Burned by Activity",
    x = "Activity",
    y = "Total Calories Burned"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(graph1)

# 10. GRAPH 2: BMI DISTRIBUTION

graph2 <- ggplot(fitness, aes(x = BMI)) +
  geom_histogram(bins = 15, fill = "skyblue",
                 color = "black") +
  labs(
    title = "BMI Distribution",
    x = "Body Mass Index (BMI)",
    y = "Number of Records"
  ) +
  theme_minimal()

print(graph2)

# 11. GRAPH 3: STEPS BY ACTIVITY

graph3 <- ggplot(fitness,
                 aes(x = Activity, y = Steps,
                     fill = Activity)) +
  geom_boxplot() +
  labs(
    title = "Steps Analysis by Activity",
    x = "Activity",
    y = "Steps"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(graph3)

# 12. GRAPH 4: SLEEP HOURS DISTRIBUTION

graph4 <- ggplot(fitness, aes(x = Sleep_Hours)) +
  geom_histogram(bins = 15, fill = "lightgreen",
                 color = "black") +
  labs(
    title = "Sleep Hours Distribution",
    x = "Sleep Hours",
    y = "Number of Records"
  ) +
  theme_minimal()

print(graph4)

# 13. GRAPH 5: WATER INTAKE VS SLEEP

graph5 <- ggplot(fitness,
                 aes(x = Water_Liters, y = Sleep_Hours)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE,
              color = "red") +
  labs(
    title = "Water Intake vs Sleep Hours",
    x = "Water Intake (Litres)",
    y = "Sleep Hours"
  ) +
  theme_minimal()

print(graph5)

# 14. GRAPH 6: WEIGHT VS BMI

graph6 <- ggplot(fitness,
                 aes(x = Weight_kg, y = BMI,
                     color = Gender)) +
  geom_point(size = 3) +
  labs(
    title = "Weight vs BMI Analysis",
    x = "Weight (kg)",
    y = "BMI"
  ) +
  theme_minimal()

print(graph6)

# 15. SAVE ANALYSIS RESULTS

write.csv(fitness,
          "fitness_analysis_results.csv",
          row.names = FALSE)

write.csv(activity_summary,
          "activity_summary.csv",
          row.names = FALSE)

# 16. FINAL OUTPUT

cat("\n==========================================\n")
cat("       ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("==========================================\n")

cat("\nTotal Records Analyzed:", nrow(fitness))
cat("\nTotal Activities:", length(unique(fitness$Activity)))

cat("\n\nGenerated 6 Graphs:")
cat("\n1. Calories Burned by Activity")
cat("\n2. BMI Distribution")
cat("\n3. Steps by Activity")
cat("\n4. Sleep Hours Distribution")
cat("\n5. Water Intake vs Sleep")
cat("\n6. Weight vs BMI")

cat("\n\nFiles Saved:")
cat("\n- fitness_analysis_results.csv")
cat("\n- activity_summary.csv")

cat("\n\nProject Finished!\n")

