#########################
#Load Packages
########################

library(htmlwidgets)
library(DT)
library(data.table)
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(kableExtra)
library(reshape2)
library(knitr)
library(RColorBrewer)

library(ClustOfVar)
library(patchwork)

library(nnet)
library(UpSetR) 
library(forestplot)
library(openxlsx)
library(scales)  # for percent formatting

library(poLCA)

library(ComplexUpset)  # for creating upset plots

library(patchwork)     # for combining plots



library(cluster)       # For pam and daisy
library(factoextra) 

library(nnet)      # for multinom()
library(broom)     # for tidy()
####################################################
#Loading the data
# ###################################################
# Setting path
# Create a folder path
folder_path <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/"

# Create a file path within the folder
file_path <- paste0(folder_path, "Cyril_Extraction_VA_Chron5.csv")

# Read data from the file using fread
VARAW <- fread(file_path)


# Extract Year of Birth (YOB) and Year of Death (YOD)
VARAW[, `:=` (YOB = year(DoB), YOD = year(DoD))]

# Deleting rows where YOD is greater than or equal to 2022 start from 2002
#VARAW <- VARAW[YOD < 2023]
VARAW <- VARAW[YOD >= 2012 & YOD <= 2022]

#Variables encoding
#table(VARAW$Sex)
VARAW[, Sex := ifelse(Sex %in% c("M","m"), "M",
                      ifelse(Sex == "F", "F", NA))]

VARAW <- VARAW[!is.na(Sex)]

#table(VARAW$Marital)
VARAW[, Marital := ifelse(
  Marital %in% c("Y"), "M",
  ifelse(Marital %in% c("single"), "S",
         ifelse(Marital == "widow", "W",
                ifelse(Marital == "formal", "M",
                       ifelse(Marital == "informal", "M",
                              ifelse(Marital %in% c("", "999", "DK","Dk"),"S",NA)
                       )
                )
         )
  )
)]


VARAW[, HIV := ifelse(Aids_disease == "Y" | HIV_positive == "Y", 1,
                      ifelse(Aids_disease == "N" | HIV_positive == "N", 0, 0))]

#table(VARAW$Liver_dissease)

variables_to_recode <- c("Mental_ill","Epilepsy", "Tuberculosis" ,"Smoking",
                         "Hypertension","Diabetes", "Asthma","Heart_disease","Cancer","COPD" ,         
                         "Dementia", "Kidney_disease","Liver_disease","Alcohol","Drugs" )

# Loop through the variables and recode them
for (variable in variables_to_recode) {
  new_variable_name <- paste0(variable, "")  # Create a new variable name
  
  VARAW[, (new_variable_name) := ifelse(.SD[[variable]] %in% c("Y","y"), 1,
                                        ifelse(.SD[[variable]] == "N", 0, 0)), .SDcols = variable]
}


#table(VARAW$HIV, VARAW$HIV_positive)

VARAW[, Age := cut(Age, c(0, 40, 54,64,Inf),
                   labels = c("<=40", "(40-54]","(54-64]", ">64"))] 


#table(VARAW$Drugs)

VARAW <- VARAW[!is.na(Age)]



names(VARAW)


cols_to_factor <- c("Age", "Sex", "Marital", "Mental_ill", "Epilepsy",
                    "Tuberculosis", "Hypertension", "Diabetes", "Asthma",
                    "Heart_disease", "Cancer", "COPD", "Dementia",
                    "Kidney_disease", "Liver_disease", "Alcohol", "Smoking",
                    "Drugs", "HIV")

# Convert these columns to factor
VARAW[, (cols_to_factor) := lapply(.SD, as.factor), .SDcols = cols_to_factor]

# Define the columns to delete.
cols_to_delete <- c("DoB", "DoD", "Aids_disease", "Mom_HIV_positive", 
                    "Drink_while_pregnant", "Smoke_while_pregnant", 
                    "HIV_positive", "YOB")

# Delete the unwanted columns
VARAW[, (cols_to_delete) := NULL]

#Convert the "YOD" column to numeric. 
VARAW[, YOD := as.numeric(YOD)]

# Optional: Check the structure of your data
str(VARAW)

# Define the disease variables used for multimorbidity analysis
variables_to_recode_1 <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV",
                           "Hypertension", "Diabetes", "Asthma", "Heart_disease",
                           "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Calculate the number of conditions (NOC) per record (assuming 1 = Yes, 0 = No)
VARAW[, NOC := rowSums(.SD == 1), .SDcols = variables_to_recode_1]

# Filter out records with no diseases (if any)
VARAW <- VARAW[NOC > 0]

#############
#Counts
###########


library(data.table)
library(tableone)
library(knitr)
library(kableExtra)

# Assuming VARAW is a data.table
categorical_vars <- c("Sex", "Marital", "Alcohol", "Smoking", "Drugs",
                      "Mental_ill","Epilepsy","Tuberculosis","HIV","Hypertension",
                      "Diabetes","Asthma","Heart_disease","Cancer","COPD",
                      "Dementia","Kidney_disease","Liver_disease")

continuous_vars <- c("Age")

# Recode categorical variables explicitly as factors
VARAW[, (categorical_vars) := lapply(.SD, factor), .SDcols = categorical_vars]

# Create grouping variable
VARAW[, Age_Sex := interaction(Age, Sex)]

# Create table using tableone
table1 <- CreateTableOne(
  vars = c(categorical_vars, continuous_vars),
  strata = "Age_Sex", 
  data = as.data.frame(VARAW),
  factorVars = categorical_vars,
  includeNA = FALSE,
  test = TRUE
)

# Generate printable table
table1_df <- print(
  table1, 
  printToggle = FALSE, 
  noSpaces = TRUE, 
  showAllLevels = FALSE,
  contDigits = 2, 
  catDigits = 1, 
  test = TRUE
)

# Convert to HTML table for easy copy-pasting into Word
kable(table1_df, format = "html", caption = "Table 1: Baseline characteristics by Age and Sex strata") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE, 
    position = "left",
    font_size = 12
  ) %>%
  scroll_box(width = "100%", height = "1000px")

# Print as Markdown in the console
library(knitr)
cat(kable(table1_df, format = "markdown", 
          caption = "Table 1: Baseline characteristics by Age and Sex strata",
          align = "l"))


######################################
#Counts Table
####################################


# Define the disease variables (clinical conditions) and the social/behavioral variables separately.
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "Hypertension", 
                  "Diabetes", "Asthma", "Heart_disease", "Cancer", "COPD", 
                  "Dementia", "Kidney_disease", "Liver_disease","HIV" )
social_vars <- c("Alcohol", "Smoking", "Drugs")

# Combine them in the desired order: diseases first, then social/behavioral variables.
disease_cols <- c(disease_vars, social_vars)

# Assuming your cleaned data.table is named VARAW.
# Create a results table with one row per variable in the desired order.
results <- data.table(Variable = disease_cols)

# For each variable, count how many individuals responded "No" (0) and "Yes" (1).
results[, `:=`(
  Count_No = sapply(disease_cols, function(col) {
    sum(as.numeric(as.character(VARAW[[col]])) == 0, na.rm = TRUE)
  }),
  Count_Yes = sapply(disease_cols, function(col) {
    sum(as.numeric(as.character(VARAW[[col]])) == 1, na.rm = TRUE)
  })
)]

# Compute the row total for each variable and then calculate percentages.
results[, Total := Count_No + Count_Yes]
results[, Percent_No := ifelse(Total > 0, round(Count_No / Total * 100, 1), 0)]
results[, Percent_Yes := ifelse(Total > 0, round(Count_Yes / Total * 100, 1), 0)]

# Format the counts and percentages for "No" and "Yes" responses.
results[, No := paste0(Count_No, " (", Percent_No, "%)")]
results[, Yes := paste0(Count_Yes, " (", Percent_Yes, "%)")]
results[, TotalStr := paste0(Total, " (100%)")]

# Reorder and select the final columns for the output table.
final_table <- results[, .(Variable, No, Yes, Total = TotalStr)]

# Define the output file path (make sure the directory exists)
output_file <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/Table1_Proportions.xlsx"

# Write the final_table to an Excel file in Sheet1.
write.xlsx(final_table, file = output_file, sheetName = "Sheet1", rowNames = FALSE)

# Display the final table in the console
print(final_table)


#################
#Age sex Table
#################

# Create a counts table by Age and Sex using data.table
age_sex_counts <- VARAW[, .N, by = .(Age, Sex)]
# Compute row totals for each Age group
age_totals <- VARAW[, .N, by = .(Age)]
setnames(age_totals, "N", "Total")
# Merge the row totals into the counts table
age_sex_counts <- merge(age_sex_counts, age_totals, by = "Age")
# Calculate percentage for each cell relative to the row total
age_sex_counts[, Percent := round((N / Total) * 100, 1)]
# Create a formatted string: e.g., "70 (15%)"
age_sex_counts[, Formatted := paste0(N, " (", Percent, "%)")]

# Reshape the data so that rows are Age groups and columns are Sex levels
final_age_sex <- dcast(age_sex_counts, Age ~ Sex, value.var = "Formatted", fill = "0 (0%)")
# Merge the Total from age_totals to add a row total column
final_age_sex <- merge(final_age_sex, age_totals, by = "Age")
# Convert to data.table in case merge returned a data.frame
final_age_sex <- as.data.table(final_age_sex)
# Format the Total column as "Total count (100%)"
final_age_sex[, Total := paste0(Total, " (100%)")]

# Reorder the rows by Age (from youngest to oldest)
# Assuming your Age groups are defined as: "<=40", "(40-54]", "(54-64]", ">64"
final_age_sex[, Age := factor(Age, levels = c("<=40", "(40-54]", "(54-64]", ">64"))]
setorder(final_age_sex, Age)

# Optionally, rearrange the columns so that the total appears last (if needed)
sex_cols <- setdiff(names(final_age_sex), c("Age", "Total"))
setcolorder(final_age_sex, c("Age", sort(sex_cols), "Total"))

# Define the output file path (make sure the directory exists)
output_file_age_sex <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/Table_Age_Sex_Proportions.xlsx"

# Write the final table to an Excel file (Sheet1)
write.xlsx(final_age_sex, file = output_file_age_sex, sheetName = "Sheet1", rowNames = FALSE)

# Print the final table to the console (optional)
print(final_age_sex)


#################################
#Proportions Plots
################################

# Define the diseases of interest (in the desired order)
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "Hypertension", 
                  "Diabetes", "Asthma", "Heart_disease", "Cancer", "COPD", 
                  "Dementia", "Kidney_disease", "Liver_disease", "HIV")

# Compute the proportion (i.e. mean of 1's) for each disease by YOD.
# This assumes that 1 means "Yes" and 0 "No". The function converts factors safely.
prop_dt <- VARAW[, lapply(.SD, function(x) mean(as.numeric(as.character(x)) == 1, na.rm = TRUE)), 
                 by = YOD, .SDcols = disease_vars]

# Reshape the data from wide to long format.
prop_melt <- melt(prop_dt, id.vars = "YOD", variable.name = "Disease", value.name = "Proportion")

# Define a color vector with the specified colors for each disease.
color_vector <- c(
  "Mental_ill"     = "#0072B2",  # Blue
  "Epilepsy"       = "#D55E00",  # Red
  "Tuberculosis"   = "#009E73",  # Green
  "Hypertension"   = "#CC79A7",  # Purple
  "Diabetes"       = "#E69F00",  # Orange
  "Asthma"         = "#8B4513",  # Brown
  "Heart_disease"  = "#FF69B4",  # Pink
  "Cancer"         = "#008080",  # Teal
  "COPD"           = "#808080",  # Gray
  "Dementia"       = "#00CED1",  # Cyan
  "Kidney_disease" = "#00008B",  # Dark Blue
  "Liver_disease"  = "#8B0000",  # Dark Red
  "HIV"            = "#000000"   # Black
)

# Generate the plot with the desired customizations.
p <- ggplot(prop_melt, aes(x = YOD, y = Proportion, color = Disease)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Disease Proportions Over Time",
       x = "Year of Death (YOD)",
       y = "Proportion") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_color_manual(values = color_vector) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

# Display the plot
print(p)


###############
#prop by sex
################

# Define the diseases of interest in the desired order
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "Hypertension", 
                  "Diabetes", "Asthma", "Heart_disease", "Cancer", "COPD", 
                  "Dementia", "Kidney_disease", "Liver_disease", "HIV")

# Compute the proportion of individuals with each disease (i.e., mean of 1's)
# Grouped by Year of Death (YOD) and Sex.
prop_dt <- VARAW[, lapply(.SD, function(x) mean(as.numeric(as.character(x)) == 1, na.rm = TRUE)), 
                 by = .(YOD, Sex), .SDcols = disease_vars]

# Reshape the data from wide to long format (each row represents a YOD-Sex-Disease combination)
prop_dt_long <- melt(prop_dt, id.vars = c("YOD", "Sex"), 
                     variable.name = "Disease", value.name = "Proportion")

# Define the custom color vector for the diseases
color_vector <- c(
  "Mental_ill"     = "#0072B2",  # Blue
  "Epilepsy"       = "#D55E00",  # Red
  "Tuberculosis"   = "#009E73",  # Green
  "Hypertension"   = "#CC79A7",  # Purple
  "Diabetes"       = "#E69F00",  # Orange
  "Asthma"         = "#8B4513",  # Brown
  "Heart_disease"  = "#FF69B4",  # Pink
  "Cancer"         = "#008080",  # Teal
  "COPD"           = "#808080",  # Gray
  "Dementia"       = "#00CED1",  # Cyan
  "Kidney_disease" = "#00008B",  # Dark Blue
  "Liver_disease"  = "#8B0000",  # Dark Red
  "HIV"            = "#000000"   # Black
)

# Generate the plot
p_sex <- ggplot(prop_dt_long, aes(x = YOD, y = Proportion, color = Disease)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Sex) +  # Creates separate panels for each sex (e.g., males and females)
  labs(title = "Disease Proportions Over Time by Sex",
       x = "Year of Death (YOD)",
       y = "Proportion") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +  # Bi-yearly breaks from 2012 to 2022
  scale_color_manual(values = color_vector) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

# Display the plot
print(p_sex)


#########
#Sex long


# Define the same diseases of interest
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "Hypertension", 
                  "Diabetes", "Asthma", "Heart_disease", "Cancer", "COPD", 
                  "Dementia", "Kidney_disease", "Liver_disease", "HIV")

# Compute the proportion for each disease by Year of Death (YOD) and Sex.
prop_dt_sex <- VARAW[, lapply(.SD, function(x) mean(as.numeric(as.character(x)) == 1, na.rm = TRUE)), 
                     by = .(YOD, Sex), .SDcols = disease_vars]

# Reshape the data from wide to long format
prop_dt_long_sex <- melt(prop_dt_sex, id.vars = c("YOD", "Sex"),
                         variable.name = "Disease", value.name = "Proportion")

# Create the plot: panels represent diseases, and lines are distinguished by Sex.
p_disease_sex <- ggplot(prop_dt_long_sex, aes(x = YOD, y = Proportion, color = Sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Disease, scales = "free_y") +
  labs(title = "Disease Proportions Over Time by Sex",
       x = "Year of Death (YOD)",
       y = "Proportion") +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 12),  # Facet labels
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

# Display the Sex-based plot
print(p_disease_sex)




###################
#prop by age
###################

# Define the diseases of interest in the desired order
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "Hypertension", 
                  "Diabetes", "Asthma", "Heart_disease", "Cancer", "COPD", 
                  "Dementia", "Kidney_disease", "Liver_disease", "HIV")

# Compute the proportion (i.e. the mean of 1's) for each disease by Year of Death (YOD) and Age.
# This assumes that each disease variable is coded as 1 = Yes and 0 = No.
prop_dt_age <- VARAW[, lapply(.SD, function(x) mean(as.numeric(as.character(x)) == 1, na.rm = TRUE)), 
                     by = .(YOD, Age), .SDcols = disease_vars]

# Reshape the data from wide to long format so that each row represents a YOD-Age-Disease combination.
prop_dt_long_age <- melt(prop_dt_age, id.vars = c("YOD", "Age"),
                         variable.name = "Disease", value.name = "Proportion")

# Define the custom color vector with your specified colors.
color_vector <- c(
  "Mental_ill"     = "#0072B2",  # Blue
  "Epilepsy"       = "#D55E00",  # Red
  "Tuberculosis"   = "#009E73",  # Green
  "Hypertension"   = "#CC79A7",  # Purple
  "Diabetes"       = "#E69F00",  # Orange
  "Asthma"         = "#8B4513",  # Brown
  "Heart_disease"  = "#FF69B4",  # Pink
  "Cancer"         = "#008080",  # Teal
  "COPD"           = "#808080",  # Gray
  "Dementia"       = "#00CED1",  # Cyan
  "Kidney_disease" = "#00008B",  # Dark Blue
  "Liver_disease"  = "#8B0000",  # Dark Red
  "HIV"            = "#000000"   # Black
)

# Create the plot, faceting by Age group so that each panel represents one Age group.
p_age <- ggplot(prop_dt_long_age, aes(x = YOD, y = Proportion, color = Disease)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Age) +
  labs(title = "Disease Proportions Over Time by Age Group",
       x = "Year of Death (YOD)",
       y = "Proportion") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_color_manual(values = color_vector) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

# Display the plot
print(p_age)

#########
#Age long 
#####

# Define the diseases of interest in the desired order
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "Hypertension", 
                  "Diabetes", "Asthma", "Heart_disease", "Cancer", "COPD", 
                  "Dementia", "Kidney_disease", "Liver_disease", "HIV")

# Compute the proportion (i.e. mean of 1's) for each disease by Year of Death (YOD) and Age.
# This assumes that 1 = Yes, 0 = No.
prop_dt_age <- VARAW[, lapply(.SD, function(x) mean(as.numeric(as.character(x)) == 1, na.rm = TRUE)), 
                     by = .(YOD, Age), .SDcols = disease_vars]

# Reshape the data from wide to long format
prop_dt_long_age <- melt(prop_dt_age, id.vars = c("YOD", "Age"),
                         variable.name = "Disease", value.name = "Proportion")

# Create the plot: panels represent diseases, and lines are different age groups.
p_disease_age <- ggplot(prop_dt_long_age, aes(x = YOD, y = Proportion, color = Age)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Disease, scales = "free_y") +
  labs(title = "Disease Proportions Over Time by Age Group",
       x = "Year of Death (YOD)",
       y = "Proportion") +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold", size = 12),   # Facet labels
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

# Display the Age-based plot
print(p_disease_age)



############################################
#___________________________________________
#Multimobidity
#___________________________________________
############################################

###########
#Overal NOC

# Define the disease variables to be used for multimorbidity analysis
variables_to_recode_1 <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV",
                           "Hypertension", "Diabetes", "Asthma", "Heart_disease",
                           "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Calculate the number of conditions (NOC) per record (assuming 1 = Yes, 0 = No)
VARAW[, NOC := rowSums(.SD == 1), .SDcols = variables_to_recode_1]

# Retain only those records with at least one disease 
VARAW <- VARAW[NOC > 0]

# Create a factor variable NOC_Level based on the number of conditions 
VARAW[, NOC_Level := ifelse(NOC == 1, "1",
                            ifelse(NOC == 2, "2", ">=3"))]
VARAW[, NOC_Level := factor(NOC_Level, levels = c("1", "2", ">=3"))]

# Compute counts by Year of Death (YOD) and NOC_Level
multimorbidity <- VARAW[, .N, by = .(YOD, NOC_Level)]

# Create a stacked bar chart showing proportions (position = "fill")
plot_multimorbidity <- ggplot(multimorbidity, aes(x = YOD, y = N, fill = NOC_Level)) +
  geom_bar(stat = "identity", position = "fill") +
  # Add dashed horizontal reference lines at 12.5%, 37.5%, and 50%
  geom_hline(yintercept = 0.125, linetype = "dashed", size = 0.5, color = "black") +
  geom_hline(yintercept = 0.375, linetype = "dashed", size = 0.5, color = "black") +
  geom_hline(yintercept = 0.5,   linetype = "dashed", size = 0.5, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +
  scale_fill_manual(values = c("1" = "#009E73", "2" = "#E69F00", ">=3" = "#D55E00")) +
  labs(title = "Multimorbidity Proportions Over Time",
       x = "Year of Death (YOD)",
       y = "Proportion of Individuals",
       fill = "Number of Conditions") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(face = "bold", size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(face = "bold", size = 10)
  )

# Display the plot
print(plot_multimorbidity)


##################
#by age


# Define the disease variables to be used for multimorbidity
variables_to_recode_1 <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV",
                           "Hypertension", "Diabetes", "Asthma", "Heart_disease",
                           "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Calculate the number of conditions (NOC) per record (assuming 1 = Yes and 0 = No)
VARAW[, NOC := rowSums(.SD == 1), .SDcols = variables_to_recode_1]

# Filter out records with no diseases (if any)
VARAW <- VARAW[NOC > 0]

# Create a new factor variable NOC_Level:
# "1" if exactly one disease, "2" if exactly two diseases, ">=3" if three or more.
VARAW[, NOC_Level := ifelse(NOC == 1, "1",
                            ifelse(NOC == 2, "2", ">=3"))]
VARAW[, NOC_Level := factor(NOC_Level, levels = c("1", "2", ">=3"))]

# Compute counts by Year of Death (YOD), Age, and NOC_Level
multimorbidity_by_age <- VARAW[, .N, by = .(YOD, Age, NOC_Level)]

# Create the stacked bar plot with proportions (position="fill")
plot_multimorbidity_age <- ggplot(multimorbidity_by_age, aes(x = YOD, y = N, fill = NOC_Level)) +
  geom_bar(stat = "identity", position = "fill") +
  # Add horizontal dashed reference lines
  geom_hline(yintercept = 0.125, linetype = "dashed", size = 0.5, color = "black") +
  geom_hline(yintercept = 0.375, linetype = "dashed", size = 0.5, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.5, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_fill_manual(values = c("1" = "#009E73", "2" = "#E69F00", ">=3" = "#D55E00")) +
  labs(title = "Multimorbidity Proportions Over Time by Age Group",
       x = "Year of Death (YOD)",
       y = "Proportion of Individuals",
       fill = "Number of Diseases") +
  facet_wrap(~ Age) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title   = element_text(face = "bold", size = 14),
    axis.text    = element_text(face = "bold", size = 12),
    strip.text   = element_text(face = "bold", size = 12),  # Facet (Age) labels
    legend.title = element_text(face = "bold", size = 12),
    legend.text  = element_text(face = "bold", size = 10)
  )

# Display the plot
print(plot_multimorbidity_age)


###################
#by sex


# Define the disease variables used for multimorbidity analysis
variables_to_recode_1 <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV",
                           "Hypertension", "Diabetes", "Asthma", "Heart_disease",
                           "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Calculate the number of conditions (NOC) per record (assuming 1 = Yes, 0 = No)
VARAW[, NOC := rowSums(.SD == 1), .SDcols = variables_to_recode_1]

# Filter out records with no diseases (if any)
VARAW <- VARAW[NOC > 0]

# Create a new factor variable NOC_Level:
# "1" for exactly one disease, "2" for exactly two diseases, and ">=3" for three or more diseases.
VARAW[, NOC_Level := ifelse(NOC == 1, "1", ifelse(NOC == 2, "2", ">=3"))]
VARAW[, NOC_Level := factor(NOC_Level, levels = c("1", "2", ">=3"))]

# Compute counts by Year of Death (YOD), Sex, and NOC_Level
multimorbidity_by_sex <- VARAW[, .N, by = .(YOD, Sex, NOC_Level)]

# Create the stacked bar plot (with proportions) using position = "fill".
plot_multimorbidity_sex <- ggplot(multimorbidity_by_sex, aes(x = YOD, y = N, fill = NOC_Level)) +
  geom_bar(stat = "identity", position = "fill") +
  # Add dashed horizontal lines at 12.5% (0.125), 37.5% (0.375), and 50% (0.5)
  geom_hline(yintercept = 0.125, linetype = "dashed", size = 0.5, color = "black") +
  geom_hline(yintercept = 0.375, linetype = "dashed", size = 0.5, color = "black") +
  geom_hline(yintercept = 0.5,   linetype = "dashed", size = 0.5, color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 2)) +
  scale_fill_manual(values = c("1" = "#009E73", "2" = "#E69F00", ">=3" = "#D55E00")) +
  labs(title = "Multimorbidity Proportions Over Time by Sex",
       x = "Year of Death (YOD)",
       y = "Proportion of Individuals",
       fill = "Number of Diseases") +
  facet_wrap(~ Sex) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.title   = element_text(face = "bold", size = 14),
    axis.text    = element_text(face = "bold", size = 12),
    strip.text   = element_text(face = "bold", size = 12),  # Facet (Sex) labels
    legend.title = element_text(face = "bold", size = 12),
    legend.text  = element_text(face = "bold", size = 10)
  )

# Display the plot
print(plot_multimorbidity_sex)




############################################
#___________________________________________
#Combination Plots
#___________________________________________
############################################


# Create the Partition variable using YOD to divide the data into three groups: 2012-2015, 2016-2019, 2020-2022.
VARAW[, Partition := cut(YOD, 
                         breaks = c(2012, 2016, 2020, 2023),
                         labels = c("2012-2015", "2016-2019", "2020-2022"),
                         right = FALSE)]

# Define the disease variables to be used in the upset plots.
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Subset the data for each partition.
PAT1 <- subset(VARAW, Partition == "2012-2015")
PAT2 <- subset(VARAW, Partition == "2016-2019")
PAT3 <- subset(VARAW, Partition == "2020-2022")

# Convert disease variables in each subset to logical (TRUE if 1, FALSE otherwise)
PAT1[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
PAT2[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
PAT3[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]

# Create upset plots for each partition with custom titles centered and bold.
upset_pat1 <- upset(PAT1, disease_vars, 
                    name = "Disease Combinations",
                    width_ratio = 0.1,
                    keep_empty_groups = TRUE,
                    wrap = TRUE,
                    min_degree = 1,
                    set_sizes = FALSE,
                    sort_sets = FALSE,
                    max_degree = 4,
                    min_size = 10,
                    base_annotations = list(
                      'Intersection size' = intersection_size(
                        text_mapping = aes(label = paste0(round(!!get_size_mode('exclusive_intersection')/nrow(PAT1)*100), "%"))
                      )
                    )
) + 
  ggtitle("2012-2015") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        text = element_text(face = "bold"))

upset_pat2 <- upset(PAT2, disease_vars, 
                    name = "Disease Combinations",
                    width_ratio = 0.1,
                    keep_empty_groups = TRUE,
                    wrap = TRUE,
                    min_degree = 1,
                    set_sizes = FALSE,
                    sort_sets = FALSE,
                    max_degree = 4,
                    min_size = 10,
                    base_annotations = list(
                      'Intersection size' = intersection_size(
                        text_mapping = aes(label = paste0(round(!!get_size_mode('exclusive_intersection')/nrow(PAT2)*100), "%"))
                      )
                    )
) + 
  ggtitle("2016-2019") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        text = element_text(face = "bold"))

upset_pat3 <- upset(PAT3, disease_vars, 
                    name = "Disease Combinations",
                    width_ratio = 0.1,
                    keep_empty_groups = TRUE,
                    wrap = TRUE,
                    min_degree = 1,
                    set_sizes = FALSE,
                    sort_sets = FALSE,
                    max_degree = 4,
                    min_size = 10,
                    base_annotations = list(
                      'Intersection size' = intersection_size(
                        text_mapping = aes(label = paste0(round(!!get_size_mode('exclusive_intersection')/nrow(PAT3)*100), "%"))
                      )
                    )
) + 
  ggtitle("2020-2022") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        text = element_text(face = "bold"))

# Combine the three upset plots vertically using patchwork.
combined_upset <- upset_pat1 / upset_pat2 / upset_pat3 +
  plot_annotation(title = "Disease Combination Upset Plots by Time Period",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)))

# Display the combined upset plots
print(combined_upset)


##############
#11 year upset

# Define the disease variables
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# (Optional) Filter VARAW to ensure YOD is between 2012 and 2022 if not already done:
VARAW <- VARAW[YOD >= 2012 & YOD <= 2022]

# Compute the number of conditions per record
VARAW[, NOC := rowSums(.SD == 1), .SDcols = disease_vars]

# Retain only records with at least one disease
VARAW <- VARAW[NOC > 0]

# Convert disease variables to logical values (TRUE if 1, FALSE otherwise)
# Using as.numeric(as.character(x)) to ensure correct conversion in case the variables are factors.
VARAW[, (disease_vars) := lapply(.SD, function(x) as.logical(as.numeric(as.character(x)))),
      .SDcols = disease_vars]

# Create the upset plot for the entire period
upset_plot <- upset(
  VARAW,
  disease_vars,
  name = "Disease Combinations",
  width_ratio = 0.1,
  keep_empty_groups = TRUE,
  wrap = TRUE,
  min_degree = 1,
  set_sizes = FALSE,
  sort_sets = FALSE,
  max_degree = 4,
  min_size = 10,
  base_annotations = list(
    'Intersection size' = intersection_size(
      text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection")/nrow(VARAW)*100), "%"))
    )
  )
) +
  ggtitle("Disease Combinations Over 2012–2022") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    text = element_text(face = "bold")
  )

# Display the upset plot
print(upset_plot)




##################
#Upset by sex
###################

#############
#Overal over 11 years

# (Optional) Ensure VARAW includes only the desired period
VARAW <- VARAW[YOD >= 2012 & YOD <= 2022]

# Define the disease variables (1 = Yes, 0 = No)
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")


# Subset data overall by Sex
male_data <- subset(VARAW, Sex == "M")
female_data <- subset(VARAW, Sex == "F")

# Convert disease columns to logical in each subset (TRUE if value equals 1, else FALSE)
male_data[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
female_data[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]

# Create upset plot for males overall
male_upset <- upset(
  male_data, disease_vars,
  name = "Disease Combinations",
  width_ratio = 0.1,
  keep_empty_groups = TRUE,
  wrap = TRUE,
  min_degree = 1,
  set_sizes = FALSE,
  sort_sets = FALSE,
  max_degree = 4,
  min_size = 10,
  base_annotations = list(
    'Intersection size' = intersection_size(
      text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(male_data) * 100), "%"))
    )
  )
) + ggtitle("Males (Overall)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    text = element_text(face = "bold")
  )

# Create upset plot for females overall
female_upset <- upset(
  female_data, disease_vars,
  name = "Disease Combinations",
  width_ratio = 0.1,
  keep_empty_groups = TRUE,
  wrap = TRUE,
  min_degree = 1,
  set_sizes = FALSE,
  sort_sets = FALSE,
  max_degree = 4,
  min_size = 10,
  base_annotations = list(
    'Intersection size' = intersection_size(
      text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(female_data) * 100), "%"))
    )
  )
) + ggtitle("Females (Overall)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    text = element_text(face = "bold")
  )

# Combine overall upset plots side-by-side
overall_upset <- male_upset + female_upset +
  plot_annotation(
    title = "Overall 2012-2022 Disease Combinations by Sex",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24))
  )

# Display the overall plot
print(overall_upset)


#################
#overal by period

# Create the Partition variable to divide the data into three periods.
VARAW[, Partition := cut(
  YOD,
  breaks = c(2012, 2016, 2020, 2023),
  labels = c("2012-2015", "2016-2019", "2020-2022"),
  right = FALSE
)]

# Define time periods for convenience
periods <- c("2012-2015", "2016-2019", "2020-2022")
# Create an empty list to store upset plots
plots_list <- list()

# Loop over each period to generate upset plots for males and females
for(p in periods) {
  
  # Subset data for the current period
  data_period <- VARAW[Partition == p]
  
  # Subset by sex for the period
  male_p <- subset(data_period, Sex == "M")
  female_p <- subset(data_period, Sex == "F")
  
  # Convert disease variables to logical for each subset
  male_p[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
  female_p[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
  
  # Create upset plot for males in the current period
  upset_male_p <- upset(
    male_p, disease_vars,
    name = "Disease Combinations",
    width_ratio = 0.1,
    keep_empty_groups = TRUE,
    wrap = TRUE,
    min_degree = 1,
    set_sizes = FALSE,
    sort_sets = FALSE,
    max_degree = 4,
    min_size = 10,
    base_annotations = list(
      'Intersection size' = intersection_size(
        text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(male_p) * 100), "%"))
      )
    )
  ) + ggtitle(paste0(p, " - Males")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      text = element_text(face = "bold")
    )
  
  # Create upset plot for females in the current period
  upset_female_p <- upset(
    female_p, disease_vars,
    name = "Disease Combinations",
    width_ratio = 0.1,
    keep_empty_groups = TRUE,
    wrap = TRUE,
    min_degree = 1,
    set_sizes = FALSE,
    sort_sets = FALSE,
    max_degree = 4,
    min_size = 10,
    base_annotations = list(
      'Intersection size' = intersection_size(
        text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(female_p) * 100), "%"))
      )
    )
  ) + ggtitle(paste0(p, " - Females")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      text = element_text(face = "bold")
    )
  
  # Save the plots in the list with keys for later assembly
  plots_list[[paste0(p, "_male")]] <- upset_male_p
  plots_list[[paste0(p, "_female")]] <- upset_female_p
}

# Arrange the six upset plots in a grid:
# (Row 1: 2012-2015 males and females, Row 2: 2016-2019, Row 3: 2020-2022)
period_sex_upset <- (plots_list[["2012-2015_male"]] + plots_list[["2012-2015_female"]]) /
  (plots_list[["2016-2019_male"]] + plots_list[["2016-2019_female"]]) /
  (plots_list[["2020-2022_male"]] + plots_list[["2020-2022_female"]]) +
  plot_annotation(
    title = "Disease Combination Upset Plots by Time Period and Sex",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24))
  )

# Display the grid of upset plots
print(period_sex_upset)

# Define the output file path (make sure the folder exists)
output_file_overall <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/overall_upset_by_sex_periods.png"

# Save the combined upset plot with a longer width, wider height, high resolution and white background.
ggsave(
  filename = output_file_overall,
  plot = period_sex_upset,  # period_sex_upset should be your combined upset plot object.
  width = 22,               # width in inches (adjust as needed)
  height = 18,              # height in inches (adjust as needed)
  dpi = 600,                # highest resolution (600 dpi)
  bg = "white"              # white background
)

# Ensure VARAW is filtered for the desired period (if not already done)
VARAW <- VARAW[YOD >= 2012 & YOD <= 2022]

# Define the Partition variable to split the data into three time periods.
VARAW[, Partition := cut(
  YOD,
  breaks = c(2012, 2016, 2020, 2023),
  labels = c("2012-2015", "2016-2019", "2020-2022"),
  right = FALSE
)]

# Define the disease variables (assumed to be coded as 1 for "Yes" and 0 for "No")
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Set up a vector for the three periods
periods <- c("2012-2015", "2016-2019", "2020-2022")

# Create an empty list to store the combined upset plots (male + female) for each time period.
plots_by_period <- list()

# Loop over each period
for (p in periods) {
  # Subset the data for the current period
  data_period <- VARAW[Partition == p]
  
  # Further subset data by Sex
  male_period <- subset(data_period, Sex == "M")
  female_period <- subset(data_period, Sex == "F")
  
  # Convert disease variables to logical (TRUE if equals 1, FALSE otherwise)
  male_period[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
  female_period[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
  
  # Create upset plot for males in the current period
  upset_male <- upset(
    male_period, disease_vars,
    name = "Disease Combinations",
    width_ratio = 0.1,
    keep_empty_groups = TRUE,
    wrap = TRUE,
    min_degree = 1,
    set_sizes = FALSE,
    sort_sets = FALSE,
    max_degree = 4,
    min_size = 10,
    base_annotations = list(
      'Intersection size' = intersection_size(
        text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(male_period) * 100), "%"))
      )
    )
  ) + 
    ggtitle(paste0(p, " - Males")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      text = element_text(face = "bold")
    )
  
  # Create upset plot for females in the current period
  upset_female <- upset(
    female_period, disease_vars,
    name = "Disease Combinations",
    width_ratio = 0.1,
    keep_empty_groups = TRUE,
    wrap = TRUE,
    min_degree = 1,
    set_sizes = FALSE,
    sort_sets = FALSE,
    max_degree = 4,
    min_size = 10,
    base_annotations = list(
      'Intersection size' = intersection_size(
        text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(female_period) * 100), "%"))
      )
    )
  ) + 
    ggtitle(paste0(p, " - Females")) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      text = element_text(face = "bold")
    )
  
  # Combine male and female upset plots horizontally for this period
  combined_period <- upset_male + upset_female +
    plot_annotation(
      title = paste("Disease Combination Upset Plots for", p),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
    )
  
  # Store the combined plot in the list with key equal to the period
  plots_by_period[[p]] <- combined_period
  
  # Define the output file path for the current period (ensure the folder exists)
  output_file <- paste0("C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/upset_by_sex_", 
                        gsub("-", "_", p), ".png")
  
  # Save the combined upset plot for the current period as a high-resolution PNG with white background.
  ggsave(
    filename = output_file,
    plot = combined_period,
    width = 16,      # Adjust width in inches as needed
    height = 12,     # Adjust height in inches as needed
    dpi = 600,       # High resolution (600 dpi)
    bg = "white"
  )
}

# Optionally, print each combined upset plot by period
for (p in periods) {
  print(plots_by_period[[p]])
}



#########
#by age


# (Optional) Filter VARAW to ensure YOD is between 2012 and 2022
VARAW <- VARAW[YOD >= 2012 & YOD <= 2022]

# Ensure the Age variable is a factor with the desired order.
# Adjust the levels as needed.
VARAW[, Age := factor(Age, levels = c("<=40", "(40-54]", "(54-64]", ">64"))]
age_levels <- levels(VARAW$Age)

# Define the disease variables
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Create an empty list to store upset plots for each Age group.
upset_plots_age <- list()

# Loop over each age group to create the upset plot.
for(a in age_levels) {
  
  # Subset the data for the current age group.
  data_age <- subset(VARAW, Age == a)
  
  # Convert the disease columns to logical (TRUE if value equals 1, otherwise FALSE)
  data_age[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
  
  # Create the upset plot for this age group.
  upset_plot_age <- upset(
    data_age, disease_vars,
    name = "Disease Combinations",
    width_ratio = 0.1,
    keep_empty_groups = TRUE,
    wrap = TRUE,
    min_degree = 1,
    set_sizes = FALSE,
    sort_sets = FALSE,
    max_degree = 4,
    min_size = 10,
    base_annotations = list(
      'Intersection size' = intersection_size(
        text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(data_age) * 100), "%"))
      )
    )
  ) +
    ggtitle(a) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
      text = element_text(face = "bold")
    )
  
  # Store this plot in the list.
  upset_plots_age[[a]] <- upset_plot_age
}

# Combine the individual upset plots into a grid.
# Here we arrange them in 2 columns. Adjust ncol (or nrow) as desired.
combined_upset_age <- wrap_plots(upset_plots_age, ncol = 2) +
  plot_annotation(
    title = "Disease Combinations Over 2012–2022 by Age Group",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24))
  )

# Display the combined upset plot
print(combined_upset_age)

# Define the output file path (ensure the folder exists)
output_file_age <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/overall_upset_by_age.png"

# Save the combined upset plot as a high-resolution PNG file with a white background.
ggsave(
  filename = output_file_age,
  plot = combined_upset_age,
  width = 24,       # Adjust width (in inches) as desired
  height = 20,      # Adjust height (in inches) as desired
  dpi = 600,        # High resolution
  bg = "white"
)


####
# time periods

# Ensure VARAW includes only YOD between 2012 and 2022
VARAW <- VARAW[YOD >= 2012 & YOD <= 2022]

# Make sure Age is a factor with proper order
VARAW[, Age := factor(Age, levels = c("<=40", "(40-54]", "(54-64]", ">64"))]

# Create a Partition variable dividing YOD into three periods
VARAW[, Partition := cut(
  YOD,
  breaks = c(2012, 2016, 2020, 2023),
  labels = c("2012-2015", "2016-2019", "2020-2022"),
  right = FALSE
)]

# Define the disease variables to be used in the upset plots
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Create an empty list to store the combined plots for each time period
combined_plots_partition <- list()

# Loop over each time period
for(period in c("2012-2015", "2016-2019", "2020-2022")) {
  
  # Subset the data for the current time period
  data_period <- VARAW[Partition == period]
  
  # Get the age levels for which there are records in this period
  age_levels <- levels(data_period$Age)
  
  # Create an empty list to store upset plots for each Age group in this period
  upset_plots_age <- list()
  
  # Loop over each Age group
  for(a in age_levels) {
    
    # Subset the data for this age group and time period
    data_age <- subset(data_period, Age == a)
    
    # Convert disease variables to logical (TRUE if 1, FALSE otherwise)
    data_age[, (disease_vars) := lapply(.SD, function(x) x == 1), .SDcols = disease_vars]
    
    # Create the upset plot for this Age group
    p <- upset(
      data_age, disease_vars,
      name = "Disease Combinations",
      width_ratio = 0.1,
      keep_empty_groups = TRUE,
      wrap = TRUE,
      min_degree = 1,
      set_sizes = FALSE,
      sort_sets = FALSE,
      max_degree = 4,
      min_size = 10,
      base_annotations = list(
        'Intersection size' = intersection_size(
          text_mapping = aes(label = paste0(round(!!get_size_mode("exclusive_intersection") / nrow(data_age) * 100), "%"))
        )
      )
    ) +
      ggtitle(a) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        text = element_text(face = "bold")
      )
    
    # Store the plot in the list (using age group as the key)
    upset_plots_age[[a]] <- p
  }
  
  # Combine the upset plots for the current period in a grid (here we use 2 columns)
  combined_age_plot <- wrap_plots(upset_plots_age, ncol = 2) +
    plot_annotation(
      title = paste("Disease Combinations by Age for", period),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20))
    )
  
  # Store the combined plot for this period
  combined_plots_partition[[period]] <- combined_age_plot
  
  # Define the output file path for the current period and save the plot as a PNG file
  output_file <- paste0("C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/upset_by_age_", period, ".png")
  ggsave(
    filename = output_file,
    plot = combined_age_plot,
    width = 16,     # Adjust width (in inches) as needed
    height = 12,    # Adjust height (in inches) as needed
    dpi = 600,      # High resolution
    bg = "white"    # White background
  )
}

# Optionally, print each time period's combined plot in the console:
for(period in names(combined_plots_partition)){
  print(combined_plots_partition[[period]])
}



###################################
#LCA
###################################

####################
# Overal Data set
####################

# Load required packages
library(poLCA)

# Define the disease variables for clustering
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Subset the dataset to include only the disease variables.
# (Assumes VARAW is your cleaned data.table.)
data_lca <- VARAW[, ..disease_vars]

# Convert these variables to numeric if needed (in case they are factors) 
# and then re-code them so that 0 becomes 1 and 1 becomes 2.
data_lca <- data.frame(lapply(data_lca, function(x) as.numeric(as.character(x)) + 1))

# Check a few rows to ensure transformation is as expected
head(data_lca)

# Create the formula for LCA: 
# This constructs a formula like: cbind(Mental_ill, Epilepsy, ..., Liver_disease) ~ 1
formula_str <- as.formula(paste0("cbind(", paste(disease_vars, collapse = ", "), ") ~ 1"))

# Set seed for reproducibility
set.seed(123)

# Fit LCA models with number of classes from 2 to 6
lca_models <- lapply(2:6, function(k) {
  poLCA(formula_str, data = data_lca, nclass = k, nrep = 10, maxiter = 500, verbose = FALSE)
})

# Extract model fit indices for comparison (e.g., BIC, AIC, log-likelihood)
model_results <- data.frame(
  Classes = 2:6,
  logLik = sapply(lca_models, function(m) m$llik),
  BIC = sapply(lca_models, function(m) m$bic),
  AIC = sapply(lca_models, function(m) m$aic)
)
print(model_results)

# Identify the best model (for example, based on the lowest BIC)
best_model_index <- which.min(model_results$BIC)
best_model <- lca_models[[best_model_index]]
cat("Best model is with", model_results$Classes[best_model_index], "classes.\n")

# View a sample of predicted class assignments
head(best_model$predclass)

# Optionally, examine the item response probabilities (the probability of endorsing each disease for each latent class)
print(best_model$probs)



# Extract the relevant probabilities and compile them in a single data frame.
prob_list <- lapply(disease_vars, function(var) {
  m <- best_model$probs[[var]]  # get the matrix for variable 'var'
  data.frame(
    Disease = var,
    LatentClass = factor(1:nrow(m)),
    Probability = m[, 2]  # extract probability of response "2" (disease present)
  )
})
prob_df <- do.call(rbind, prob_list)

# Visualize the item response probabilities as a heatmap
ggplot(prob_df, aes(x = Disease, y = LatentClass, fill = Probability)) +
  geom_tile(color = "gray90") +
  scale_fill_gradient(low = "white", high = "red", name = "Probability") +
  labs(title = "Item Response Probabilities for Disease Presence\nacross Latent Classes",
       x = "Disease",
       y = "Latent Class") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold")
  )


# Determine the number of classes using the predicted class assignments.
n_classes <- length(unique(best_model$predclass))
cat("Number of latent classes:", n_classes, "\n")

# Define the disease variables (as used in your LCA)
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV",
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease",
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Initialize list to store diseases meeting the 0.8 threshold for each class.
high_prob_diseases <- list()

# Loop through each latent class and extract diseases with probability >= 0.8
for(i in 1:n_classes){
  diseases_in_class <- sapply(disease_vars, function(var) {
    prob_val <- best_model$probs[[var]][i, 2]  # column 2: probability of disease presence
    if(prob_val >= 0.1) return(var) else return(NA)
  })
  # Remove NA values and collapse into a comma-separated string
  diseases_high <- paste(na.omit(diseases_in_class), collapse = ", ")
  high_prob_diseases[[i]] <- diseases_high
}

# Compute the proportion of individuals in each latent class
class_counts <- table(best_model$predclass)
class_proportions <- round(100 * class_counts / sum(class_counts), 2)

# Create a summary data frame
summary_df <- data.frame(
  LatentClass = 1:n_classes,
  HighProbabilityDiseases = unlist(high_prob_diseases),
  ProportionPercent = as.numeric(class_proportions)
)

# Print the summary table
print(summary_df)



### Pie Chat

# Helper function to split a comma-separated disease list into two lines if needed
split_diseases <- function(x) {
  # Split the input string by commas
  diseases <- unlist(strsplit(x, ",\\s*"))
  n <- length(diseases)
  
  if(n == 0) {
    return("")
  } else if(n == 1) {
    # Only one disease, return as is
    return(diseases)
  } else if(n == 2) {
    # Two diseases, put each on a new line
    return(paste(diseases, collapse = "\n"))
  } else {
    # For three or more diseases, split into three roughly equal groups
    idx1 <- ceiling(n/3)
    idx2 <- ceiling(2*n/3)
    line1 <- paste(diseases[1:idx1], collapse = ", ")
    # Ensure we have items for line2; if none, it will be an empty string
    line2 <- if(idx1 < idx2) paste(diseases[(idx1 + 1):idx2], collapse = ", ") else ""
    # Similarly for line3
    line3 <- if(idx2 < n) paste(diseases[(idx2 + 1):n], collapse = ", ") else ""
    return(paste(line1, line2, line3, sep = "\n"))
  }
}


# Create wrapped disease string and custom legend label
summary_df$WrappedDiseases <- sapply(summary_df$HighProbabilityDiseases, split_diseases)
summary_df$LegendLabel <- paste0("Class ", summary_df$LatentClass, ":\n", 
                                 summary_df$WrappedDiseases, "\n", 
                                 summary_df$ProportionPercent, "%")

# For the pie chart, we use a bar plot transformed into polar coordinates.
pie_chart <- ggplot(summary_df, aes(x = "", y = ProportionPercent, fill = factor(LatentClass))) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Overall (2012-2022) Latent Classes Distribution", fill = "Classes") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
  )

# Define fixed colors: red, green, orange, blue. 
# If the number of latent classes is less than 4, subset the colors.
n_classes <- nrow(summary_df)
fixed_palette <- c("#D55E00", "#009E73", "#E69F00", "#00008B")
palette_colors <- fixed_palette[1:n_classes]

# Apply the fixed colors and custom legend labels
pie_chart <- pie_chart +
  scale_fill_manual(values = palette_colors, labels = summary_df$LegendLabel)

# Display the pie chart
print(pie_chart)

# Optionally save the plot to file
output_file <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/latent_class_pie_chart.png"
ggsave(filename = output_file, plot = pie_chart, width = 12, height = 10, dpi = 600, bg = "white")



#######################
#By periods
######################


VARAW[, Partition := cut(YOD, 
                         breaks = c(2012, 2016, 2020, 2023),
                         labels = c("2012-2015", "2016-2019", "2020-2022"),
                         right = FALSE)]

# Load required packages

# Define the disease variables for clustering
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Define a helper function to split a comma-separated string into three lines
split_diseases <- function(x) {
  diseases <- unlist(strsplit(x, ",\\s*"))
  n <- length(diseases)
  
  if(n == 0) {
    return("")
  } else if(n == 1) {
    return(diseases)
  } else if(n == 2) {
    return(paste(diseases, collapse = "\n"))
  } else {
    idx1 <- ceiling(n/3)
    idx2 <- ceiling(2*n/3)
    line1 <- paste(diseases[1:idx1], collapse = ", ")
    line2 <- if(idx1 < idx2) paste(diseases[(idx1 + 1):idx2], collapse = ", ") else ""
    line3 <- if(idx2 < n) paste(diseases[(idx2 + 1):n], collapse = ", ") else ""
    return(paste(line1, line2, line3, sep = "\n"))
  }
}

# Define the time periods (assumed to be in Partition)
periods <- c("2012-2015", "2016-2019", "2020-2022")

# Define the fixed color palette as specified:
fixed_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                   "#8B4513", "#FF69B4", "#008080", "#808080", "#00CED1",
                   "#00008B", "#8B0000", "#000000")

# Loop over each time period to perform LCA and generate a pie chart
for(p in periods) {
  
  cat("\nProcessing period:", p, "\n")
  
  # Subset the data for the current period (assumes Partition exists in VARAW)
  data_period <- VARAW[Partition == p]
  
  # Prepare the LCA data from the disease variables
  data_lca <- data_period[, ..disease_vars]
  # Convert these variables to numeric and re-code (0 becomes 1 and 1 becomes 2)
  data_lca <- data.frame(lapply(data_lca, function(x) as.numeric(as.character(x)) + 1))
  
  # Create the formula for LCA
  formula_str <- as.formula(paste0("cbind(", paste(disease_vars, collapse = ", "), ") ~ 1"))
  
  # Fit LCA models with 2 to 6 classes (set seed for reproducibility)
  set.seed(123)
  lca_models <- lapply(2:6, function(k) {
    poLCA(formula_str, data = data_lca, nclass = k, nrep = 10, maxiter = 500, verbose = FALSE)
  })
  
  # Extract model fit indices
  model_results <- data.frame(
    Classes = 2:6,
    logLik = sapply(lca_models, function(m) m$llik),
    BIC = sapply(lca_models, function(m) m$bic),
    AIC = sapply(lca_models, function(m) m$aic)
  )
  print(model_results)
  
  # Select the best model based on lowest BIC
  best_model_index <- which.min(model_results$BIC)
  best_model <- lca_models[[best_model_index]]
  cat("Best model for", p, "is with", model_results$Classes[best_model_index], "classes.\n")
  
  # Determine number of latent classes using predicted class assignments.
  n_classes <- length(unique(best_model$predclass))
  cat("Number of latent classes:", n_classes, "\n")
  
  # Summarize latent classes:
  # For each latent class, list diseases with probability (column 2) >= 0.8
  high_prob_diseases <- list()
  for(i in 1:n_classes){
    diseases_in_class <- sapply(disease_vars, function(var) {
      prob_val <- best_model$probs[[var]][i, 2]  # probability of disease presence
      if(prob_val >= 0.1) return(var) else return(NA)
    })
    diseases_high <- paste(na.omit(diseases_in_class), collapse = ", ")
    high_prob_diseases[[i]] <- diseases_high
  }
  
  # Compute the proportion (%) of the population in each latent class
  class_counts <- table(best_model$predclass)
  class_proportions <- round(100 * class_counts / sum(class_counts), 2)
  
  # Create a summary data frame for the period
  summary_df <- data.frame(
    LatentClass = 1:n_classes,
    HighProbabilityDiseases = unlist(high_prob_diseases),
    ProportionPercent = as.numeric(class_proportions)
  )
  print(summary_df)
  
  # Generate custom legend labels
  summary_df$WrappedDiseases <- sapply(summary_df$HighProbabilityDiseases, split_diseases)
  summary_df$LegendLabel <- paste0("Class ", summary_df$LatentClass, ":\n",
                                   summary_df$WrappedDiseases, "\n",
                                   summary_df$ProportionPercent, "%")
  
  # Create the pie chart using a bar plot transformed into polar coordinates
  pie_chart <- ggplot(summary_df, aes(x = "", y = ProportionPercent, fill = factor(LatentClass))) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    labs(title = paste0("Latent Class Distribution (", p, ")"), fill = "Classes") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
    )
  
  # Set the palette for the number of classes (if n_classes < length(fixed_palette) take first n, else recycle)
  palette_colors <- rep(fixed_palette, length.out = n_classes)
  
  pie_chart <- pie_chart +
    scale_fill_manual(values = palette_colors, labels = summary_df$LegendLabel)
  
  # Display the pie chart
  print(pie_chart)
  
  # Save the pie chart; generate unique filename for each period (replace hyphens with underscores)
  output_file <- paste0("C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/latent_class_pie_chart_", 
                        gsub("-", "_", p), ".png")
  ggsave(filename = output_file, plot = pie_chart, width = 12, height = 10, dpi = 600, bg = "white")
}




##################
#Combined pie chats


# Define the disease variables for clustering
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Define a helper function to split a comma-separated disease list into three lines
split_diseases <- function(x) {
  diseases <- unlist(strsplit(x, ",\\s*"))
  n <- length(diseases)
  if(n == 0) {
    return("")
  } else if(n == 1) {
    return(diseases)
  } else if(n == 2) {
    return(paste(diseases, collapse = "\n"))
  } else {
    idx1 <- ceiling(n/3)
    idx2 <- ceiling(2*n/3)
    line1 <- paste(diseases[1:idx1], collapse = ", ")
    line2 <- if(idx1 < idx2) paste(diseases[(idx1 + 1):idx2], collapse = ", ") else ""
    line3 <- if(idx2 < n) paste(diseases[(idx2 + 1):n], collapse = ", ") else ""
    return(paste(line1, line2, line3, sep = "\n"))
  }
}

# Define the time periods (assuming the variable 'Partition' in VARAW holds these labels)
periods <- c("2012-2015", "2016-2019", "2020-2022")

# Define the fixed color palette as provided:
fixed_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                   "#8B4513", "#FF69B4", "#008080", "#808080", "#00CED1",
                   "#00008B", "#8B0000", "#000000")

# Create an empty list to store the pie charts for each period
pie_charts_list <- list()

# Loop over each time period
for(p in periods) {
  
  cat("\nProcessing period:", p, "\n")
  
  # Subset the data for the current period (assumes VARAW already contains Partition)
  data_period <- VARAW[Partition == p]
  
  # Prepare the LCA data: select disease variables and recode (0->1, 1->2)
  data_lca <- data_period[, ..disease_vars]
  data_lca <- data.frame(lapply(data_lca, function(x) as.numeric(as.character(x)) + 1))
  
  # Create the formula for LCA (e.g., cbind(Mental_ill, ..., Liver_disease) ~ 1)
  formula_str <- as.formula(paste0("cbind(", paste(disease_vars, collapse = ", "), ") ~ 1"))
  
  # Fit latent class analysis models with 2 to 6 classes
  set.seed(123)
  lca_models <- lapply(2:6, function(k) {
    poLCA(formula_str, data = data_lca, nclass = k, nrep = 10, maxiter = 500, verbose = FALSE)
  })
  
  # Extract model fit indices (logLik, BIC, AIC)
  model_results <- data.frame(
    Classes = 2:6,
    logLik = sapply(lca_models, function(m) m$llik),
    BIC = sapply(lca_models, function(m) m$bic),
    AIC = sapply(lca_models, function(m) m$aic)
  )
  print(model_results)
  
  # Select the best model based on the lowest BIC
  best_model_index <- which.min(model_results$BIC)
  best_model <- lca_models[[best_model_index]]
  cat("Best model for", p, "is with", model_results$Classes[best_model_index], "classes.\n")
  
  # Determine the number of latent classes from predicted class assignments
  n_classes <- length(unique(best_model$predclass))
  cat("Number of latent classes:", n_classes, "\n")
  
  # Summarize each latent class:
  # For each latent class, list diseases with probability (column 2) >= 0.8
  high_prob_diseases <- list()
  for(i in 1:n_classes) {
    diseases_in_class <- sapply(disease_vars, function(var) {
      prob_val <- best_model$probs[[var]][i, 2]  # probability of "disease present"
      if(prob_val >= 0.1) return(var) else return(NA)
    })
    diseases_high <- paste(na.omit(diseases_in_class), collapse = ", ")
    high_prob_diseases[[i]] <- diseases_high
  }
  
  # Compute the proportion of individuals in each latent class
  class_counts <- table(best_model$predclass)
  class_proportions <- round(100 * class_counts / sum(class_counts), 2)
  
  # Create a summary data frame
  summary_df <- data.frame(
    LatentClass = 1:n_classes,
    HighProbabilityDiseases = unlist(high_prob_diseases),
    ProportionPercent = as.numeric(class_proportions)
  )
  print(summary_df)
  
  # Create custom legend labels: split the diseases into three lines using our helper function
  summary_df$WrappedDiseases <- sapply(summary_df$HighProbabilityDiseases, split_diseases)
  summary_df$LegendLabel <- paste0("Class ", summary_df$LatentClass, ":\n",
                                   summary_df$WrappedDiseases, "\n",
                                   summary_df$ProportionPercent, "%")
  
  # Create a pie chart using ggplot2: 
  pie_chart <- ggplot(summary_df, aes(x = "", y = ProportionPercent, fill = factor(LatentClass))) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    coord_polar("y", start = 0) +
    labs(title = paste0("Class Distribution (", p, ")"), fill = "Classes") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
    )
  
  # Use the fixed palette—select as many colors as there are latent classes (recycling if needed)
  palette_colors <- rep(fixed_palette, length.out = n_classes)
  pie_chart <- pie_chart + scale_fill_manual(values = palette_colors, labels = summary_df$LegendLabel)
  
  # Store the pie chart for this period in the list
  pie_charts_list[[p]] <- pie_chart
  
  # Also, save the individual pie chart to file
  output_file <- paste0("C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/latent_class_pie_chart_", 
                        gsub("-", "_", p), ".png")
  ggsave(filename = output_file, plot = pie_chart, width = 12, height = 10, dpi = 600, bg = "white")
}

# Combine the three pie charts side-by-side to show evolution over time
combined_pie_chart <- pie_charts_list[[1]] + pie_charts_list[[2]] + pie_charts_list[[3]] +
  plot_annotation(title = "Evolution of Latent Classes Over Time",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)))

# Display the combined figure
print(combined_pie_chart)

# Optionally, save the combined figure to file
output_file_combined <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/latent_class_evolution.png"
ggsave(filename = output_file_combined, plot = combined_pie_chart, width = 36, height = 20, dpi = 600, bg = "white")

######
#new code

###################################
# LCA WITH CLASS ASSIGNMENTS SAVED
###################################

# setDT(VARAW)
# 
# #### 1) OVERALL DATASET LCA ################################
# 
# # Define disease variables
# disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
#                   "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
#                   "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")
# 
# # Prepare data for LCA
# data_lca <- VARAW[, ..disease_vars]
# data_lca <- data.frame(lapply(data_lca, function(x) as.numeric(as.character(x)) + 1))
# 
# # Formula
# formula_str <- as.formula(paste0("cbind(", paste(disease_vars, collapse = ", "), ") ~ 1"))
# 
# set.seed(123)
# lca_models <- lapply(2:6, function(k) {
#   poLCA(formula_str, data = data_lca, nclass = k, nrep = 10, maxiter = 500, verbose = FALSE)
# })
# 
# # Compare fit
# model_results <- data.frame(
#   Classes = 2:6,
#   logLik = sapply(lca_models, function(m) m$llik),
#   BIC    = sapply(lca_models, function(m) m$bic),
#   AIC    = sapply(lca_models, function(m) m$aic)
# )
# print(model_results)
# 
# # Select best by BIC
# best_model_index <- which.min(model_results$BIC)
# best_model <- lca_models[[best_model_index]]
# cat("Best overall model:", model_results$Classes[best_model_index], "classes\n")
# 
# # Save overall assignments
# lca_overall_assignments <- best_model$predclass
# 
# #### 2) PERIOD-SPECIFIC LCA ###############################
# 
# # Ensure VARAW$Partition exists
# VARAW[, Partition := cut(YOD,
#                          breaks = c(2012, 2016, 2020, 2023),
#                          labels = c("2012-2015","2016-2019","2020-2022"),
#                          right = FALSE)]
# 
# periods <- c("2012-2015","2016-2019","2020-2022")
# period_lca_assignments <- list()
# 
# for(p in periods) {
#   
#   cat("\nPeriod:", p, "\n")
#   dp <- VARAW[Partition == p]
#   
#   # Prepare LCA data
#   dl <- dp[, ..disease_vars]
#   dl <- data.frame(lapply(dl, function(x) as.numeric(as.character(x)) + 1))
#   
#   # Fit LCA
#   set.seed(123)
#   models <- lapply(2:6, function(k) {
#     poLCA(formula_str, data = dl, nclass = k, nrep = 10, maxiter = 500, verbose = FALSE)
#   })
#   
#   fits <- data.frame(
#     Classes = 2:6,
#     BIC     = sapply(models, function(m) m$bic)
#   )
#   print(fits)
#   
#   best_idx <- which.min(fits$BIC)
#   bm <- models[[best_idx]]
#   cat(" Best for", p, ":", fits$Classes[best_idx], "classes\n")
#   
#   # Save assignments for this period
#   period_lca_assignments[[p]] <- bm$predclass
# }
# 
# #### 3) ATTACH ASSIGNMENTS TO VARAW ######################
# 
# # Overall
# VARAW[, LCA_overall := factor(lca_overall_assignments)]
# 
# # Period-specific
# for(p in names(period_lca_assignments)) {
#   col <- paste0("LCA_", gsub("-", "_", p))
#   VARAW[Partition == p, (col) := factor(period_lca_assignments[[p]])]
# }
# 
# 



######################################
# Pam
#####################################

#############
#pam overall
###############


#################
#Overall
################


# ----- Data Preparation -----
# Define the 13 disease variables for clustering
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV", 
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease", 
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

# Extract these columns from your cleaned dataset VARAW and convert them to factors
data_pam <- VARAW[, ..disease_vars]
data_pam <- data.frame(lapply(data_pam, as.factor))

# Compute the dissimilarity matrix using Gower's metric
diss_matrix <- daisy(data_pam, metric = "gower")
numeric_diss <- as.matrix(diss_matrix)

# ----- Determine Optimal Number of Clusters Using a Subsample -----
set.seed(123)
n_obs <- nrow(numeric_diss)
sample_size <- min(500, n_obs)
sample_idx <- sample(1:n_obs, sample_size)
numeric_diss_sample <- numeric_diss[sample_idx, sample_idx]

# Silhouette method: Evaluate average silhouette width for k = 2:8
silhouette_values <- sapply(2:8, function(k) {
  pam_fit <- pam(as.dist(numeric_diss_sample), k = k, diss = TRUE)
  mean(silhouette(pam_fit)[, 3])
})
names(silhouette_values) <- 2:8
cat("Silhouette widths:\n")
print(silhouette_values)
optimal_k_sil <- as.numeric(names(silhouette_values)[which.max(silhouette_values)])
cat("Optimal k by Silhouette method:", optimal_k_sil, "\n")

# Gap statistic method: Use numeric version of the disease data.
data_pam_numeric <- VARAW[, ..disease_vars]
data_pam_numeric <- data.frame(lapply(data_pam_numeric, function(x) as.numeric(as.character(x))))
n_numeric <- nrow(data_pam_numeric)
sample_size_numeric <- min(500, n_numeric)
sample_idx_numeric <- sample(1:n_numeric, sample_size_numeric)
data_pam_numeric_sample <- data_pam_numeric[sample_idx_numeric, ]

gap_stat <- clusGap(data_pam_numeric_sample, FUNcluster = pam, K.max = 8, B = 50)

print(gap_stat$Tab)
optimal_k_gap <- which.max(gap_stat$Tab[,"gap"])
cat("Optimal k by Gap statistic method:", optimal_k_gap, "\n")

# Use majority rule: If the two methods disagree, take the rounded average.
if(optimal_k_sil == optimal_k_gap){
  best_k <- optimal_k_sil
} else {
  best_k <- round(mean(c(optimal_k_sil, optimal_k_gap)))
}
cat("Chosen best k using majority rule:", best_k, "\n")

# ----- Perform PAM Clustering on the Whole Dataset -----
# Use the full dissimilarity matrix computed earlier from the full dataset.
final_pam_full <- pam(as.dist(diss_matrix), k = best_k, diss = TRUE)
full_cluster_assignments <- final_pam_full$clustering


# We want to know, for each cluster, which diseases are present in at least 80% of the cases.
threshold <- 0.1

# Get the original disease data as numeric (assuming VARAW has the original 0/1 coding)
data_numeric <- VARAW[, ..disease_vars]
data_numeric <- data.frame(lapply(data_numeric, function(x) as.numeric(as.character(x))))

# Add the cluster assignments from PAM to the dataset
data_numeric$Cluster <- full_cluster_assignments

# Compute the mean (i.e. proportion of cases with disease present) for each disease per cluster
cluster_disease_means <- aggregate(. ~ Cluster, data = data_numeric, FUN = mean)

# For each cluster, extract the diseases with a mean >= threshold
cluster_disease_labels <- sapply(1:best_k, function(cl) {
  row <- cluster_disease_means[cluster_disease_means$Cluster == cl, ]
  # Remove the "Cluster" column
  row_values <- row[ , !(names(row) %in% "Cluster")]
  selected_diseases <- names(row_values)[which(row_values >= threshold)]
  if(length(selected_diseases) == 0){
    return("None")
  } else {
    return(paste(selected_diseases, collapse = ", "))
  }
})
print(cluster_disease_labels)

# ---------------------------------------------
split_diseases <- function(x) {
  diseases <- unlist(strsplit(x, ",\\s*"))
  n <- length(diseases)
  
  if(n == 0) {
    return("")
  } else if(n == 1) {
    return(diseases)
  } else if(n == 2) {
    return(paste(diseases, collapse = "\n"))
  } else {
    idx1 <- ceiling(n/3)
    idx2 <- ceiling(2 * n/3)
    line1 <- paste(diseases[1:idx1], collapse = ", ")
    line2 <- if(idx1 < idx2) paste(diseases[(idx1 + 1):idx2], collapse = ", ") else ""
    line3 <- if(idx2 < n) paste(diseases[(idx2 + 1):n], collapse = ", ") else ""
    return(paste(line1, line2, line3, sep = "\n"))
  }
}

# ---------------------------------------------
# Create final legend labels for the pie chart.
# First, wrap the disease names using the helper function
wrapped_disease_labels <- sapply(cluster_disease_labels, split_diseases)

# Now, create the final label for each cluster, including cluster number and percentage.
# Compute cluster proportions from the full_cluster_assignments.
cluster_counts <- table(full_cluster_assignments)
cluster_proportions <- round(100 * cluster_counts / sum(cluster_counts), 2)

# Create a summary data frame for clusters
cluster_df <- data.frame(Cluster = names(cluster_counts),
                         Count = as.numeric(cluster_counts))
cluster_df$Percentage <- as.numeric(cluster_proportions)
# The labels: "Cluster X:" then wrapped disease names on new lines, then the percentage.
cluster_df$Label <- mapply(function(cl, perc) {
  paste0("Cluster ", cl, ":\n", wrapped_disease_labels[as.numeric(cl)], "\n", perc, "%")
}, cluster_df$Cluster, cluster_df$Percentage)

# ---------------------------------------------
# Create the pie chart to display the cluster distribution
# Define your full color palette (13 colors as provided)
fixed_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                   "#8B4513", "#FF69B4", "#008080", "#808080", "#00CED1",
                   "#00008B", "#8B0000", "#000000")
# Use as many colors as there are clusters (recycle if needed)
palette_cluster <- rep(fixed_palette, length.out = length(unique(cluster_df$Cluster)))

pie_chart <- ggplot(cluster_df, aes(x = "", y = Count, fill = Cluster)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  labs(title = "Overall (2012-2022) PAM Cluster Distribution", fill = "Clusters") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_fill_manual(values = palette_cluster, labels = cluster_df$Label)

# Display the pie chart
print(pie_chart)

# Optionally, save the pie chart
output_file <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/pam_cluster_distribution_with_diseases.png"
ggsave(filename = output_file, plot = pie_chart, width = 8, height = 8, dpi = 600, bg = "white")



####################
#pam by period
####################

# # ------------------------------
# # Helper function: split_diseases
# # Splits a comma-separated string into up to three lines.
# split_diseases <- function(x) {
#   diseases <- unlist(strsplit(x, ",\\s*"))
#   n <- length(diseases)
#   
#   if(n == 0) {
#     return("")
#   } else if(n == 1) {
#     return(diseases)
#   } else if(n == 2) {
#     return(paste(diseases, collapse = "\n"))
#   } else {
#     idx1 <- ceiling(n/3)
#     idx2 <- ceiling(2 * n/3)
#     line1 <- paste(diseases[1:idx1], collapse = ", ")
#     line2 <- if(idx1 < idx2) paste(diseases[(idx1 + 1):idx2], collapse = ", ") else ""
#     line3 <- if(idx2 < n) paste(diseases[(idx2 + 1):n], collapse = ", ") else ""
#     return(paste(line1, line2, line3, sep = "\n"))
#   }
# }
# 
# # ------------------------------
# # Define the disease variables for clustering
# disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV",
#                   "Hypertension", "Diabetes", "Asthma", "Heart_disease",
#                   "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")
# 
# # Set the threshold for selecting characteristic diseases (e.g., 80%)
# threshold <- 0.1
# 
# # Define the periods to loop over (make sure VARAW$Partition exists)
# periods <- c("2012-2015", "2016-2019", "2020-2022")
# 
# # Define the full color palette (13 colors as provided)
# fixed_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
#                    "#8B4513", "#FF69B4", "#008080", "#808080", "#00CED1",
#                    "#00008B", "#8B0000", "#000000")
# 
# # Create a list to store the resulting pie charts for each period
# pie_charts_list <- list()
# 
# period_assignments <- list()
# 
# # Loop over each time period
# for(p in periods) {
#   
#   cat("\nProcessing period:", p, "\n")
#   
#   # Subset the data for the current period
#   data_period <- VARAW[Partition == p]
#   
#   # --- Determine Optimal Number of Clusters (Subsample) ---
#   # Prepare the data for PAM clustering: extract disease columns and convert to factors
#   data_pam_period <- data_period[, ..disease_vars]
#   data_pam_period <- data.frame(lapply(data_pam_period, as.factor))
#   
#   # Compute the dissimilarity matrix using Gower's metric
#   diss_matrix_period <- daisy(data_pam_period, metric = "gower")
#   numeric_diss_period <- as.matrix(diss_matrix_period)
#   
#   # Subsample up to 500 observations for speed
#   set.seed(123)
#   n_obs <- nrow(numeric_diss_period)
#   sample_size <- min(250, n_obs)
#   sample_idx <- sample(1:n_obs, sample_size)
#   numeric_diss_sample <- numeric_diss_period[sample_idx, sample_idx]
#   
#   # Silhouette method for k = 2:8
#   silhouette_values <- sapply(2:8, function(k) {
#     pam_fit <- pam(as.dist(numeric_diss_sample), k = k, diss = TRUE)
#     mean(silhouette(pam_fit)[, 3])
#   })
#   names(silhouette_values) <- 2:8
#   cat("Silhouette widths for period", p, ":\n")
#   print(silhouette_values)
#   optimal_k_sil <- as.numeric(names(silhouette_values)[which.max(silhouette_values)])
#   cat("Optimal k by Silhouette method:", optimal_k_sil, "\n")
#   
#   # Gap statistic method: use numeric version of the disease data.
#   data_pam_numeric <- data_period[, ..disease_vars]
#   data_pam_numeric <- data.frame(lapply(data_pam_numeric, function(x) as.numeric(as.character(x))))
#   n_numeric <- nrow(data_pam_numeric)
#   sample_size_numeric <- min(250, n_numeric)
#   sample_idx_numeric <- sample(1:n_numeric, sample_size_numeric)
#   data_pam_numeric_sample <- data_pam_numeric[sample_idx_numeric, ]
#   
#   gap_stat <- clusGap(data_pam_numeric_sample, FUNcluster = pam, K.max = 8, B = 50)
#   print(gap_stat$Tab)
#   optimal_k_gap <- which.max(gap_stat$Tab[,"gap"])
#   cat("Optimal k by Gap statistic method:", optimal_k_gap, "\n")
#   
#   # Majority rule: if they differ, take the rounded average
#   if(optimal_k_sil == optimal_k_gap){
#     best_k <- optimal_k_sil
#   } else {
#     best_k <- round(mean(c(optimal_k_sil, optimal_k_gap)))
#   }
#   cat("Chosen best k using majority rule for", p, ":", best_k, "\n")
#   
#   # --- Perform PAM Clustering on the Whole Data for the Period ---
#   final_pam_period <- pam(as.dist(diss_matrix_period), k = best_k, diss = TRUE)
#   period_cluster_assignments <- final_pam_period$clustering
#   
#   period_assignments[[p]] <- period_cluster_assignments
#   
#   # --- Compute Disease Composition for Each Cluster ---
#   # Get original disease data (0/1 coding) for the period
#   data_numeric <- data_period[, ..disease_vars]
#   data_numeric <- data.frame(lapply(data_numeric, function(x) as.numeric(as.character(x))))
#   data_numeric$Cluster <- period_cluster_assignments
#   # Compute mean (proportion) for each disease by cluster
#   cluster_disease_means <- aggregate(. ~ Cluster, data = data_numeric, FUN = mean)
#   
#   # For each cluster, select diseases with mean >= threshold
#   cluster_disease_labels <- sapply(1:best_k, function(cl) {
#     row <- cluster_disease_means[cluster_disease_means$Cluster == cl, ]
#     # Remove the "Cluster" column
#     row_values <- row[, !(names(row) %in% "Cluster")]
#     selected_diseases <- names(row_values)[which(as.numeric(row_values) >= threshold)]
#     if(length(selected_diseases) == 0){
#       return("None")
#     } else {
#       return(paste(selected_diseases, collapse = ", "))
#     }
#   })
#   cat("Cluster disease labels for", p, ":\n")
#   print(cluster_disease_labels)
#   
#   # --- Build the Legend Labels ---
#   # Wrap the disease names using the helper function
#   wrapped_disease_labels <- sapply(cluster_disease_labels, split_diseases)
#   
#   # Compute cluster distribution (using the full PAM clustering of the period)
#   cluster_counts <- table(period_cluster_assignments)
#   cluster_proportions <- round(100 * cluster_counts / sum(cluster_counts), 2)
#   # Build a summary data frame for clusters in this period
#   cluster_df <- data.frame(Cluster = names(cluster_counts),
#                            Count = as.numeric(cluster_counts))
#   cluster_df$Percentage <- as.numeric(cluster_proportions)
#   # Construct the final legend label: "Cluster X:" then wrapped diseases on new lines, then percentage.
#   cluster_df$Label <- mapply(function(cl, perc) {
#     paste0("Cluster ", cl, ":\n", wrapped_disease_labels[as.numeric(cl)], "\n", perc, "%")
#   }, cluster_df$Cluster, cluster_df$Percentage)
#   
#   # --- Create the Pie Chart for the Period ---
#   # Use the fixed palette (recycling as needed)
#   palette_cluster <- rep(fixed_palette, length.out = length(unique(cluster_df$Cluster)))
#   
#   pie_chart <- ggplot(cluster_df, aes(x = "", y = Count, fill = Cluster)) +
#     geom_bar(stat = "identity", width = 1, color = "white") +
#     coord_polar(theta = "y", start = 0) +
#     labs(title = paste0("Cluster Distribution (", p, ")"), fill = "Clusters") +
#     theme_void() +
#     theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
#     scale_fill_manual(values = palette_cluster, labels = cluster_df$Label)
#   
#   # Display the pie chart for this period
#   print(pie_chart)
#   
#   # Save the pie chart for this period (adjust file path if needed)
#   output_file <- paste0("C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/pam_cluster_distribution_with_diseases_", 
#                         gsub("-", "_", p), ".png")
#   ggsave(filename = output_file, plot = pie_chart, width = 8, height = 8, dpi = 600, bg = "white")
#   
#   # Store the pie chart in the list for later combination
#   pie_charts_list[[p]] <- pie_chart
# }
# 
# # ------------------------------
# # Combine the 3 Pie Charts into One Figure to Show Evolution Over Time
# combined_pie_chart <- pie_charts_list[[1]] + pie_charts_list[[2]] + pie_charts_list[[3]] +
#   plot_annotation(title = "Evolution of PAM Cluster Distributions Over Time",
#                   theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24)))
# 
# # Display the combined figure
# print(combined_pie_chart)
# 
# # Optionally, save the combined figure
# output_file_combined <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/pam_cluster_evolution.png"
# ggsave(filename = output_file_combined, plot = combined_pie_chart, width = 36, height = 12, dpi = 600, bg = "white")


#########
#new code


# ------------------------------
# Helper function: split_diseases
# (unchanged)
split_diseases <- function(x) {
  diseases <- unlist(strsplit(x, ",\\s*"))
  n <- length(diseases)
  if(n == 0) {
    return("")
  } else if(n == 1) {
    return(diseases)
  } else if(n == 2) {
    return(paste(diseases, collapse = "\n"))
  } else {
    idx1 <- ceiling(n/3)
    idx2 <- ceiling(2 * n/3)
    line1 <- paste(diseases[1:idx1], collapse = ", ")
    line2 <- if(idx1 < idx2) paste(diseases[(idx1+1):idx2], collapse = ", ") else ""
    line3 <- if(idx2 < n) paste(diseases[(idx2+1):n], collapse = ", ") else ""
    return(paste(line1, line2, line3, sep = "\n"))
  }
}

# ------------------------------
# Define the disease variables for clustering
# (unchanged)
disease_vars <- c("Mental_ill", "Epilepsy", "Tuberculosis", "HIV",
                  "Hypertension", "Diabetes", "Asthma", "Heart_disease",
                  "Cancer", "COPD", "Dementia", "Kidney_disease", "Liver_disease")

threshold <- 0.1
periods <- c("2012-2015", "2016-2019", "2020-2022")
fixed_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                   "#8B4513", "#FF69B4", "#008080", "#808080", "#00CED1",
                   "#00008B", "#8B0000", "#000000")

pie_charts_list    <- list()
period_assignments <- list()

for(p in periods) {
  
  cat("\nProcessing period:", p, "\n")
  data_period <- VARAW[Partition == p]
  
  # prepare dissimilarity
  data_pam_period <- data_period[, ..disease_vars]
  data_pam_period <- data.frame(lapply(data_pam_period, as.factor))
  diss_matrix_period <- daisy(data_pam_period, metric = "gower")
  numeric_diss_period <- as.matrix(diss_matrix_period)
  
  # subsample
  set.seed(123)
  n_obs <- nrow(numeric_diss_period)
  sample_size <- min(250, n_obs)
  idx <- sample(n_obs, sample_size)
  numeric_diss_sample <- numeric_diss_period[idx, idx]
  
  # silhouette k=2:8
  silhouette_values <- sapply(2:8, function(k) {
    pam_fit <- pam(as.dist(numeric_diss_sample), k = k, diss = TRUE)
    mean(silhouette(pam_fit)[,3])
  })
  optimal_k_sil <- as.numeric(names(silhouette_values)[which.max(silhouette_values)])
  
  # gap statistic on numeric data
  data_pam_numeric <- data_period[, ..disease_vars]
  data_pam_numeric <- data.frame(lapply(data_pam_numeric, function(x) as.numeric(as.character(x))))
  n_num <- nrow(data_pam_numeric)
  sample_size_num <- min(250, n_num)
  idx2 <- sample(n_num, sample_size_num)
  gap_stat <- clusGap(data_pam_numeric[idx2, ], FUNcluster = pam, K.max = 8, B = 50)
  optimal_k_gap <- which.max(gap_stat$Tab[,"gap"])
  
  # **ROBUST** majority rule
  if (length(optimal_k_sil) != 1) {
    best_k <- optimal_k_gap
  } else if (length(optimal_k_gap) != 1) {
    best_k <- optimal_k_sil
  } else if (optimal_k_sil == optimal_k_gap) {
    best_k <- optimal_k_sil
  } else {
    best_k <- round(mean(c(optimal_k_sil, optimal_k_gap)))
  }
  cat("Chosen best k for", p, ":", best_k, "\n")
  
  # PAM clustering full period
  final_pam_period <- pam(as.dist(diss_matrix_period), k = best_k, diss = TRUE)
  period_cluster_assignments <- final_pam_period$clustering
  period_assignments[[p]] <- period_cluster_assignments
  
  # disease composition per cluster
  data_numeric <- data_period[, ..disease_vars]
  data_numeric <- data.frame(lapply(data_numeric, function(x) as.numeric(as.character(x))))
  data_numeric$Cluster <- period_cluster_assignments
  cluster_disease_means <- aggregate(. ~ Cluster, data = data_numeric, FUN = mean)
  
  cluster_disease_labels <- sapply(1:best_k, function(cl) {
    row <- cluster_disease_means[cluster_disease_means$Cluster==cl, ]
    vals <- row[, setdiff(names(row),"Cluster")]
    sel  <- names(vals)[which(as.numeric(vals) >= threshold)]
    if (length(sel)==0) "None" else paste(sel, collapse=", ")
  })
  wrapped <- sapply(cluster_disease_labels, split_diseases)
  
  cluster_counts <- table(period_cluster_assignments)
  cluster_props  <- round(100 * cluster_counts / sum(cluster_counts), 2)
  cluster_df <- data.frame(Cluster=names(cluster_counts),
                           Count=as.numeric(cluster_counts),
                           Percentage=as.numeric(cluster_props))
  cluster_df$Label <- mapply(function(cl,pc) {
    paste0("Cluster ",cl,":\n", wrapped[as.numeric(cl)], "\n", pc, "%")
  }, cluster_df$Cluster, cluster_df$Percentage)
  
  palette_cluster <- rep(fixed_palette, length.out=nrow(cluster_df))
  
  pie_chart <- ggplot(cluster_df, aes(x="",y=Count,fill=Cluster))+
    geom_bar(stat="identity",width=1,color="white")+
    coord_polar(theta="y",start=0)+
    labs(title=paste0("Cluster Distribution (",p,")"), fill="Clusters")+
    theme_void()+
    theme(plot.title=element_text(hjust=0.5,face="bold"))+
    scale_fill_manual(values=palette_cluster, labels=cluster_df$Label)
  
  print(pie_chart)
  ggsave(paste0("C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/pam_cluster_",
                gsub("-","_",p),".png"),
         pie_chart, width=8, height=8, dpi=600, bg="white")
  
  pie_charts_list[[p]] <- pie_chart
}

# Combine
combined_pie_chart <- pie_charts_list[[1]] + pie_charts_list[[2]] + pie_charts_list[[3]] +
  plot_annotation(title="Evolution of PAM Cluster Distributions Over Time",
                  theme=theme(plot.title=element_text(hjust=0.5,face="bold",size=24, margin = margin(t = 15, b = 10))))
print(combined_pie_chart)
ggsave("C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/pam_cluster_evolution.png",
       combined_pie_chart, width=36, height=12, dpi=600, bg="white")


######################
#Multinomial regression


# =============================================
# MULTINOMIAL LOGISTIC REGRESSION ON PAM CLUSTERS
# =============================================

# 1. Load required packages


# 2. Attach your existing cluster assignments to VARAW
# ----------------------------------------------------

# Overall clusters (from `full_cluster_assignments`)
VARAW[, Cluster_overall := factor(full_cluster_assignments)]

# Period-specific clusters (from `period_assignments` list)
for(period in names(period_assignments)) {
  col <- paste0("Cluster_", gsub("-", "_", period))
  # initialize the column
  VARAW[, (col) := NA_integer_]
  # fill where Partition matches
  VARAW[Partition == period, (col) := period_assignments[[period]]]
  # convert to factor
  VARAW[, (col) := factor(get(col))]
}

# 3. Multinomial on overall PAM clusters
# ---------------------------------------

# Fit the model
mod_overall <- multinom(
  Cluster_overall ~ Age + Sex + Marital + Alcohol + Smoking + Drugs,
  data = VARAW
)

# Tidy with 95% CI
tidy_overall <- tidy(mod_overall, conf.int = TRUE, conf.level = 0.95)
if("y.level" %in% names(tidy_overall)) {
  tidy_overall <- rename(tidy_overall, outcome = y.level)
} else {
  tidy_overall$outcome <- "Overall"
}

# Plot coefficients
plot_overall <- ggplot(tidy_overall, aes(x = estimate, y = term, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    title = "Multinomial Coefficients — Overall PAM Clusters",
    x = "Estimate", y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(plot_overall)
ggsave(
  filename = "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/multinom_overall.png",
  plot = plot_overall,
  width = 10, height = 6, dpi = 300, bg = "white"
)

# 4. Multinomial by period
# --------------------------

period_plots <- list()

for(period in names(period_assignments)) {
  col <- paste0("Cluster_", gsub("-", "_", period))
  dp  <- VARAW[Partition == period]
  
  # rename the period-specific cluster for modeling
  dp[, Cluster_period := get(col)]
  
  # fit model
  mod_p <- multinom(
    Cluster_period ~ Age + Sex + Marital + Alcohol + Smoking + Drugs,
    data = dp
  )
  
  # tidy
  td_p <- tidy(mod_p, conf.int = TRUE, conf.level = 0.95)
  if("y.level" %in% names(td_p)) {
    td_p <- rename(td_p, outcome = y.level)
  } else {
    td_p$outcome <- paste0("Period ", period)
  }
  
  # plot
  plt <- ggplot(td_p, aes(x = estimate, y = term, color = outcome)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(size = 2.5) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    facet_wrap(~ outcome, scales = "free_y") +
    labs(
      title = paste("Multinomial Coefficients —", period),
      x = "Estimate", y = NULL, color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  print(plt)
  ggsave(
    filename = paste0(
      "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/multinom_",
      gsub("-", "_", period), ".png"
    ),
    plot = plt,
    width = 10, height = 6, dpi = 300, bg = "white"
  )
  
  period_plots[[period]] <- plt
}

# 5. Combine period plots
# -------------------------

combined_periods <- wrap_plots(period_plots, ncol = 1) +
  plot_annotation(
    title = "Multinomial Coefficients by Period",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
  )
print(combined_periods)
ggsave(
  filename = "C:/Users/cyrch/OneDrive - University of Witwatersrand/Changes_In_Chronic_Mortality/scripts/results/plots/multinom_by_periods.png",
  plot = combined_periods,
  width = 25, height = 25, dpi = 300, bg = "white"
)













