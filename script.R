# set working directory
setwd("C:/Users/SURHUD/Desktop/Desktop/Stats/For/For Abhijit Nair/Survival")

# import libraries
library(tidyverse)
library(gtsummary)
library(flextable)
library(ggsci)

# import data
df <- read.csv("C:/Users/SURHUD/Desktop/Desktop/Stats/For/For Abhijit Nair/Survival/data.csv",
               check.names = F)
attach(df)
df %>% colnames

# create a summary statistics table
table1 <- df[,-c(1,2)] %>% # remove the first variable (ID)
            tbl_summary(by = "Outcome",
                        type = list(where(is.numeric) ~ "continuous"),
                        missing_text = "Missing Data") %>%
            add_p() %>%
            add_overall()

# save the table as a docx file
table1 %>%
  as_flex_table() %>%
  save_as_docx(path = "Table 1.docx")

# plots
df <- df %>%
  mutate(Survival = recode(Outcome, `0` = "Death", `1` = "Survived"))

plot1 <- df %>%
  drop_na(Outcome, Age) %>%
  ggplot(aes(x = Survival, y = Age, fill = Survival)) + 
  geom_boxplot() + 
  scale_fill_jco() +
  labs(title = "Boxplot of Age by Survival Status",
       x = "Survival Status")
ggsave(plot1,
       filename = "Plot 1.png",
       height = 4,
       width = 8,
       dpi = 600)

plot2 <- df %>%
  drop_na(Outcome, Gender) %>%
  count(Survival, Gender) %>%
  complete(Survival, Gender, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Gender by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot2,
       filename = "Plot 2.png",
       height = 4,
       width = 8,
       dpi = 600)

plot3 <- df %>%
  drop_na(Outcome, `Reason for ICU admission`) %>%
  count(Survival, `Reason for ICU admission`) %>%
  complete(Survival, `Reason for ICU admission`, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = `Reason for ICU admission`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Reason for ICU admission by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot3,
       filename = "Plot 3.png",
       height = 4,
       width = 8,
       dpi = 600)

plot4 <- df %>%
  drop_na(Outcome, HTN) %>%
  count(Survival, HTN) %>%
  complete(Survival, HTN, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = HTN)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Hypertensive Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot4,
       filename = "Plot 4.png",
       height = 4,
       width = 8,
       dpi = 600)

plot5 <- df %>%
  drop_na(Outcome, DM) %>%
  count(Survival, DM) %>%
  complete(Survival, DM, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = DM)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Diabetic Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot5,
       filename = "Plot 5.png",
       height = 4,
       width = 8,
       dpi = 600)

plot6 <- df %>%
  drop_na(Outcome, CAD) %>%
  count(Survival, CAD) %>%
  complete(Survival, CAD, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = CAD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Coronary Artery Disease Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot6,
       filename = "Plot 6.png",
       height = 4,
       width = 8,
       dpi = 600)

plot7 <- df %>%
  drop_na(Outcome, `Heart Failure`) %>%
  count(Survival, `Heart Failure`) %>%
  complete(Survival, `Heart Failure`, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = `Heart Failure`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Heart Failure Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot7,
       filename = "Plot 7.png",
       height = 4,
       width = 8,
       dpi = 600)

plot8 <- df %>%
  drop_na(Outcome, CKD) %>%
  count(Survival, CKD) %>%
  complete(Survival, CKD, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = CKD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Chronic Kidney Disease Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot8,
       filename = "Plot 8.png",
       height = 4,
       width = 8,
       dpi = 600)

plot9 <- df %>%
  drop_na(Outcome, Malignancy) %>%
  count(Survival, Malignancy) %>%
  complete(Survival, Malignancy, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = Malignancy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Malignancy Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot9,
       filename = "Plot 9.png",
       height = 4,
       width = 8,
       dpi = 600)

plot10 <- df %>%
  drop_na(Outcome, COPD) %>%
  count(Survival, COPD) %>%
  complete(Survival, COPD, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = COPD)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Chronic Obstructive Pulmonary Disease Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot10,
       filename = "Plot 10.png",
       height = 4,
       width = 8,
       dpi = 600)

plot11 <- df %>%
  drop_na(Outcome, `Liver Failure`) %>%
  count(Survival, `Liver Failure`) %>%
  complete(Survival, `Liver Failure`, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = `Liver Failure`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Liver Failure Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot11,
       filename = "Plot 11.png",
       height = 4,
       width = 8,
       dpi = 600)

plot12 <- df %>%
  drop_na(Outcome, CNS) %>%
  count(Survival, CNS) %>%
  complete(Survival, CNS, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = CNS)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Central Nervous System Disease Status by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot12,
       filename = "Plot 12.png",
       height = 4,
       width = 8,
       dpi = 600)

plot13 <- df %>%
  drop_na(Outcome, `Cause of Arrest`) %>%
  count(Survival, `Cause of Arrest`) %>%
  complete(Survival, `Cause of Arrest`, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = `Cause of Arrest`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Cause of Arrest by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot13,
       filename = "Plot 13.png",
       height = 4,
       width = 8,
       dpi = 600)

plot14 <- df %>%
  drop_na(Outcome, `Rhythm at Arrest`) %>%
  count(Survival, `Rhythm at Arrest`) %>%
  complete(Survival, `Rhythm at Arrest`, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = `Rhythm at Arrest`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Type of Rhythm at Arrest by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot14,
       filename = "Plot 14.png",
       height = 4,
       width = 8,
       dpi = 600)

plot15 <- df %>%
  drop_na(Outcome, `Return of Spontaneous Circulation`) %>%
  count(Survival, `Return of Spontaneous Circulation`) %>%
  complete(Survival, `Return of Spontaneous Circulation`, fill = list(n = 0)) %>%
  group_by(Survival) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = Survival, y = percentage, fill = `Return of Spontaneous Circulation`)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Grouped Bar Plot of Return of Spontaneous Circulation by Survival Status",
       x = "Survival Status",
       y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_jco()
ggsave(plot15,
       filename = "Plot 15.png",
       height = 4,
       width = 8,
       dpi = 600)

plot16 <- df %>%
  drop_na(Outcome, `Numer of Times CPR was Attempted`) %>%
  ggplot(aes(x = Survival, y = `Numer of Times CPR was Attempted`, fill = Survival)) + 
  geom_boxplot() + 
  scale_fill_jco() +
  labs(title = "Boxplot of Number of Times of CPR Attempts by Survival Status",
       x = "Survival Status")
ggsave(plot16,
       filename = "Plot 16.png",
       height = 4,
       width = 8,
       dpi = 600)