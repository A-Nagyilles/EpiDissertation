#Loading libraries
install.packages("reshape2")
install.packages("gtsummary")
install.packages("moments")
install.packages("GGally")
install.packages(("vtable"))

library(tidyr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(purrr)
library(broom)
library(ggplot2)
library(reshape2)
library(gtsummary)
library(forcats)
library(moments)
library(MASS)
library(ggpubr)
library(car)
library(GGally)
library(vtable)

#CSV file names
vacc_file_21<-"cover-gp-annual-2020-to-2021v2.csv"
vacc_file_22<-"ChildVaccStat_2021-22_GP.csv"
vacc_file_23<-"cover-gp-annual-2022-to-2023.csv"
gp_file<-"LCR_GPs_23Jan24.csv"
gp_file2<-"LCR_NAs_19Mar24.csv"

#Data Load in
LCR_GP<- read.csv(gp_file, header = TRUE)
LCR_NA<-read.csv(gp_file2,header = TRUE)
LCR_GP<-rbind(LCR_GP,LCR_NA)

Vacc_Stat_21 <- read.csv(vacc_file_21, header = TRUE, skip = 2)
Vacc_Stat_22 <- read.csv(vacc_file_22, header = TRUE)
Vacc_Stat_23 <- read.csv(vacc_file_23, header = TRUE, skip = 2)

#Select MMR vaccine stats based on LACs from GP dataset
local_authority_codes<-c("321","316","315","317","319","318")

MMR_Stat_21 <- data.frame()

for (code in local_authority_codes) {
  MMR_Subset_21 <- subset(Vacc_Stat_21, Local.authority.code == code)
  MMR_Stat_21 <- rbind(MMR_Stat_21, MMR_Subset_21)
}

MMR_Stat_22 <- data.frame()

for (code in local_authority_codes) {
  MMR_Subset_22 <- subset(Vacc_Stat_22, Local.authority.code == code)
  MMR_Stat_22 <- rbind(MMR_Stat_22, MMR_Subset_22)
}

MMR_Stat_23 <- data.frame()

for (code in local_authority_codes) {
  MMR_Subset_23 <- subset(Vacc_Stat_23, Local.authority.code == code)
  MMR_Stat_23 <- rbind(MMR_Stat_23, MMR_Subset_23)
}

sanity_check<-anti_join(MMR_Stat_21,MMR_Stat_23, by = "GP.code")

#Select relevant columns to the 24m and 5y search from MMR dataset

MMR24m_Vacc_21<- MMR_Stat_21 %>% select(GP.code,X24m.Denominator,X24m.MMR1.) %>%
  dplyr::rename(Denominator.24m = X24m.Denominator,
                MMR1.24m = X24m.MMR1.
  )

MMR5y_Vacc_21<- MMR_Stat_21 %>% select(GP.code,X5y.Denominator,X5y.MMR1.,X5y.MMR2.) %>%
  dplyr::rename(Denominator.5y = X5y.Denominator,
                MMR1.5y = X5y.MMR1.,
                MMR2.5y = X5y.MMR2.
  )

MMR24m_Vacc_22<- MMR_Stat_22 %>% select(GP.code,X24m.Denominator,X24m.MMR1.) %>%
  dplyr::rename(Denominator.24m = X24m.Denominator,
                MMR1.24m = X24m.MMR1.
  )

MMR5y_Vacc_22<- MMR_Stat_22 %>% select(GP.code,X5y.Denominator,X5y.MMR1.,X5y.MMR2.) %>%
  dplyr::rename(Denominator.5y = X5y.Denominator,
                MMR1.5y = X5y.MMR1.,
                MMR2.5y = X5y.MMR2.
  )

MMR24m_Vacc_23<- MMR_Stat_23 %>% select(GP.code,X24m.Denominator,X24m.MMR1.) %>%
  dplyr::rename(Denominator.24m = X24m.Denominator,
                MMR1.24m = X24m.MMR1.
  )

MMR5y_Vacc_23<- MMR_Stat_23 %>% select(GP.code,X5y.Denominator,X5y.MMR1.,X5y.MMR2.) %>%
  dplyr::rename(Denominator.5y = X5y.Denominator,
                MMR1.5y = X5y.MMR1.,
                MMR2.5y = X5y.MMR2.
  )

#Select Relevant year from GP dataset
LCR_GP_23<- LCR_GP %>% select(Indicator.Name,Area.Code,Sex,Age,Category,Time.period,Value, Count, Denominator) %>%
  mutate(Time.period = if_else(Time.period == "2022/23", "2023", Time.period)) %>%
  filter(Time.period %in% "2023")

LCR_GP_22<- LCR_GP %>% select(Indicator.Name,Area.Code,Sex,Age,Category,Time.period,Value, Count, Denominator) %>%
  mutate(Time.period = if_else(Time.period == "2021/22", "2022", Time.period)) %>%
  filter(Time.period %in% "2022")

LCR_GP_21 <- LCR_GP %>%
  select(Indicator.Name, Area.Code, Sex, Age, Category, Time.period, Value, Count, Denominator) %>%
  mutate(Time.period = if_else(Time.period == "2020/21", "2021", Time.period)) %>%
  filter(Time.period %in% "2021")

#Creating indicator variable names

create_pivot_wider <- function(data, indicator_column, value_prefix, denominator_prefix, count_prefix) {
  data$Indicators <- paste(data[[indicator_column]], data$Age, sep = "_")
  
  pv <- pivot_wider(data, id_cols = Area.Code, names_from = Indicators, values_from = Value, names_prefix = value_prefix)
  pd <- pivot_wider(data, id_cols = Area.Code, names_from = Indicators, values_from = Denominator, names_prefix = denominator_prefix)
  pc <- pivot_wider(data, id_cols = Area.Code, names_from = Indicators, values_from = Count, names_prefix = count_prefix)
  
  ind_list <- list(pv, pc, pd)
  combined_df <- reduce(ind_list, left_join, by = "Area.Code")
  
  # Ensure each column contains only a single value
  for (col in names(combined_df)) {
    combined_df[[col]] <- sapply(combined_df[[col]], function(x) if (is.vector(x)) x[1] else x)
  }
  
  return(combined_df)
}

LCR_GP_21<-create_pivot_wider(data = LCR_GP_21, 
                                   indicator_column = "Indicator.Name", 
                                   value_prefix = "Value", 
                                   denominator_prefix = "Denominator", 
                                   count_prefix = "Count")

LCR_GP_22<-create_pivot_wider(data = LCR_GP_22, 
                              indicator_column = "Indicator.Name", 
                              value_prefix = "Value", 
                              denominator_prefix = "Denominator", 
                              count_prefix = "Count")

LCR_GP_23<-create_pivot_wider(data = LCR_GP_23, 
                              indicator_column = "Indicator.Name", 
                              value_prefix = "Value", 
                              denominator_prefix = "Denominator", 
                              count_prefix = "Count")

#Ensuring columns are in the same order for each of the subsets
LCR_GP_23 <- LCR_GP_23 %>% select(names(LCR_GP_22))
LCR_GP_21 <- LCR_GP_21 %>% select(names(LCR_GP_22))


#Reordering and renaming of relevant columns from GP dataset
reordered_columns<-c(1,
                     2,17,32,
                     3,18,33,
                     4,19,34,
                     5,20,35,
                     6,21,36,
                     7,22,37,
                     8,23,38,
                     9,24,39,
                     10,25,40,
                     11,26,41,
                     12,27,42,
                     13,28,43,
                     14,29,44,
                     15,30,45,
                     16,31,46)

ind_LCR_21<-LCR_GP_21[,reordered_columns]
ind_LCR_22<- LCR_GP_22[,reordered_columns]
ind_LCR_23<- LCR_GP_23[,reordered_columns]


#Renaming of columns from GP dataset
colnames_LCR <- c(
  "Area.Code",
  paste(rep(c("GPPop.65", #65+ population
              "GPPop.0.4", #0-4 population
              "GPPop.85", #85+ population
              "GPPop.5.14", #5-14 population
              "GPPop.75", #75+ population
              "GPPop.u18", #Under 18s population
              "satPhone", #Satisfied with phone access
              "posExp", #Positive experience of their GP practice
              "satApp", #Satisfied with appointment times
              "overallApp", #Overall good experience making an appointment
              "healthCon", #Long-standing health conditions
              "caring", #With caring responsibilities
              "empEdu", #in paid work or full-time education
              "unemp", #Reporting to be unemployed
              "QOF" #QOF points achieved
              ), each = 3),
        c("Value", "Count", "Denom"), sep = ".")
)

for (i in seq_along(colnames_LCR)) {
  ind_LCR_21 <- ind_LCR_21 %>%
    rename_with(~ colnames_LCR[i], .cols = i)
}

for (i in seq_along(colnames_LCR)) {
  ind_LCR_22 <- ind_LCR_22 %>%
    rename_with(~ colnames_LCR[i], .cols = i)
}

for (i in seq_along(colnames_LCR)) {
  ind_LCR_23 <- ind_LCR_23 %>%
    rename_with(~ colnames_LCR[i], .cols = i)
}

#Addition of IMD column to GP dataset
LCR_GP_Cat<-LCR_GP[grep("Deprivation", LCR_GP$Indicator.Name), ]

ind_LCR_21$IMD <- LCR_GP_Cat$Value[match(ind_LCR_21$Area.Code, LCR_GP_Cat$Area.Code)]
ind_LCR_22$IMD <- LCR_GP_Cat$Value[match(ind_LCR_22$Area.Code, LCR_GP_Cat$Area.Code)]
ind_LCR_23$IMD <- LCR_GP_Cat$Value[match(ind_LCR_23$Area.Code, LCR_GP_Cat$Area.Code)]

# Join two datasets together and omitting null values and values not present in all datasets

left_join_and_omit <- function(vacc_data, ind_data, area_code) {
  left_join(vacc_data, ind_data, by = c("GP.code" = "Area.Code")) %>%
    na.omit() %>%
    filter(GP.code != area_code)
}

MMR_24m_21 <- left_join_and_omit(MMR24m_Vacc_21, ind_LCR_21, list( "Y00110", "N82117"))
MMR_5y_21 <- left_join_and_omit(MMR5y_Vacc_21, ind_LCR_21, list( "Y00110", "N82117"))

MMR_24m_22 <- left_join_and_omit(MMR24m_Vacc_22, ind_LCR_22, list( "Y00110", "N82117"))
MMR_5y_22 <- left_join_and_omit(MMR5y_Vacc_22, ind_LCR_22, list( "Y00110", "N82117"))

MMR_24m_23 <- left_join_and_omit(MMR24m_Vacc_23, ind_LCR_23, list( "Y00110", "N82117"))
MMR_5y_23 <- left_join_and_omit(MMR5y_Vacc_23, ind_LCR_23, list( "Y00110", "N82117"))

#Changing data class to allow for calculations

as.data.frame(sapply(MMR_24m_21,class)) #Checking class of each variable to ensure they are numeric for calculations to come

convert_to_numeric <- function(data, MMR_denominator) {
  data <- data %>%
    mutate(across(3:ncol(data), as.numeric)) %>%
    mutate({{ MMR_denominator }} := as.numeric({{ MMR_denominator }}))
  
  return(data)
}

MMR_24m_21 <- convert_to_numeric(MMR_24m_21,Denominator.24m)
MMR_24m_22 <- convert_to_numeric(MMR_24m_22,Denominator.24m)
MMR_24m_23 <- convert_to_numeric(MMR_24m_23,Denominator.24m)

MMR_5y_21 <- convert_to_numeric(MMR_5y_21,Denominator.5y)
MMR_5y_22 <- convert_to_numeric(MMR_5y_22,Denominator.5y)
MMR_5y_23 <- convert_to_numeric(MMR_5y_23,Denominator.5y)

####IMD quintile breakdown####
postcodes<-read.csv("GP codes with deprivation.csv") %>% select(Area.Code,Decile)
IMD<- left_join(MMR_24m_21,postcodes, by = join_by(GP.code == Area.Code))

MMR_24m_21$quintiles <- as.factor(cut(IMD$Decile, breaks = 5, labels = FALSE)) %>% 
  fct_collapse(MMR_24m_21$quintiles, "1&2" = c(1, 2),"3" = "3", "4" = "4", "5" = "5")

MMR_24m_22$quintiles <- as.factor(cut(IMD$Decile, breaks = 5, labels = FALSE)) %>% 
  fct_collapse(MMR_24m_22$quintiles, "1&2" = c(1, 2),"3" = "3", "4" = "4", "5" = "5")

MMR_24m_23$quintiles <- as.factor(cut(IMD$Decile, breaks = 5, labels = FALSE)) %>% 
  fct_collapse(MMR_24m_23$quintiles, "1&2" = c(1, 2),"3" = "3", "4" = "4", "5" = "5")


MMR_5y_21$quintiles <- as.factor(cut(IMD$Decile, breaks = 5, labels = FALSE)) %>% 
  fct_collapse(MMR_5y_21$quintiles, "1&2" = c(1, 2),"3" = "3", "4" = "4", "5" = "5")

MMR_5y_22$quintiles <- as.factor(cut(IMD$Decile, breaks = 5, labels = FALSE)) %>% 
  fct_collapse(MMR_5y_22$quintiles, "1&2" = c(1, 2),"3" = "3", "4" = "4", "5" = "5")

MMR_5y_23$quintiles <- as.factor(cut(IMD$Decile, breaks = 5, labels = FALSE)) %>% 
  fct_collapse(MMR_5y_23$quintiles, "1&2" = c(1, 2),"3" = "3", "4" = "4", "5" = "5")

###Summary Stats###


#Vaccine uptake/GP -> Scatterplot too busy. 

ggplot(MMR_24m, aes(x=GP.code, y=MMR1.24m)) +
  geom_point(shape=18)

ggplot(MMR_5y_omit, aes(x=GP.code, y=MMR2.5y)) +
  geom_point(shape=18)

ggplot(MMR_5y_omit, aes(x=GP.code, y=MMR1.5y)) +
  geom_point(shape=18)

MMR_24m_70<- MMR_24m %>% filter_at(vars(3), any_vars(. < 70))

#Vaccine uptake vs Deprivation

depMMR1<-ggplot() +
  geom_smooth(data = MMR_24m_21, aes(x = IMD, y = MMR1.24m), method = "lm", se = TRUE, color = "red") +
  geom_smooth(data = MMR_24m_22, aes(x = IMD, y = MMR1.24m), method = "lm", se = TRUE, color = "blue") +
  geom_smooth(data = MMR_24m_23, aes(x = IMD, y = MMR1.24m), method = "lm", se = TRUE, color = "green") +
  labs(x = "Index of Multiple Deprivation Score", y = " % Vaccine Uptake") +
  ggtitle("MMR1 Vaccine Uptake at 24 months by Year \n against Index of Multiple Deprivation") + 
  scale_color_manual(name = "Year", 
                     values = c("2021" = "red", "2022" = "blue", "2023" = "green"))+
  ylim(65,100)


depMMR1.5y<-ggplot() +
  geom_smooth(data = MMR_5y_21, aes(x = IMD, y = MMR1.5y), method = "lm", se = TRUE, color = "red") +
  geom_smooth(data = MMR_5y_22, aes(x = IMD, y = MMR1.5y), method = "lm", se = TRUE, color = "blue") +
  geom_smooth(data = MMR_5y_23, aes(x = IMD, y = MMR1.5y), method = "lm", se = TRUE, color = "green") +
  labs(x = "Index of Multiple Deprivation Score", y = " % Vaccine Uptake") +
  ggtitle("MMR1 Vaccine Uptake at 5 years by Year \n against Index of Multiple Deprivation") + 
  scale_color_manual(name = "Year", 
                     values = c("2021" = "red", "2022" = "blue", "2023" = "green"))+
  ylim(65,100)

depMMR2<-ggplot() +
  geom_smooth(data = MMR_5y_21, aes(x = IMD, y = MMR2.5y), method = "lm", se = TRUE, color = "red") +
  geom_smooth(data = MMR_5y_22, aes(x = IMD, y = MMR2.5y), method = "lm", se = TRUE, color = "blue") +
  geom_smooth(data = MMR_5y_23, aes(x = IMD, y = MMR2.5y), method = "lm", se = TRUE, color = "green") +
  labs(x = "Index of Multiple Deprivation Score", y = "% Vaccine Uptake") +
  ggtitle("MMR2 Vaccine Uptake at 5 years by Year \n against Index of Multiple Deprivation") + 
  scale_color_manual(name = "Year", 
                     values = c("2021" = "red", "2022" = "blue", "2023" = "green"))+
  ylim(65,100)

Figure1 <- ggarrange(depMMR1 + theme(legend.position = "none"), 
                     depMMR1.5y + theme(legend.position = "none"), 
                     depMMR2 + theme(legend.position = "none"),
                     labels = c("A", "B", "C"),
                     ncol = 3, nrow = 1)

legend_data <- data.frame(year = c("2021", "2022", "2023"),
                          color = c("red", "blue", "green"))

Figure1 <- Figure1 +
  geom_point(data = legend_data, aes(x = 0.1, y = 0.1, color = year), size = 3, shape = 15) +
  #geom_text(data = legend_data, aes(x = x - 0.03, y = 0.1, label = year), vjust = 0, hjust = 1) +
  theme(legend.position = "bottom",  # Place legend at the bottom
        plot.margin = unit(c(1, 1, 3, 1), "lines")) # Adjust bottom margin

Figure1
#Summary tables
selected_columns21<- MMR_24m_21[, grep("Value", names(MMR_24m_21))]
selected_columns22<- MMR_24m_22[, grep("Value", names(MMR_24m_21))]
selected_columns23<- MMR_24m_23[, grep("Value", names(MMR_24m_21))]

new_col_names <- list(
  "GPPop.65.Value" = "% 65+ population in GP registered population",
  "GPPop.0.4.Value" = "% 0-4 population in GP registered population",
  "GPPop.85.Value" = "% 85+ population in GP registered population",
  "GPPop.5.14.Value" = "% 5-14 population in GP registered population",
  "GPPop.75.Value" = "% 75+ population in GP registered population",
  "GPPop.u18.Value" = "% Under 18s population in GP registered population",
  "satPhone.Value" = "% satisfied with phone access",
  "posExp.Value" = "% who have a positive experience \n of their GP practice",
  "satApp.Value" = "% satisfied with practice appointment times",
  "overallApp.Value" = "% reporting good overall experience \n of making an appointment",
  "healthCon.Value" = "% with a long-standing health condition",
  "caring.Value" = "% with caring responsibility",
  "empEdu.Value" = "% reporting to be in paid work \n or in full-time education",
  "unemp.Value" = "% reporting to be unemployed",
  "QOF.Value" = "% QOF points achieved"
)

tbl_summary21_modified <- selected_columns21 %>%
  tbl_summary(label = new_col_names) 

tbl_summary22_modified <- selected_columns22 %>%
  tbl_summary(label = new_col_names) 

tbl_summary23_modified <- selected_columns23 %>%
  tbl_summary(label = new_col_names) 

merged_tbl_GP<- tbl_merge(
  tbls = list(
    tbl_summary21_modified,
    tbl_summary22_modified,
    tbl_summary23_modified
  ),
  tab_spanner = c("2021", "2022", "2023")
) 

#Density & Histogram
cols<-names(MMR_24m_21)[2:49]
MMR_long <- tidyr::pivot_longer(MMR_24m_21, cols, names_to = "Variable", values_to = "Value")

ggplot(data = MMR_long, aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5) +
  geom_density(color = "red") +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle("Histograms and Density Plots of")

cols_5y<-names(MMR_5y_omit)[2:50]
MMR_long_5y<- tidyr::pivot_longer(MMR_5y_omit, cols_5y, names_to = "Variable", values_to = "Value")

ggplot(data = MMR_long_5y, aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5) +
  geom_density(color = "red") +
  facet_wrap(~ Variable, scales = "free") +
  ggtitle("Histograms and Density Plots of")


#Quantitative normality testing

for (col in colnames(MMR_24m_21[2:49])) {
  if (is.numeric(MMR_24m_omit[[col]])) {
    print(paste("Kolmogorov-Smirnov Test for", col))
    print(ks.test(MMR_24m_omit[[col]], "pnorm", mean = mean(MMR_24m_omit[[col]]), sd = sd(MMR_24m_omit[[col]])))
  } else {
    print(paste("Column", col, "is not numeric"))
  }
}

#Google recommended ks Test for any sample n>50

for (col in colnames(MMR_24m_21[2:49])) {
  if (is.numeric(MMR_24m_omit[[col]])) {
    print(paste("Shapiro-Wilks Test for", col))
    print(ks.test(MMR_24m_21[[col]], "pnorm", mean = mean(MMR_24m_21[[col]]), sd = sd(MMR_24m_omit[[col]])))
  } else {
    print(paste("Column", col, "is not numeric"))
  }
}

for (col in colnames(MMR_5y_21[2:50])) {
  if (is.numeric(MMR_5y_omit[[col]])) {
    print(paste("Shapiro-Wilk Test for", col))
    print(shapiro.test(MMR_5y_21[[col]]))
  } else {
    print(paste("Column", col, "is not numeric"))
  }
}

#Conversation with Rob Treharne -> Suggested Shapiro-Wilks


#Skewness testing

for (col in colnames(MMR_24m_21[2:49])) {
  if (is.numeric(MMR_24m_21[[col]])) {
    print(paste("Skewness Test for", col))
    print(skewness(MMR_24m_21[[col]]))
  } else {
    print(paste("Column", col, "is not numeric"))
  }
}

for (col in colnames(MMR_5y_21[2:50])) {
  if (is.numeric(MMR_5y_omit[[col]])) {
    print(paste("Skewness Test for", col))
    print(skewness(MMR_5y_21[[col]]))
  } else {
    print(paste("Column", col, "is not numeric"))
  }
}

#Kurtosis testing

for (col in colnames(MMR_24m_21[2:49])) {
  if (is.numeric(MMR_24m_21[[col]])) {
    print(paste("Kurtosis Test for", col))
    print(kurtosis(MMR_24m_21[[col]]))
  } else {
    print(paste("Column", col, "is not numeric"))
  }
}

for (col in colnames(MMR_5y_21[2:50])) {
  if (is.numeric(MMR_5y_21[[col]])) {
    print(paste("Skewness Test for", col))
    print(kurtosis(MMR_5y_21[[col]]))
  } else {
    print(paste("Column", col, "is not numeric"))
  }
}

# Boxplot of MMR Datasets
mmr_columns <- function(dataset, dataset_name) {
  selected_cols <- grep("MMR", names(dataset), value = TRUE)
  renamed_cols <- setNames(dataset[selected_cols], paste0(dataset_name, "_", selected_cols))
  return(renamed_cols)
}

df <- list(MMR_24m_21, MMR_24m_22, MMR_24m_23, MMR_5y_21,MMR_5y_22,MMR_5y_23)
dataset_names <- c("MMR_24m_21", "MMR_24m_22", "MMR_24m_23",  "MMR_5y_21","MMR_5y_22", "MMR_5y_23")

MMR_data <- Map(mmr_columns, df, dataset_names)

combined_data <- do.call(cbind, MMR_data)

melted_data <- melt(combined_data)

legend_labels <- c("MMR_24m_21_MMR1.24m" = "MMR1.24m,2021",
                   "MMR_24m_22_MMR1.24m" = "MMR1.24m,2022",
                   "MMR_24m_23_MMR1.24m" = "MMR1.24m,2023",
                   "MMR_5y_21_MMR1.5y"  = "MMR1.5y,2021",
                   "MMR_5y_22_MMR1.5y"  = "MMR1.5y,2022",
                   "MMR_5y_23_MMR1.5y"  = "MMR1.5y,2023",
                   "MMR_5y_21_MMR2.5y"  = "MMR2.5y,2021",
                   "MMR_5y_22_MMR2.5y"  = "MMR2.5y,2022",
                   "MMR_5y_23_MMR2.5y"  = "MMR2.5y,2023")

ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(coef = 1.5) + 
  theme_bw() +
  labs(title = "MMR1 and MMR2 Vaccine uptake between 2021 and 2023",
       x = "Vaccine Dose",
       y = "% Vaccine Uptake",
       fill = "Vaccine dose, Data Collection Time \n and Year of Record") +
  scale_fill_manual(values = c("MMR_24m_21_MMR1.24m" = "steelblue",
                               "MMR_5y_21_MMR1.5y"  = "steelblue1",
                               "MMR_5y_21_MMR2.5y"  = "steelblue3",
                               "MMR_24m_22_MMR1.24m" = "wheat",
                               "MMR_5y_22_MMR1.5y"  = "wheat3",
                               "MMR_5y_22_MMR2.5y"  = "wheat4",
                               "MMR_24m_23_MMR1.24m" = "springgreen",
                               "MMR_5y_23_MMR1.5y"  = "springgreen3",
                               "MMR_5y_23_MMR2.5y"  = "springgreen4"),
                    labels = legend_labels) +
  theme(axis.text.x = element_blank(), legend.position = "bottom") +
  facet_wrap(~ variable, scales = "free", nrow = 1, labeller = labeller(.cols = legend_labels), strip.position = "bottom")+ 
  coord_cartesian(ylim = c(40, 100))



# try 

df <- list(MMR_24m_21, MMR_24m_22, MMR_24m_23, MMR_5y_21, MMR_5y_22, MMR_5y_23)
dataset_names <- c("MMR_24m_21", "MMR_24m_22", "MMR_24m_23", "MMR_5y_21", "MMR_5y_22", "MMR_5y_23")

MMR_data <- Map(mmr_columns, df, dataset_names)

combined_data <- do.call(cbind, MMR_data)

melted_data <- melt(combined_data)

# Reorder melted data columns to match the order of legend labels
melted_data <- melted_data[, match(names(melted_data), names(legend_labels))]

legend_labels <- c("MMR_24m_21_MMR1.24m" = "MMR1.24m,2021",
                   "MMR_24m_22_MMR1.24m" = "MMR1.24m,2022",
                   "MMR_24m_23_MMR1.24m" = "MMR1.24m,2023",
                   "MMR_5y_21_MMR1.5y"  = "MMR1.5y,2021",
                   "MMR_5y_22_MMR1.5y"  = "MMR1.5y,2022",
                   "MMR_5y_23_MMR1.5y"  = "MMR1.5y,2023",
                   "MMR_5y_21_MMR2.5y"  = "MMR2.5y,2021",
                   "MMR_5y_22_MMR2.5y"  = "MMR2.5y,2022",
                   "MMR_5y_23_MMR2.5y"  = "MMR2.5y,2023")

ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(coef = 1.5) + 
  theme_bw() +
  labs(title = "MMR1 and MMR2 Vaccine uptake between 2021 and 2023",
       x = "Vaccine Dose",
       y = "% Vaccine Uptake",
       fill = "Vaccine dose, Data Collection Time \n and Year of Record") +
  scale_fill_manual(values = c("MMR_24m_21_MMR1.24m" = "steelblue",
                               "MMR_24m_22_MMR1.24m" = "wheat",
                               "MMR_24m_23_MMR1.24m" = "springgreen",
                               "MMR_5y_21_MMR1.5y"  = "steelblue1",
                               "MMR_5y_22_MMR1.5y"  = "wheat3",
                               "MMR_5y_23_MMR1.5y"  = "springgreen3",
                               "MMR_5y_21_MMR2.5y"  = "steelblue3",
                               "MMR_5y_22_MMR2.5y"  = "wheat4",
                               "MMR_5y_23_MMR2.5y"  = "springgreen4"),
                    labels = legend_labels) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none ") +
  facet_wrap(~ variable, scales = "free", nrow = 1, labeller = labeller(.cols = legend_labels), strip.position = "bottom") + 
  coord_cartesian(ylim = c(40, 100))
#Colinearity check 

intrest_var_ref<-grep("Value|IMD", colnames(MMR_24m_21), value = TRUE)
intrest_var<-MMR_24m_21[, intrest_var_ref]

correlation_matrix <- cor(intrest_var)
melted_correlation <- melt(correlation_matrix)

# Plot heatmap
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limit = c(-1,1),
                       space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()

#Univariate analysis
interest_var<-names(intrest_var)
response_var <- "MMR1.24m"
response_var1<-"MMR1.5y"
response_var2<-"MMR2.5y"

univariate_models <- interest_var %>%       
  str_c(response_var, .) %>%         
   
  # iterate through each univariate formula
  map(                               
    .f = ~glm(                       
      formula = as.formula(.x),      
      family = "gaussian",           
      data = MMR_24m_21)) %>% 
  map(
    .f = ~tidy(
      .x,            
      conf.int = TRUE)) %>%          
  
  # collapse the list of regression outputs in to one data frame
  bind_rows() %>% 
  
  # round all numeric columns
  mutate(across(where(is.numeric), round, digits = 2))

univariate_models <- interest_var %>%       
  map(~ {
    formula_str <- paste(response_var, .x, sep = "~")
    formula <- as.formula(formula_str)
    glm(formula = formula, data = MMR_24m_21, family = "gaussian")
  }) %>% 
  map(tidy, conf.int = TRUE) %>%          
  bind_rows() %>% 
  mutate(across(where(is.numeric), round, digits = 2))

plot(univariate_models)

#Table of results for univariate analysis

univ_tab <- MMR_24m_21 %>% 
  dplyr::select(interest_var, response_var) %>% ## select variables of interest
  
  tbl_uvregression(                         ## produce univariate table
    method = glm,                           ## define regression want to run (generalised linear model)
    y = response_var,                            ## define outcome variable
    method.args = list(family = gaussian)  ## define what type of glm want to run (gaussian for normal distribution?)
  )

run_univariate_analysis <- function(data, interest_var, response_var) {
  data %>%
    dplyr::select({{ interest_var }}, {{ response_var }}) %>%
    tbl_uvregression(
      method = glm,
      y = {{ response_var }},
      method.args = list(family = gaussian)
    )
}

univariate_tables <- list(
  run_univariate_analysis(MMR_24m_21, interest_var, response_var),
  run_univariate_analysis(MMR_5y_21, interest_var, response_var1),
  run_univariate_analysis(MMR_5y_21, interest_var, response_var2),
  
  run_univariate_analysis(MMR_24m_22, interest_var, response_var),
  run_univariate_analysis(MMR_5y_22, interest_var, response_var1),
  run_univariate_analysis(MMR_5y_22, interest_var, response_var2),
  
  run_univariate_analysis(MMR_24m_23, interest_var, response_var),
  run_univariate_analysis(MMR_5y_23, interest_var, response_var1),
  run_univariate_analysis(MMR_5y_23, interest_var, response_var2)
)
                         


combined_table_21 <- tbl_merge(
  tbls = univariate_tables[1:3],
  tab_spanner = c(
    rep("**MMR1 24m 2021**", 1),
    rep("**MMR1 5y 2021**", 1),
    rep("**MMR2 5y 2021**", 1)
  )
)
combined_table_21

combined_table_22 <- tbl_merge(
  tbls = univariate_tables[4:6],
  tab_spanner = c(
    rep("**MMR1 24m 2022**", 1),
    rep("**MMR1 5y 2022**", 1),
    rep("**MMR2 5y 2022**", 1)
  )
)
combined_table_22

combined_table_23 <- tbl_merge(
  tbls = univariate_tables[7:9],
  tab_spanner = c(
    rep("**MMR1 24m 2023**", 1),
    rep("**MMR1 5y 2023**", 1),
    rep("**MMR2 5y 2023**", 1)
  )
)
combined_table_23


#Multivariate stepwise linear modeling
response_var <- "MMR1.24m"
response_var1<-"MMR1.5y"
response_var2<-"MMR2.5y"

apriori_var1 <- "quintiles"
GP_size_var <- "GPPop.0.4.Denom"
targetpop_var<-"GPPop.0.4.Count"
explanatory_var1<-list("overallApp.Value","QOF.Value","caring.Value")
explanatory_var2<-list("satPhone.Value","QOF.Value","caring.Value")
explanatory_var3<-list("posExp.Value","QOF.Value","caring.Value")
explanatory_var4<-list("satApp.Value","QOF.Value","caring.Value")

explanatory_vars <- list(explanatory_var1, explanatory_var2, explanatory_var3, explanatory_var4)



for (explanatory_var in explanatory_vars) {
  model_formula <- formula(paste(response_var, "~", apriori_var1, "+", GP_size_var, "+", paste(explanatory_var3, collapse = "+")))
  step_model <- step(lm(model_formula, data = ), direction = "backward")
  step_results[[length(step_results) + 1]] <- summary(step_model)
}

# Print the summaries of all stepwise selected models
for (summary_result in step_results) {
  print(summary_result)
}




# Construct the formula using actual variable names
crude_model_MMR.24 <- formula(paste(response_var, "~", apriori_var1, "+",GP_size_var, "+",targetpop_var, "+", paste(explanatory_var1, collapse = "+")))
crude_model_MMR.5y <- formula(paste(response_var1, "~", apriori_var1, "+", GP_size_var, "+",targetpop_var, "+", paste(explanatory_var1, collapse = "+")))
crude_model_MMR2.5y <- formula(paste(response_var2, "~", apriori_var1, "+", GP_size_var, "+",targetpop_var, "+", paste(explanatory_var1, collapse = "+")))

crude_model_MMR.24 <- formula(paste(response_var, "~", apriori_var1, "+", GP_size_var, "+",targetpop_var, "+", paste(explanatory_var2, collapse = "+")))
crude_model_MMR.5y <- formula(paste(response_var1, "~", apriori_var1, "+", GP_size_var, "+",targetpop_var, "+", paste(explanatory_var2, collapse = "+")))
crude_model_MMR2.5y <- formula(paste(response_var2, "~", apriori_var1, "+", GP_size_var, "+",targetpop_var, "+", paste(explanatory_var2, collapse = "+")))

crude_model_MMR.24 <- formula(paste(response_var, "~", apriori_var1, "+", GP_size_var, "+",targetpop_var, "+", paste(explanatory_var3, collapse = "+")))
crude_model_MMR.5y <- formula(paste(response_var1, "~", apriori_var1, "+", GP_size_var, "+",targetpop_var, "+", paste(explanatory_var3, collapse = "+")))
crude_model_MMR2.5y <- formula(paste(response_var2, "~", apriori_var1, "+", GP_size_var, "+", targetpop_var, "+", paste(explanatory_var3, collapse = "+")))

crude_model_MMR.24 <- formula(paste(response_var, "~", apriori_var1, "+", GP_size_var, "+", targetpop_var, "+", paste(explanatory_var4, collapse = "+")))
crude_model_MMR.5y <- formula(paste(response_var1, "~", apriori_var1, "+", GP_size_var, "+", targetpop_var, "+", paste(explanatory_var4, collapse = "+")))
crude_model_MMR2.5y <- formula(paste(response_var2, "~", apriori_var1, "+", GP_size_var, "+", targetpop_var, "+", paste(explanatory_var4, collapse = "+")))
# Fit the model and perform stepwise selection

step_model_24_21<- step(lm(crude_model_MMR.24, data = MMR_24m_21), direction = "backward")
step_model_24_22<- step(lm(crude_model_MMR.24, data = MMR_24m_22), direction = "backward")
step_model_24_23<- step(lm(crude_model_MMR.24, data = MMR_24m_23), direction = "backward")

step_model_1.5_21<- step(lm(crude_model_MMR.5y, data = MMR_5y_21), direction = "backward")
step_model_1.5_22<- step(lm(crude_model_MMR.5y, data = MMR_5y_22), direction = "backward")
step_model_1.5_23<- step(lm(crude_model_MMR.5y, data = MMR_5y_23), direction = "backward")

step_model_2.5_21<- step(lm(crude_model_MMR2.5y, data = MMR_5y_21), direction = "backward")
step_model_2.5_22<- step(lm(crude_model_MMR2.5y, data = MMR_5y_22), direction = "backward")
step_model_2.5_23<- step(lm(crude_model_MMR2.5y, data = MMR_5y_23), direction = "backward")

summary(step_model_24_21) # Estimate in quintile 5 is in relation to 1&2
summary(step_model_24_22)
summary(step_model_24_23)
summary(step_model_1.5_21)
summary(step_model_1.5_22)
summary(step_model_1.5_23)
summary(step_model_2.5_21)
summary(step_model_2.5_22)
summary(step_model_2.5_23)



# Define the list of explanatory variable combinations


# Define a list to store the results


# Loop through each combination of explanatory variables
for (explanatory_var in explanatory_vars) {
  # Define the formula for the model
  model_formula <- formula(paste(response_var, "~", apriori_var1, "+", GP_size_var, "+",GP_size_var, "+" paste(explanatory_var, collapse = "+")))
  
  # Fit the model and perform stepwise selection
  step_model <- step(lm(model_formula, data = your_data), direction = "backward")
  
  # Store the results
  step_results[[length(step_results) + 1]] <- summary(step_model)
}

# Print the summaries of all stepwise selected models
for (summary_result in step_results) {
  print(summary_result)
}



#Final model
sig_var<-"QOF.Value"
sig_var2<-"caring.Value"
sig_var3<-"posExp.Value"

final_model<- formula(paste(response_var, "~", apriori_var1, "+", targetpop_var, "+", sig_var, "+", sig_var2, "+", sig_var3 ))
final_model1<- formula(paste(response_var1, "~", apriori_var1, "+", targetpop_var, "+", sig_var, "+" , sig_var2, "+", sig_var3 ))
final_model2<- formula(paste(response_var2, "~", apriori_var1, "+", targetpop_var, "+", sig_var, "+" , sig_var2, "+", sig_var3   ))

MMR1_21_mod<-lm(final_model, data = MMR_24m_21)
MMR1_22_mod<-lm(final_model, data = MMR_24m_22)
MMR1_23_mod<-lm(final_model, data = MMR_24m_23)

MMR1.5_21_mod<-lm(final_model1, data = MMR_5y_21)
MMR1.5_22_mod<-lm(final_model1, data = MMR_5y_22)
MMR1.5_23_mod<-lm(final_model1, data = MMR_5y_23)

MMR2_21_mod<-lm(final_model2, data = MMR_5y_21)
MMR2_22_mod<-lm(final_model2, data = MMR_5y_22)
MMR2_23_mod<-lm(final_model2, data = MMR_5y_23)

par(mfrow= c(2,2))

summary(MMR1_21_mod)
vif(MMR1_21_mod)
plot(MMR1_21_mod)

MMR1_21_modf<-tbl_regression(MMR1_21_mod)

summary(MMR1_22_mod)
vif(MMR1_22_mod)
plot(MMR1_22_mod)

MMR1_22_modf<-tbl_regression(MMR1_22_mod)

summary(MMR1_23_mod)
vif(MMR1_23_mod)
plot(MMR1_23_mod)

MMR1_23_modf<-tbl_regression(MMR1_21_mod)

combined_table_21_mod <- tbl_merge(
  tbls = list(MMR1_21_modf,MMR1_22_modf, MMR1_23_modf),
  tab_spanner = c(
    rep("**MMR1 24m 2021**", 1),
    rep("**MMR1 24m 2022**", 1),
    rep("**MMR1 24m 2023**", 1)
  )
)

combined_table_21_mod

summary(MMR1.5_21_mod)
vif(MMR1.5_21_mod)
plot(MMR1.5_21_mod)
MMR1.5_21_modf<-tbl_regression(MMR1.5_21_mod)

summary(MMR1.5_22_mod)
vif(MMR1.5_22_mod)
plot(MMR1.5_22_mod)
MMR1.5_22_modf<-tbl_regression(MMR1.5_22_mod)

summary(MMR1.5_23_mod)
vif(MMR1.5_23_mod)
plot(MMR1.5_23_mod)
MMR1.5_23_modf<-tbl_regression(MMR1.5_23_mod)

combined_table_1.5_mod <- tbl_merge(
  tbls = list(MMR1.5_21_modf,MMR1.5_22_modf, MMR1.5_23_modf),
  tab_spanner = c(
    rep("**MMR1 5y 2021**", 1),
    rep("**MMR1 5y 2022**", 1),
    rep("**MMR1 5y 2023**", 1)
  )
)
combined_table_1.5_mod

summary(MMR2_21_mod)
vif(MMR2_21_mod)
plot(MMR2_21_mod)
MMR2_21_modf<-tbl_regression(MMR2_21_mod)

summary(MMR2_22_mod)
vif(MMR2_22_mod)
plot(MMR2_22_mod)
MMR2_22_modf<-tbl_regression(MMR2_22_mod)

summary(MMR2_23_mod)
vif(MMR2_23_mod)
plot(MMR2_23_mod)
MMR2_23_modf<-tbl_regression(MMR2_23_mod)

combined_table_2_mod <- tbl_merge(
  tbls = list(MMR2_21_modf,MMR2_22_modf, MMR2_23_modf),
  tab_spanner = c(
    rep("**MMR2 5y 2021**", 1),
    rep("**MMR2 5y 2022**", 1),
    rep("**MMR2 5y 2023**", 1)
  )
)
combined_table_2_mod

#Model visualisation attempt 

MMR1_21_mod %>%
  augment() %>%
  melt(measure.vars = c(GP_size_var, sig_var, sig_var2, sig_var3), variable.name = c("IV")) %>%
  ggplot(., aes(value, response_var)) +
  geom_smooth(method = "lm") +
  facet_wrap(~IV, scales = "free_x")

#Model visualisation -> try facets for visualising the models, facet per quintile. 

ggplot(MMR_24m_omit , aes(x=QOF_Value,y = MMR1.24m)) + 
  geom_point(aes(size=GPPop.0.4.Denom)) +geom_smooth(method="lm")

ggpairs(data, aes(colour = quintiles))

avPlots(MMR1_21_mod)
avPlots(MMR1_22_mod)
avPlots(MMR1_23_mod)

avPlots(MMR1.5_21_mod)
avPlots(MMR1.5_22_mod)
avPlots(MMR1.5_23_mod)

avPlots(MMR2_21_mod)
avPlots(MMR2_22_mod)
avPlots(MMR2_23_mod)



#Consider for model fit -> Greater relationship at 5y? Greater drop off in uptake. 
#Exclude Brownlow => Unusual population => Model output for Brownlow. 
# Maybe sans sensitivity analysis. 10% change in effect estimates 
#Check how coefficients are interpreted 
#Relationship between deprivation and uptake as part of descriptives

#Maybe include Cheshire & Merseyside
#Fit deprivation as continuous -> Although it's not 
#Cut into quintiles for just LCR
#Combine quintile 1 & 2 -> Distribution of GP practices in each quintile, then combine
#Model without QOF 

