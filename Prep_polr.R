#Preparation before running PoPLR 
setwd("~/Desktop")

#################################################
### 24-2 VF Data Run 
#load original long spreadsheet 
library(readr)
all_locs_long_24d2 <- read_csv("~/Desktop/all_locs_long_24d2.csv")
uid_242 <- all_locs_long_24d2 %>% distinct(`MAPS_IDs`,`eye`)

#load location sheet for VF locations
library(readxl)
Location_242 <- read_excel("~/Desktop/Location_242.xlsx")

#merge two sheets 
all_242_long1 <- all_locs_long_24d2%>%
  left_join(Location_242, by = c('eye', 'x_coord', 'y_coord'))
uid_242 <- all_242_long1 %>% distinct(`MAPS_IDs`,`eye`)

write.csv(all_242_long1, file = "all_242_long1.csv", row.names = FALSE)

#load in cleaned long dataset with only the variables we want
cleaned_242_long1 <- read_csv("~/Desktop/all_242_long1.csv")
uid_242 <- cleaned_242_long1 %>% distinct(`id`,`eye`)

#pivot to wide format 
library(tidyverse)
all_242_wide1 <- pivot_wider(cleaned_242_long1, names_from = location, values_from = td_value)

all_242_wide1$time <- str_sub(all_242_wide1$time, start = 11)
uid_242 <- all_242_wide1 %>% distinct(`id`,`eye`)

ID_dates <- read_excel("~/Desktop/ID_dates.xlsx")
uid_242 <- ID_dates %>% distinct(`id`,`eye`)

ID_dates$baseline_date <- as.Date(ID_dates$baseline_date)
all_242_wide1$date <- as.Date(all_242_wide1$date)

withDx <- read_csv("withDx.csv")
ID_dates1 <- withDx%>%
  left_join(ID_dates, by = c('id','eye')) 
uid_242 <- ID_dates %>% distinct(`id`,`eye`)

#Keep only MAPS visits 
merged_data_242 <- all_242_wide1 %>%
  left_join(ID_dates, by = c('id','eye')) %>%
  filter(date >= baseline_date)
uid_242 <- merged_data_242 %>% distinct(`id`,`eye`)

write.csv(all_242_wide1, file = "all_242_wide1.csv", row.names = FALSE)
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)

count_results <- merged_data_242 %>%
  group_by(id, eye) %>%
  summarize(Count = n())

ID_242 <- count_results %>% filter(Count>5)

merged_data_242_1 <- merged_data_242 %>%
  inner_join(ID_242, by = c('id','eye'))
uid_242 <- merged_data_242_1 %>% distinct(`id`,`eye`)
write.csv(merged_data_242_1, file = "merged_data_242_1.csv", row.names = FALSE)

write.csv(all_242_wide1, file = "all_242_wide1.csv", row.names = FALSE)


#############################################################################
### 10-2 VF Data Run 
#load original long spreadsheet 
library(readr)
all_locs_long_10d2 <- read_excel("~/Desktop/all_locs_long_10d2.xlsx")

#load location sheet for VF locations
library(readxl)
Location_102 <- read_excel("~/Desktop/Location_102.xlsx")

#merge two sheets 
all_102_long1 <- all_locs_long_10d2%>%
  left_join(Location_102, by = c('eye', 'x_coord', 'y_coord'))
uid_102 <- all_102_long1 %>% distinct(`MAPS_IDs`)

write.csv(all_102_long1, file = "all_102_long1.csv", row.names = FALSE)

#load in cleaned long dataset with only the variables we want
cleaned_102_long1 <- read_csv("~/Desktop/all_102_long1.csv")
uid_102 <- cleaned_102_long1 %>% distinct(`id`)

#pivot to wide format 
library(tidyverse)
all_102_wide1 <- pivot_wider(cleaned_102_long1, names_from = location, values_from = td_value)

all_102_wide1$time <- str_sub(all_102_wide1$time, start = 11)
uid_102 <- all_102_wide1 %>% distinct(`id`)

ID_dates <- read_excel("ID_dates.xlsx")

ID_dates$baseline_date <- as.Date(ID_dates$baseline_date)
all_102_wide1$date <- as.Date(all_102_wide1$date)

withDx <- read_csv("withDx.csv")
ID_dates1 <- withDx%>%
  left_join(ID_dates, by = c('id','eye')) 
uid_102 <- ID_dates1 %>% distinct(`id`)

#Keep only MAPS visits 
merged_data_102 <- all_102_wide1 %>%
  left_join(ID_dates1, by = c('id','eye')) %>%
  filter(date >= baseline_date)

merged_data_102$time <- str_sub(merged_data_102$time, start = 11)

write.csv(all_102_wide1, file = "all_102_wide1.csv", row.names = FALSE)
write.csv(merged_data_102, file = "merged_data_102.csv", row.names = FALSE)

count_results <- merged_data_102 %>%
  group_by(id, eye) %>%
  summarize(Count = n())

ID_102 <- count_results %>% filter(Count>5)

merged_data_102_1 <- merged_data_102 %>%
  inner_join(ID_102, by = c('id','eye'))

write.csv(merged_data_102_1, file = "merged_data_102_1.csv", row.names = FALSE)


###################################################################################
### 24-2 VF Central 12 Data Run 
#load original long spreadsheet 
library(readr)
all_locs_242_central <- read_csv("all_locs_long_24d2_central.csv")

#load location sheet for VF locations
library(readxl)
Location_242 <- read_excel("~/Desktop/Location_242.xlsx")

#merge two sheets 
all_242_clong1 <- all_locs_242_central%>%
  left_join(Location_242, by = c('eye', 'x_coord', 'y_coord'))
uid_242c <- all_242_long1 %>% distinct(`MAPS_IDs`)

write.csv(all_242_clong1, file = "all_242_clong1.csv", row.names = FALSE)

#load in cleaned long dataset with only the variables we want
cleaned_242_clong1 <- read_csv("all_242_clong1.csv")
uid_242c <- cleaned_242_clong1 %>% distinct(`id`)

#pivot to wide format 
library(tidyverse)
all_242_cwide1 <- pivot_wider(cleaned_242_clong1, names_from = location, values_from = td_value)

all_242_cwide1$time <- str_sub(all_242_cwide1$time, start = 11)
uid_242c <- all_242_cwide1 %>% distinct(`id`)

ID_dates <- read_excel("ID_dates.xlsx")

ID_dates$baseline_date <- as.Date(ID_dates$baseline_date)
all_102_wide1$date <- as.Date(all_102_wide1$date)

withDx <- read_csv("withDx.csv")
ID_dates1 <- withDx%>%
  left_join(ID_dates, by = c('id','eye')) 
uid_102 <- ID_dates1 %>% distinct(`id`)

#Keep only MAPS visits 
merged_data_242c <- all_242_cwide1 %>%
  left_join(ID_dates1, by = c('id','eye')) %>%
  filter(date >= baseline_date)

write.csv(all_102_wide1, file = "all_102_wide1.csv", row.names = FALSE)
write.csv(merged_data_242c, file = "merged_data_242c.csv", row.names = FALSE)

count_results <- merged_data_242c %>%
  group_by(id, eye) %>%
  summarize(Count = n())

write.csv(all_242_wide1, file = "all_242_wide1.csv", row.names = FALSE)

###################################################################################
### Combination Data Run 
#load original long spreadsheet 
library(readr)
all_locs_long_b <- read_csv("~/Desktop/all_locs_long_24and10_combined.csv")

#load location sheet for VF locations
library(readxl)
Locations_forCombines <- read_csv("~/Desktop/Locations_forCombines.csv")

#merge two sheets 
all_242_blong1 <- all_locs_long_b%>%
  left_join(Locations_forCombines, by = c('eye', 'x_coord', 'y_coord'))
uid_242b <- all_242_blong1 %>% distinct(`MAPS_IDs`)

write.csv(all_242_blong1, file = "all_242_blong1.csv", row.names = FALSE)

#load in cleaned long dataset with only the variables we want
cleaned_242_blong <- read_excel("all_242_blong1.xlsx")
uid_242b <- cleaned_242_blong %>% distinct(`id`)

#pivot to wide format 
library(tidyverse)
all_242_bwide1 <- pivot_wider(cleaned_242_blong, names_from = Location, values_from = td_value)

all_242_bwide1$time <- str_sub(all_242_bwide1$time, start = 11)
uid_242b <- all_242_bwide1 %>% distinct(`id`)

ID_dates <- read_excel("~/Desktop/ID_dates.xlsx")

ID_dates$baseline_date <- as.Date(ID_dates$baseline_date)
all_242_bwide1$date <- as.Date(all_242_bwide1$date)

withDx <- read_csv("~/Desktop/withDx.csv")
ID_dates1 <- withDx%>%
  left_join(ID_dates, by = c('id','eye')) 
uid_102 <- ID_dates1 %>% distinct(`id`)

#Keep only MAPS visits 
merged_data_242b <- all_242_bwide1 %>%
  left_join(ID_dates1, by = c('id','eye')) %>%
  filter(date >= baseline_date)

write.csv(all_242_bwide1, file = "all_242_bwide1.csv", row.names = FALSE)
write.csv(merged_data_242b, file = "merged_data_242b.csv", row.names = FALSE)

count_results <- merged_data_242b %>%
  group_by(id, eye) %>%
  summarize(Count = n())

write.csv(all_242_wide1, file = "all_242_wide1.csv", row.names = FALSE)

merged_data_242b <- read_excel("merged_data_242b.xlsx")
merged_data_242b_2 <- read_excel("merged_data_242b_2.xlsx")
merged_data_242b <- merged_data_242b %>%
  left_join(merged_data_242b_2, by = c('id','eye','date'))

write.csv(merged_data_242b, file = "merged_data_242b.csv", row.names = FALSE)

merged_all_data <- read_csv("merged_all_data.csv")
count_results <- merged_all_data %>%
  group_by(id, eye) %>%
  summarize(Count = n())

###################################################################################
X10_2_dates <- read_excel("~/Desktop/10-2_dates.xlsx")
matched_data_242 <- X10_2_dates %>%
  inner_join(merged_data_242, by = c('id','eye','date'))
count_results <- matched_data_242 %>%
  group_by(id, eye) %>%
  summarize(Count = n())
ID_242 <- count_results %>% filter(Count>5)
final_242 <- matched_data_242 %>%
  inner_join(ID_242, by = c('id','eye'))
write.csv(final_242, file = "final_242.csv", row.names = FALSE)

final_102 <- merged_data_102_1%>%
  inner_join(ID_242, by = c('id','eye'))
write.csv(final_102, file = "final_102.csv", row.names = FALSE)

all_242_long1 <- read_csv("all_242_long1.csv")
all_242_long1 <- left_join(all_242_long1,
                           Location_242,
                           by = c("eye", "location")) %>%
  select(-location)

all_102_long_tbcombined <- all_102_long1 %>% select(MAPS_IDs, eye,
                                                    `Perimetry Exam Date`,
                                                    `Perimetry Exam Time`,
                                                    td_value,
                                                    x_coord, y_coord)
write.csv(all_102_long_tbcombined , file = "all_102_long_tbcombined.csv", row.names = FALSE)
write.csv(all_242_long1 , file = "all_242_long1.csv", row.names = FALSE)

all_242_long1 <- read_csv("all_242_long1.csv")
all_102_long_tbcombined <- read_csv("all_102_long_tbcombined.csv")

final_combined <- bind_rows(all_242_long1,
          all_102_long_tbcombined)

Locations_forCombines <- read_csv("~/Desktop/Locations_forCombines.csv")

final_combined <- final_combined%>%
  left_join(Locations_forCombines, by = c('eye', 'x_coord', 'y_coord'))
final_combined <- final_combined %>% select(id, eye, date, td_value, time, Location)
final_combined$time <- str_sub(final_combined$time, start = 11)

final_combined <- final_combined%>%
  inner_join(ID_242, by = c('id','eye'))
uid_c <- final_combined %>% distinct(`id`,'eye')

write.csv(final_combined, file = "final_combined.csv", row.names = FALSE)
write.csv(ID_242, file = "ID_242.csv", row.names = FALSE)

#pivot to wide format 
final_combined <- read_csv("final_combined.csv")

final_combined_wide <- pivot_wider(final_combined, names_from = Location, values_from = td_value)
df_filtered <- final_combined_wide %>% filter(l33 != "NULL")
df_filtered <- df_filtered %>% filter(l38 != "NULL")
write.csv(df_filtered, file = "df_filtered.csv", row.names = FALSE)

df_filtered$time <- 12

problematic_columns <- sapply(df_filtered, function(col) inherits(col[[1]], "list"))

# Exclude problematic columns
df_filtered <- df_filtered %>% select(!problematic_columns)


write.csv(res_all_eye_102, file = "res_all_eye_102.csv", row.names = FALSE)
write.csv(res_all_eyes_242, file = "res_all_eyes_242.csv", row.names = FALSE)
write.csv(res_all_eyes_242C, file = "res_all_eyes_242C.csv", row.names = FALSE)


final_242_copy <- read_csv("final_242 copy.csv")
final_102_copy <- read_csv("final_102 copy.csv")

combined_dates <- final_242_copy%>%
  left_join(final_102_copy, by = c('id','eye', 'date'))
