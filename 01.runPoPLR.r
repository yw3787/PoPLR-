setwd("")

library(visualFields)
library(combinat)
library(matrixStats)
source("polr.r")

nperm <- factorial(7)

# run analysis for each eye
runAnalysisDataset <- function(vf, nperm) {
  uid <- unique(data.frame(id = vf$id, eye = vf$eye)) # get all eyes
  return(lapply(1:nrow(uid), function(i) #for each eye run PoPLR
    return(list(id = uid$id[i], eye = uid$eye[i], polr(vf[vf$id == uid$id[i] & vf$eye == uid$eye[i],], nperm)))))
}

#######################################################################
#another way 

#run on 24-2 data 
vf24 <- vfsort(read_csv("final_242.csv")) 

vf24$date <- as.Date(vf24$date) # convert to R date class

vf24[,c("l26", "l35")] <- NULL # remove data from the blind spot

library(dplyr)
uid <- vf24 %>% distinct(id,eye)

res_all_eyes <- tibble()
for(i in 1:nrow(uid)){
  one_eye <- vf24 %>% filter(id == uid$id[i],
                             eye == uid$eye[i])
  
  res24 <- runAnalysisDataset(one_eye, nperm)
  
  p_value <- res24[[1]][[3]]$p
  s_value <- res24[[1]][[3]]$S
  
  res_one_eye <- tibble(id = uid$id[i],
                        eye = uid$eye[i],
                        first_date = one_eye$date[1],
                        last_date = one_eye$date[nrow(one_eye)],
                        p_value = p_value,
                        s_value = s_value)
  
  
  res_all_eyes <- bind_rows(res_all_eyes,
                            res_one_eye)
  
  
  print(c(i,p_value, s_value))

  
}

#add a column to tell us progressors 
res_all_eyes_242 <- res_all_eyes %>%
  mutate(Grade = case_when(
    p_value < 0.05 ~ "Progressors",
    p_value >= 0.05 ~ "Non-Progressors"))

#52/166 Progressors = 31.33% 

#################################################
#run on 10-2 Data 
vf10 <- vfsort(read_csv("final_102.csv")) 
vf10$date <- as.Date(vf10$date)

uid <- vf10 %>% distinct(id,eye)
res_all_eyes <- tibble()
for(i in 1:nrow(uid)){
  one_eye <- vf10 %>% filter(id == uid$id[i],
                             eye == uid$eye[i])
  
  res10 <- runAnalysisDataset(one_eye, nperm)
  
  p_value <- res10[[1]][[3]]$p
  s_value <- res10[[1]][[3]]$S
  
  res_one_eye <- tibble(id = uid$id[i],
                        eye = uid$eye[i],
                        first_date = one_eye$date[1],
                        last_date = one_eye$date[nrow(one_eye)],
                        p_value = p_value,
                        s_value = s_value)
  
  
  res_all_eyes <- bind_rows(res_all_eyes,
                            res_one_eye)
  
  
  print(c(i,p_value, s_value))
  
  
}

res_all_eye_102 <- res_all_eyes %>%
  mutate(Grade = case_when(
    p_value < 0.05 ~ "Progressors",
    p_value >= 0.05 ~ "Non-Progressors"))


#################################################
#run on 24-2 Central Data 
vf24c <- vfsort(read_csv("final_242C.csv")) 
vf24c$date <- as.Date(vf24c$date)

uid <- vf24c %>% distinct(id,eye)
res_all_eyes <- tibble()
for(i in 1:nrow(uid)){
  one_eye <- vf24c %>% filter(id == uid$id[i],
                             eye == uid$eye[i])
  
  res24c <- runAnalysisDataset(one_eye, nperm)
  
  p_value <- res24c[[1]][[3]]$p
  s_value <- res24c[[1]][[3]]$S
  
  res_one_eye <- tibble(id = uid$id[i],
                        eye = uid$eye[i],
                        first_date = one_eye$date[1],
                        last_date = one_eye$date[nrow(one_eye)],
                        p_value = p_value,
                        s_value = s_value)
  
  
  res_all_eyes <- bind_rows(res_all_eyes,
                            res_one_eye)
  
  
  print(c(i,p_value, s_value))
  
  
}

res_all_eyes_242C <- res_all_eyes %>%
  mutate(Grade = case_when(
    p_value < 0.05 ~ "Progressors",
    p_value >= 0.05 ~ "Non-Progressors"))

#30/117 Progressors = 25.64%

#################################################
#run on combined data 
library(readr)
vf24b <- vfsort(read_csv("final_combined.csv")) 

vf24b$date <- as.Date(vf24b$date, format = "%m/%d/%y")

columns_to_convert <- 4:114

# Loop through specified columns and convert to integer
for (col in columns_to_convert) {
  vf24b[[col]] <- as.integer(vf24b[[col]])
}



vf24b <- vfsort(df_filtered)
vf24b <- read_excel("merged_data_242b.xlsx")
library(magrittr)
library(dplyr)
vf24b$date <- as.Date(vf24b$date)

uid <- vf24b %>% distinct(id,eye)
res_all_eyes <- tibble()
for(i in 1:nrow(uid)){
  one_eye <- vf24b %>% filter(id == uid$id[i],
                              eye == uid$eye[i])
  
  res24b <- runAnalysisDataset(one_eye, nperm)
  
  p_value <- res24b[[1]][[3]]$p
  s_value <- res24b[[1]][[3]]$S
  
  res_one_eye <- tibble(id = uid$id[i],
                        eye = uid$eye[i],
                        first_date = one_eye$date[1],
                        last_date = one_eye$date[nrow(one_eye)],
                        p_value = p_value,
                        s_value = s_value)
  
  
  res_all_eyes <- bind_rows(res_all_eyes,
                            res_one_eye)
  
  
  print(c(i,p_value, s_value))
  
  
}


library(dplyr)

res_all_eyes_combined <- res_all_eyes %>%
  mutate(Grade = case_when(
    p_value < 0.05 ~ "Progressors",
    p_value >= 0.05 ~ "Non-Progressors"))

write.csv(res_all_eyes, file = "res_242b_eyes.csv", row.names = FALSE)
