library(dplyr)

######################## less1 ################################################# 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Presses_8_5_less1_wo_5/Presses_8_5_less1_wo_5"   

file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

read_and_fix <- function(file) {
  df <- read.csv(file)
   
  if ("NextNewestActor" %in% names(df)) {
    df$NextNewestActor <- as.character(df$NextNewestActor)
  }
   
  if ("IdActor_pr" %in% names(df)) {
    df$IdActor_pr <- as.character(df$IdActor_pr)
  }
  
  return(df)
}


data_less1 <- file_list %>%
  lapply(read_and_fix) %>%
  bind_rows(.id = "source")

 
str(data_less1)

data_less1$is_within_180_mid = abs(data_less1$theta_deg1) <= 90

data_less1  <- data_less1  %>%
  filter(is_within_180_mid == TRUE)

head(data_less1)
nrow(data_less1)

################################## 1-1.5 ####################################### 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Presses_8_5_1_1.5_wo_5/Presses_8_5_1_1.5_wo_5"   

file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

read_and_fix <- function(file) {
  df <- read.csv(file)
   
  if ("NextNewestActor" %in% names(df)) {
    df$NextNewestActor <- as.character(df$NextNewestActor)
  }
   
  if ("IdActor_pr" %in% names(df)) {
    df$IdActor_pr <- as.character(df$IdActor_pr)
  }
  
  return(df)
}


data_1_1.5 <- file_list %>%
  lapply(read_and_fix) %>%
  bind_rows(.id = "source")
 
str(data_1_1.5)

data_1_1.5$is_within_180_mid = abs(data_1_1.5$theta_deg1) <= 90

data_1_1.5  <- data_1_1.5  %>% 
  filter(is_within_180_mid == TRUE) 

head(data_1_1.5)
nrow(data_1_1.5)

############################## 1.5-2 ###########################################  
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Presses_8_5_1.5_2_wo_5/Presses_8_5_1.5_2_wo_5"   

file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

read_and_fix <- function(file) {
  df <- read.csv(file)
   
  if ("NextNewestActor" %in% names(df)) {
    df$NextNewestActor <- as.character(df$NextNewestActor)
  } 
  if ("IdActor_pr" %in% names(df)) {
    df$IdActor_pr <- as.character(df$IdActor_pr)
  }
  
  return(df)
}


data_1.5_2 <- file_list %>%
  lapply(read_and_fix) %>%
  bind_rows(.id = "source")
 
str(data_1.5_2)

data_1.5_2$is_within_180_mid = abs(data_1.5_2$theta_deg1) <= 90

data_1.5_2  <- data_1.5_2  %>% 
  filter(is_within_180_mid == TRUE) 

head(data_1.5_2)
nrow(data_1.5_2)

######################################### 2-2.5 ############################### 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Presses_8_5_2_2.5_wo_5/Presses_8_5_2_2.5_wo_5"  

file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

read_and_fix <- function(file) {
  df <- read.csv(file)
   
  if ("NextNewestActor" %in% names(df)) {
    df$NextNewestActor <- as.character(df$NextNewestActor)
  }
   
  if ("IdActor_pr" %in% names(df)) {
    df$IdActor_pr <- as.character(df$IdActor_pr)
  }
  
  return(df)
}


data_2_2.5 <- file_list %>%
  lapply(read_and_fix) %>%
  bind_rows(.id = "source")
 
str(data_2_2.5)

data_2_2.5$is_within_180_mid = abs(data_2_2.5$theta_deg1) <= 90

data_2_2.5  <- data_2_2.5  %>% 
  filter(is_within_180_mid == TRUE) 

head(data_2_2.5)
nrow(data_2_2.5)

############################ 2.5-3 #############################################
 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Presses_8_5_2.5_3_wo_5/Presses_8_5_2.5_3_wo_5"   

file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

read_and_fix <- function(file) {
  df <- read.csv(file)
  
   
  if ("NextNewestActor" %in% names(df)) {
    df$NextNewestActor <- as.character(df$NextNewestActor)
  }
  
 
  if ("IdActor_pr" %in% names(df)) {
    df$IdActor_pr <- as.character(df$IdActor_pr)
  }
  
  return(df)
}


data_2.5_3 <- file_list %>%
  lapply(read_and_fix) %>%
  bind_rows(.id = "source")

 
str(data_2.5_3)

data_2.5_3$is_within_180_mid = abs(data_2.5_3$theta_deg1) <= 90

data_2.5_3  <- data_2.5_3  %>% 
  filter(is_within_180_mid == TRUE) 

head(data_2.5_3)
nrow(data_2.5_3)

data_1_1.5 <- dplyr::select(data_1_1.5, -is_within_200_mid, -is_within_200)
data_2_2.5 <- dplyr::select(data_2_2.5, -is_within_200_mid, -is_within_200)

data_less1$To_identify_pressers_po = data_less1$opponents_within_15_po+data_less1$opponents_greater_15_po

data_1_1.5$To_identify_pressers_po = data_1_1.5$opponents_within_15_po+data_1_1.5$opponents_greater_15_po

data_1.5_2$To_identify_pressers_po = data_1.5_2$opponents_within_15_po+data_1.5_2$opponents_greater_15_po

data_2_2.5$To_identify_pressers_po = data_2_2.5$opponents_within_15_po+data_2_2.5$opponents_greater_15_po

data_2.5_3$To_identify_pressers_po = data_2.5_3$opponents_within_15_po+data_2.5_3$opponents_greater_15_po

data_less1$condition = "<1"
data_1_1.5$condition = "1-1.5"
data_1.5_2$condition = "1.5-2"
data_2_2.5$condition = "2-2.5"
data_2.5_3$condition = "2.5-3"

combined_data = rbind(data_less1, data_1_1.5, data_1.5_2, data_2_2.5, data_2.5_3)
combined_data <- combined_data %>%
  distinct(across(1:7), .keep_all = TRUE)
head(combined_data)
nrow(combined_data)

combined_data$absorbing_status <- ifelse(
  combined_data$OB & combined_data$NextNowSameTeam, "OB",
  ifelse(!combined_data$OB & !combined_data$NextNowSameTeam, "TO",
         ifelse(combined_data$OB & !combined_data$NextNowSameTeam, "OB", 
                "BP")))  

 
combined_data$absorbing_status[is.na(combined_data$absorbing_status)] <- "BP"
 




