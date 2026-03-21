# Load libraries
library(dplyr) 

# Load "Final" datasets

################################# less1 ########################################
 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Final_8_5_less1_wo_5/Final_8_5_less1_wo_5"   

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
head(data_less1)
nrow(data_less1)

################################### 1-1.5 #####################################
 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Final_8_5_1_1.5_wo_5/Final_8_5_1_1.5_wo_5"  

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
head(data_1_1.5)
nrow(data_1_1.5)


####################################### 1.5-2 ##################################
 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Final_8_5_1.5_2_wo_5/Final_8_5_1.5_2_wo_5"   
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

read_and_fix <- function(file) {
  df <- read.csv(file)
  
  # Convert NextNewestActor to character if it exists
  if ("NextNewestActor" %in% names(df)) {
    df$NextNewestActor <- as.character(df$NextNewestActor)
  }
  
  # Convert IdActor_pr to character if it exists
  if ("IdActor_pr" %in% names(df)) {
    df$IdActor_pr <- as.character(df$IdActor_pr)
  }
  
  return(df)
}


data_1.5_2 <- file_list %>%
  lapply(read_and_fix) %>%
  bind_rows(.id = "source")

 
str(data_1.5_2) 
head(data_1.5_2)
nrow(data_1.5_2)

############################ 2-2.5 ############################################
 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Final_8_5_2_2.5_wo_5/Final_8_5_2_2.5_wo_5"   

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
head(data_2_2.5)
nrow(data_2_2.5)

################################ 2.5-3 ######################################### 
 
folder_path <- "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Final_8_5_2.5_3_wo_5/Final_8_5_2.5_3_wo_5"   

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
head(data_2.5_3)
nrow(data_2.5_3)

data_less1$condition = "<1"
data_1_1.5$condition = "1-1.5"
data_1.5_2$condition = "1.5-2"
data_2_2.5$condition = "2-2.5"
data_2.5_3$condition = "2.5-3"

Final_data = rbind(data_less1, data_1_1.5, data_1.5_2, data_2_2.5, data_2.5_3)
head(Final_data)
nrow(Final_data)


# Load "Presses" datasets

############################### less1 ########################################## 
 
folder_path <- "C:/Users/pavan/Downloads/OneDrive_1_8-27-2025/Presses_8_5_less1_wo_5/Presses_8_5_less1_wo_5"  

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

############################# 1-1.5 ############################################ 
 
folder_path <- "C:/Users/pavan/Downloads/OneDrive_1_8-27-2025/Presses_8_5_1_1.5_wo_5/Presses_8_5_1_1.5_wo_5"   

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
 
folder_path <- "C:/Users/pavan/Downloads/OneDrive_1_8-27-2025/Presses_8_5_1.5_2_wo_5/Presses_8_5_1.5_2_wo_5"   

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

############################ 2-2.5 ############################################# 
 
folder_path <- "C:/Users/pavan/Downloads/OneDrive_1_8-27-2025/Presses_8_5_2_2.5_wo_5/Presses_8_5_2_2.5_wo_5"  

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

################################ 2.5-3 ######################################### 
 
folder_path <- "C:/Users/pavan/Downloads/OneDrive_1_8-27-2025/Presses_8_5_2.5_3_wo_5/Presses_8_5_2.5_3_wo_5"  

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

press_data = rbind(data_less1, data_1_1.5, data_1.5_2, data_2_2.5, data_2.5_3)
head(press_data)
nrow(press_data)

press_data <- press_data %>%
  distinct(across(1:7), .keep_all = TRUE)
nrow(press_data)

press_data$absorbing_status <- ifelse(
  press_data$OB & press_data$NextNowSameTeam, "OB",
  ifelse(!press_data$OB & !press_data$NextNowSameTeam, "TO",
         ifelse(press_data$OB & !press_data$NextNowSameTeam, "OB", 
                "BP")))  
 
press_data$absorbing_status[is.na(press_data$absorbing_status)] <- "BP"

 
table(press_data$absorbing_status)
press_data = press_data[press_data$PrevNowSameTeam==TRUE,]
 
 
Final_merged <- Final_data %>%
  left_join(
    press_data,
    by = c("source", "match_id", "IdHalf", "Time", "X_player_po", "Y_player_po"),   
    suffix = c("_final", "_press")                   
  )

dim(Final_data)
dim(press_data)
dim(Final_merged)

Presses_filtered <- Final_merged %>%
  arrange(source, match_id, IdHalf, Time) %>%
  group_by(source, match_id, IdHalf) %>%
  mutate(time_diff = c(Inf, diff(Time))) %>%   
  mutate(press_group = NA_integer_) %>%
  {
    df <- .
    grp_id <- 1
     
    idxs <- which(!is.na(df$HomeTeam_press))
    for (i in idxs) {
      j <- i
       
      while (j >= 1 && (j == i || df$time_diff[j + 1] <= 300)) {
     
        if (is.na(df$press_group[j])) {
          df$press_group[j] <- grp_id
        }
        j <- j - 1
      }
      grp_id <- grp_id + 1
    }
    df
  } %>%
  filter(!is.na(press_group)) %>%   
  ungroup() %>%
  select(-time_diff)

dim(Presses_filtered)

Presses_filtered <- Presses_filtered %>%
  group_by(source, match_id, IdHalf, press_group) %>%
  mutate(
    FirstTimeInGroup = first(Time),
    LastTimeInGroup  = last(Time)
  ) %>%
  ungroup()

Presses_filtered <- Presses_filtered %>%
  rowwise() %>%
  mutate(
    press_outcome_time = case_when(
      is.na(absorbing_status) ~ NA_real_,
      absorbing_status == "TO" ~ TO_Time,
      absorbing_status == "BP" ~ BP_Time,
      absorbing_status == "OB" ~ OB_Time,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()

Presses_filtered$press_outcome_time_rounded <- floor(Presses_filtered$press_outcome_time / 100) * 100

Presses_filtered_matched <- Presses_filtered %>%
  dplyr::filter(!is.na(absorbing_status))


Presses_filtered_nonNA <- Presses_filtered_matched %>%
  distinct(across(1:7), .keep_all = TRUE)

press_path = "C:/Users/pavan/Desktop/SFU/Research/Datasets/8_5/Presses_filtered_nonNA.csv"
write.csv(Presses_filtered_nonNA, press_path, row.names = FALSE)

