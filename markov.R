library(purrr)
library(dplyr)
 
data <- read.csv("C:/Users/pavan/Desktop/SFU/Research/Datasets/9_26/Presses_final_tracking_W_2m_wo_event_9_26.csv")

 
data$absorbing_status[data$absorbing_status == "OB" & data$output == "e"] <- "TO"

data <- data %>%
  mutate(
    sequence_nolr = pmap_chr(
      list(
        strsplit(collapsed_region_sequence, ",\\s*"),
        absorbing_status,
        collapsed_region_sequence,
        output
      ),
      function(seq_raw, absorb, coll_seq, out) {
        
        seq_nolr <- sub("^[rl]", "", seq_raw)
         
        if (length(seq_nolr) == 1 && !is.na(coll_seq)) {
          return(paste0(seq_nolr, ",", absorb))
        }
         
        if (length(seq_nolr) == 1 && is.na(coll_seq)) {
          return(paste0(seq_nolr, ",", out))
        }
         
        paste(seq_nolr, collapse = ",")
      }
    )
  )

 
data$sequence_nolr_w_output  <-
  paste(data$sequence_nolr, ",", data$absorbing_status, ",", data$absorbing_status)

data <- data %>%
  mutate(
    sequence_nolr_w_output =
      sapply(
        strsplit(sequence_nolr_w_output, ","),
        function(x) paste(x[x != "NA"], collapse = ",")
      )
  )



sequence_column <- "sequence_nolr_w_output"
 
data[[sequence_column]] <- gsub("c", "a", data[[sequence_column]])
data[[sequence_column]] <- gsub("f", "d", data[[sequence_column]])

data[[sequence_column]] <- gsub("\\s+", "", data[[sequence_column]])
data[[sequence_column]] <- gsub(",+", ",", data[[sequence_column]])
data[[sequence_column]] <- gsub("^,|,$", "", data[[sequence_column]])

data$first_one <- sapply(
  data$sequence_nolr_w_output,
  function(x) strsplit(x, ",")[[1]][1]
)

prop.table(table(data$first_one))

P0 <- c(a = 0.1554766, 
        b = 0.1086098, 
        d = 0.3339916, 
        e = 0.4019221,
        BP = 0,
        OB = 0,
        TO = 0)
P0

data$first_two <- sapply(
  data$sequence_nolr_w_output,
  function(x) paste(strsplit(x, ",")[[1]][1:2], collapse = ",")
)

 

SEPARATOR <- "END_SEQ"
data_sequence_separated <- character(0)

for (s in data$first_two) {
  if (is.na(s) || nchar(s) == 0) next
  states_vector <- unlist(strsplit(s, ","))
  states_vector <- states_vector[states_vector != ""]
  data_sequence_separated <- c(data_sequence_separated, states_vector, SEPARATOR)
}

 

N <- length(data_sequence_separated)
if (N < 2) stop("Sequence too short for 1-step transitions")
 
i_index <- data_sequence_separated[1:(N-1)]
j_index <- data_sequence_separated[2:N]
 
valid_indices <- (i_index != SEPARATOR) & (j_index != SEPARATOR)
i_valid <- i_index[valid_indices]
j_valid <- j_index[valid_indices]

desired_order <- c("a", "b", "d", "e", "BP", "OB", "TO")

 
OneStep_Counts <- table(
  factor(i_valid, levels = desired_order),
  factor(j_valid, levels = desired_order)
)

OneStep_Counts = OneStep_Counts[1:4,]
BP = c(0,0,0,0,4961,0,0)
OB = c(0,0,0,0,0,376,0)
TO = c(0,0,0,0,0,0,2259)
OneStep_Counts = rbind(OneStep_Counts, BP, OB, TO)
OneStep_Counts

paste("Total: ",sum(OneStep_Counts[1:4,]))

P1 = OneStep_Counts / rowSums(OneStep_Counts)
round(P1,3)

data$first_three <- sapply(
  data$sequence_nolr_w_output,
  function(x) {
    parts <- strsplit(x, ",")[[1]]
    if (length(parts) >= 3) {
      paste(parts[1:3], collapse = ",")
    } else {
      NA
    }
  }
)

data$first_three_rm_first <- sapply(
  data$first_three,
  function(x) {
    if (is.na(x)) return(NA)
    parts <- strsplit(x, ",")[[1]]
    paste(parts[2:3], collapse = ",")   # always length 2
  }
)

bad_patterns <- c("BP,BP", "TO,TO", "OB,OB")

data$first_three_rm_first <- ifelse(
  data$first_three_rm_first %in% bad_patterns,
  NA,
  data$first_three_rm_first
)

 
SEPARATOR <- "END_SEQ"
data_sequence_separated <- character(0)

for (s in data$first_three_rm_first) {
  
  if (is.na(s)) next
  
  parts <- unlist(strsplit(s, ","))
  parts <- parts[parts != ""]
  
  if (length(parts) < 2) next
  
  data_sequence_separated <- c(data_sequence_separated, parts, SEPARATOR)
}

 
N <- length(data_sequence_separated)
if (N < 2) stop("Sequence too short")

i_index <- data_sequence_separated[1:(N-1)]
j_index <- data_sequence_separated[2:N]

valid <- (i_index != SEPARATOR) & (j_index != SEPARATOR)
i_valid <- i_index[valid]
j_valid <- j_index[valid]

desired_order <- c("a", "b", "d", "e", "BP", "OB", "TO")

TwoStep_Counts <- table(
  factor(i_valid, levels = desired_order),
  factor(j_valid, levels = desired_order)
)
 

BP <- setNames(c(0,0,0,0,4961,0,0), desired_order)
OB <- setNames(c(0,0,0,0,0,376,0), desired_order)
TO <- setNames(c(0,0,0,0,0,0,2259), desired_order)

TwoStep_Counts <- rbind(
  TwoStep_Counts[1:4, ],    
  BP,
  OB,
  TO
)

TwoStep_Counts
 
sum(TwoStep_Counts[1:4, ])

P2 = TwoStep_Counts / rowSums(TwoStep_Counts)
round(P2,3)

P1P2 = P1%*%P2 
round(P1P2,3)

P0P1P2 = P0%*%P1P2
round(P0P1P2,2)

data$third_one <- sapply(
  data$sequence_nolr_w_output,
  function(x) strsplit(x, ",")[[1]][3]
)

prop.table(table(data$third_one))

P_at2 <- c(a = 0.06516588, 
           b = 0.05621380, 
           d = 0.11940495, 
           e = 0.20918905,
           BP = 0.30581885,
           OB = 0.03659821,
           TO = 0.20760927)
round(P_at2,2)

P0P1P2

P1_2 = P1%*%P1
P0P1_2 = P0%*%P1_2
round(P0P1_2,2)

