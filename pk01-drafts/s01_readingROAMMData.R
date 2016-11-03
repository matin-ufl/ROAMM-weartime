#####################################################
# Draft...                                          # 
# Set a standard to read PROMPT data for analysis.  #
#                                                   #
# _________________________________________________ #
# Matin Kheirkhahan                                 #
#####################################################

setwd("~/Workspaces/R workspace/ROAMM - Real-time wear-time detection/pk01-drafts/")

# Functions ----------------------------------------------

# Converts log file's time strings to YYYY-MM-DD hh:mm
readDateTime.logFile <- function(dateTimeStr) {
     tokens <- unlist(strsplit(dateTimeStr, split = " "))
     date <- as.character(tokens[1])
     tokens <- unlist(strsplit(tokens[2], split = ":"))
     time.hh.mm <- paste(tokens[1], tokens[2], sep = ":")
     paste(date, time.hh.mm)
}

# Converts ROAMM file's time strings to YYYY-MM-DD hh:mm
readDateTime.ROAMMFile <- function(dateTimeStr) {
     tokens <- unlist(strsplit(dateTimeStr, split = " "))
     date <- as.character(tokens[1])
     tokens <- unlist(strsplit(tokens[2], split = ":"))
     time.hh.mm <- paste(tokens[1], tokens[2], sep = ":")
     paste(date, time.hh.mm)
}

# Reads ROAMM file (.csv) and selects the required set of features
read.ROAMMFile <- function(filename) {
     roamm.df <- read.csv(filename)
     # Selecting required features
     roamm.df <- roamm.df[, c("id", "collectedTimeStamp", "mvm", "sdvm", "mangle", "sdangle", "p625", "df", "fpdf", "heartRate", "battery")]
     # Sorting based on the order they had been put into database
     roamm.df <- roamm.df[with(data = roamm.df, order(id, decreasing = F)), ]
     # Correcting variables' class types
     
     roamm.df$collectedTimeStamp <- as.character(roamm.df$collectedTimeStamp)
     roamm.df$heartRate <- as.character(roamm.df$heartRate); roamm.df$heartRate[roamm.df$heartRate == "None"] <- "NA"
     for(c in c(1, 3:ncol(roamm.df))) {
          roamm.df[, c] <- as.numeric(roamm.df[, c])
     }
     roamm.df$dateTime <- sapply(roamm.df$collectedTimeStamp, readDateTime.ROAMMFile)
     roamm.df <- roamm.df[, c(1, 12, 2:11)]
     roamm.df
}

# Reads log file and corrects time stampes
read.logFile <- function(filename) {
     log.df <- read.csv(filename, colClasses = c("character", "character", "factor"))
     log.df$Start.time <- sapply(log.df$Start.time, readDateTime.logFile)
     log.df$End.time <- sapply(log.df$End.time, readDateTime.logFile)
     log.df
}

# Script ----------------------------------------------------

# Point rawDataFolder to a folder in which sensor_data and log_ files exist.
rawDataFolder <- "~/Dropbox/Work-Research/Current Directory/Smart Data Collection - Realtime Weartime Detection/Datasets/Raw watch data/110316/"
outputFileName <- "rawROAMM_110316.Rdata"

# Final output is a data.frame with the following features:
# 1. id: sort order
# 2. dateTime: YYYY-MM-DD hh:mm (seconds not included since they are not present in the log file)
# [3, 9]: Accelerometer features <mvm, sdvm, mangle, sdangle, p625, df, fpdf>
# 10: heartRate
# 11: battery
# 12: status
weartime_dataset.df <- data.frame(matrix(nrow = 0, ncol = 12))

roamm.df <- read.ROAMMFile(paste(rawDataFolder, dir(path = rawDataFolder, pattern = "^sensor_data_.*.csv$"), sep = ""))
log.df <- read.logFile(paste(rawDataFolder, dir(path = rawDataFolder, pattern = "^log_.*.csv$"), sep = ""))

for(i in 1:nrow(log.df)) {
     start.idx <- which(roamm.df$dateTime == log.df$Start.time[i])
     if(length(start.idx) > 0) {
          start.idx <- start.idx[1] # in case there are multiple instances in that minute
          end.idx <- which(roamm.df$dateTime == log.df$End.time[i])
          if(length(end.idx) > 0) {
               end.idx <- end.idx[1] # in case there are multiple instances in that minute
               selected.chunk <- roamm.df[start.idx:end.idx, c("id", "dateTime", "mvm", "sdvm", "mangle", "sdangle", "p625", "df", "fpdf", "heartRate", "battery")]
               selected.chunk$status <- log.df$Status[i]
               weartime_dataset.df <- rbind(weartime_dataset.df,
                                            selected.chunk)
          } else {
               print(paste(i, "No data found for the <LOG END TIME>."))
          }
     } else {
          print(paste(i, "No data found for the <LOG START TIME>"))
     }
}


save(weartime_dataset.df, file = paste(rawDataFolder, outputFileName, sep = ""))
