# =============================================================
# Macroeconometrics
# Data Transformation 
# Mathieu Schneider and Maxime Fleury
# =============================================================

library(dplyr) 

###IMPORT THE DATAS###
###----------------###
#Sacramento
Sacramento <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Treated_Sacramento_data.csv"
data_Sacramento <- read.csv(Sacramento, header = TRUE, sep = ";")

#Springfield 
Springfield <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Treated_Springfield_data.csv"
data_Springfield <- read.csv(Springfield, header = TRUE, sep = ";")

#Austin
Austin <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Treated_Austin_data.csv"
data_Austin <- read.csv(Austin, header = TRUE, sep = ";")

#Albany
Albany <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Treated_Albany_data.csv"
data_Albany <- read.csv(Albany, header = TRUE, sep = ";")


###LIMIT THE DATAS TO 2005-2024###
###----------------------------###
#Sacramento
data_Sacramento$year <- as.numeric(substr(data_Sacramento$DATE, 1, 4))
data_Sacramento_modern <- data_Sacramento[data_Sacramento$year >= 2005 & data_Sacramento$year <= 2024, ]
# (Optional) Delete temporary column 'year'
data_Sacramento_modern$year <- NULL
# Verify the result 
head(data_Sacramento_modern)

#Springfield 
data_Springfield$year <- as.numeric(substr(data_Springfield$DATE, 1, 4))
data_Springfield_modern <- data_Springfield[data_Springfield$year >= 2005 & data_Springfield$year <= 2024, ]
# (Optional) Delete temporary column 'year'
data_Springfield_modern$year <- NULL

#Austin
data_Austin$year <- as.numeric(substr(data_Austin$DATE, 1, 4))
data_Austin_modern <- data_Austin[data_Austin$year >= 2005 & data_Austin$year <= 2024, ]
# (Optional) Delete temporary column 'year'
data_Austin_modern$year <- NULL

#Albany
data_Albany$year <- as.numeric(substr(data_Albany$DATE, 1, 4))
data_Albany_modern <- data_Albany[data_Albany$year >= 2005 & data_Albany$year <= 2024, ]
# (Optional) Delete temporary column 'year'
data_Albany_modern$year <- NULL


###SAVE NEW DATA FILES LOCALLY###
###---------------------------###
#write.csv(data_Sacramento_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Sacramento_modern.csv", row.names = FALSE)
#write.csv(data_Springfield_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Springfield_modern.csv", row.names = FALSE)
#write.csv(data_Austin_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Austin_modern.csv", row.names = FALSE)
#write.csv(data_Albany_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Albany_modern.csv", row.names = FALSE)


###TRANSFORM INTO TRIMESTRIAL DATAS###
###--------------------------------###

#Sacramento
data_Sacramento_modern$YEAR <- as.numeric(substr(data_Sacramento_modern$DATE, 1, 4))
data_Sacramento_modern$MONTH <- as.numeric(substr(data_Sacramento_modern$DATE, 6, 7))
data_Sacramento_modern$QUARTER <- ceiling(data_Sacramento_modern$MONTH / 3)

data_Sacramento_modern$PERIODE <- paste0(data_Sacramento_modern$YEAR, ":Q", data_Sacramento_modern$QUARTER)

cols_to_convert = c("EMNT", "EMXP", "EMXT","PRCP", "TAVG", "TMAX", "TMIN")
for (col in cols_to_convert) {
  data_Sacramento_modern[[col]] <- as.numeric(gsub(",", ".", data_Sacramento_modern[[col]]))
}


data_Trim_Sacramento_modern <- data_Sacramento_modern %>%
  group_by(PERIODE) %>%
  summarise(
    across(c(DP01, DP10, DP1X, DSND), ~ sum(., na.rm = TRUE)),  
    across(c(PRCP, TAVG, TMAX, TMIN), ~ mean(., na.rm = TRUE)),  
    across(c(EMXP, EMXT), ~ max(., na.rm = TRUE)),               
    across(c(EMNT), ~ min(., na.rm = TRUE)) 
  ) %>%
  ungroup()

#Springfield
data_Springfield_modern$YEAR <- as.numeric(substr(data_Springfield_modern$DATE, 1, 4))
data_Springfield_modern$MONTH <- as.numeric(substr(data_Springfield_modern$DATE, 6, 7))
data_Springfield_modern$QUARTER <- ceiling(data_Springfield_modern$MONTH / 3)

data_Springfield_modern$PERIODE <- paste0(data_Springfield_modern$YEAR, ":Q", data_Springfield_modern$QUARTER)

cols_to_convert = c("EMNT", "EMXP", "EMXT","PRCP", "TAVG", "TMAX", "TMIN")
for (col in cols_to_convert) {
  data_Springfield_modern[[col]] <- as.numeric(gsub(",", ".", data_Springfield_modern[[col]]))
}

data_Trim_Springfield_modern <- data_Springfield_modern %>%
  group_by(PERIODE) %>%
  summarise(
    across(c(DP01, DP10, DP1X, DSND), ~ sum(., na.rm = TRUE)),  
    across(c(PRCP, TAVG, TMAX, TMIN), ~ mean(., na.rm = TRUE)),  
    across(c(EMXP, EMXT), ~ max(., na.rm = TRUE)),               
    across(c(EMNT), ~ min(., na.rm = TRUE)) 
  ) %>%
  ungroup()
#Austin
data_Austin_modern$YEAR <- as.numeric(substr(data_Austin_modern$DATE, 1, 4))
data_Austin_modern$MONTH <- as.numeric(substr(data_Austin_modern$DATE, 6, 7))
data_Austin_modern$QUARTER <- ceiling(data_Austin_modern$MONTH / 3)

data_Austin_modern$PERIODE <- paste0(data_Austin_modern$YEAR, ":Q", data_Austin_modern$QUARTER)

cols_to_convert = c("EMNT", "EMXP", "EMXT","PRCP", "TAVG", "TMAX", "TMIN")
for (col in cols_to_convert) {
  data_Austin_modern[[col]] <- as.numeric(gsub(",", ".", data_Austin_modern[[col]]))
}


data_Trim_Austin_modern <- data_Austin_modern %>%
  group_by(PERIODE) %>%
  summarise(
    across(c(DP01, DP10, DP1X, DSND), ~ sum(., na.rm = TRUE)),  
    across(c(PRCP, TAVG, TMAX, TMIN), ~ mean(., na.rm = TRUE)),  
    across(c(EMXP, EMXT), ~ max(., na.rm = TRUE)),               
    across(c(EMNT), ~ min(., na.rm = TRUE)) 
  ) %>%
  ungroup()
#Albany
data_Albany_modern$YEAR <- as.numeric(substr(data_Albany_modern$DATE, 1, 4))
data_Albany_modern$MONTH <- as.numeric(substr(data_Albany_modern$DATE, 6, 7))
data_Albany_modern$QUARTER <- ceiling(data_Albany_modern$MONTH / 3)

data_Albany_modern$PERIODE <- paste0(data_Albany_modern$YEAR, ":Q", data_Albany_modern$QUARTER)

cols_to_convert = c("EMNT", "EMXP", "EMXT","PRCP", "TAVG", "TMAX", "TMIN")
for (col in cols_to_convert) {
  data_Albany_modern[[col]] <- as.numeric(gsub(",", ".", data_Albany_modern[[col]]))
}


data_Trim_Albany_modern <- data_Albany_modern %>%
  group_by(PERIODE) %>%
  summarise(
    across(c(DP01, DP10, DP1X, DSND), ~ sum(., na.rm = TRUE)),  
    across(c(PRCP, TAVG, TMAX, TMIN), ~ mean(., na.rm = TRUE)),  
    across(c(EMXP, EMXT), ~ max(., na.rm = TRUE)),               
    across(c(EMNT), ~ min(., na.rm = TRUE))    
  ) %>%
  ungroup()

###SAVE THE DATAS LOCALLY###
###----------------------###

# Create folder if it doesn't exist
dir.create("C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data", recursive = TRUE, showWarnings = FALSE)

# Save each data set locally
write.csv(data_Trim_Sacramento_modern, "C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project/data_Trim_Sacramento_modern.csv", row.names = FALSE)
write.csv(data_Trim_Springfield_modern, "C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project//data_Trim_Springfield_modern.csv", row.names = FALSE)
write.csv(data_Trim_Austin_modern, "C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project/data_Trim_Austin_modern.csv", row.names = FALSE)
write.csv(data_Trim_Albany_modern, "C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project/data_Trim_Albany_modern.csv", row.names = FALSE)


###GDP DATAS###
###---------###

#Real GDP 
Real_GDP <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Raw_realGDP.csv"
data_Real_GDP <- read.csv(Real_GDP, header = TRUE, sep = ";")

#Real GDP Agriculture
Real_Agri_GDP <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Raw_realGDP_Agri.csv"
data_Real_Agri_GDP <- read.csv(Real_Agri_GDP, header = TRUE, sep = ";")   


###INVERT RAWS AND COLUMNS###
###-----------------------###

#Real GDP 
data_Real_GDP_Matrice <- t(data_Real_GDP[, -1]) 
colnames(data_Real_GDP_Matrice) <- data_Real_GDP$GeoName
data_Real_GDP_Trim <- as.data.frame(data_Real_GDP_Matrice)
data_Real_GDP_Trim$PERIODE <- rownames(data_Real_GDP_Trim)
data_Real_GDP_Trim <- data_Real_GDP_Trim[, c("PERIODE", setdiff(names(data_Real_GDP_Trim), "PERIODE"))]
data_Real_GDP_Trim$PERIODE <- gsub("^X", "", data_Real_GDP_Trim$PERIODE)
data_Real_GDP_Trim$PERIODE <- gsub("\\.", ":", data_Real_GDP_Trim$PERIODE)
rownames(data_Real_GDP_Trim) <- NULL

#Real GDP Agriculture
data_Real_Agri_GDP_Matrice <- t(data_Real_Agri_GDP[, -1]) 
colnames(data_Real_Agri_GDP_Matrice) <- data_Real_Agri_GDP$GeoName
data_Real_Agri_GDP_Trim <- as.data.frame(data_Real_Agri_GDP_Matrice)
data_Real_Agri_GDP_Trim$PERIODE <- rownames(data_Real_Agri_GDP_Trim)
data_Real_Agri_GDP_Trim <- data_Real_Agri_GDP_Trim[, c("PERIODE", setdiff(names(data_Real_Agri_GDP_Trim), "PERIODE"))]
data_Real_Agri_GDP_Trim$PERIODE <- gsub("^X", "", data_Real_Agri_GDP_Trim$PERIODE)
data_Real_Agri_GDP_Trim$PERIODE <- gsub("\\.", ":", data_Real_Agri_GDP_Trim$PERIODE)
rownames(data_Real_Agri_GDP_Trim) <- NULL


###SAVE DATAS LOCALLY###
###------------------###
write.csv(data_Real_GDP_Trim, "C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project/Treated_Trim_real_GDP.csv",row.names = FALSE)
write.csv(data_Real_Agri_GDP_Trim,"C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project/Treated_Trim_real_GDP_agri.csv",row.names = FALSE)





