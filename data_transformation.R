#ici on importe et organise les fichiers
dossier_data_path <- "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data"
library(dplyr)

  #Sacramento
    Sacramento <- paste0(dossier_data_path,"/Treated_Sacramento_data.csv")
    data_Sacramento <- read.csv(Sacramento, header = TRUE, sep = ";")
  #Springfield 
    Springfield <- paste0(dossier_data_path,"/Treated_Springfield_data.csv")
    data_Springfield <- read.csv(Springfield, header = TRUE, sep = ";")
  #Austin
    Austin <- paste0(dossier_data_path,"/Treated_Austin_data.csv")
    data_Austin <- read.csv(Austin, header = TRUE, sep = ";")
  #Albany
    Albany <- paste0(dossier_data_path,"/Treated_Albany_data.csv")
    data_Albany <- read.csv(Albany, header = TRUE, sep = ";")
    


#ici on limite les data à 2005/2024
  #Sacramento
    data_Sacramento$year <- as.numeric(substr(data_Sacramento$DATE, 1, 4))
    data_Sacramento_modern <- data_Sacramento[data_Sacramento$year >= 2005 & data_Sacramento$year <= 2024, ]
      # (Optionnel) Supprimer la colonne temporaire 'year'
      data_Sacramento_modern$year <- NULL
      # Vérifier le résultat
      head(data_Sacramento_modern)
  #Springfield 
      data_Springfield$year <- as.numeric(substr(data_Springfield$DATE, 1, 4))
      data_Springfield_modern <- data_Springfield[data_Springfield$year >= 2005 & data_Springfield$year <= 2024, ]
      # (Optionnel) Supprimer la colonne temporaire 'year'
      data_Springfield_modern$year <- NULL
      # Vérifier le résultat
      head(data_Springfield_modern)
  #Austin
      data_Austin$year <- as.numeric(substr(data_Austin$DATE, 1, 4))
      data_Austin_modern <- data_Austin[data_Austin$year >= 2005 & data_Austin$year <= 2024, ]
      # (Optionnel) Supprimer la colonne temporaire 'year'
      data_Austin_modern$year <- NULL
      # Vérifier le résultat
      head(data_Austin_modern)
  #Albany
      data_Albany$year <- as.numeric(substr(data_Albany$DATE, 1, 4))
      data_Albany_modern <- data_Albany[data_Albany$year >= 2005 & data_Albany$year <= 2024, ]
      # (Optionnel) Supprimer la colonne temporaire 'year'
      data_Albany_modern$year <- NULL
      # Vérifier le résultat
      head(data_Albany_modern)
      
  #Pour enregistrer les fichiers de data moderne
    #write.csv(data_Sacramento_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Sacramento_modern.csv", row.names = FALSE)
    #write.csv(data_Springfield_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Springfield_modern.csv", row.names = FALSE)
    #write.csv(data_Austin_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Austin_modern.csv", row.names = FALSE)
    #write.csv(data_Albany_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Albany_modern.csv", row.names = FALSE)


#ici on transforme les mois en trimestre.
      
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
        summarise(across(c(DP01, DP10, DP1X, DSND, EMNT, EMXP, EMXT, PRCP, TAVG, TMAX, TMIN),
                         ~ mean(., na.rm = TRUE))) %>%
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
        summarise(across(c(DP01, DP10, DP1X, DSND, EMNT, EMXP, EMXT, PRCP, TAVG, TMAX, TMIN),
                         ~ mean(., na.rm = TRUE))) %>%
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
        summarise(across(c(DP01, DP10, DP1X, DSND, EMNT, EMXP, EMXT, PRCP, TAVG, TMAX, TMIN),
                         ~ mean(., na.rm = TRUE))) %>%
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
        summarise(across(c(DP01, DP10, DP1X, DSND, EMNT, EMXP, EMXT, PRCP, TAVG, TMAX, TMIN),
                         ~ mean(., na.rm = TRUE))) %>%
        ungroup()
  #Enregister les datas:
      #write.csv(data_Trim_Sacramento_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Trim_Sacramento_modern.csv", row.names = FALSE)
      #write.csv(data_Trim_Springfield_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Trim_Springfield_modern.csv", row.names = FALSE)
      #write.csv(data_Trim_Austin_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Trim_Austin_modern.csv", row.names = FALSE)
      #write.csv(data_Trim_Albany_modern, "C:/Unil/Master/Printemps 1/Macroeconometrics/Projet/Code/data/data_Trim_Albany_modern.csv", row.names = FALSE)
      
      
      
      
