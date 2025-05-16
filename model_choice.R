# =============================================================
# Macroeconometrics
# MODEL CHOICE AND GRAPHS 
# Mathieu Schneider and Maxime Fleury
# =============================================================

###IMPORT THE DATAS###
###----------------###

#Sacramento
Sacramento <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/data_Trim_Sacramento_modern.csv"
data_Sacramento <- read.csv(Sacramento, header = TRUE, sep = ",")

#Springfield 
Springfield <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/data_Trim_Springfield_modern.csv"
data_Springfield <- read.csv(Springfield, header = TRUE, sep = ",")

#Austin
Austin <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/data_Trim_Austin_modern.csv"
data_Austin <- read.csv(Austin, header = TRUE, sep = ",")

#Albany
Albany <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/data_Trim_Albany_modern.csv"
data_Albany <- read.csv(Albany, header = TRUE, sep = ",")

#Real GDP 
Real_GDP <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Treated_Trim_real_GDP.csv"
data_Real_GDP <- read.csv(Real_GDP, header = TRUE, sep = ",")

#Real GDP Agriculture
Real_GDP_agri <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/Treated_Trim_real_GDP_agri.csv"
data_Real_GDP_agri <- read.csv(Real_GDP_agri, header = TRUE, sep = ",")  

#-------------------------------------------------------------------------------

# ================================================
# TESTS — STATIONARITY ANALYSIS GDP & GDP GROWTH
# ================================================
# Load required package
library(tseries)

# Define a robust function to run three stationarity tests
run_stationarity_tests <- function(series) {
  series <- as.numeric(as.character(series))  # Convert to numeric if needed
  series <- na.omit(series)                   # Remove missing values
  if (length(series) < 2) {
    return(list(adf = NA, pp = NA, kpss = NA))
  }
  list(
    adf  = tryCatch(adf.test(series, k = 4)$p.value, error = function(e) NA),
    pp   = tryCatch(pp.test(series)$p.value, error = function(e) NA),
    kpss = tryCatch(kpss.test(series)$p.value, error = function(e) NA)
  )
}

# Apply the tests to all columns except "PERIODE" in the agricultural GDP dataset
results_agri <- lapply(
  data_Real_GDP_agri[ , setdiff(names(data_Real_GDP_agri), "PERIODE")],
  run_stationarity_tests
)

# Apply the tests to all columns except "PERIODE" in the total GDP dataset
results_gdp <- lapply(
  data_Real_GDP[ , setdiff(names(data_Real_GDP), "PERIODE")],
  run_stationarity_tests
)

# Convert the results into readable data frames
results_agri_df <- do.call(rbind, results_agri)
print(results_agri_df)
results_gdp_df  <- do.call(rbind, results_gdp)
print(results_gdp_df) #Given the result and the high probability of 
#the presence of non-stationarity, we will compute Growth rates. 

# --- Compute GDP growth (log-diff), then multiply by 100 to express as percentage ---
gdp_growth <- data_Real_GDP
gdp_growth[-1] <- lapply(data_Real_GDP[-1], function(x) {
  growth <- c(NA, diff(log(x)))
  growth[1] <- 0
  return(growth * 100)
})

# --- Compute Agricultural GDP growth, then multiply by 100 to express as percentage ---
gdp_agri_growth <- data_Real_GDP_agri
gdp_agri_growth[-1] <- lapply(data_Real_GDP_agri[-1], function(x) {
  growth <- c(NA, diff(log(x)))
  growth[1] <- 0
  return(growth * 100)
})

write.csv(gdp_growth, "C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project/gdp_grwoth.csv",row.names = FALSE)
write.csv(gdp_agri_growth,"C:/Users/Maxime/Documents/Semestre 2/Macroeconometrics/Project/gdp_agri_growth.csv",row.names = FALSE)

#-------------------------------------------------------------------------------

# ================================================
# GDP AND AGRI GDP GROWTH - PLOTS 
# ================================================
states <- c("Texas", "California", "New.York", "Illinois")

par(mfrow = c(2, 2))
for (state in states) {
  plot(gdp_growth[[state]],
       type = "l",
       main = paste("GDP Growth –", gsub("\\.", " ", state)),
       xlab = "Time (Quarters)",
       ylab = "Growth rate (%)",
       col = "red", lwd = 2)
  abline(h = 0, col = "gray", lty = 2)
}
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
for (state in states) {
  plot(gdp_agri_growth[[state]],
       type = "l",
       main = paste("GDP Agriculture Growth –", gsub("\\.", " ", state)),
       xlab = "Time (Quarters)",
       ylab = "Growth rate (%)",
       col = "blue", lwd = 2)
  abline(h = 0, col = "gray", lty = 2)
}
par(mfrow = c(1, 1))

#-------------------------------------------------------------------------------
# =============================================================
# Quarterly Average Temperature Plot for 4 U.S. States
# =============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(scales)

# Function to deseasonalize a time series using STL
deseasonalize_tavg <- function(series) {
  ts_data <- ts(series, frequency = 4, start = c(2005, 1))
  seasadj(stl(ts_data, s.window = "periodic"))
}

# Apply to each state's TAVG
texas_tavg_deseason <- deseasonalize_tavg(data_Trim_Austin_modern$TAVG)
california_tavg_deseason <- deseasonalize_tavg(data_Trim_Sacramento_modern$TAVG)
illinois_tavg_deseason <- deseasonalize_tavg(data_Trim_Springfield_modern$TAVG)
newyork_tavg_deseason <- deseasonalize_tavg(data_Trim_Albany_modern$TAVG)

# Create a data frame for plotting
n <- length(texas_tavg_deseason)
quarters <- data_Trim_Austin_modern$PERIODE[1:n]

df_plot <- data.frame(
  Quarter = quarters,
  Texas = as.numeric(texas_tavg_deseason),
  California = as.numeric(california_tavg_deseason),
  Illinois = as.numeric(illinois_tavg_deseason),
  NewYork = as.numeric(newyork_tavg_deseason)
) %>%
  pivot_longer(cols = -Quarter, names_to = "State", values_to = "TAVG_deseason")

# Plot
ggplot(df_plot, aes(x = Quarter, y = TAVG_deseason, color = State, group = State)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(title = "Quarterly Deseasonalized Average Temperature (TAVG)",
       subtitle = "2005–2024 – Seasonal effects removed using STL",
       x = "Quarter", y = "Deseasonalized Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#-------------------------------------------------------------------------------
# =============================================================
# Quarterly Deseasonalized Precipitation Plot for 4 U.S. States
# =============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(scales)

# Function to deseasonalize a time series using STL
deseasonalize_prcp <- function(series) {
  ts_data <- ts(series, frequency = 4, start = c(2005, 1))
  seasadj(stl(ts_data, s.window = "periodic"))
}

# Apply to each state's PRCP
texas_prcp_deseason <- deseasonalize_prcp(data_Trim_Austin_modern$PRCP)
california_prcp_deseason <- deseasonalize_prcp(data_Trim_Sacramento_modern$PRCP)
illinois_prcp_deseason <- deseasonalize_prcp(data_Trim_Springfield_modern$PRCP)
newyork_prcp_deseason <- deseasonalize_prcp(data_Trim_Albany_modern$PRCP)

# Create a data frame for plotting
n <- length(texas_prcp_deseason)
quarters <- data_Trim_Austin_modern$PERIODE[1:n]

df_plot_prcp <- data.frame(
  Quarter = quarters,
  Texas = as.numeric(texas_prcp_deseason),
  California = as.numeric(california_prcp_deseason),
  Illinois = as.numeric(illinois_prcp_deseason),
  NewYork = as.numeric(newyork_prcp_deseason)
) %>%
  pivot_longer(cols = -Quarter, names_to = "State", values_to = "PRCP_deseason")

# Plot
ggplot(df_plot_prcp, aes(x = Quarter, y = PRCP_deseason, color = State, group = State)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Quarterly Deseasonalized Precipitation (PRCP)",
    subtitle = "2005–2024 – Seasonal effects removed using STL",
    x = "Quarter", y = "Deseasonalized Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#-------------------------------------------------------------------------------

# =============================================================
# ARMA Model Selection – GDP and Agricultural GDP by State
# =============================================================

library(forecast)
library(tseries)

# Plot ACF and PACF for Real GDP
par(mfrow = c(2, 4))
for (state in states) {
  ts_data <- ts(gdp_growth[[state]])
  acf(ts_data, main = paste("ACF –", state, "GDP Growth"))
  pacf(ts_data, main = paste("PACF –", state, "GDP Growth"))
}

# Plot ACF and PACF for Real GDP Agriculture
par(mfrow = c(2, 4))
for (state in states) {
  ts_data <- ts(gdp_agri_growth[[state]])
  acf(ts_data, main = paste("ACF –", state, "GDP Agri Growth"))
  pacf(ts_data, main = paste("PACF –", state, "GDP Agri Growth"))
}

# Define list of states
states <- c("Texas", "California", "New.York", "Illinois")

##Construct a loop to find the best model###
# Set maximum p and q
max.p <- 2
max.q <- 2

# Function to select best ARMA model by AIC
select_arma_aic <- function(series) {
  series <- as.numeric(as.character(series))  # Ensure numeric
  series <- na.omit(series)                   # Remove NAs
  
  best.aic <- Inf
  best.p <- NA
  best.q <- NA
  
  for (p in 0:max.p) {
    for (q in 0:max.q) {
      tryCatch({
        model <- Arima(series, order = c(p, 0, q))
        if (model$aic < best.aic) {
          best.aic <- model$aic
          best.p <- p
          best.q <- q
        }
      }, error = function(e) {})
    }
  }
  
  return(c(Best.p = best.p, Best.q = best.q, AIC = round(best.aic, 2)))
}

# Initialize result tables
results_gdp_model <- data.frame(State = character(), Best.p = integer(), Best.q = integer(), AIC = numeric(), stringsAsFactors = FALSE)
results_agri_model <- data.frame(State = character(), Best.p = integer(), Best.q = integer(), AIC = numeric(), stringsAsFactors = FALSE)

# Loop over each state
for (state in states) {
  y_gdp <- gdp_growth[[state]]
  y_agri <- gdp_agri_growth[[state]]
  
  gdp_model <- select_arma_aic(y_gdp)
  agri_model <- select_arma_aic(y_agri)
  
  results_gdp_model <- rbind(results_gdp_model, data.frame(State = state, t(gdp_model), stringsAsFactors = FALSE))
  results_agri_model <- rbind(results_agri_model, data.frame(State = state, t(agri_model), stringsAsFactors = FALSE))
}

# Print results
cat("==== Best ARMA Models for GDP Growth ====\n")
print(results_gdp_model, row.names = FALSE)

cat("\n==== Best ARMA Models for Agricultural GDP Growth ====\n")
print(results_agri_model, row.names = FALSE)