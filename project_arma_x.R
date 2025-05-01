# =============================================================
# Macroeconometrics
# ARMA-X : INFLUENCE OF WEATHER ON GDP
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
# SIMPLE ARMAX – Real GDP ~ Precipitation
# ================================================

# TEXAS
Y_texas <- data_Real_GDP$Texas
X_texas <- data_Austin$PRCP
res_texas <- estim.armax(Y = Y_texas, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california <- data_Real_GDP$California
X_california <- data_Sacramento$PRCP
res_california <- estim.armax(Y = Y_california, p = 1, q = 0, X = X_california)

# NEW YORK
Y_newyork <- data_Real_GDP$New.York
X_newyork <- data_Albany$PRCP
res_newyork <- estim.armax(Y = Y_newyork, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois <- data_Real_GDP$Illinois
X_illinois <- data_Springfield$PRCP
res_illinois <- estim.armax(Y = Y_illinois, p = 1, q = 0, X = X_illinois)

#-------------------------------------------------------------------------------

# ====================================================
# SIMPLE ARMAX – Agricultural Real GDP ~ Precipitation
# ====================================================

# TEXAS
Y_texas_agri <- data_Real_GDP_agri$Texas
X_texas <- data_Austin$PRCP
res_texas_agri <- estim.armax(Y = Y_texas_agri, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california_agri <- data_Real_GDP_agri$California
X_california <- data_Sacramento$PRCP
res_california_agri <- estim.armax(Y = Y_california_agri, p = 1, q = 1, X = X_california)

# NEW YORK
Y_newyork_agri <- data_Real_GDP_agri$New.York
X_newyork <- data_Albany$PRCP
res_newyork_agri <- estim.armax(Y = Y_newyork_agri, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois_agri <- data_Real_GDP_agri$Illinois
X_illinois <- data_Springfield$PRCP
res_illinois_agri <- estim.armax(Y = Y_illinois_agri, p = 1, q = 0, X = X_illinois)

#-------------------------------------------------------------------------------

# ================================================
# SIMPLE ARMAX – Real GDP ~ Temperature (TAVG)
# ================================================

# TEXAS
Y_texas <- data_Real_GDP$Texas
X_texas <- data_Austin$TAVG
res_texas <- estim.armax(Y = Y_texas, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california <- data_Real_GDP$California
X_california <- data_Sacramento$TAVG
res_california <- estim.armax(Y = Y_california, p = 1, q = 0, X = X_california)

# NEW YORK
Y_newyork <- data_Real_GDP$New.York
X_newyork <- data_Albany$TAVG
res_newyork <- estim.armax(Y = Y_newyork, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois <- data_Real_GDP$Illinois
X_illinois <- data_Springfield$TAVG
res_illinois <- estim.armax(Y = Y_illinois, p = 1, q = 0, X = X_illinois)

#-------------------------------------------------------------------------------

# ====================================================
# SIMPLE ARMAX – Agricultural Real GDP ~ Temperature (TAVG)
# ====================================================

# TEXAS
Y_texas_agri <- data_Real_GDP_agri$Texas
X_texas <- data_Austin$TAVG
res_texas_agri <- estim.armax(Y = Y_texas_agri, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california_agri <- data_Real_GDP_agri$California
X_california <- data_Sacramento$TAVG
res_california_agri <- estim.armax(Y = Y_california_agri, p = 1, q = 1, X = X_california)

# NEW YORK
Y_newyork_agri <- data_Real_GDP_agri$New.York
X_newyork <- data_Albany$TAVG
res_newyork_agri <- estim.armax(Y = Y_newyork_agri, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois_agri <- data_Real_GDP_agri$Illinois
X_illinois <- data_Springfield$TAVG
res_illinois_agri <- estim.armax(Y = Y_illinois_agri, p = 1, q = 0, X = X_illinois)

#-------------------------------------------------------------------------------
# ================================================
# SIMPLE ARMAX – Real GDP ~ Maximum Temperature (TMAX)
# ================================================

# TEXAS
Y_texas <- data_Real_GDP$Texas
X_texas <- data_Austin$TMAX
res_texas <- estim.armax(Y = Y_texas, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california <- data_Real_GDP$California
X_california <- data_Sacramento$TMAX
res_california <- estim.armax(Y = Y_california, p = 1, q = 0, X = X_california)

# NEW YORK
Y_newyork <- data_Real_GDP$New.York
X_newyork <- data_Albany$TMAX
res_newyork <- estim.armax(Y = Y_newyork, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois <- data_Real_GDP$Illinois
X_illinois <- data_Springfield$TMAX
res_illinois <- estim.armax(Y = Y_illinois, p = 1, q = 0, X = X_illinois)

#-------------------------------------------------------------------------------
# ====================================================
# SIMPLE ARMAX – Agricultural Real GDP ~ TMAX
# ====================================================

# TEXAS
Y_texas_agri <- data_Real_GDP_agri$Texas
X_texas <- data_Austin$TMAX
res_texas_agri <- estim.armax(Y = Y_texas_agri, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california_agri <- data_Real_GDP_agri$California
X_california <- data_Sacramento$TMAX
res_california_agri <- estim.armax(Y = Y_california_agri, p = 1, q = 1, X = X_california)

# NEW YORK
Y_newyork_agri <- data_Real_GDP_agri$New.York
X_newyork <- data_Albany$TMAX
res_newyork_agri <- estim.armax(Y = Y_newyork_agri, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois_agri <- data_Real_GDP_agri$Illinois
X_illinois <- data_Springfield$TMAX
res_illinois_agri <- estim.armax(Y = Y_illinois_agri, p = 1, q = 0, X = X_illinois)

#-------------------------------------------------------------------------------

# ================================================
# SIMPLE ARMAX – Real GDP ~ Extreme Precipitation (EMXP)
# ================================================

# TEXAS
Y_texas <- data_Real_GDP$Texas
X_texas <- data_Austin$EMXP
res_texas <- estim.armax(Y = Y_texas, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california <- data_Real_GDP$California
X_california <- data_Sacramento$EMXP
res_california <- estim.armax(Y = Y_california, p = 1, q = 0, X = X_california)

# NEW YORK
Y_newyork <- data_Real_GDP$New.York
X_newyork <- data_Albany$EMXP
res_newyork <- estim.armax(Y = Y_newyork, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois <- data_Real_GDP$Illinois
X_illinois <- data_Springfield$EMXP
res_illinois <- estim.armax(Y = Y_illinois, p = 1, q = 0, X = X_illinois)

#-------------------------------------------------------------------------------
# ====================================================
# SIMPLE ARMAX – Agricultural Real GDP ~ EMXP
# ====================================================

# TEXAS
Y_texas_agri <- data_Real_GDP_agri$Texas
X_texas <- data_Austin$EMXP
res_texas_agri <- estim.armax(Y = Y_texas_agri, p = 1, q = 0, X = X_texas)

# CALIFORNIA
Y_california_agri <- data_Real_GDP_agri$California
X_california <- data_Sacramento$EMXP
res_california_agri <- estim.armax(Y = Y_california_agri, p = 1, q = 1, X = X_california)

# NEW YORK
Y_newyork_agri <- data_Real_GDP_agri$New.York
X_newyork <- data_Albany$EMXP
res_newyork_agri <- estim.armax(Y = Y_newyork_agri, p = 1, q = 0, X = X_newyork)

# ILLINOIS
Y_illinois_agri <- data_Real_GDP_agri$Illinois
X_illinois <- data_Springfield$EMXP
res_illinois_agri <- estim.armax(Y = Y_illinois_agri, p = 1, q = 0, X = X_illinois)


#-------------------------------------------------------------------------------


plot_irf_weather <- function(res, T = 20, title = "IRF to Weather Shock") {
  X_shock <- matrix(0, nrow = T, ncol = length(res$beta))
  X_shock[1, ] <- 1  # Shock at t = 1 only
  
  irf <- sim.arma(
    c = 0,
    phi = res$phi,
    theta = res$theta,
    sigma = 1,
    T = T,
    y.0 = rep(0, length(res$phi)),
    nb.sim = 1,
    make.IRF = 1,
    X = X_shock,
    beta = res$beta
  )
  
  plot(irf, type = "l", lwd = 2, col = "blue",
       main = title,
       xlab = "Periods after shock",
       ylab = "GDP Response")
}

#-------------------------------------------------------------------------------


plot_irf_weather(res_texas, title = "Texas: IRF to Precipitation Shock")
plot_irf_weather(res_california, title = "California: IRF to Precipitation Shock")
plot_irf_weather(res_illinois, title = "Illinois: IRF to Precipitation Shock")
plot_irf_weather(res_newyork, title = "New-York: IRF to Precipitation Shock")
