# =============================================================
# Macroeconometrics
# ARMA-X : INFLUENCE OF WEATHER ON GDP growth 
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

#GDP Growth
data_gdp_growth  <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/gdp_growth.csv"
gdp_growth <- read.csv(data_gdp_growth, header = TRUE, sep = ",")

#GDP Agriculture Growth 
data_gdp_agri_growth <- "https://raw.githubusercontent.com/maximefleury27/desktop-tutorial/refs/heads/main/gdp_agri_growth.csv"
gdp_agri_growth<- read.csv(data_gdp_agri_growth, header = TRUE, sep = ",")  

#-------------------------------------------------------------------------------
library(forecast)
# ==============================#
# SEASONALITY - Graphs to test  #
# ==============================#
library(forecast)

# List of datasets per state
weather_data_list <- list(
  Texas = data_Austin,
  California = data_Sacramento,
  New.York = data_Albany,
  Illinois = data_Springfield
)

# Variables to process
weather_vars <- c("PRCP", "TAVG", "TMAX", "TMIN", "EMXP", "EMXT", "EMNT", "DP01", "DP10", "DP1X")

# Function to prepare + return original & deseasonalized series
prepare_series <- function(series, frequency = 4) {
  series <- as.numeric(as.character(series))
  series <- na.omit(series)
  if (length(series) < frequency * 2) return(NULL)
  ts_series <- ts(series, frequency = frequency)
  stl_result <- stl(ts_series, s.window = "periodic")
  deseasonalized <- seasadj(stl_result)
  return(list(original = ts_series, deseasonalized = deseasonalized))
}

# Loop per state
for (state in names(weather_data_list)) {
  df <- weather_data_list[[state]]
  
  cat("\n=== Plotting series for", state, "===\n")
  
  par(mfrow = c(2, 4), mar = c(3, 4, 2, 1))  # 2x3 layout
  
  plot_count <- 0
  for (var in weather_vars) {
    if (var %in% names(df)) {
      result <- prepare_series(df[[var]])
      
      if (!is.null(result)) {
        # Plot original in black
        plot(result$original, main = paste(state, "-", var),
             ylab = "Value", col = "black", type = "l", lwd = 1.5)
        # Add deseasonalized in red
        lines(result$deseasonalized, col = "red", lwd = 2)
        
        legend("topright", legend = c("Original", "Deseasonalized"),
               col = c("black", "red"), lty = 1, bty = "n", cex = 0.8)
        
        # Kruskal-Wallis test for seasonality
        seasons <- cycle(result$original)
        kw_test <- kruskal.test(as.numeric(result$original) ~ as.factor(seasons))
        pval <- round(kw_test$p.value, 4)
        msg <- if (pval < 0.05) {
          paste("↪ Seasonal effect detected (p =", pval, ")")
        } else {
          paste("↪ No significant seasonality (p =", pval, ")")
        }
        cat(state, "-", var, ": ", msg, "\n")
        
      } else {
        plot.new()
        title(main = paste(state, "-", var, "\nNot enough data"))
        cat(state, "-", var, ": Not enough data for analysis\n")
      }
      
      plot_count <- plot_count + 1
      
      if (plot_count %% 7 == 0) {
        readline(prompt = "Press [Enter] to continue...")
        par(mfrow = c(2, 3), mar = c(3, 4, 2, 1))
      }
    }
  }
}


#-------------------------------------------------------------------------------
list_dep_variable <- c("PRCP","TAVG", "TMAX", "TMIN","EMXP", "EMXT", "EMNT")

# ================================#
# ARMAX – Agricultural GDP Growth #
# ================================#
# === Function to deseasonalize a time series ===
prepare_series <- function(series, frequency = 4) {
  series <- as.numeric(as.character(series))
  series <- na.omit(series)
  if (length(series) < frequency * 2) return(rep(NA, length(series)))
  ts_series <- ts(series, frequency = frequency)
  return(seasadj(stl(ts_series, s.window = "periodic")))
}

# === Settings ===
weather_variables <- c("PRCP", "TAVG", "TMAX", "TMIN", "EMXP", "EMXT", "EMNT", "DP01", "DP10", "DP1X")
states <- c("Texas", "California", "New.York", "Illinois")

# === Main loop over each weather variable ===
for (depend_var in weather_variables) {
  cat("\n========== Processing:", depend_var, "==========\n")
  
  beta_vec <- c()
  sd_vec <- c()
  results_list <- list()  # Store ARMAX results for IRF
  
  # --- Estimation for each state ---
  for (state in states) {
    Y <- gdp_agri_growth[[state]]
    X <- switch(state,
                "Texas" = data_Austin[[depend_var]],
                "California" = data_Sacramento[[depend_var]],
                "New.York" = data_Albany[[depend_var]],
                "Illinois" = data_Springfield[[depend_var]]
    )
    
    # Clean and deseasonalize
    Y_clean <- prepare_series(Y)
    X_clean <- prepare_series(X)
    keep <- complete.cases(cbind(Y_clean, X_clean))
    Y_final <- Y_clean[keep]
    X_final <- X_clean[keep]
    
    # ARMA orders
    p <- ifelse(state == "Texas", 0, 1)
    q <- 1
    
    # Estimate ARMAX
    res <- tryCatch({
      estim.armax(Y = Y_final, p = p, q = q, X = X_final)
    }, error = function(e) NULL)
    
    # Store beta & std.dev (even if SD is NA)
    if (!is.null(res)) {
      beta_val <- ifelse(length(res$beta) > 0, res$beta[1], NA)
      sd_val <- ifelse(length(res$st.dev) > 0, tail(res$st.dev, 1), NA)
      beta_vec <- c(beta_vec, beta_val)
      sd_vec <- c(sd_vec, sd_val)
    } else {
      beta_vec <- c(beta_vec, NA)
      sd_vec <- c(sd_vec, NA)
    }
    
    results_list[[state]] <- res
  }
  
  # --- Summary table ---
  t_ratio_vec <- beta_vec / sd_vec
  summary_table <- data.frame(
    State = states,
    Beta = round(beta_vec, 5),
    SD = round(sd_vec, 5),
    t_ratio = round(t_ratio_vec, 3)
  )
  
  cat("\n=== Summary Table -", depend_var, "on Agri GDP Growth ===\n")
  print(summary_table)
  
  # --- IRFs ---
  cat("\n=== IRFs for", depend_var, "on Agri GDP Growth ===\n")
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  for (state in states) {
    res <- results_list[[state]]
    
    if (is.null(res) || is.null(res$phi) || is.null(res$beta)) {
      plot.new()
      title(main = paste(state, "- No model estimated"))
      next
    }
    
    T <- 10
    X_shock <- matrix(0, nrow = T, ncol = length(res$beta))
    X_shock[1, ] <- 1
    
    irf <- tryCatch({
      sim.arma(
        c = 0,
        phi = res$phi,
        theta = res$beta,
        sigma = 1,
        T = T,
        y.0 = rep(0, length(res$phi)),
        nb.sim = 1,
        make.IRF = 1
      )
    }, error = function(e) rep(NA, T))
    
    irf_vec <- as.numeric(irf)
    sd <- tail(res$st.dev, 1)
    
    # Check if IRF or SD is invalid
    if (any(is.na(irf_vec)) || is.na(sd) || is.nan(sd)) {
      plot.new()
      title(main = paste(state, "- IRF or SD invalid"))
      next
    }
    
    upper <- irf_vec + 1.96 * sd
    lower <- irf_vec - 1.96 * sd
    
    plot(irf_vec, type = "l", lwd = 2, col = "darkblue",
         ylim = range(c(upper, lower), na.rm = TRUE),
         main = paste(state, ": IRF – Agri GDP Growth ~", depend_var),
         xlab = "Periods after shock", ylab = "Response (%)")
    lines(upper, lty = 2, col = "darkblue")
    lines(lower, lty = 2, col = "darkblue")
    abline(h = 0, col = "gray")
  }
  
  readline(prompt = paste0("→ Press [Enter] to continue to the next variable (", depend_var, ")..."))
  par(mfrow = c(1, 1))  # Reset layout
}


#-------------------------------------------------------------------------------
# ===================#
# ARMAX – GDP Growth #
# ===================#

# === Main loop over each weather variable ===
for (depend_var in weather_variables) {
  cat("\n========== Processing:", depend_var, "==========\n")
  
  beta_vec <- c()
  sd_vec <- c()
  results_list <- list()  # Store ARMAX results for IRF
  
  # --- Estimation for each state ---
  for (state in states) {
    Y <- gdp_growth[[state]]  # ✅DP growth instead of agri
    X <- switch(state,
                "Texas" = data_Austin[[depend_var]],
                "California" = data_Sacramento[[depend_var]],
                "New.York" = data_Albany[[depend_var]],
                "Illinois" = data_Springfield[[depend_var]]
    )
    
    # Clean and deseasonalize
    Y_clean <- prepare_series(Y)
    X_clean <- prepare_series(X)
    keep <- complete.cases(cbind(Y_clean, X_clean))
    Y_final <- Y_clean[keep]
    X_final <- X_clean[keep]
    
    # ARMA orders
    p <- ifelse(state == "Texas", 0, 1)
    q <- 1
    
    # Estimate ARMAX
    res <- tryCatch({
      estim.armax(Y = Y_final, p = p, q = q, X = X_final)
    }, error = function(e) NULL)
    
    # Store beta & std.dev (even if SD is NA)
    if (!is.null(res)) {
      beta_val <- ifelse(length(res$beta) > 0, res$beta[1], NA)
      sd_val <- ifelse(length(res$st.dev) > 0, tail(res$st.dev, 1), NA)
      beta_vec <- c(beta_vec, beta_val)
      sd_vec <- c(sd_vec, sd_val)
    } else {
      beta_vec <- c(beta_vec, NA)
      sd_vec <- c(sd_vec, NA)
    }
    
    results_list[[state]] <- res
  }
  
  # --- Summary table ---
  t_ratio_vec <- beta_vec / sd_vec
  summary_table <- data.frame(
    State = states,
    Beta = round(beta_vec, 5),
    SD = round(sd_vec, 5),
    t_ratio = round(t_ratio_vec, 3)
  )
  
  cat("\n=== Summary Table -", depend_var, "on GDP Growth ===\n")
  print(summary_table)
  
  # --- IRFs ---
  cat("\n=== IRFs for", depend_var, "on GDP Growth ===\n")
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  for (state in states) {
    res <- results_list[[state]]
    
    if (is.null(res) || is.null(res$phi) || is.null(res$beta)) {
      plot.new()
      title(main = paste(state, "- No model estimated"))
      next
    }
    
    T <- 10
    X_shock <- matrix(0, nrow = T, ncol = length(res$beta))
    X_shock[1, ] <- 1
    
    irf <- tryCatch({
      sim.arma(
        c = 0,
        phi = res$phi,
        theta = res$beta,
        sigma = 1,
        T = T,
        y.0 = rep(0, length(res$phi)),
        nb.sim = 1,
        make.IRF = 1
      )
    }, error = function(e) rep(NA, T))
    
    irf_vec <- as.numeric(irf)
    sd <- tail(res$st.dev, 1)
    
    # Check if IRF or SD is invalid
    if (any(is.na(irf_vec)) || is.na(sd) || is.nan(sd)) {
      plot.new()
      title(main = paste(state, "- IRF or SD invalid"))
      next
    }
    
    upper <- irf_vec + 1.96 * sd
    lower <- irf_vec - 1.96 * sd
    
    plot(irf_vec, type = "l", lwd = 2, col = "darkgreen",
         ylim = range(c(upper, lower), na.rm = TRUE),
         main = paste(state, ": IRF – GDP Growth ~", depend_var),
         xlab = "Periods after shock", ylab = "Response (%)")
    lines(upper, lty = 2, col = "darkgreen")
    lines(lower, lty = 2, col = "darkgreen")
    abline(h = 0, col = "gray")
  }
  
  readline(prompt = paste0("→ Press [Enter] to continue to the next variable (", depend_var, ")..."))
  par(mfrow = c(1, 1))  # Reset layout
}