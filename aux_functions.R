# Fit baseline
fit_ARIMA_baseline <- function(df) {
  
  all_dates <- seq(min(df$Date), max(df$Date), by = "month")
  
  df2 <- right_join(df, tibble(Date = all_dates), by = "Date") %>%
    arrange(Date) %>%
    mutate(incidence_per_1k = replace_na(incidence_per_1k, 0))
  
  ts_d <- ts(
    df2$incidence_per_1k,
    start = c(year(min(df2$Date)), month(min(df2$Date))),
    frequency = 12
  )
  
  fit <- tryCatch(
    Arima(
      ts_d,
      order = c(0, 0, 0)
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  se <- sqrt(diag(vcov(fit)))
  ci <- confint(fit)
  
  coef_table <- tibble(
    District = df$District[1],
    Parameter = names(coef(fit)),
    Estimate  = as.numeric(coef(fit)),
    SE        = as.numeric(se),
    CI_low    = ci[, 1],
    CI_high   = ci[, 2],
    sigma2    = fit$sigma2,
    BIC       = BIC(fit),
    p = 0,
    d = 0,
    q = 0
  )
  
  list(
    model = fit,
    coefficients = coef_table,
    data = df2,
    ts = ts_d
  )
}

# Rolling baseline
rolling_baseline <- function(data,
                             window_size = 60,
                             n_windows = 23) {
  
  rolling_incidence <- list()
  
  for (i in 1:n_windows) {
    incidence_window <- data[(1 + i):(window_size + i), ]
    rolling_incidence[[i]] <- fit_ARIMA_baseline(incidence_window)
    print(i)
  }
  
  return(rolling_incidence)
}



fit_SARIMA <- function(df, SARIMAX = FALSE, xreg_col = NULL) {
  
  all_dates <- seq(min(df$Date), max(df$Date), by = "month")
  
  df2 <- right_join(df, tibble(Date = all_dates), by = "Date") %>%
    arrange(Date) %>%
    mutate(incidence_per_1k = replace_na(incidence_per_1k, 0))
  
  ts_d <- ts(
    df2$incidence_per_1k,
    start = c(year(min(df2$Date)), month(min(df2$Date))),
    frequency = 12
  )
  
  xreg_mat <- NULL
  
  # Build xreg only if SARIMAX = TRUE
  if (SARIMAX) {
    
    if (is.null(xreg_col)) {
      stop("You must provide xreg_col when SARIMAX = TRUE")
    }
    
    xreg_mat <- as.matrix(df2[, xreg_col, drop = FALSE])
    
    fit <- tryCatch(
      auto.arima(
        ts_d,
        xreg = xreg_mat,
        seasonal = TRUE,
        stepwise = FALSE,
        approximation = FALSE
      ),
      error = function(e) NULL
    )
    
  } else {
    
    fit <- tryCatch(
      auto.arima(
        ts_d,
        seasonal = TRUE,
        stepwise = FALSE,
        approximation = FALSE
      ),
      error = function(e) NULL
    )
  }
  
  if (is.null(fit)) return(NULL)
  
  se <- sqrt(diag(vcov(fit)))
  ci <- confint(fit)
  
  coef_table <- tibble(
    District = df$District[1],
    Parameter = names(coef(fit)),
    Estimate  = as.numeric(coef(fit)),
    SE        = as.numeric(se),
    CI_low    = ci[, 1],
    CI_high   = ci[, 2],
    sigma2    = fit$sigma2,
    BIC       = BIC(fit),
    p = arimaorder(fit)["p"],
    d = arimaorder(fit)["d"],
    q = arimaorder(fit)["q"],
    P = arimaorder(fit)["P"],
    D = arimaorder(fit)["D"],
    Q = arimaorder(fit)["Q"]
  )
  
  list(
    model = fit,
    coefficients = coef_table,
    data = df2,
    ts = ts_d,
    xreg = xreg_mat
  )
}



# Forecasts SARIMA models
forecast_SARIMAs <- function(model_list, h = 4, future_xreg_list = NULL) {
  
  fc_list <- vector("list", length(model_list))
  
  for (i in seq_along(model_list)) {
    
    obj <- model_list[[i]]
    
    if (is.null(obj)) {
      fc_list[[i]] <- NULL
      next
    }
    
    fc_list[[i]] <- tryCatch({
      
      if (is.null(obj$xreg)) {
        
        forecast(obj$model, h = h)
        
      } else {
        
        if (is.null(future_xreg_list) || is.null(future_xreg_list[[i]])) {
          stop("Missing future xreg values")
        }
        
        future_xreg <- future_xreg_list[[i]]
        
        future_xreg <- matrix(
          as.numeric(future_xreg),
          ncol = ncol(obj$xreg)
        )
        
        colnames(future_xreg) <- colnames(obj$xreg)
        
        forecast(
          obj$model,
          xreg = future_xreg,
          h = h
        )
      }
      
    }, error = function(e) {
      message("Forecast failed for model ", i, ": ", e$message)
      NULL
    })
  }
  
  names(fc_list) <- paste0("window_", seq_along(model_list))
  
  fc_list
}


# Rolling SARIMA
rolling_sarima <- function(
    national_incidence,
    n_windows = 23,
    SARIMAX = FALSE,
    xreg_col = NULL
) {
  
  rolling_incidence <- list()
  
  for (i in 1:n_windows) {
    
    incidence_window <- national_incidence[(1 + i):(60 + i), ]
    
    rolling_incidence[[i]] <- fit_SARIMA(
      df = incidence_window,
      SARIMAX = SARIMAX,
      xreg_col = xreg_col
    )
    
    print(i)
  }
  
  return(rolling_incidence)
}

forecast_baselines <- function(model_list, h = 4) {
  
  fc_list <- vector("list", length(model_list))
  
  for (i in seq_along(model_list)) {
    
    obj <- model_list[[i]]
    
    if (is.null(obj) || is.null(obj$model)) {
      fc_list[[i]] <- NULL
      next
    }
    
    fc_list[[i]] <- tryCatch({
      
      forecast::forecast(obj$model, h = h)
      
    }, error = function(e) {
      message("Forecast failed for baseline model ", i, ": ", e$message)
      NULL
    })
  }
  
  names(fc_list) <- paste0("window_", seq_along(model_list))
  
  return(fc_list)
}

# get forecasted quantiles

get_forecasted_quantiles <- function(forecast_list) {
  
  probabilities <- c(
    0.01, 0.025,
    seq(0.05, 0.95, by = 0.05),
    0.975, 0.99
  )
  
  lapply(forecast_list, function(fc) {
    
    if (is.null(fc)) return(NULL)
    
    # Recover SE from the 95% interval
    se <- (fc$upper[, "95%"] - fc$mean) / qnorm(0.975)
    
    qmat <- sapply(probabilities, function(p) {
      qnorm(
        p,
        mean = fc$mean,
        sd = se
      )
    })
    
    colnames(qmat) <- probabilities
    rownames(qmat) <- paste0("h", seq_along(fc$mean))
    
    qmat
  })
}
