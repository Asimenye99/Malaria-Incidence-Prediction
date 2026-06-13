##################################
# ARIMA, SARIMA and SARIMAX models
# VICTOR ARRAES ROCHA FELIX


##############################################
# Fit ARIMA (0,1,0) random walk baseline model
fit_ARIMA_baseline <- function(df) {
  
  set.seed(1)
  
  # organize the data in the correct order
  all_dates <- seq(min(df$Date), max(df$Date), by = "month") 
  df2 <- right_join(df, tibble(Date = all_dates), by = "Date") %>%
    arrange(Date) 
  
  # create a time series in the correct order
  ts_d <- ts(
    df2$Incidence_per_1k,
    start = c(year(min(df2$Date)), month(min(df2$Date))),
    frequency = 12
  )
  
  fit <- tryCatch(
    Arima(
      ts_d,
      order = c(0, 1, 0) # fit a arima random walk
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  # Save the Arima model
  
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
  
  # Output of the function
  list(
    model = fit,
    coefficients = coef_table,
    data = df2,
    ts = ts_d,
    forecast_date = max(df$Date) # export forecast date
  )
}
#######################################
# Rolling window for the baseline model
rolling_baseline <- function(data,
                             window_size = 60,
                             n_windows = 23) {
  
  rolling_incidence <- list()
  
  # crop the dataframe based the same number of rows as window size
  # save the dataframe inside the list
  # roll to the next subset and crop again
  # save as a second item of the list
  # and so on, based on the n_windows
  
  for (i in 1:n_windows) {
    incidence_window <- data[(i):(window_size +  i - 1), ] # I needed the -1 so window size begin on 1 to 60
    rolling_incidence[[i]] <- fit_ARIMA_baseline(incidence_window)
    print(i)
  }
  
  return(rolling_incidence)
}

#################
# Baseline models 
# function for forecasting ARIMA (0,1,0) baseline models

forecast_baselines <- function(model_list, h = 3) {
  
  set.seed(1) # set seed for reproducibility
  
  fc_list <- vector("list", length(model_list)) # create a list to save the forecasts
  
  # for each model in model_list
  for (i in seq_along(model_list)) {
    # get the model information
    obj <- model_list[[i]]
    # Check if the model is valid
    if (is.null(obj) || is.null(obj$model)) {
      fc_list[[i]] <- NULL
      next
    }
    # Try to make a forecast with the model, if valid
    # and save to a given element of the list
    fc_list[[i]] <- tryCatch({
      # forecast using each baseline trained before
      fc <- forecast::forecast(
        obj$model, # get the model
        h = h, # forecast h number of months ahead
        level = 99 # forecast with 99 prediction interval
      )
      # save the forecast function output
      # and the forecast date in a list
      list(
        forecast = fc,
        forecast_date = obj$forecast_date
      )
      # show an error message if the model don't work
    }, error = function(e) {
      message("Forecast failed for baseline model ", i, ": ", e$message)
      NULL
    })
  }
  # set the forecast name as window number
  names(fc_list) <- paste0("window_", seq_along(model_list))
  # return the final list with each forecast as one of its elements
  return(fc_list)
}


##################################
# Fit SARIMA or SARIMAX models
# use "xreg_col" if SARIMAX = TRUE

fit_SARIMA <- function(df, SARIMAX = FALSE, xreg_col = NULL) {

  set.seed(1) # set seed for reproducibility
  
  # organize the data in the correct order
  all_dates <- seq(min(df$Date), max(df$Date), by = "month")
  df2 <- right_join(df, tibble(Date = all_dates), by = "Date") %>%
    arrange(Date) 
  
  # create a time series in the correct order
  ts_d <- ts(
    df2$Incidence_per_1k,
    start = c(year(min(df2$Date)), month(min(df2$Date))),
    frequency = 12
  )
  # create a xreg_mat value, which we will update if using SARIMAX
  xreg_mat <- NULL
  
  # Use xreg only if SARIMAX = TRUE
  if (SARIMAX) {
    # print a warning if you do not provide a xreg_col if SARIMAX = TRUE
    if (is.null(xreg_col)) {
      stop("You must provide xreg_col when SARIMAX = TRUE")
    }
    # transform the xreg_col into a matrix for the model
    xreg_mat <- as.matrix(df2[xreg_col])
    # try to fit the model
    fit <- tryCatch(
      auto.arima(
        ts_d,
        xreg = xreg_mat,
        seasonal = TRUE,
        stepwise = TRUE,
        approximation = FALSE
      ),
      error = function(e) NULL
    )
    
  } else {
    # Fit a SARIMA if SARIMAX is not TRUE
    # It does not require a xreg_mat
    fit <- tryCatch(
      auto.arima(
        ts_d,
        seasonal = TRUE,
        stepwise = TRUE,
        approximation = FALSE
      ),
      error = function(e) NULL
    )
  }
  # If it cannot fit the model return NULL
  if (is.null(fit)) return(NULL)
  # get model results
  se <- sqrt(diag(vcov(fit)))
  ci <- confint(fit)
  # get model coefficients
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
  # results that will be exported
  list(
    model = fit,
    coefficients = coef_table,
    data = df2,
    ts = ts_d,
    xreg = xreg_mat,
    forecast_date = max(df$Date)
  )
}

############################################
# this function gets the exogenous variables 
# that are used on each ARIMAX forecasts
# based on 1-lag, 2-lag or 3-lag approach

get_exog_variables <- function(data, variable, lag = 1,
                               n_windows = 23, train_length = 60) {
  
  future_exog <- data.frame()
  
  for (i in 1:n_windows) {
    
    exog60 <- data[[variable]][train_length + i - 1]
    exog59 <- data[[variable]][train_length + i - 2]
    exog58 <- data[[variable]][train_length + i - 3]
    
    if (lag == 1) {
      row <- data.frame(
        window = i,
        lag = "lag1",
        h1 = exog60,
        h2 = exog60,
        h3 = exog60
      )
    }
    
    if (lag == 2) {
      row <- data.frame(
        window = i,
        lag = "lag2",
        h1 = exog59,
        h2 = exog60,
        h3 = exog60
      )
    }
    
    if (lag == 3) {
      row <- data.frame(
        window = i,
        lag = "lag3",
        h1 = exog58,
        h2 = exog59,
        h3 = exog60
      )
    }
    
    future_exog <- rbind(future_exog, row)
  }
  
  future_exog
}

# Forecast SARIMA and SARIMAX models
# SARIMAX need exogenous variables
forecast_SARIMAs <- function(model_list, h = 4, future_xreg_list = NULL) {

  set.seed(1)
  
  fc_list <- vector("list", length(model_list))
  
  for (i in seq_along(model_list)) {
    
    obj <- model_list[[i]]
    
    if (is.null(obj)) {
      fc_list[[i]] <- NULL
      next
    }
    
    fc_list[[i]] <- tryCatch({
      
      fc <- if (is.null(obj$xreg)) {
        
        forecast(
          obj$model,
          h = h,
          level = 99
        )
        
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
          h = h,
          level = 99
        )
      }
      
      list(
        forecast = fc,
        forecast_date = obj$forecast_date
      )
      
    }, error = function(e) {
      message("Forecast failed for model ", i, ": ", e$message)
      NULL
    })
  }
  
  names(fc_list) <- paste0("window_", seq_along(model_list))
  
  fc_list
}


# Rolling windows for SARIMA
# and for SARIMAX models
rolling_sarima <- function(
    national_incidence,
    n_windows = 23,
    SARIMAX = FALSE,
    xreg_col = NULL,
    window_size = 60
) {
  
  rolling_incidence <- list()
  
  for (i in 1:n_windows) {
    
    incidence_window <- national_incidence[(i):(window_size + i -1), ] # I needed the -1 so window size begin on 1 to 60
    
    rolling_incidence[[i]] <- fit_SARIMA(
      df = incidence_window,
      SARIMAX = SARIMAX,
      xreg_col = xreg_col
    )
    
    print(i)
  }
  
  return(rolling_incidence)
}


###################################
# build quantiles for each forecast
# based on 99% prediction interval
##################################

compute_quantiles <- function(forecast_list) {
  
  probabilities <- c(
    0.01, 0.025,
    seq(0.05, 0.95, by = 0.05),
    0.975, 0.99
  )
  
  lapply(forecast_list, function(fc_obj) {
    
    if (is.null(fc_obj)) return(NULL)
    
    fc <- fc_obj$forecast
    
    # Recover SE from the 99% interval
    se <- (fc$mean - fc$lower[, "99%"]) / qnorm(0.995)
    
    qmat <- sapply(probabilities, function(p) {
      qnorm(
        p,
        mean = fc$mean,
        sd = se
      )
    })
    
    colnames(qmat) <- probabilities
    rownames(qmat) <- paste0("h", seq_along(fc$mean))
    
    list(
      quantiles = qmat,
      forecast_date = fc_obj$forecast_date
    )
  })
}


# format forecasted quantiles for WIS and MAE

format_forecast_for_WIS<- function(quantile_object) {
  
  model_name <- deparse(substitute(quantile_object))
  
  bind_rows(
    lapply(names(quantile_object), function(w) {
      
      qmat <- quantile_object[[w]]$quantiles
      fc_date <- quantile_object[[w]]$forecast_date
      
      quantile_rows <- qmat %>%
        as.data.frame() %>%
        rownames_to_column("horizon") %>%
        pivot_longer(
          cols = -horizon,
          names_to = "quantile",
          values_to = "value"
        ) %>%
        mutate(
          model = model_name,
          forecast_date = as.Date(fc_date),
          location = "National",
          horizon = as.numeric(gsub("h", "", horizon)),
          temporal_resolution = "month",
          target_variable = "incidence",
          target_end_date = forecast_date %m+% months(horizon),
          type = "quantile",
          quantile = as.numeric(quantile)
        )
      
      point_rows <- quantile_rows %>%
        filter(quantile == 0.5) %>%
        mutate(
          type = "point",
          quantile = NA_real_
        )
      
      bind_rows(quantile_rows, point_rows)
    })
  ) %>%
    select(
      model,
      forecast_date,
      location,
      horizon,
      temporal_resolution,
      target_variable,
      target_end_date,
      type,
      quantile,
      value
    )
}


# Format distric forecasts so we can evaluate later
format_district_forecasts <- function(forecast_list,
                                      model_name = "MODEL") {
  
  output <- data.frame()
  
  for (district_name in names(forecast_list)) {
    
    forecast_of_one_district <- format_forecast_for_WIS(
      forecast_list[[district_name]]
    )
    
    forecast_of_one_district$location <- district_name
    forecast_of_one_district$model <- model_name
    
    output <- bind_rows(
      output,
      forecast_of_one_district
    )
  }
  
  output
}
