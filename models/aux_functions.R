##################################
# ARIMA, SARIMA and SARIMAX models
# VICTOR ARRAES ROCHA FELIX


##############################################
# Fit ARIMA (0,1,0) random walk baseline model
fit_ARIMA_baseline <- function(df) {
  
  set.seed(1)
  
  # organize the data in the correct order
  df <- df %>%
    arrange(Date)
  
  # create a time series in the correct order
  ts_d <- ts(
    df$Incidence_per_1k,
    start = c(year(min(df$Date)), month(min(df$Date))),
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
    data = df,
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

fit_SARIMA <- function(df, SARIMAX = FALSE, xvar_col = NULL) {

  set.seed(1) # set seed for reproducibility
  
  # organize the data in the correct order
  df <- df %>%
    arrange(Date)
  
  # create a time series in the correct order
  ts_d <- ts(
    df$Incidence_per_1k,
    start = c(year(min(df$Date)), month(min(df$Date))),
    frequency = 12
  )
  # create a xvar_mat value, which we will update if using SARIMAX
  xvar_mat <- NULL
  
  # Use xreg only if SARIMAX = TRUE
  if (SARIMAX) {
    # print a warning if you do not provide a xvar_col if SARIMAX = TRUE
    if (is.null(xvar_col)) {
      stop("You must provide xreg_col when SARIMAX = TRUE")
    }
    # transform the xvar_col into a matrix for the model
    xvar_mat <- as.matrix(df[xvar_col])
    # try to fit the model
    fit <- tryCatch(
      auto.arima(
        ts_d,
        xreg = xvar_mat,
        seasonal = TRUE,
        stepwise = TRUE,
        approximation = FALSE
      ),
      error = function(e) NULL
    )
    
  } else {
    # Fit a SARIMA if SARIMAX is not TRUE
    # It does not require a xvar_mat
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
    data = df,
    ts = ts_d,
    xvar = xvar_mat,
    forecast_date = max(df$Date)
  )
}


# Forecast SARIMA and SARIMAX models
# SARIMAX need exogenous variables
forecast_SARIMAs <- function(model_list, h = 3, future_xvar_list = NULL) {

  set.seed(1)
  
  fc_list <- vector("list", length(model_list)) # create a forecast list to store the results the same size of the model list
  
  # For each model in the model list
  for (i in seq_along(model_list)) {
    # get the model
    obj <- model_list[[i]]
    # check if the model is valid
    if (is.null(obj)) {
      fc_list[[i]] <- NULL # if not valid set the forecast result as NULL
      next
    }
    # if valid try to forecast
    fc_list[[i]] <- tryCatch({
      # if the model was fitted without exogenous variable
      fc <- if (is.null(obj$xvar)) {
        # forecast a sarima model
        forecast(
          obj$model,
          h = h,
          level = 99
        )
        
      } else { # if the model was fitted with a exogenous variable
        # check if we have future exogeous variables
        if (is.null(future_xvar_list) || is.null(future_xvar_list[[i]])) {
          stop("Missing future xreg values")
        }
        # get one element from the future exogenous variables list
        future_xvar <- future_xvar_list[[i]]
        # transform into a matrix to run in the code
        future_xvar <- matrix(
          as.numeric(future_xvar),
          ncol = ncol(obj$xvar)
        )
        # make sure the future and fitted xvar have the same name
        # just to avoid possible errors in the model
        colnames(future_xvar) <- colnames(obj$xvar)
        # forecast using the future exogenous variables
        forecast(
          obj$model,
          xreg = future_xvar,
          h = h,
          level = 99 # 99 prediction interval
        )
      }
      # export forecast and forecast date to fc_list
      list(
        forecast = fc,
        forecast_date = obj$forecast_date
      )
      
    }, error = function(e) {
      message("Forecast failed for model ", i, ": ", e$message)
      NULL
    })
  }
  # assing names to the elements of fc_list based on the forecast window
  names(fc_list) <- paste0("window_", seq_along(model_list))
  
  fc_list
}


# Rolling windows for SARIMA
# and for SARIMAX models
rolling_sarima <- function(
    district_data, # a single district dataframe which come from the list of dataframes that we input in the lapply
    n_windows = 23, # number of windows you want to crop the data to fit the model
    SARIMAX = FALSE, # if you are using a SARIMAX model, which will expect a exogenous variable
    xvar_col = NULL, # exougenous variable column
    window_size = 60 # size of the window, same as the number of observations you want to use to fit the model
) {
  
  fitted_models_list <- list() # empty list to store the fitted models
  
  for (i in 1:n_windows) { # for 1 ti the number of windows
    # crop the data for a given window size, then move to the next window
    rolling_window <- district_data[(i):(window_size + i -1), ] # I needed the -1 so window size begin on 1 to 60
    # fit the model using the data from the cropped data, for each given window
    fitted_models_list[[i]] <- fit_SARIMA(
      df = rolling_window, # dataframe cropped on a rolling window
      SARIMAX = SARIMAX, # SARIMAX TRUE or FALSE
      xvar_col = xvar_col # NULL or the selected exogenous variable column
    )
    
    print(i) # print so we can see models fitting (we may comment you this part)
  }
  
  return(fitted_models_list) # return fitted models for each rolling window and each district
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
