
# 1 - packages -----------------------------------------------------------

#list of packages required
list_of_packages <- c("remotes", "renv", "shiny", "forecast",
                      "ahead", "memoise", "RColorBrewer")

#checking missing packages from list
new_packages <- list_of_packages[!(list_of_packages %in% utils::installed.packages()[,"Package"])]

#install missing ones
options(repos = c("MyRepo"='https://techtonique.r-universe.dev', 
                  "CRAN"="https://cran.rstudio.com"))
if(length(new_packages)) 
{
  for (pkg in new_packages)
  {
    if (identical(pkg, "ahead"))
    {
      install.packages("ahead", 
                       repos = c("https://techtonique.r-universe.dev", "https://cloud.r-project.org"))
    } else {
      utils::install.packages(pkg, dependencies = TRUE) 
    }
  }
}

library(shiny)
library(memoise)

# 2 - functions -----------------------------------------------------------

# moving block bootstrap
mbb2 <- function(r,
                 n,
                 b,
                 seed = 123,
                 return_indices = FALSE)
{
  n_obs <- dim(r)[1]
  n_series <- dim(r)[2]
  b <- floor(min(max(3L, b), n_obs - 1L))
  if(n >= n_obs)
    stop("forecasting horizon must be < number of observations")
  n <- min(n_obs, n)
  
  set.seed(seed) # important for base::sample below
  
  r_bt <- matrix(NA, nrow = n_obs, ncol = dim(r)[2])  # local vector for a bootstrap replication
  
  #cat("n_obs", n_obs, "\n")
  #cat("b", b, "\n")
  for (i in 1:ceiling(n_obs/b)) {
    #cat("i: ", i, "----- \n")
    endpoint <- sample(b:n_obs, size = 1)
    #cat("endpoint", endpoint, "\n")
    try(r_bt[(i - 1)*b + 1:b, ] <- r[endpoint - (b:1) + 1, ],
        silent = TRUE)
  }
  
  tmp <- matrix(r_bt[(1:n), ], nrow = n, ncol = n_series)
  
  if(return_indices == FALSE)
  {
    return(tmp)
  } else {
    return(arrayInd(match(tmp, r), .dim = dim(r))[1:n, 1])
  }
}
mbb2 <- memoise::memoise(mbb2)

# split data set 
split_dataset <- function(dataset=c("airpassengers",
                                    "fdeaths",
                                    "lynx",
                                    "nile",
                                    "ukgas",
                                    "usaccdeaths"),
                          transformation=c("none", 
                                           "boxcox",
                                           "diff",
                                           "diffboxcox"))
{
  dataset <- match.arg(dataset)
  transformation <- match.arg(transformation)
  x <- switch(dataset,
              airpassengers = datasets::AirPassengers,
              fdeaths = datasets::fdeaths,
              lynx = datasets::lynx,
              nile = datasets::Nile,
              ukgas = datasets::UKgas,
              usaccdeaths = datasets::USAccDeaths)
  if (identical(transformation, "boxcox"))
  {
    x <- forecast::BoxCox(x, lambda = "auto")
  }
  if (identical(transformation, "diff"))
  {
    x <- diff(x)
  }
  if (identical(transformation, "diffboxcox"))
  {
    x <- diff(forecast::BoxCox(x, lambda = "auto"))
  }
  
  freq_x <- frequency(x)
  n <- floor(0.9*length(x))
  x_train <- ts(x[1:floor(n/2)], 
                start=start(x), 
                frequency = freq_x)
  x_calib <- ts(x[(floor(n/2) + 1):n], 
                start=tsp(x_train)[2] + 1 / freq_x, 
                frequency = freq_x)
  x_test <- ts(x[(n + 1):length(x)], 
               start=tsp(x_calib)[2] + 1 / freq_x, 
               frequency = freq_x)
  res <- list()
  res$x <- x
  res$x_train <- x_train
  res$x_calib <- x_calib
  res$x_test <- x_test
  return(res)
}
split_dataset <- memoise::memoise(split_dataset)

# forecasting function 
forecast_function <- function(obj_ts,
                              method = c("dynrm", 
                                         "theta", 
                                         "snaive"), 
                              block_size = 5, 
                              B = 250, 
                              level = 95, 
                              seed=123)
{
  method <- match.arg(method)
  freq_x <- frequency(obj_ts$x)
  fcast_func <- switch(method, 
                       dynrm = ahead::dynrmf,
                       theta = forecast::thetaf,
                       snaive = forecast::snaive)
  
  obj <- fcast_func(obj_ts$x_train, 
                    h=length(obj_ts$x_calib)) # train on training set predict on calibration set
  calibrated_resids <- obj_ts$x_calib - obj$mean # obtain calibrated residuals
  obj_fcast <- fcast_func(obj_ts$x_calib, 
                          h=length(obj_ts$x_test)) # train on calibration set 
  
  sims <- ts(sapply(1:B, function(i) mbb2(matrix(calibrated_resids, 
                                                 ncol = 1), 
                                          n=length(obj_ts$x_test), 
                                          b=block_size, 
                                          seed=i+seed*100)),
             start = start(obj_ts$x_test), 
             frequency = frequency(obj_ts$x_test))
  
  preds <- obj_fcast$mean + sims
  obj_fcast2 <- list()
  obj_fcast2$level <- level 
  obj_fcast2$x <- obj_ts$x_calib
  start_preds <- start(obj_fcast$mean)
  obj_fcast2$mean <- ts(rowMeans(preds), 
                        start = start_preds, 
                        frequency = freq_x)
  obj_fcast2$upper <- ts(apply(preds, 1, function(x)
    stats::quantile(x, probs = 1 - (1 - level / 100) / 2)), 
    start = start_preds, 
    frequency = freq_x)
  obj_fcast2$lower <- ts(apply(preds, 1, function(x)
    stats::quantile(x, probs = (1 - level / 100) / 2)), 
    start = start_preds, 
    frequency = freq_x)
  class(obj_fcast2) <- "forecast"
  
  res <- list()
  res$x <- obj_ts$x 
  res$x_train <- obj_ts$x_train 
  res$x_calib <- obj_ts$x_calib
  res$x_test <- obj_ts$x_test
  res$obj <- obj 
  res$calibrated_resids <- calibrated_resids
  res$obj_fcast2 <- obj_fcast2
  res$sims <- sims 
  
  return(res)
}
forecast_function <- memoise::memoise(forecast_function)


# plot results 
plot_results <- function(obj)
{
  par(mfrow=c(2, 2))
  
  plot(obj$x, ylab="Value", 
       main="input data = black + green + blue \n calibration set = green \n forecast on calibration set = red", 
       lwd=2, col="black")
  lines(obj$x_calib, col="green", lwd=2)
  lines(obj$obj$mean, col="red", lwd=2)
  lines(obj$x_test, col="blue", lwd=2)
  
  plot(obj$calibrated_resids, ylab="Value", 
       main="calibrated residuals \n = green - red = orange",
       col="orange", lwd=2)
  abline(h = 0, lty=2)
  
  # matplot(as.numeric(time(obj$sims)), 
  #         obj$sims, type='l', xlab = "Time", 
  #         main = "calibrated residuals simulations (100) \n (block bootstrap)")
  time_sims <- as.numeric(time(obj$sims))
  #vivid_colors <- RColorBrewer::brewer.pal(8, "Set1")
  custom_palette <- c("red", "magenta", "yellow", "orange", "black", "green", "blue")
  custom_palette <- colorRampPalette(custom_palette)(ncol(obj$sims))
  plot(1, type="n", 
       xlim=c(min(time_sims), max(time_sims)), 
       ylim=c(min(obj$sims), max(obj$sims)),
       xlab="Time", ylab="Value", 
       main="calibrated residuals simulations (100) \n (moving block bootstrap)")
  for (i in 1:ncol(obj$sims)) {
    lines(time_sims, obj$sims[,i], col=custom_palette[i], lty=2, lwd=2)
  }
  
  plot(obj$obj_fcast2, xlab = "Time", ylab="Value", 
       main = "test set = blue \n mean forecast = light blue \n 95% pred. intervals for simulations = grey shade")
  lines(obj$x_test, col='blue', lwd=2)
  lines(obj$x_calib, col="green", lwd=2)
}

