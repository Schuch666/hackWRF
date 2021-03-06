#' Model statistical evaluation
#'
#' @description Statistical evaluation from 2 data.frames. The input data.frames (model and observation)
#' must contain a date collum (with POSIXlt data) and also contains the same station (or pollutant) name.
#' The function test and combine the time pairs and perform some basic tests.
#' If a data.frame is provided to table argument a crbind is performed with a new row.
#'
#' @param mo model data.frame
#' @param ob observed data.frame
#' @param station name of the station
#' @param table a data.frame with output from evaluate or stats
#' @param wd default is FALSE, see notes
#' @param clean remove rows with zero observations
#' @param summaryze add a last line with the the average values and format the table
#' @param use_n only for summaryze = TRUE, use n as weight to calculate the average
#' @param formate works only with summaryzee, format the output for 2 digit (default)
#' @param cutoff minimum (optionally the maximum) valid value for observation
#' @param no_tz ignore tz from input
#' @param nobs minimum number of valid observations, default is 8
#' @param verbose display additional information
#' @param ... arguments to be passing to stats and plot
#'
#' @note for wind direction some the ME and MB are calculated using Mughal et al. (2017)
#'
#' @references
#'
#' Mughal MO, Lynch M, Yu F, McGann B, Jeanneret F, Sutton J (2017)
#' Wind modeling, validation and sensitivity study using Weather
#' Research and Forecasting model in complex terrain. Environ
#' Model Softw 90:107–125. https://doi.org/10.1016/j.envsoft.2017.
#' 01.009
#'
#' @export
#'
#' @examples
#' model <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/model.Rds"))
#' obs   <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/obs.Rds"))
#'
#' # if first a test with no observed data
#' # the function return an empty row
#' table <- evaluation(mo = model, ob = obs, station = "VVIbes")
#' print(table)
#'
#' # now a test with a few observed values
#' table <- evaluation(mo = model, ob = obs, station = "Americana", table = table)
#' print(table)
#'
#' # new tests with no data will be discated
#' table <- evaluation(mo = model, ob = obs, station = "VVIbes", table = table)
#' print(table)
#'
#' # if the station are not in the input data frame a message is displayed
#' # and the function return an empty row
#' table <- evaluation(mo = model, ob = obs, station = "Ibirapuera", table = table)
#' print(table)
#'
#' # if the first evaluation has no data, the last call can remove the line
#' table <- evaluation(mo = model, ob = obs, station = "Americana", table = table, clean = TRUE)
#' print(table)

evaluation <- function(mo, ob, station, table = NULL, wd = FALSE, clean = FALSE, cutoff = 0,
                       no_tz=FALSE, summaryze = FALSE, use_n = F, formate = T, nobs = 8,
                       verbose = TRUE, ...){
  if(summaryze){
    cat('creating the summary\n')
    if(last(row.names(table)) == 'GERAL')  table <- table[-nrow(table),]

    summa    <- 1:ncol(table)

    if(use_n){
      summa[1] <- sum(table$n,na.rm = T)
      for(i in 2:ncol(table)){
        summa[i] <- weighted.mean(table[,i], table$n, na.rm = T)
      }
    }else{
      summa[1] <- nrow(table)
      for(i in 2:ncol(table)){
        summa[i] <- mean(table[,i],na.rm = T)
      }
    }

    table          <- rbind(table,'GERAL' = summa)
    if(formate){
      table$n         = as.integer(table$n)
      table$Obs       = round(table$Obs,2)
      table$Sim       = round(table$Sim,2)
      table$r         = round(table$r,2)
      table$FA2       = round(table$FA2,2)
      table$RMSE      = round(table$RMSE,2)
      table$MB        = round(table$MB,2)
      table$`MFB (%)` = round(table$`MFB (%)`,2)
      table$`MFE (%)` = round(table$`MFE (%)`,2)
      table$`NMB (%)` = round(table$`NMB (%)`,2)
      table$`NME (%)` = round(table$`NME (%)`,2)
      table$ME        = round(table$ME,2)
      table$IOA       = round(table$IOA,2)
      table$GE        = round(table$GE,2)
    }

    return(table)
  }
  if(!station %in% names(ob)){
    cat(station,'not found in observation input\n')
    RESULT <- stats((1:199)/100,(1:199)/100)
    RESULT$n = 0
    row.names(RESULT) <- station
    if(is.null(table)){
      return(RESULT)
    }
    else{
      RESULT <- rbind(table,RESULT)
      if(clean)
        RESULT <- RESULT[RESULT$n > 0,]
      return(RESULT)
    }
  }
  if(!station %in% names(mo)){
    cat(station,'not found in model input\n')
    RESULT <- stats((1:199)/100,(1:199)/100)
    RESULT$n = 0
    row.names(RESULT) <- station
    if(is.null(table)){
      return(RESULT)
    }
    else{
      RESULT <- rbind(table,RESULT)
      if(clean)
        RESULT <- RESULT[RESULT$n > 0,]
      return(RESULT)
    }
  }

  model        <- mo[,c("date",station)]
  names(model) <- c("date","model")
  obser        <- ob[,c("date",station)]
  names(obser) <- c("date","obser")
  if(no_tz){
    f <- function(x,tz="GMT") return(as.POSIXct(as.numeric(x), origin="1970-01-01", tz=tz))
    model$date <- f(model$date)
    obser$date <- f(model$date)
  }
  DATA  <- merge(model, obser, by = "date", all.x = TRUE)
  A     <- DATA$model
  B     <- DATA$obser

  # special case of zeros and NAs (work for constant values and NAs)
  to_run = TRUE
  if(suppressWarnings( max(A,na.rm = T) ) == suppressWarnings( min(A,na.rm = T)) ){
    if(verbose)
      cat(station,'contains only zeros (or constant values) and NA values for model\n')
    to_run = FALSE
  }
  if(suppressWarnings(  max(B,na.rm = T) ) == suppressWarnings( min(B,na.rm = T)) ){
    if(verbose)
      cat(station,'contains only zeros (or constant values) and NA values for observations\n')
    to_run = FALSE
  }

  if(length(B[!is.na(B)]) > nobs & to_run){
    if(verbose)
      cat(station,'has',length(B[!is.na(B)]),'valid observations\n')
    RESULT <- stats(A,B, cutoff=cutoff, wd = wd, nobs = nobs, ...)
    row.names(RESULT) <- station
  }else{
    if(verbose & to_run)
      cat(station,'has only',length(B[!is.na(B)]),'valid observations (lesser than',nobs,'obs)\n')
    RESULT <- stats((1:199)/100,(1:199)/100)
    RESULT$n = 0
    row.names(RESULT) <- station
  }

  if(RESULT$n > 0){
    if(max(B,na.rm = T) == min(B, na.rm = T)){
      if(verbose)
        cat(station,'values are constant, No of observation set to 0\n')
      RESULT$n = 0
    }
  }

  if(is.null(table)){
    return(RESULT)
  }
  else{
    RESULT <- crbind(table,RESULT,RESULT$n > 0)
    if(clean)
      RESULT <- RESULT[RESULT$n > 0,]
    return(RESULT)
  }
}
