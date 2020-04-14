#' Model statistical evaluation
#'
#' @description Statistical evaluation
#'
#' @param mo model data.frame
#' @param ob observed data.frame
#' @param station name of the station
#' @param table a data.frame with output from evaluate or stats
#' @param clean remove rows with zero observations
#' @param verbose display additional information
#' @param ... arguments to be passing to stats
#'
#' @export
#'
#' @examples
#' model <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/model.Rds"))
#' obs   <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/obs.Rds"))
#'
#' # if first a test with no observed data
#' # the function return an empty row
#' table <- evaluate(mo = model, ob = obs, station = "VVIbes")
#'
#' # now a test with a few observed values
#' table <- evaluate(mo = model, ob = obs, station = "Americana", table = table)
#'
#' # new tests with no data will be discated
#' table <- evaluate(mo = model, ob = obs, station = "VVIbes", table = table)
#'
#' # if the station are not in the input data frame a message is displayed
#' # and the function return an empty row
#' table <- evaluate(mo = model, ob = obs, station = "Ibirapuera", table = table)
#'
#' # if the first evaluation has no data, the last call can remove the line
#' table <- evaluate(mo = model, ob = obs, station = "Americana", table = table, clean = TRUE)

evaluate <- function(mo, ob, station, table = NULL, clean = FALSE ,verbose = TRUE, ...){
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
  DATA  <- merge(model, obser, by = "date", all.x = TRUE)
  A     <- DATA$model
  B     <- DATA$obser

  if(length(B[!is.na(B)]) > 8){
    if(verbose)
      cat(station,'has',length(B[!is.na(B)]),'valid observations\n')
    RESULT <- stats(A,B, ...)
    row.names(RESULT) <- station
  }else{
    if(verbose)
      cat(station,'has',length(B[!is.na(B)]),'valid observations\n')
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

# test <- evaluate(station = "VVIbes")
# test <- evaluate(station = "Americana", table = test)
# test <- evaluate(station = "Americana", table = test)
# test <- evaluate(station = "VVIbes", table = test)
# test <- evaluate(station = "VVIbes", table = test)
# test <- evaluate(station = "VVIbes", table = test, clean = TRUE)
#
# ex_model <- model[1:240,c(1,2,12,22)]
# ex_data  <- observed[1:240,c(1,2,12,20)]
#
# saveRDS(ex_model,'C:/Users/under/Documents/R-Packages/hackWRF/data/model.Rds')
# saveRDS(ex_data, 'C:/Users/under/Documents/R-Packages/hackWRF/data/obs.Rds')
