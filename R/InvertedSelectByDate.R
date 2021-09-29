#' Utility function to make it easier to remove select periods from a data frame
#'
#' @param mydata A data frame containing a date field in hourly or high resolution format.
#' @param start A start date string in the form d/m/yyyy e.g. “1/2/1999” or in ‘R’ format i.e. “YYYY-mm-dd”, “1999-02-01”
#' @param end See start for format.
#' @param year not used at the moment
#' @param month not used at the moment
#' @param day not used at the moment
#' @param hour not used at the moment
#' @param incert_na incert a NA between between the two parts
#' @param verbose to display additional information
#'
#' @importFrom openair selectByDate
#'
#' @examples
#' model <- readRDS(paste0(system.file("extdata",package="hackWRF"),"/model.Rds"))
#' plot(model$SAndre~model$date, ty = 'l', col = '#777777', lwd = 2)
#'
#' model2 <- SelectByInvertedDate(model, start = '2012-01-03', end = '2012-01-05')
#' lines(model2$SAndre~model2$date, col = '#44AA44', lwd = 2)
#'
#' @export
#'

SelectByInvertedDate <- function(mydata,
                                 start = "1/9/2008",
                                 end   = "30/9/2008",
                                 year,
                                 month,
                                 day,
                                 hour,
                                 incert_na = TRUE,
                                 verbose = T){

  data_start <- format(mydata$date[1],    format="%Y-%m-%d")
  data_end   <- format(last(mydata$date), format="%Y-%m-%d")

  part1    <- selectByDate(mydata = mydata, start = data_start, end = start)
  part2    <- selectByDate(mydata = mydata, start = end,        end = data_end)

  if(incert_na){
    NAS      <- last(mydata)
    dur      <- last(part1$date) - part2$date[1]
    NAS[1,1] <- last(part1$date) + dur/2
    NAS[,-1] <- NA
    return(rbind(part1,NAS,part2))

  }else{
    return(rbind(part1,part2))
  }
}
