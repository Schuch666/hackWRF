#' Summary the emission from two emission files (00Z and 12z) from WRF-Chem model
#'
#' @description Calculate the summary for each specie of gas and aerosol in two emission file
#'
#' @param file_00 file patch and name for 00z emission file
#' @param file_12 file patch and name for 12z emission file
#' @param ... arguments passed to summary_emission
#'
#' @return a data.frame containing the pollutant name, minimum, average, maximum, units, molar weight and total (in kt/year)
#'
#' @seealso summary_emission
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' summary_emission_24h(file_00 = 'wrfchemi_00z_d01',
#'                      file_12 = 'wrfchemi_12z_d01')
#'
#' }
#'
summary_emission_24h <- function(file_00, file_12 = file_00[2], ...){

  if(file_00[1] == file_12)
    warning('file_00 == file_12, use summaty_emission instead!')

  cat('* processing',file_00[1],'...\n')
  sum_00 <- summary_emission(file_00[1], ... )

  cat('* processing',file_12,'...\n')
  sum_12 <- summary_emission(file_12, ... )

  summary_e <- sum_00
  summary_e$med   = sum_00$med/2   + sum_12$med/2
  summary_e$total = sum_00$total/2 + sum_12$total/2
  for(i in 1:nrow(summary_e)){
    if(is.na(sum_00$min[i])){
      summary_e$min[i] = NA
      summary_e$max[i] = NA
    }else{
      summary_e$min[i] = min(sum_00$min[i], sum_12$min[i], na.rm = T)
      summary_e$max[i] = max(sum_00$max[i], sum_12$max[i], na.rm = T)
    }
  }
  return(summary_e)
}
