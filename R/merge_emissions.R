#' Function to merge two different emissions
#'
#' @description Merge different emissions, the main emission or background emission
#' (usually from a global inventory) and an auxiliary emission. The resulting emissions
#' has the values of the auxiliar emissions for grid points with emissions and background
#' emission for grid points with zero emissions.
#'
#' @param background main input file (WRF-Chem emission file)
#' @param auxiliar auxliar input file (WRF-Chem emission file)
#' @param output output file (WRF-Chem emission file), or NA to return the emission array
#' @param name variable/pollutant name
#' @param proj projection to convert the raster, default is NA (no conversion)
#' @param plot to plot from output file
#' @param verbose display additional information
#'
#' @return if output is NA, returns an array or a matrix
#'
#' @import eixport
#'
#' @examples
#' \dontrun{
#' NEW_e <- merge_emission(background = 'base/wrfchemi_00z_d01',
#'                         auxiliar   = 'extra/wrfchemi_00z_d01',
#'                         name       = 'E_NO')
#'}
#' @export
merge_emission <- function(background,
                           auxiliar,
                           output  = NA,
                           name,
                           proj    = NA,
                           plot    = T,
                           verbose = T){
  # if(verbose){
    cat('Background emission:',background, '\n')
    cat('Auxiliar emission  :',auxiliar,'\n')
    if(!is.na(output))
      cat('Output file        :',output, '\n')
  # }
  main_input  <- wrf_raster(background, name, raster_crs = proj, verbose = F)
  aux_input   <- wrf_raster(auxiliar,name, raster_crs = proj, verbose = F)
  if(!is.na(output)){
    main_output <- wrf_raster(output, name, raster_crs = proj, verbose = F)
  }else{
    main_output <- main_input
  }
  # REPALCING ZEROS OF NAs in aux_input
  aux_input[aux_input == 0] <- NA
  # getting number of times
  N_times <- dim(main_output)[3]
  # COMBINING aux_input and main_input
  for(i in 1:N_times){
    if(verbose)
      cat('- processing',name,'time',i,'\n')
    main_output[[i]] <- merge(aux_input[[i]], main_input[[i]])
  }
  # TO EXPORT FOR NETCDF OUTPUT FILE
  VALUES <- raster_to_ncdf(main_output, na_value = 0)
  # VALUES <- wrf_get(output,name)
  # for(i in 1:N_times){
  #   VALUES[,,i] = as.matrix(t(raster::flip(main_output[[i]],2)))
  # }
  if(is.na(output)){
    cat('Returning emissions\n')
    return(VALUES)
  }
  wrf_put(output,name,VALUES,verbose = T)
  if(plot){
    cat('Ploting',name,'from file',output,'\n')
    wrf_plot(output,name,verbose = F,skip = T)
  }
}
