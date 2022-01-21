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
#' @param coef_backgroud coefficient to multiply main input
#' @param coef_auxiliar coefficient to multiply auxliar input
#' @param aux_n2 extra auxliar input file (will be merged with auxliar)
#' @param coe_n2 coefficient for to multiply extra auxliar input
#' @param plot to plot from output file
#' @param verbose display additional information
#' @param ... param for plot function
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
                           coef_backgroud = 1.0,
                           coef_auxiliar = 1.0,
                           aux_n2  = NA,
                           coe_n2  = 1.0,
                           plot    = T,
                           verbose = T,
                           ...){

  cat('Background emission:',background, '\n')
  if(coef_backgroud != 1.0)
    cat('Background coefficient:',coef_backgroud,'\n')
  cat('Auxiliar emission  :',auxiliar,'\n')
  if(coef_auxiliar  != 1.0)
    cat('Auxiliar coefficient:',coef_auxiliar,'\n')
  if(!is.na(output))
    cat('Output file        :',output, '\n')

  main_input  <- coef_backgroud * wrf_raster(background,name, verbose = F)
  if(is.na(aux_n2)){
    aux_input <- coef_auxiliar * wrf_raster(auxiliar,  name, verbose = F)
  }else{
    cat('Auxiliar emission 2:',aux_n2,'\n')
    if(coe_n2  != 1.0)
      cat('Auxiliar coefficient 2:',coe_n2,'\n')
    aux_input <- coef_auxiliar * wrf_raster(auxiliar,  name, verbose = F) +
                 coe_n2        * wrf_raster(aux_n2,    name, verbose = F)
  }

  if(!is.na(output)){
    main_output <- wrf_raster(output,  name, verbose = F)
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
    if(plot){
      cat('Ploting',name,'\n')
      plot(main_output[[1]],...)
    }
    cat('Returning emissions\n')
    return(VALUES)
  }
  wrf_put(output,name,VALUES,verbose = T)
  if(plot){
    cat('Ploting',name,'from file',output,'\n')
    wrf_plot(output,name,verbose = F,skip = T,...)
  }
}
