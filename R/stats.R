#' Calculate evaluation statistics from 2 vectors
#'
#' @description Calculate statistical indexes
#'
#' @param mo model
#' @param ob observed data
#' @param spinup number of model points excluded to the statistic
#' @param wd logical, true for wind direction, see notes
#' @param scatter set TRUE to plot a scatter plot
#' @param add set TRUE to add the points to a scatter plot
#' @param cor color of scatterplot dots
#' @param lim scatter plot limits
#' @param cutoff (optionally the maximum) valid value for observation
#' @param nobs minimum number of observations
#' @param verbose display additional information
#' @param ... extra arguments passed to scatter plot
#'
#' @importFrom openair modStats
#' @import graphics
#'
#' @note MB and ME are calculated using Mughal et al. (2017) for wind direction
#'
#' @export
#'
#' @examples
#' model <- 1:100
#' data  <- model + rnorm(100,0.2)
#' stats(mo = model, ob = data, scatter = TRUE)
#'

stats <- function(mo,ob,spinup = 0, wd = FALSE, scatter = F,add = F, cor="#FF000088",lim = NA,
                  cutoff = 0, nobs = 8, verbose = T, ...){

  if(spinup != 0){
    mo <- mo[(spinup+1):length(mo)]
    ob <- ob[(spinup+1):length(ob)]
  }
  if(length(mo) != length(ob))
    stop("mo and ob need to have the same length!") #nocov

  NA_mod <- is.na(mo)
  NA_obs <- is.na(ob)

  mo  <- mo[!NA_obs & !NA_mod]
  ob  <- ob[!NA_obs & !NA_mod]

  if(cutoff[1] > 0 ){
    cat('using',cutoff[1],'for min cutoff\n')

    mo  <- mo[ob >= cutoff[1]]
    ob  <- ob[ob >= cutoff[1]]

    cat(length(mo),'values left\n')

    if(length(mo) < nobs){
      RESULT <- stats((1:199)/100,(1:199)/100)
      RESULT$n = 0
      return(RESULT)
    }
  }
  if(length(cutoff)>1){
    cat('using',cutoff[2],'for max cutoff\n')

    mo  <- mo[ob < cutoff[2]]
    ob  <- ob[ob < cutoff[2]]

    cat(length(mo),'values left\n')
    if(length(mo) < nobs){
      RESULT <- stats((1:199)/100,(1:199)/100)
      RESULT$n = 0
      return(RESULT)
    }
  }

  MFBE <- function(mo,ob){
    MFB <- 0.0
    MFE <- 0.0
    NME <- 0.0
    for(i in 1:length(ob)){
      if(mo[i] - ob[i] != 0){
        MFB = MFB +    (mo[i] - ob[i]) / (mo[i] + ob[i])
        MFE = MFE + abs(mo[i] - ob[i]) / (mo[i] + ob[i])
        NME = NME + abs(mo[i] - ob[i])
      }
    }
    ME  = NME * 1.0 / length(ob)
    MFB = MFB * 200 / length(ob)
    MFE = MFE * 200 / length(ob)
    NME = NME * 100 / sum(ob,na.rm = T)
    out <- cbind(MFB,MFE,NME,ME)
    return(as.data.frame(out))
  }
  DATA <- cbind(WRF = mo, observado = ob)
  DATA <- as.data.frame(DATA)
  ind  <- openair::modStats(DATA,mod = "WRF",
                            obs = "observado",
                            statistic = c("n", "FAC2","MB","RMSE", "r","NMB","IOA","MGE"))
  ind$NMB <- ind$NMB * 100 # to transform in %
  ind     <- cbind(ind,MFBE(DATA$WRF,DATA$observado))

  MBME <- function(mo,ob){
    MB <- 0.0
    ME <- 0.0
    for(i in 1:length(ob)){
      if(mo[i] - ob[i] != 0){
        if(abs(mo[i] - ob[i]) > 180){
          a = abs(1 - (360 / abs(mo[i] - ob[i]) ) )
        }else{
          a = 1
        }
        MB = MB +    (mo[i] - ob[i]) * a
        ME = ME + abs(mo[i] - ob[i]) * a
      }
    }
    MB  =  MB / length(ob)
    ME  =  ME / length(ob)
    out <- cbind(MB,ME)
    return(as.data.frame(out))
  }

  if(wd){
    cat('using Mughal et al. (2017) for MB and ME for wind direction\n')
    ws_stats <- MBME(DATA$WRF,DATA$observado)
    ind$MB   =  ws_stats$MB
    ind$ME   =  ws_stats$ME
  }

  if(is.na(lim)){
    limites <- range(c(mo,ob),na.rm = T)
  }else{
    limites <- lim
  }
  if(scatter){
    plot(ob,mo,xlim=c(0,limites[2]),ylim=c(0,limites[2]),col = cor,
         pch=20,ylab="",xlab="",cex=1.8, ...)
    mtext("MODEL",2,2.5); mtext("OBS",1,2.5)
    abline(a = 0, b = 1  ,lty = 2, col = "#00000088")
    abline(a = 0, b = 0.5,lty = 2, col = "#00000055")
    abline(a = 0, b = 2.0,lty = 2, col = "#00000055")
    polygon(x = c(0,0,-50,-50,0), y = c(0,-50,-50,0,0) ,col = "white", border= "white")
    box()
    add = F
  }
  if(add){
    points(ob,mo,pch = 20, cex=1.8, col = cor, ... )
  }
  ind <- as.data.frame(cbind(n         = length(ob),
                             Obs       = mean(ob),
                             Sim       = mean(mo),
                             r         = ind$r,
                             IOA       = ind$IOA,
                             FA2       = ind$FAC2,
                             RMSE      = ind$RMSE,
                             MB        = ind$MB,
                             ME        = ind$ME ,
                             GE        = ind$MGE,
                             `MFB (%)` = ind$MFB,
                             `MFE (%)` = ind$MFE,
                             `NMB (%)` = ind$NMB,
                             `NME (%)` = ind$NME))
  return(ind)
}
