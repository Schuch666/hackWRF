#' Calculate evaluation statistics from 2 vectors
#'
#' @description Calculate statistical indexes
#'
#' @param mo model
#' @param ob observed data
#' @param spinup number of model points excluded to the statistic
#' @param wd logical, true for wind direction, see notes
#' @param Mughal logical, to use Mughal et al. (2017) for MB and ME for wind direction
#' @param scatter set TRUE to plot a scatter plot
#' @param add set TRUE to add the points to a scatter plot
#' @param cor color of scatterplot dots
#' @param lim scatter plot limits
#' @param cutoff (optionally the maximum) valid value for observation
#' @param cutoff_NME (optionally the maximum) valid value for observation for NME
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

stats <- function(mo,ob,spinup = 0, wd = FALSE, Mughal = FALSE,scatter = F,add = F,
                  cor="#FF000088",lim = NA,cutoff = NA, cutoff_NME = NA, nobs = 8,
                  verbose = T, ...){

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

  MFBE_cutoff <- function(mo,ob,nobs,cutoff = cutoff_NME){
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
    ME  = NME * 1.0 / length(ob)
    MFB = MFB * 200 / length(ob)
    MFE = MFE * 200 / length(ob)

    if(!is.na(cutoff[1])){
      cat('using',cutoff[1],'for min NME cutoff\n')

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
      cat('using',cutoff[2],'for max NME cutoff\n')

      mo  <- mo[ob < cutoff[2]]
      ob  <- ob[ob < cutoff[2]]

      cat(length(mo),'values left\n')
      if(length(mo) < nobs){
        RESULT <- stats((1:199)/100,(1:199)/100)
        RESULT$n = 0
        return(RESULT)
      }
    }

    NME <- 0.0
    for(i in 1:length(ob)){
      if(mo[i] - ob[i] != 0){
        NME = NME + abs(mo[i] - ob[i])
      }
    }
    NME = NME * 100 / sum(ob,na.rm = T)
    out <- cbind(MFB,MFE,NME,ME)
    return(as.data.frame(out))
  }

  wind_direction <- function(obs,mod, verbose = F){
    for(i in 1:length(mod)){
      diff_value <- mod[i] - obs[i]
      if(abs(diff_value)>180){
        temp_value <- 360 - abs(diff_value)
        if(diff_value < 0){
          previous <- mod[i]
          mod[i] <- obs[i] + temp_value
          if(verbose)
            cat(paste("model WD was changed from",previous,"to", mod[i],'\n'))
        } else {
          previous <- mod[i]
          mod[i] <- obs[i] - temp_value
          if(verbose)
            cat(paste("model WD was changed from",previous,"to", mod[i],'\n'))
        }
      }
    }
    return(mod)
  }

  IOA <- function(sim,obs){
    Om          <- mean(obs, na.rm=TRUE)
    denominator <- sum( ( abs(sim - Om) + abs(obs - Om)  )^2 )

    if (denominator != 0) {
      d <- 1 - ( sum( (obs - sim)^2 ) / denominator )
    } else {
      d <- NA
      warning("'sum((abs(sim-Om)+abs(obs-Om))^2)=0', it is not possible to compute 'IoA'")
    }
    return(d)
  }

  if(spinup != 0){
    mo <- mo[(spinup+1):length(mo)]
    ob <- ob[(spinup+1):length(ob)]
  }
  if(length(mo) != length(ob))
    stop("mo and ob need to have the same length!") # nocov

  NA_mod <- is.na(mo)
  NA_obs <- is.na(ob)

  mo  <- mo[!NA_obs & !NA_mod]
  ob  <- ob[!NA_obs & !NA_mod]

  if(!is.na(cutoff[1])){
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

  DATA <- cbind(WRF = mo, observado = ob)
  DATA <- as.data.frame(DATA)
  if(wd){
    DATA$WRF <- wind_direction(obs = DATA$observado, mod = DATA$WRF)
  }
  ind  <- openair::modStats(DATA,
                            mod = "WRF",
                            obs = "observado",
                            statistic = c("n", "FAC2","MB","RMSE", "r","NMB","IOA","MGE")) #"NMGE"
  # "NMGE" == NME
  ind$NMB <- ind$NMB * 100 # to transform in %
  if(is.na(cutoff_NME)){
    ind     <- cbind(ind,MFBE(DATA$WRF,DATA$observado))
  }else{
    ind     <- cbind(ind,MFBE_cutoff(DATA$WRF,DATA$observado,nobs))
  }
  # to calculate the new index of aggrement
  ind$IOA <- IOA(DATA$WRF,DATA$observado)

  if(Mughal){
    cat('using Mughal et al. (2017) for MB and ME for wind direction\n')

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
  ind <- as.data.frame(cbind(n         = length(DATA$WRF),
                             Obs       = mean(DATA$observado),
                             Sim       = mean(DATA$WRF),
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
