#' Calculate evaluation statistics from 2 vectors
#'
#' @description Calculate statistical indexes
#'
#' @param mo model
#' @param ob observed data
#' @param spinup numer of model points excluded to the statistic
#' @param scatter set TRUE to plot a scatter plot
#' @param add set TRUE to add the points to a scatter plot
#' @param cor color of scatterplot dots
#' @param lim scatter plot limits
#' @param cutoff minimum valid value for observation
#' @param verbose display additional information
#' @param ... extra arguments passed to scatter plot
#'
#' @importFrom openair modStats
#' @import graphics
#'
#' @export
#'
#' @examples
#' model <- 1:100
#' data  <- model + rnorm(100,0.2)
#' stats(mo = model, ob = data, scatter = TRUE)
#'

stats <- function(mo,ob,spinup = 0, scatter = F,add = F, cor="#FF000088",lim = NA,cutoff = 0, verbose = T, ...){

  if(spinup != 0){
    mo <- mo[(spinup+1):length(mo)]
    ob <- ob[(spinup+1):length(ob)]
  }
  if(length(mo) != length(ob))
    stop("mo and ob need to have the same length!") #nocov

  mo  <- mo[!is.na(ob)]
  ob  <- ob[!is.na(ob)]
  mo  <- mo[!is.na(mo)]
  ob  <- ob[!is.na(mo)]

  if(cutoff > 0 ){
    cat('using',cutoff,'for scutoff\n')

    mo  <- mo[ob >= cutoff]
    ob  <- ob[ob >= cutoff]
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
  ind <- cbind(ind,MFBE(DATA$WRF,DATA$observado))
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
