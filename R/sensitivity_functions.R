calculateFacets = function(indicators, Fmult, smooth=TRUE, dF=0.01) {
  UseMethod("calculateFacets")
}

calculateFacets.default = function(indicators, Fmult, smooth=TRUE, dF=0.01) {
  
  input = list(indicators=indicators, Fmult=Fmult)
  
  if(all(is.na(indicators))) {
    return(list(output=rep(NA, 11), input=input))
  }

  smoothInd = splinefun(x = Fmult, y=indicators)
  
  if(isTRUE(smooth)) {  
    Fmult = seq(from=min(Fmult, na.rm=TRUE), to=max(Fmult, na.rm=TRUE), by=dF)
    indicators = smoothInd(Fmult)
  }
  
#   indicators = (indicators - indicators[1])/sd(indicators, na.rm=TRUE)
  
  Dind = diff(indicators)/diff(Fmult)
  
  # Facets
  # 1) degree of response
  #--------------------------
  # maximum value of derivative
  
  mvd = max(Dind)
  
  # area under the absolute value of the curve
  
  Fmids = seq(from=min(Fmult, na.rm=TRUE), to=max(Fmult, na.rm=TRUE), by=0.005)
  yValue = smoothInd(0.5*(head(Fmult, -1) + tail(Fmult, -1)))
  auc   = sum(abs(yValue)*diff(Fmids), na.rm=TRUE)
#   auc = integrate(smoothInd)
  # mean rate of change
  
  mrc = mean(Dind, na.rm=TRUE)
  
  # 2) shape of response
  #--------------------------
  
  spearman = cor(Fmult, indicators, method = "spearman", use="complete")
  pearson  = cor(Fmult, indicators, method = "pearson", use="complete")
  kendall  = cor(Fmult, indicators, method = "kendall", use="complete")
  
  # absolute value Spearman correlation
  avs = abs(spearman)
  # absolute value of pearson correlation
  avp = abs(pearson)
  # absolute value of kendall's tau
  avk = abs(kendall)
  # sd of derivative
  sdd = sd(Dind, na.rm=TRUE)
  
  # 3) direction of change
  #--------------------------
  # sign of spearman correlation and of pearson r
  spc = sign(pearson)
  ssc = sign(spearman)
  skc = sign(kendall)
  # sum/average of signs of derivatives
  
  mds = mean(sign(Dind), na.rm=TRUE)
  
  output = c(mvd=mvd, auc=auc, mrc=mrc, avs=avs, avp=avp, avk=avk, sdd=sdd, 
             spc=spc, ssc=ssc, skc=skc, mds=mds)
  
  input  = list(indicators=indicators, Fmult=Fmult)
  
  return(list(output=output, input=input))
  
}

calculateFacets.data.frame = function(indicator, Fmult, smooth=TRUE, dF=0.01) {

  ind = c("ecosystem", "fishingStrategy", "value", "indicator") %in% names(indicator)
  if(!all(ind)) stop("data frame does not match sensitivity data frame variables.")
  
  indValue = apply(tapply(indicators$value, 
                          INDEX = list(DF$ecosystem, DF$fishingStrategy, DF$indicator), 
                          FUN=identity), 1:3, FUN = unlist)
  
  Fmult    = unique(indicators$Fmult) # to check
  
  facets = apply(indValue, 2:4, .calculateFacets, Fmult=Fmult)
  
  rownames(facets) = c("mvd", "auc", "mrc", "avs", "avp", "avk", "sdd", 
                       "spc", "ssc", "skc", "mds")
  
  return(facets)
  
}

.calculateFacets = function(indicator, Fmult, smooth=TRUE, dF=0.01) {
  
  x = calculateFacets(indicator=indicator, Fmult=Fmult, smooth=smooth, dF=dF)
  return(x$output)
  
}
