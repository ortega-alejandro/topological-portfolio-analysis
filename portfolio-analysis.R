# Load Data
library("TDA")
vasax.s = read.csv("data/VASAX_securities.csv")
vasax.s = transform(vasax.s, date = as.Date(as.character(date), "%Y%m%d"))
vtsax.s = read.csv("data/VTSAX_securities.csv")
vtsax.s = transform(vtsax.s, date = as.Date(as.character(date), "%Y%m%d"))
vhdyx.s = read.csv("data/VHDYX_securities.csv")
vhdyx.s = transform(vhdyx.s, date = as.Date(as.character(date), "%Y%m%d"))

# Function Defintions for Sliding Window Analysis

persistence = function(df, w, t, d, scale) {
  temp = sliding.window2(df, w, t)
  temp.data = prepData(temp)
  names(temp.data) = unique(temp.data$TICKER)
  temp.cor = cor(temp.data)
  Diag <- ripsDiag(X = temp.cor, maxdimension = d, maxscale = scale, library = "Dionysus", 
                   printProgress = FALSE, dist = "arbitrary")
  return(Diag)
}

compute.distance = function(df1, df2, w, d, s, t){
  X = data.frame()
  d1 = persistence(df1, w, t, d, s)
  d2 = persistence(df2, w, t, d, s)
  w = wasserstein(d1[["diagram"]], d2[["diagram"]], p = 1, dimension = d)
  #print(w)
  X = rbind(X, c(t, w))
  return(c(t, w))
}

compute.distance2 = function(df1, w, d, s, t){
  X = data.frame()
  d1 = persistence(df1, w, t, d, s)
  d2 = persistence(df1, w, t+1, d, s)
  w = wasserstein(d1[["diagram"]], d2[["diagram"]], p = 1, dimension = d)
  X = rbind(X, c(t, w))
  return(c(t, w))
}

gen.windows = function(df1, df2, w, d, s) {
  X = data.frame()
  for(num in 1:(60 - w)){
    wass = compute.distance(df1, df2, w, d, s, num)
    X = rbind(X, wass)
  }
  return(X)
}

gen.windows2 = function(df1, w, d, s) {
  X = data.frame()
  for(num in 1:(60 - w)){
    wass = compute.distance2(df1, w, d, s, num)
    X = rbind(X, wass)
  }
  return(X)
}

prepData = function(df){
  X = data.frame()
  for (ticker in unique(df$TICKER)){
    r = df[df$TICKER == ticker,]
    r = r$RET
    r = data.frame(r)
    X = cbindPad(X,r)
  }
  return(X)
}

# Cbindpad from a stackoverflow answer by Kevin Ushey; link: https://stackoverflow.com/questions/14899306/transform-a-splitted-data-frame-in-a-new-data-frame

cbindPad <- function(...){
  args <- list(...)
  n <- sapply(args,nrow)
  mx <- max(n)
  pad <- function(x, mx){
    if (nrow(x) < mx){
      nms <- colnames(x)
      padTemp <- matrix(NA, mx - nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if (ncol(x)==0) {
        return(padTemp)
      } else {
        return(rbind(x,padTemp))
      }
    }
    else{
      return(x)
    }
  }
  rs <- lapply(args,pad,mx)
  return(do.call(cbind,rs))
}

# Sliding window plot

X = gen.windows(vasax.s, vtsax.s, 5, 1, .5)

plot(X$X8.55360840981008 ~ (X$X1), type = "l", xlab = "Time (Window Step)", ylab = "Wasserstein Distance", main = "Wasserstein Distance Sliding Window, dimension 1", col="black")
