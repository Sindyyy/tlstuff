

library(plotly)
library(quantmod)
require(reshape)
require(plyr)
require(tidyr)
require(dplyr)
library(openxlsx)
library(Rblpapi)
blpConnect()
# find all trendlines cbind(x1, x2)

# Filter out the trendlines using these conditions
# 1. filter out interbreaks
# 2. filter out near points; x2-x1 >= width
# 3. filter out area
# 4. filter out duplicated lines with same x2; the one with the most number of touches -> return one trendline for each x2

# Analyze the effectiveness of the trendline
# 1. Extend the trendline to x3 = min( x2 + 1.5*(x2-x1), x_limit)
# 2. is.break between(x2, x3)
# 3. number of touches in between x1, x2

# Backtesting (given a list of trendlines with unique end points)
# 1. Given a trendline x1~x3, buypoint = the time when price near resistance trendline; sellpoint = time when price near support
# 2. close when price hit tp or sl
# 3. return = log(close.price - open.price)

tickers = c(
  "USDJPY BGN CURNCY",
  "EURUSD BGN CURNCY",
  "GBPUSD BGN CURNCY",
  "AUDUSD BGN CURNCY",
  "AUDNZD BGN CURNCY",
  "NZDUSD BGN CURNCY",
  "USDCAD BGN CURNCY",
  "USDCHF BGN CURNCY",
  "XAUUSD BGN CURNCY",
  "CCN+1M CURNCY",
  "USDCNH BGN CURNCY",
  "KWN+1M CURNCY",
  "NTN+1M CURNCY",
  "IRN+1M CURNCY",
  "IHN+1M CURNCY",
  "PPN+1M CURNCY",
  "USDTHB BGN CURNCY",
  "USDSGD BGN CURNCY",
  "USDTWD BGN CURNCY",
  "USDKRW BGN CURNCY",
  "USDPHP BGN CURNCY",
  "AUDJPY BGN CURNCY",
  "AUDCAD BGN CURNCY",
  "AUDCHF BGN CURNCY",
  "EURJPY BGN CURNCY",
  "EURGBP BGN CURNCY",
  "EURAUD BGN CURNCY",
  "EURNZD BGN CURNCY",
  "EURCAD BGN CURNCY",
  "GBPJPY BGN CURNCY",
  "GBPAUD BGN CURNCY",
  "GBPNZD BGN CURNCY",
  "GBPCAD BGN CURNCY",
  "GBPCHF BGN CURNCY",
  "NZDJPY BGN CURNCY",
  "NZDCAD BGN CURNCY",
  "NZDCHF BGN CURNCY",
  "CADJPY BGN CURNCY",
  "CADCHF BGN CURNCY",
  "JPYKRW BGN CURNCY",
  "SGDMYR BGN CURNCY",
  "SGDTWD BGN CURNCY",
  "MYRJPY BGN CURNCY"
)
tickers = c(
  "USDJPY BGN Curncy",
  "EURUSD BGN Curncy",
  "GBPUSD BGN Curncy",
  "AUDUSD BGN Curncy",
  "NZDUSD BGN Curncy",
  "USDCAD BGN Curncy",
  "USDCHF BGN Curncy",
  "XAUUSD BGN Curncy",
  "DXY Curncy",
  "USDOLLAR Index",
  "USDCNH BGN Curncy",
  "KWN+1M BGN Curncy",
  "NTN+1M BGN Curncy",
  "IRN+1M BGN Curncy",
  "IHN+1M BGN Curncy",
  "PPN+1M BGN Curncy",
  "MRN+1M BGN Curncy",
  "USDTHB BGN Curncy",
  "USDSGD BGN Curncy",
  "JPYKRW BGN Curncy",
  "ADXY Index",
  "EURJPY BGN Curncy",
  "GBPJPY BGN Curncy",
  "AUDJPY BGN Curncy",
  "NZDJPY BGN Curncy",
  "CADJPY BGN Curncy",
  "EURGBP BGN Curncy",
  "EURAUD BGN Curncy",
  "EURNZD BGN Curncy",
  "EURCAD BGN Curncy",
  "GBPAUD BGN Curncy",
  "GBPNZD BGN Curncy",
  "GBPCAD BGN Curncy",
  "AUDNZD BGN Curncy",
  "AUDCAD BGN Curncy",
  "AUDSGD BGN Curncy",
  "NZDCAD BGN Curncy",
  "USGG2YR Index",
  "USGG10YR Index",
  "USYC2Y10 Index",
  "EDZ9 Comdty",
  "EDZ7EDZ9 Comdty",
  "ES1 Index",
  "NH1 Index",
  "KOSPI Index",
  "TWSE Index",
  "SHCOMP Index",
  "HSCEI Index",
  "HSI Index",
  "GDBR10 Index"
)
#----------------------Data Retrieval------------------------------------

tsho <- function(tkrs) {
  for (i in 1:length(tkrs)) {
    tkrs[i] <- substr(tkrs[i], 1, 6)
  }
  return(tkrs)
}

read.tickdata <- function(curncy,
                          len = 288,
                          interval = 5) {
  #return xts format: index = time ticks, cbind[open, high, low, close]
  #curncy=tickers[6] ; interval=15; len = 288
  shortname <- tsho(curncy)
  filename = paste0(shortname, " Intraday")
  path <- paste0("U:/Intraday Data/", filename, ".csv")
  database <- as.xts(read.zoo(
    path,
    sep = ',',
    tz = "",
    header = T,
    format = '%Y-%m-%d %H:%M:%S'
  ))
  
  dat = tail(database[database$time == interval, 1:4], len)
  colnames(dat) = c("Open", "High", "Low", "Close")
  return(dat)
}
#

## Retrieve daily data

get.live.tickData <-
  function(ticker,
           interval = 5,
           days = NULL,
           start = "2017-5-25 12:00",
           end = Sys.time()) {
    # ticker = curncy
    # interval = 5 # tick interval
    # d = 2 # number of days
    if (is.null(days)) {
      start = start #strptime(start, "%Y-%m-%d %H:%M")
      end = end
      # strptime(end, "%Y-%m-%d %H:%M")
    }
    else{
      start =  round(Sys.time(), units = "hours") - 60 * 60 * 24 * days
      end =  round(Sys.time(), units = "hours")
    }
    data <-
      getBars(
        ticker,
        eventType = "TRADE",
        barInterval = interval,
        startTime = start,
        endTime = end,
        returnAs = getOption("blpType", "matrix")
      )
    data = data[, 1:5]
    colnames(data) = c("Date", "Open", "High", "Low", "Close")
    data = as.xts(data[, 2:5], order.by = data$Date)
    
    return(data)
  }

pull.ohlc = function(idx,
                     start = NULL,
                     end = NULL,
                     tf = "DAILY",
                     period = NULL) {
  if (is.null(period)) {
    a <- start
    b <- iif(is.null(end), Sys.Date(), end)
  } else {
    b = Sys.Date()
    #period = 100
    a = b - period
  }
  overrides.px <- structure(tf, names = c("periodicitySelection"))
  
  data = bdh(
    securities = idx,
    fields = c("OPEN", "HIGH", "LOW", "PX_LAST"),
    start.date = a,
    end.date = b,
    options = overrides.px
  )
  
  if (length(idx) == 1) {
    colnames(data) = c("date", "Open", "High", "Low", "Close")
    data = as.xts(data[, 2:5], order.by = data$date)
    data
  } else {
    for (i in 1:length(idx)) {
      temp = data[[i]]
      colnames(temp) = c("date", "Open", "High", "Low", "Close")
      temp = as.xts(temp[, 2:5], order.by = temp$date)
      data[[i]] = temp
    }
    data
  }
  
}


#------------------ Helper functions for lines ---------------------------
# Given 2 points, return the intercept and slope

find.grad.constant = function(x) {
  x1 = x[1]
  y1 = x[2]
  x2 = x[3]
  y2 = x[4]
  m = (y2 - y1) / (x2 - x1)
  constant = y2 - m * x2
  return(c(m, constant))
}
ln <- function(x, m, c) {
  if (m != 0)
    m * x + c
  else
    rep(c, length(x))
}#Y=mX+c

iif <-
  function(statement, true.ret, false.ret) {
    if (statement) {
      return(true.ret)
    } else{
      return (false.ret)
    }
  }

extend.timeperiod <- function(data, extend.length = 60,is.weekly=F) {
  require(timeDate)
  # return x with extended rows filled with NA
  #data = tick.dat
  Date = index(data)
  time.interval = min(Date[2:11] - Date[1:10])
  endtime = tail(Date, 1)
  new.index = seq(
    from = endtime + time.interval,
    by = time.interval,
    length.out = extend.length * 10
  )
  if (is.weekly) trading.day.index = new.index[1:extend.length]
  else trading.day.index = new.index[isBizday(as.timeDate(new.index))][1:extend.length]
  extended.data = tail(data, extend.length)
  index(extended.data) = trading.day.index
  coredata(extended.data) = as.matrix(rep(NA, 4 * extend.length, nrow = extend.length, ncol =
                                            4))
  
  data = rbind(data, extended.data)
  Date = as.POSIXlt(index(data))
  df = data.frame(cbind(Date, as.data.frame(data)), row.names = NULL)
  return(df)
  
}

find_peaks <- function(x,
                       m = 2,
                       pip.thresh = 0,
                       interpeak.thres = 3) {
  # x: series to find the peaks
  # m: neighborhood. larger m -> less peak. Each peak i, x[i+1] is the largest in the neighborhood of radius=m
  # pip.thresh: filter out the peaks that are too similar to the previous peak. larger thres -> less peaks
  # return peak indices
  # x= tick.dat[,2]
  # m = 1
  # pip.thresh=0
  # interpeak.thres=1
  shape <-
    diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) # find the general peaks
  pks <-
    sapply(
      which(shape < 0),
      FUN = function(i) {
        # get peaks that is local maxima in the neighborhood of m
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        
        if (all(x[c(z:i, (i + 2):w)] <= x[min(i + 1, length(x))]))
          return(i + 1)
        else
          return(numeric(0))
      }
    )
  pks <- sort(unlist(pks))
  if(length(pks)==1) return(pks)
  rm.idx = c()
  for (i in c(1:(length(pks) - 1))) {

    if (abs(pks[i + 1] - pks[i]) <= interpeak.thres)
      rm.idx = c(rm.idx, i) # for consecutive peaks with same value, return the latest peak
  }
  if (length(rm.idx) != 0)
    pks = pks[-rm.idx]
  
  
  if (pip.thresh != 0) {
    if (sign(pip.thresh) < 0)
      pip.thresh <- -pip.thresh
    return(pks[(abs((x[pks] / coredata(x[pks + 1])) - 1) * 100)  > pip.thresh]) # filter out the peaks that are too similar to the previous peak.
  }
  else
    return(pks)
}



npivotpoint <-
  function(data,
           n = 1,
           pip.swing = 0,
           useHighLow = T,
           interpeak.thres = 1)
    
  {
    # data is df Open High Low Close
    # n=#bars on each side of pivot (peaks are local maxima in the radius of n). default=1: highest for 3 days on either side
    # useHighLow: if true, use high low, else use open, close
    # pip.swing: filter out the peaks that are too similar to the previous peak. larger thres -> less peaks
    require(quantmod)
    
    if (ncol(data) > 4) {
      colnames(data) = c("Open", "High", "Low", "Close", colnames(data)[5:ncol(data)])
    }
    
    else{
      colnames(data) = c("Open", "High", "Low", "Close")
    }
    
    
    if (is.xts(data)) {
      data = as.data.frame(as.matrix(data[, 1:4]))
      row.names(data) <- index(data)
    }
    else {
      row.names(data) <- index(data)
    }
    
    if (useHighLow)
    {
      h = data$High
      l = data$Low
    } else {
      h = apply(cbind(data$Open, data$Close), 1, max)
      h = t(t(h)) # m*1 matrix
      l = apply(cbind(data$Open, data$Close), 1, min)
      l = t(t(l))
    }
    
    
    p = find_peaks(x = h,
                   m = n,
                   interpeak.thres = interpeak.thres)
    #p=p[which(p<=.max)]
    t = find_peaks(x = -l,
                   m = n,
                   interpeak.thres = interpeak.thres) # find minimal with -x
    
    #t=t[which(t<=.max)]
    highs = cbind(p, h[p])
    lows = cbind(t, l[t])
    return(list(pivot.high = highs, pivot.low = lows))
  }

is.interbreak <-
  function(line,
           data,
           tol,
           type,
           start = "x1",
           end = "x2",
           useHighLow) {
    x1 = as.numeric(line[start])
    x2 = as.numeric(line[end])
    
    if (abs(x1 - x2) <= 1) {
      return(NA)
    }
    
    x = c(x1:x2)
    if (useHighLow) {
      highs = data[x, 2]
      lows = data[x, 3]
    }
    else {
      highs = apply(data[x, c(1, 4)], 1, max)
      lows = apply(data[x, c(1, 4)], 1, min)
    }
    lin = ln(x, m = as.numeric(line['m']), c = as.numeric(line['constant']))
    if (type == "resistance") {
      deviate = highs - lin
      
    }
    else {
      deviate = lin - lows
    }
    is.break = any(deviate > tol)
    return(is.break)
    
  }

find.dup.pivots = function(result, col) {
  if (is.na(nrow(result))) {
    return(NA)
  }
  # return index of rows with duplicated values in result$col
  # eg: result[unlist(find.dup.pivots, "x2)] -> rows with duplicated x2's
  #result = filteredout.breaks.lines
  #col="x2"
  a = result[, col]
  ans = list()
  temp = c()
  checker = c(0)
  for (i in 1:(length(a) - 1)) {
    if (!i %in% checker) {
      p = a[i]
      for (j in (i + 1):length(a)) {
        q = a[j]
        
        if (!is.na(p) & !is.na(q))
          if (p == q)
            temp = c(temp, i, j) # index i, j have same value
      }
      temp = temp[!duplicated(temp)]
      ans = c(ans, list(temp))
      checker = c(checker, temp)
      temp = c()
    }
  }
  ans = ans[lapply(ans, is.null) == F]
  ans
}


find.similar.points = function(lines, col, tol = 0.001) {
  if (is.na(nrow(lines))) {
    return(NA)
  }
  # return index of rows with duplicated values in lines$col
  # eg: lines[unlist(find.dup.pivots, "x2)] -> rows with duplicated x2's
  #lines = filteredout.breaks.lines
  #col="x2"
  a = lines[, col]
  ans = list()
  temp = c()
  checker = c(0)
  for (i in 1:(length(a) - 1)) {
    if (!i %in% checker) {
      p = a[i]
      for (j in (i + 1):length(a)) {
        q = a[j]
        if (!is.na(p) & !is.na(q)) {
          if (abs(p - q) <= tol)
            temp = c(temp, i, j) # index i, j have same value
        }
      }
      temp = temp[!duplicated(temp)]
      ans = c(ans, list(temp))
      checker = c(checker, temp)
      temp = c()
    }
  }
  ans = ans[lapply(ans, is.null) == F]
  ans
}

count.touch <- function(line, pivots, tol) {
  x = pivots[, 1]
  y = pivots[, 2]
  y.line = x * line["m"] + line["constant"]
  return(sum(abs(y - y.line) <= tol))
}

rm.similar.lines <- function(lines, by.point = "x.now", tol) {
  ## for lines with the same end points, return the one with the moderate slope
  if (is.na(nrow(lines)))
    return(lines)
  groupby.endpoint = find.similar.points(lines, by.point, tol)
  
  if (length(groupby.endpoint) == 0)
    return(lines)
  filtered = lines[-unlist(groupby.endpoint), , drop = F]
  for (group in groupby.endpoint) {
    lines.group = lines[unlist(group), , drop = F]
    max.touch.indx = which(lines.group[, "num.touch"] == max(lines.group[, "num.touch"]))
    selected.line = lines.group[max.touch.indx,]
    filtered = rbind(filtered, selected.line)
  }
  
  rownames(filtered) = NULL
  return(filtered)
}

rm.dup.endlines <- function(lines, endpoint = "x2") {
  ## for lines with the same end points, return the one with the moderate slope
  if (is.na(nrow(lines)) | nrow(lines) == 1)
    return(lines)
  groupby.endpoint = find.dup.pivots(lines, endpoint)
  
  if (length(groupby.endpoint) == 0)
    return(lines)
  filtered = lines[-unlist(groupby.endpoint), ]
  for (group in groupby.endpoint) {
    lines.group = lines[unlist(group), , drop = F]
    
    # min.grad.index = which(abs(grad.list)==min(abs(grad.list)))
    max.touch.indx = which(lines.group[, "num.touch"] == max(lines.group[, "num.touch"]))
    # print(max.touch.indx)
    selected.line = lines.group[max.touch.indx, , drop = F]
    min.grad.index = which(abs(selected.line[, "m"]) == min(abs(selected.line[, "m"])))
    selected.line = selected.line[min.grad.index, , drop = F]
    
    filtered = rbind(filtered, selected.line)
  }
  
  rownames(filtered) = NULL
  return(filtered)
}

# ----------------------- Trendline properties -----------------
# Find all trendlines (x1, y1, x2, y2, intercept, slope), ( #touches, x3, y3 ) without interbreaks
find.all.trendlines <-
  function(data,
           extend.window.length,
           support.color = "red",
           resistance.color = "blue",
           extend.trend.multiplier = 10,
           days.pivots = 3,
           useHighLow = T,
           show.pivots = T,
           type = "resistance",
           break.thres = 0.1,
           break.thres.2 = 30 ,
           min.trend.length = 3,
           interpeak.thres = 5,
           touch.tol = 0.01,
           max.pip.from.now = 100,
           similar.tol = 1,
           start.shift = 0
           ) {
    #data = tick.dat
    days = nrow(data)
    
    # days.pivots=2
    # interpeak.thres=1
    pivots.list = npivotpoint(data,
                              n = days.pivots,
                              interpeak.thres = interpeak.thres,
                              useHighLow = useHighLow)
    
    if (show.pivots) {
      allowance = median(abs(data[, 1] - data[, 4])) # abs(open-close) for plotting(distance of the label above the peaks)
      piplabel = function(x) {
        return(substring(gsub('[.]', '', as.character(x)), 2, 5))
      } # return number after the decimal points
      if (type == "resistance") {
        points(
          pivots.list$pivot.high[, 1]-start.shift,
          pivots.list$pivot.high[, 2] + allowance,
          pch = 25,
          bg = resistance.color,
          col = resistance.color,
          cex = 0.7
        ) # Mark the peaks
        text(
          pivots.list$pivot.high[, 1]-start.shift,
          pivots.list$pivot.high[, 2] + 2 * allowance,
          labels = piplabel(pivots.list$pivot.high[, 2]),
          col = 'black',
          cex = 0.7,
          font = 2
        )
        
      }
      else{
        points(
          pivots.list$pivot.low[, 1]-start.shift,
          pivots.list$pivot.low[, 2] - allowance,
          pch = 24,
          bg = support.color,
          col = support.color,
          cex = 0.7
        ) #Mark the lows
        text(
          pivots.list$pivot.low[, 1]-start.shift,
          pivots.list$pivot.low[, 2] - 2 * allowance,
          labels = piplabel(pivots.list$pivot.low[, 2]),
          col = 'black',
          cex = 0.7,
          font = 2
        )
        
      }
    }
    # type = "resistance"
    
    if (type == "resistance") {
      pivots = pivots.list$pivot.high
    }
    else
      pivots = pivots.list$pivot.low
    if (all(is.na(pivots))) {
      print("No pivots found")
      return(NA)
    }
    
    ln = nrow(pivots) # number of pivots
    
    if (ln <= 1) {
      print("Less than two pivots found. Couldn't plot trendline")
      return(NA)
    }
    totalcounts = choose(ln, 2)
    result = matrix(rep(NA, 4 * totalcounts), ncol = 4, nrow = totalcounts)
    k = 1
    for (i in 1:ln){
      if (i == ln)
        break
      for (j in (i + 1):ln)
      {
        if (j == i + 1)
        {
          result[k, ] <- c(pivots[i, ], pivots[j, ])
          k = k + 1
        } else {
          result[k, ] <- c(pivots[i, ], pivots[j, ])
          k = k + 1
        }
      }
    }
    colnames(result) = c('x1', 'y1', 'x2', 'y2')
    #fill columns of m and constant
    
    result = cbind(result, length = abs(result[, "x1"] - result[, "x2"]))
    result = result[result[, "length"] >= min.sup.trend.length, , drop = F]
    
    print(paste0(
      "number of trendlines after deleting the near trend: ",
      nrow(result)
    ))
    if (nrow(result)==0){
      return(result)
    }
    if (is.null(nrow(result)) | nrow(result) == 1) {
      # incase of only one line
      t1 = find.grad.constant(result)
    }
    else t1 = apply(result, 1, find.grad.constant)
    new.colnames = c(colnames(result), 'm', 'constant')

    
    result = cbind(result, t(t1))
    colnames(result) <- new.colnames
    
    extend.band = break.thres / 2
    ylim = c(min(data[, "Low"]) - extend.band, max(data[, "High"]) + extend.band)
    l = result[, "x2"] - result[, "x1"]
    
    
    # filter out lines already broken
    result = cbind(result,
                   x.now = rep(days, nrow(result)),
                   y.now = days * result[, "m"] + result[, "constant"])
    
    x3 =  rep(days + extend.window.length, nrow(result))
    y3 = result[, "m"] * x3 + result[, "constant"] 
    for (i in c(1:length(y3))) {
      if (y3[i] > ylim[2]) {
        y3[i] = ylim[2]
        x3[i] = (y3[i] - result[i, "constant"]) / result[i, "m"]
      }
      else if (y3[i] < ylim[1]) {
        y3[i] = ylim[1]
        x3[i] = (y3[i] - result[i, "constant"]) / result[i, "m"]
      }
    }
    
    result = cbind(result, x3, y3)
    result = result[x3 >= (days + extend.window.length), , drop = F]
    
    require("plyr")
    
    interbreaks = unlist(
      alply(
        result,
        1,
        is.interbreak,
        data = data,
        start = "x1",
        end = "x2",
        tol = break.thres,
        type = type,
        useHighLow = useHighLow
      )
    )
    filteredout.breaks.lines = result[coredata(which(interbreaks == F)), , drop =
                                        F]
    print(paste0(
      "number of trendlines after deleting the trendlines with interbreaks: ",
      nrow(filteredout.breaks.lines)
    ))
    result = filteredout.breaks.lines
    
    interbreaks.2 = unlist(
      alply(
        result,
        1,
        is.interbreak,
        data = data,
        start = "x2",
        end = "x.now",
        tol = break.thres.2,
        type = type,
        useHighLow = useHighLow
      )
    )
    
    filteredout.breaks.lines = result[coredata(which(interbreaks.2 == F)), , drop =
                                        F]
    
    
    print(paste0(
      "number of trendlines after deleting the trendlines with interbreaks: ",
      nrow(filteredout.breaks.lines)
    ))
    result = filteredout.breaks.lines
    
    result = cbind(result, num.touch = unlist(apply(result, 1, FUN = count.touch, pivots, touch.tol)))
    # Filter such that each end point only have one trendline
    dist =   result[, "y.now"] - as.numeric(data[days, "Close"])
    result = cbind(result, dist.to.now = dist)
    if (is.null(nrow(result)) | nrow(result) <= 1) {
      print(result)
      
      return(result)
    }
    
    filterout.dup.end = rm.dup.endlines(result)
    
    print(
      paste0(
        "number of trendlines after deleting the trendlines with same end points: ",
        nrow(filterout.dup.end)
      )
    )
    result = filterout.dup.end
    
    
    
    filterout.offlines = result[abs(result[, "dist.to.now"]) <= max.pip.from.now, , drop =
                                  F]
    print(
      paste0(
        "number of trendlines after deleting (y.now-trendline.level)>max.pip: ",
        nrow(filterout.offlines)
      )
    )
    result = result[order(abs(result[, "dist.to.now"]))[1:min(nrow(result), 5)], , drop =
                      F]
    
    return(result)
    #filter similar lines
    
    #filter breaks x2 and x.now
    
  }
# -------------------------Plot functions-------------------
plot.candlestick <- function(df, title) {
  candlestick = plot_ly(
    df,
    type = "candlestick",
    x = df$Date ,
    open = ~ Open,
    high = ~ High,
    low = ~ Low,
    close = ~ Close,
    yaxis = "y",
    increasing = list(line = list(color = "#455D7A")),
    decreasing = list(line = list(color = "#F95959")),
    name = "Price",
    height = 600,
    width = 1024
  ) %>%
    
    layout(
      showlegend = F,
      
      yaxis = list(
        title = "Price",
        domain = c(0, 0.9),
        showgrid = T
      ),
      # xaxis = list(type = "category"),
      annotations = list(
        # title of the plot
        list(
          xref = "paper",
          yref = "paper",
          x = 0,
          y = 1,
          showarrow = F,
          xanchor = "left",
          yanchor = "top",
          align = "left",
          text = paste0("<b>", title, "</b>")
        ),
        # date range of the plot
        list(
          xref = "paper",
          yref = "paper",
          x = 0.75,
          y = 1,
          showarrow = F,
          xanchor = "left",
          yanchor = "top",
          align = "left",
          text = paste(range(df$Date), collapse = " : "),
          font = list(size = 8)
        )
      ),
      plot_bgcolor = "#f2f2f2"
    )
  
  return(candlestick)
}

add.lines <- function (base.plot,
            xaxis,
            start.shift=0,
            lines,
            line.name,
            line.color) {
    if (all(is.na(lines))) {
      print("No trendline to draw")
      return(base.plot)
    }
    max.val = max(lines[, c("y1", "y.now")])
    min.val = min(lines[, c("y1", "y.now")])
    ymax = (max.val - min.val) * 0.3 + max.val
    ymin = -(max.val - min.val) * 0.3 + min.val
    plt = base.plot
    for (i in 1:nrow(lines)) {
      line = lines[i, ]
      x = line["x1"]:floor(line["x3"])
      x.pts = xaxis[x]

      y = ln(x, line['m'], line['constant'])
      # 
      # x.pts = x.pts[(y <= ymax) & (y > ymin)]
      # y = y[(y <= ymax) & (y > ymin)]
# print(x)
# print(xaxis)
# print(x.pts)
      if (tail(x.pts, 1) >= line["x.now"]) {
        plt = plt %>% add_lines(
          x = x.pts-start.shift,
          y = y,
          line = list(width = 1, color = line.color),
          name = line.name,
          inherit = F
        )
      }
    }
    return(plt)
  }

draw.trendlines <- function(lines,
                            color,
                            start.shift=0,
                            lwd = 1,
                            lty = 1) {

    if (all(is.na(lines))) {
      print("No trendline to draw")
      return()
    }
    print(nrow(lines))
    if ( is.null(nrow(lines))){
      x0=lines["x1"]-start.shift
      y0=lines["y1"]
      x1 = lines["x3"] - start.shift
      y1 = lines["y3"]
    } else{
    lines = as.data.frame(lines)
    for (i in c(1:nrow(lines))) {
      line = lines[i, ]
      x0 = line$x1 - start.shift
      y0 = line$y1
      if (x0<=0) {
        y0= start.shift*line$m+line$constant
        x0= 1
      }
      segments(
        x0 = x0,
        y0 = y0,
        x1 = line$x3 - start.shift,
        y1 = line$y3,
        lwd = lwd,
        lty = lty,
        col = color
      )
    }
  }
}



# ------------------------ run -------------------------------

find.trendlines <-
  function(dat,
           curncy,
           extend.trend.multiplier = 10,
           extend.window.length = 60,
           days.pivots = 4,
           useHighLow = T,
           show.pivots = T,
           interpeak.thres = 5,
           break.thres = 30,
           break.thres.2 = 100 ,
           min.res.trend.length = 50,
           min.sup.trend.length = 30,
           touch.tol = 0.01,
           max.pip.from.now = 100,
           start.shift=0) {
    # extend.trend.multiplier=4
    # extend.window.length=60
    # days.pivots=4
    # useHighLow=T; show.pivots=T; interpeak.thres=5; break.thres.pip=0.0001; min.trend.length=50
    #break.thres: price difference value counted as a break
    print("Finding resistance lines")
    resistance = find.all.trendlines(
      dat,
      extend.trend.multiplier = extend.trend.multiplier,
      extend.window.length = extend.window.length,
      days.pivots = days.pivots,
      useHighLow = useHighLow,
      show.pivots = show.pivots,
      interpeak.thres = interpeak.thres,
      type = "resistance",
      break.thres = break.thres,
      break.thres.2 = break.thres.2,
      min.trend.length = min.res.trend.length,
      touch.tol = touch.tol,
      max.pip.from.now = max.pip.from.now,
      start.shift=start.shift
    )
    
    print("Finding support lines")
    support = find.all.trendlines(
      dat,
      extend.window.length = extend.window.length,
      extend.trend.multiplier = extend.trend.multiplier,
      days.pivots = days.pivots,
      useHighLow = useHighLow,
      show.pivots = show.pivots,
      interpeak.thres = interpeak.thres,
      type = "support",
      break.thres = break.thres,
      break.thres.2 = break.thres.2,
      min.trend.length = min.sup.trend.length,
      touch.tol = touch.tol ,
      max.pip.from.now = max.pip.from.now,
      start.shift=start.shift
    )
    
    return(list(support = support, resistance = resistance))
  }

plot.chart = function(df, name = "Time Series")
{
  myPars <- chart_pars()
  #myPars$mar <- c(3, 2, 0, .2) # default is c(3, 1, 0, 1)  # bottom, left, top, right
  myPars$cex <-
    0.8 #' Increase font size of both x and y axis scale ticks
  mychartTheme <- chart_theme()
  mychartTheme$rylab = FALSE  #' Don't show y-axis on right side of plot to save space
  mychartTheme$format.labels = '%b %d'
  mychartTheme$col$dn.col = 'black'
  mychartTheme$col$up.col = 'white'
  mychartTheme$col$bg.col = "white"
  x = xts(df[,-1], order.by = as.POSIXct(df$Date))
  chart_Series(
    x,
    type = "candlesticks",
    pars = myPars,
    name = name,
    theme =  mychartTheme
  )
}
