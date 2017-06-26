
library("grid")
library("gridExtra")
library(lubridate)
source("trendline_function_draft.r")
# Processing time:
# 5Y_daily: 30 min
# 3Y_daily: 10 min
# 1Y_daily: 5 min
# 3Y_weekly: 3 min
# 5Y_weekly: 6 min
# 10Y_weekly: 30 min
# 10D_hourly: 5 min
# 30D_hourly: 20 min

# send report: daily 5Y & 1Y


blgformat <-function(x) {
  options(scipen = 999)
  # function to format numbers according to bloomberg convention
  if (is.na(x)) return(x)
  if (x<1) return(format(x, digits = 4, nsmall = 4, big.mark=","))
  if(x<10) return(format(x,nsmall = 4,big.mark=","))
  if(x<100) return(format(x, nsmall= 3,big.mark=","))
  return(format(x, nsmall=2,big.mark=","))
}

generate.trendline.plot <-
  function(dat,# dat:xts format, cols : OHLC
           curncy, # ticker to generate trendlines
           min.sup.trend.length = 60, # minimum length of support trendline (length = x2- x1)
           min.res.trend.length = 90, # minimum length of resistance
           alert.thres.pip = 60, # alert threshold in pip
           break.thres.pip = 30, # threshold for breaks in [x1,x2]
           break.thres.pip.2 = 30, # # threshold for breaks in [x2,x.now]
           extend.window.length = 60, # Empty period to extend the trendlines
           # extend.trend.multiplier = 10, 
           days.pivots = 2,# defines the neignborhood of local minimum/maximum: peaks are the maximum in the neighborhood of 2*days.pivots+1
           show.pivots = T, # whether show pivots on pdf
           useHighLow = T, # method to find peaks and account for peaks/touches
           interpeak.thres = 4,# minimum distance between peaks
           gen.pdf = T,
           support.color = "red",
           resistance.color = "blue",
           touch.tol.pip = 0.01, 
           max.pip.from.now = 800, # filterout trendlines with (current level-current close price）> max.pip.from.now
           start.shift=0 # start time(row number) to plot out the candlestick and trendline. default set to 0.(useful when want to show part of graph)  
           ) {
    
    
    # Generate candlestick with support and resistance trendlines
    
    # Return: 
    #         graph: plotly object-- candlestick with support and resisitance
    #         res: table with each row representing resistance line
    #         sup: table with each row representing support line
    #         chosen.sup: support line with current level within the alert threshold of current close price
    #         chosen.res: resistance line with current level within the alert threshold of current close price
    # 
    
    dates = index(dat)
    close = as.numeric(dat[, "Close"])
    high = as.numeric(dat[, "High"])
    low = as.numeric(dat[, "Low"])
    open = as.numeric(dat[, "Open"])
    if (mean(dat, na.rm = T) <= 75) {pip = 0.0001} else {pip = 0.01}
    alert.thres = pip * alert.thres.pip
    break.thres = pip * break.thres.pip
    break.thres.2 = pip * break.thres.pip.2
    touch.tol = touch.tol.pip * pip
    
    max.pip.from.now = max.pip.from.now * pip
    df = extend.timeperiod(dat, extend.window.length,is.weekly = (ticksize == "Weekly"))
    
    extended.dates = df$Date
    if (gen.pdf) {
      print(plot.chart(df, curncy))
    }
    # Find all trendlines
    lines = find.trendlines(
      dat,
      curncy,
      extend.window.length = extend.window.length,
     # extend.trend.multiplier = extend.trend.multiplier,
      days.pivots = days.pivots,
      useHighLow = useHighLow,
      show.pivots = show.pivots,
      interpeak.thres = interpeak.thres,
      break.thres = break.thres,
      break.thres.2 = break.thres.2,
      min.sup.trend.length = min.sup.trend.length,
      min.res.trend.length = min.res.trend.length,
      max.pip.from.now = max.pip.from.now,
      start.shift=start.shift
    )
    #
    resistance = lines$resistance
    support = lines$support
    print(resistance)
    if (all(is.na(resistance))){
      chosen.res = NA
    }else{
      chosen.res = resistance[order(abs(resistance[, "dist.to.now"]))[1], ]
    }
    if (all(is.na(support))){
      chosen.sup = NA
    }else{

      chosen.sup = support[order(abs(support[, "dist.to.now"]))[1], ]
    }

    # ------------------------ Generate chart-----------------
    
    # -------------------------for pdf----------------------
    g = plot.candlestick(df[(start.shift+1):nrow(dat),], title = curncy) # return a plotly candlestick
    g.addresis = add.lines(
      base.plot = g,
      xaxis = extended.dates,
      lines = resistance,
      line.name = "resistance",
      line.color = "blue",
      start.shift = start.shift
    )
    g.addsupport = add.lines(
      base.plot = g.addresis,
      xaxis = extended.dates,
      lines = support,
      line.name = "support",
      line.color = "green",
      start.shift = start.shift
    )
    graph = g.addsupport
    if (gen.pdf) {
      draw.trendlines(resistance, color = resistance.color, start.shift = start.shift)
      draw.trendlines(support , color = support.color, start.shift = start.shift)
      if (!all(is.na(chosen.sup))) {
        if (chosen.sup["dist.to.now"] < alert.thres)
          draw.trendlines(chosen.sup,
                          color = support.color,
                          start.shift = start.shift,
                          lwd = 3,
                          lty = "dashed")
        
      }
      if (!all(is.na(chosen.res))) {
        if (chosen.res["dist.to.now"] < alert.thres)
          draw.trendlines(chosen.res,
                          color = resistance.color,
                          start.shift = start.shift,
                          lwd = 3,
                          lty = "dashed")
        
      }
      
    }
    #
    return(
      list(
        graph = graph,
        res = resistance,
        sup = support,
        chosen.sup = chosen.sup,
        chosen.res = chosen.res
      )
    )
  }

gen_chart_table<-function(ticksize, start.date, duration,tickers,generate.table=T ,days=NULL){
  # ticksize = c("Daily", "Weekly", "Hourly")
  # start.date: startdate to read data(used for non-tick data)
  # duration: duration of the data (Used only as a name for pdf)
  # tickers: tickers to be included in the pdf report and shiny app
  # generate.table: whether to generate summary table
  # days: the number of days to retrieve hourly data but in general it is the number 
  # in front of the the date units in duration. eg: if duration = "10Y", days = 10
  
  # Return:
  #       generate pdf with all trendline graphs
  #       generate pdf with summary table ( formated according to blg convention )
  #       table: summary table
  #       plotly.chart: list of plotly trendline graphs
  plotly.chart = list()
  
  result.df = matrix(NA, length(tickers), 5) # summary table
  row.names(result.df) = tickers
  colnames(result.df) = c("Resistance",
                          "Current" ,
                          "Support",
                          "Dist to Resist.(pip)",
                          "Dist to Support(pip)")
  pdf(paste0(duration,"_", ticksize,"_trendlines.pdf"), width = 21, height = 13) # generate pdf for trendlines
  
  for (curncy in tickers) {
    
    message(paste0("Generating ", curncy, "___", duration,"___",ticksize,"\n Progress: ", which(curncy==tickers),"/", length(tickers)))
    if (toupper(ticksize) %in% c("WEEKLY", "DAILY")) {
      dat = pull.ohlc(curncy,  start = start.date , tf=toupper(ticksize) , end = Sys.Date())
    }else if(ticksize == "Hourly") {
      dat = get.live.tickData(ticker = curncy, interval = 60, days=days)
    }
   
    dat = na.omit(dat) # dat is in xts format with OHLC columns
   
    # set up parameters for trendline filtering
    min.sup.trend.length = floor( nrow(dat) / 12)
    if (mean(dat,rm.na=T)>=75) {pip = 0.01} else {pip=0.0001}
    if ( (ticksize=="Weekly" & days <= 1 )| (ticksize=="Hourly" & days<=15 )){
      days.pivots = 1
    }else if((ticksize=="Daily" & days== 1) ){
      days.pivots = 2
    } else if(ticksize=="Daily" & days >= 2|  (ticksize=="Hourly" & days>=20 ) ) {
      days.pivots=10
    }else {days.pivots = 5}
    if (ticksize=="Hourly"){
      break.thres.pip = 0.01*diff(range(dat))/pip
      break.thres.pip.2 = 0.01 * diff(range(dat)) /pip
    }else {
      break.thres.pip = 50
      break.thres.pip.2 = 60
    }

    extend.window.length = floor(nrow(dat) / 5)
    result = generate.trendline.plot(
      dat,
      curncy = curncy,
      min.sup.trend.length = min.sup.trend.length,
      min.res.trend.length = min.sup.trend.length,
      alert.thres.pip = 30,
      break.thres.pip = break.thres.pip,
      break.thres.pip.2 = break.thres.pip.2,
      extend.window.length = extend.window.length,
     # extend.trend.multiplier = 10,
      days.pivots = days.pivots,
      useHighLow = F,
      show.pivots = T,
      interpeak.thres = 1,
      gen.pdf = T,
      touch.tol.pip = 1,
      max.pip.from.now = 500,
      start.shift=0
    )
   # pdf.chart.daily[[duration]][[curncy]] = recordPlot()
   
    plotly.chart[[curncy]] = result$graph
    if (generate.table) {
     
      if (all(is.na(result$chosen.sup))) {
        dist.sup = NA
        sup = NA
      } else{
        dist.sup = as.integer(abs(result$chosen.sup["dist.to.now"]/pip))
        sup = result$chosen.sup["y.now"]
      }
      if (all(is.na(result$chosen.res))) {
        dist.res = NA
        res = NA
      } else{
        dist.res = as.integer(abs(result$chosen.res["dist.to.now"]/pip))
        res = result$chosen.res["y.now"]
      }

      result.df[curncy,] = c(blgformat(res), blgformat(dat[nrow(dat), "Close"]), blgformat(sup), format(dist.res, big.mark = ","), format(dist.sup, big.mark = ","))
      
    }
    
  }
  dev.off()

  pdf(paste0(duration,"_", ticksize,"_table.pdf"), width = 12, height = 16)
  grid.table(result.df)
  dev.off()

  return(list(table = result.df, plotly.chart=plotly.chart))
}

## -----------------------------------  run -----------------------------------------------------


tickers = unique(tickers)
table.daily = rep(list(list()), length(chart.daily.duration))
chart.daily = rep(list(list()), length(chart.daily.duration))
chart.weekly = rep(list(list()), length(chart.weekly.duration))
table.weekly = rep(list(list()), length(chart.weekly.duration))
chart.hourly = rep(list(list()), length(chart.hourly.duration))
table.hourly = rep(list(list()), length(chart.hourly.duration))
names(chart.daily) = chart.daily.duration

chart.duration=list()
chart.duration[["Daily"]] = chart.daily.duration
chart.duration[["Weekly"]] = chart.weekly.duration
chart.duration[["Hourly"]] = chart.hourly.duration
chart.ticksize = c("Daily", "Weekly", "Hourly") #"15 Min",
for (tick in chart.ticksize){
  print(tick)
  for (duration in chart.duration[[tick]]) {
    duration.chars = strsplit(duration, split = "")[[1]]
    unit = duration.chars[length(duration.chars)]
    num = as.numeric(duration.chars[-length(duration.chars)])
    num = as.numeric(gsub(", ","",toString(num)))
    if (unit == "D") {
      start.date = Sys.Date() - days(num)
    } else if (unit == "M") {
      start.date = Sys.Date() - months(num)
    } else if (unit == "Y") {
      start.date = Sys.Date() - years(num)
    }

    res = gen_chart_table(ticksize=tick, start.date = start.date, days = num, duration = duration, tickers)
    print(tick)
    if (toupper(tick) == "DAILY") {
      chart.daily[[duration]] = res$plotly.chart
      table.daily[[duration]] = res$table
      } else if(toupper(tick)=="WEEKLY") {
 
      chart.weekly[[duration]] = res$plotly.chart
      table.weekly[[duration]] = res$table
      }else if (tick=="Hourly") {
      chart.hourly[[duration]] = res$plotly.chart
      table.hourly[[duration]] = res$table
      } 
    
  }
}


