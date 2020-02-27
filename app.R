library(DT)
library(shinythemes)
library(shiny)
library(dplyr)
library(ECharts2Shiny)
library(shinycustomloader)
library(quantmod)
library(plotly)
library(e1071)
require(gridExtra)
library(normtest)
library(gridExtra)
library(nortest)
library(ggpubr)
library(formattable)
library(TTR)
library(caTools)
library(lubridate)
library(prophet)
library(Metrics)
library(shinycssloaders)
library(randomcoloR)
library(e1071)
require(gridExtra)
library(normtest)
library(gridExtra)
library(nortest)
library(ggpubr)
library(formattable)
library(aTSA)

#################
# Fonction TA
ichimoku <- function(data, Tenkan=9, Kijun=26, Senkou=52){
    
    HLC = HLC(data)
    # Tenkan-sen (Conversion Line): (9-period high + 9-period low)/2))
    tenkan_san <- (runMax(Hi(HLC), Tenkan)+runMin(Lo(HLC), Tenkan))/2
    
    # Kijun-sen (Base Line): (26-period high + 26-period low)/2))
    kijun_sen <- (runMax(Hi(HLC), Kijun)+runMin(Lo(HLC), Kijun))/2
    
    # Senkou Span A (Leading Span A): (Conversion Line + Base Line)/2))
    senkou__spanA <- lag((tenkan_san+kijun_sen)/2, Kijun)
    
    # Senkou Span B (Leading Span B): (52-period high + 52-period low)/2))
    senkou__spanB <- lag((runMax(Hi(HLC), Senkou)+runMin(Lo(HLC), Senkou))/2, Kijun)
    
    # The most current closing price plotted 26 time periods behind (optional)
    chikou_span <- lag(Cl(HLC),Kijun)
    
    df <- data.frame(Date=index(data),as.data.frame(data))
    colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
    ichi <- cbind(tenkan_san=tenkan_san, kijun_sen=kijun_sen, senkou__spanA=senkou__spanA, senkou__spanB=senkou__spanB,
                  chikou_span=chikou_span)
    colnames(ichi) <- c("tenkan_san", "kijun_sen", "senkou__spanA", "senkou__spanB", "chikou_span")
    out<-  cbind(df, data.frame(ichi))
    
    
    
    for (i in 1:length(out[,1])) {
        if (out$Close[i] >= out$Open[i]) {
            out$direction[i] = 'Increasing'
        } else {
            out$direction[i] = 'Decreasing'
        }
    }
    
    i <- list(line = list(color = '#17BECF'))
    d <- list(line = list(color = '#7F7F7F'))
    
    
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                   list(count=1,
                        label='RESET',
                        step='all'),
                   list(count=1,
                        label='1 YR',
                        step='year',
                        stepmode='backward'),
                   list(count=3,
                        label='3 MO',
                        step='month',
                        stepmode='backward'),
                   list(count=1,
                        label='1 MO',
                        step='month',
                        stepmode='backward')
               ))
    
    
    
    p <- out %>%
        plot_ly(x = ~Date, type="candlestick",
                open = ~Open, close = ~Close,
                high = ~High, low = ~Low,
                increasing = i, decreasing = d,
                name="OHLC")%>%
        add_lines(x = ~Date, y = ~tenkan_san ,
                  line = list(color = '#33BDFF', width = 0.5), 
                  name = "Tenkan",
                  hoverinfo = "none", inherit = F)%>%
        add_lines(x = ~Date, y = ~kijun_sen ,
                  line = list(color = '#F1F316', width = 0.5),
                  name = "Kijun",
                  hoverinfo = "none", inherit = F)%>%
        add_lines(x = ~Date, y = ~chikou_span ,
                  line = list(color = '#D105F5', width = 0.5),
                  name = "Chikou",
                  hoverinfo = "none", inherit = F)%>%
        add_lines(x = ~Date, y = ~senkou__spanA ,
                  line = list(color = '#228B22', width = 0.5),
                  name = "Senkou A",
                  hoverinfo = "none", inherit = F, 
                  legendgroup = "Senkou")%>%
        add_lines(x = ~Date, y = ~senkou__spanB ,
                  line = list(color = '#FF3342', width = 0.5),
                  name = "Senkou B",
                  hoverinfo = "none", inherit = F,
                  fill = 'tonexty', fillcolor="rgba( 255, 51, 72,0.3)", 
                  legendgroup = "Senkou")%>%
        
        layout(yaxis = list(title = "Price"),title = "Ichimoku Kinko Hyo",
               xaxis = list(rangeselector = rs, rangeslider = list(visible = F)),
               legend = list(orientation = 'h', x = 0.5, y = 1,
                             xanchor = 'center', yref = 'paper',
                             font = list(size = 10),
                             bgcolor = 'transparent'))
    
    
    
    
    return (p)}
MACD_plot <- function(data, ewa1 = 12, ewa2=26,signal = 9){
    datamacd <- data.frame(Date=index(data),coredata(data))
    
    colnames(datamacd) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    
    datamacd$ewa1 = EMA(datamacd$Close,n = ewa1)
    datamacd$ewa2 = EMA(datamacd$Close,n = ewa2)
    datamacd$macd = datamacd$ewa1 - datamacd$ewa2
    datamacd$macd_signal = EMA(datamacd$macd,n = signal)
    datamacd$macd_histo = datamacd$macd - datamacd$macd_signal
    
    
    
    profit = 0
    move = 'buy'
    datamacd$position = 0
    datamacd$budget = 0
    
    for (i in (ewa2+10):nrow(datamacd)) {
        if (datamacd[i, "macd"] < 0 && datamacd[i-1, "macd"] > 0 && move == 'buy') {
            datamacd[i, "position"] = 'buy'
            move = 'sell'
            datamacd[i, "budget"] = datamacd[i-1, "budget"] - datamacd[i, "Close"]
        } else if (datamacd[i, "macd"] > 0 && datamacd[i-1, "macd"] < 0 && move == 'sell') {
            datamacd[i, "position"] = 'sell'
            move = 'buy'
            datamacd[i, "budget"] = datamacd[i-1, "budget"] + datamacd[i, "Close"]
        } else {
            datamacd[i, "position"] = 'hold'
            datamacd[i, "budget"] = datamacd[i-1, "budget"]
        }
    }  
    
    
    for (i in (ewa2+10):nrow(datamacd)) {
        if (datamacd[i, "position"] == "buy") {
            datamacd[i, "buy"] = datamacd[i, "Close"]
        } else if (datamacd[i, "position"]== "sell") {
            datamacd[i, "sell"] = datamacd[i, "Close"]
        }}
    
    summary(datamacd)
    p <- datamacd %>%
        plot_ly(x = ~Date,type = 'scatter', mode = 'lines',
                y=~Close, name= "Close") %>%
        add_lines(x = ~Date, y = ~ewa1 , name = "ewa 1",
                  line = list(color = '#DE1738', width = 0.5),
                  legendgroup = "Moving average",
                  hoverinfo = "none", inherit = F) %>%
        add_lines(x = ~Date, y = ~ewa2 , name = "ewa 2",
                  line = list(color = '#DE1738', width = 0.5),
                  legendgroup = "Moving average",
                  hoverinfo = "none", inherit = F) %>%
        add_markers(x = ~Date,y = ~buy, marker = list(symbol ="triangle-up", color="green",size ="10"), name="Buy strat", legendgroup = "strategies") %>%
        add_markers(x = ~Date,y = ~sell, marker = list(symbol ="triangle-down", color="red",size ="10"), name="Sell strat", legendgroup = "strategies" ) %>%
        layout(yaxis = list(title = "Price"))
    
    # plot volume bar chart
    
    pp <- datamacd %>%
        plot_ly(x=~Date, y=~macd_histo, type = 'bar', name = "MACD histo",
                marker = list(color = '#C9EFF9', legendgroup = "macd")) %>%
        add_lines(x = ~Date, y = ~macd  , name = "MACD",
                  line = list(color = '#386d13', width = 0.5),
                  hoverinfo = "none", inherit = F, legendgroup = "macd") %>%
        add_lines(x = ~Date, y = ~macd_signal , name = "MACD SIGNAL",
                  line = list(color = '#8f2020', width = 0.5),
                  hoverinfo = "none", inherit = F, legendgroup = "macd") %>%
        layout(yaxis = list(title = "MACD"))
    
    # create rangeselector buttons
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                   list(count=1,
                        label='RESET',
                        step='all'),
                   list(count=1,
                        label='1 YR',
                        step='year',
                        stepmode='backward'),
                   list(count=3,
                        label='3 MO',
                        step='month',
                        stepmode='backward'),
                   list(count=1,
                        label='1 MO',
                        step='month',
                        stepmode='backward')
               ))
    
    # subplot with shared x axis
    p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE) %>%
        layout(title = "Relative Strenght Index",
               xaxis = list(rangeselector = rs),
               legend = list(orientation = 'h', x = 0.5, y = 1,
                             xanchor = 'center', yref = 'paper',
                             font = list(size = 10),
                             bgcolor = 'transparent'))
    return(p)
}
bbands = function(data){
    
    df <- data.frame(Date=index(data),coredata(data))
    
    colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    # create Bollinger Bands
    bbands <- BBands(df[,c("High","Low","Close")])
    
    # join and subset data
    df <- cbind(df, data.frame(bbands[,1:3]))
    
    # colors column for increasing and decreasing
    for (i in 1:length(df[,1])) {
        if (df$Close[i] >= df$Open[i]) {
            df$direction[i] = 'Increasing'
        } else {
            df$direction[i] = 'Decreasing'
        }
    }
    
    i <- list(line = list(color = '#17BECF'))
    d <- list(line = list(color = '#7F7F7F'))
    
    # plot candlestick chart
    p <- df %>%
        plot_ly(x = ~Date, type="candlestick",
                open = ~Open, close = ~Close,
                high = ~High, low = ~Low, name = "OHLC",
                increasing = i, decreasing = d) %>%
        add_lines(x = ~Date, y = ~up , name = "B Bands",
                  line = list(color = '#ccc', width = 0.5),
                  legendgroup = "Bollinger Bands",
                  hoverinfo = "none", inherit = F) %>%
        add_lines(x = ~Date, y = ~dn, name = "B Bands",
                  line = list(color = '#ccc', width = 0.5),
                  legendgroup = "Bollinger Bands", inherit = F,
                  showlegend = FALSE, hoverinfo = "none",
                  fill = 'tonexty', fillcolor="rgba(0, 46, 99,0.3)") %>%
        add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                  line = list(color = '#E377C2', width = 0.5),
                  hoverinfo = "none", inherit = F) %>%
        layout(yaxis = list(title = "Price"))
    
    # plot volume bar chart
    pp <- df %>%
        plot_ly(x=~Date, y=~Volume, type='bar', name = "Volume",
                color = ~direction, colors = c('#17BECF','#7F7F7F')) %>%
        layout(yaxis = list(title = "Volume"))
    
    # create rangeselector buttons
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                   list(count=1,
                        label='RESET',
                        step='all'),
                   list(count=1,
                        label='1 YR',
                        step='year',
                        stepmode='backward'),
                   list(count=3,
                        label='3 MO',
                        step='month',
                        stepmode='backward'),
                   list(count=1,
                        label='1 MO',
                        step='month',
                        stepmode='backward')
               ))
    
    # subplot with shared x axis
    p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE) %>%
        layout(title = "Boldringer bands",
               xaxis = list(rangeselector = rs),
               legend = list(orientation = 'h', x = 0.5, y = 1,
                             xanchor = 'center', yref = 'paper',
                             font = list(size = 10),
                             bgcolor = 'transparent'))
    
    return (p)
    
}
RSI_plot = function(data, matype){
    
    df <- data.frame(Date=index(data),coredata(data))
    
    colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    df$RSI = RSI(df$Close, maType = matype)
    
    if (matype == "SMA"){
        
        df$MA = SMA(df$Close)
    } else if (matype == "EMA") {
        df$MA = EMA(df$Close)
    } else { print("I don't understand this kind of moving average")}
    
    df$high_bound = 70
    df$low_bound = 30
    
    
    # colors column for increasing and decreasing
    for (i in 1:length(df[,1])) {
        if (df$Close[i] >= df$Open[i]) {
            df$direction[i] = 'Increasing'
        } else {
            df$direction[i] = 'Decreasing'
        }
    }
    
    i <- list(line = list(color = '#17BECF'))
    d <- list(line = list(color = '#7F7F7F'))
    
    # plot candlestick chart
    p <- df %>%
        plot_ly(x = ~Date, type="candlestick",
                open = ~Open, close = ~Close,
                high = ~High, low = ~Low, name = "OHLC",
                increasing = i, decreasing = d) %>%
        add_lines(x = ~Date, y = ~MA , name = "Moving Average",
                  line = list(color = '#DE1738', width = 0.5),
                  legendgroup = "Moving average",
                  hoverinfo = "none", inherit = F)
    
    # plot volume bar chart
    pp <- df %>%
        plot_ly(x=~Date, y=~RSI, type = 'scatter', mode = 'lines', name = "RSI",
                line = list(color = '#660000', width = 1)) %>%
        add_lines(x = ~Date, y = ~high_bound , name = "high limit",
                  line = list(color = '#386d13', width = 0.5),
                  hoverinfo = "none", inherit = F) %>%
        add_lines(x = ~Date, y = ~low_bound , name = "low limit",
                  line = list(color = '#8f2020', width = 0.5),
                  fill = 'tonexty', fillcolor="rgba(0, 78, 56,0.3)",
                  hoverinfo = "none", inherit = F) %>%
        layout(yaxis = list(title = "RSI"))
    
    # create rangeselector buttons
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                   list(count=1,
                        label='RESET',
                        step='all'),
                   list(count=1,
                        label='1 YR',
                        step='year',
                        stepmode='backward'),
                   list(count=3,
                        label='3 MO',
                        step='month',
                        stepmode='backward'),
                   list(count=1,
                        label='1 MO',
                        step='month',
                        stepmode='backward')
               ))
    
    # subplot with shared x axis
    p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE) %>%
        layout(title = "Relative Strenght Index",
               xaxis = list(rangeselector = rs),
               legend = list(orientation = 'h', x = 0.5, y = 1,
                             xanchor = 'center', yref = 'paper',
                             font = list(size = 10),
                             bgcolor = 'transparent'))
    
    return (p)
    
}
runmax_perso <- function (x, n = 10, cumulative = FALSE) {
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop(sprintf("n = %d is outside valid range: [1, %d]", 
                     n, NROW(x)))
    if (cumulative) {
        NAs <- sum(is.na(x))
        if (NAs > 0) {
            if (any(is.na(x[-(1:NAs)]))) 
                stop("Series contains non-leading NAs")
            if (NAs + n > NROW(x)) 
                stop("not enough non-NA values")
        }
        beg <- 1 + NAs
        len <- NROW(x) - NAs
        if (NCOL(x) > 1) {
            stop("ncol(x) > 1. runMax only supports univariate 'x'")
        }
        result <- double(NROW(x))
        result[beg:NROW(x)] <- cummax(x[beg:NROW(x)])
        is.na(result) <- c(1:(n - 1 + NAs))
    }
    else {
        result <- .Call("runmax", x, n, PACKAGE = "TTR")
    }
    reclass(result, x)
}
runmin_perso <- function (x, n = 10, cumulative = FALSE) {
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop(sprintf("n = %d is outside valid range: [1, %d]", 
                     n, NROW(x)))
    if (cumulative) {
        NAs <- sum(is.na(x))
        if (NAs > 0) {
            if (any(is.na(x[-(1:NAs)]))) 
                stop("Series contains non-leading NAs")
            if (NAs + n > NROW(x)) 
                stop("not enough non-NA values")
        }
        beg <- 1 + NAs
        len <- NROW(x) - NAs
        result <- double(NROW(x))
        result[beg:NROW(x)] <- cummin(x[beg:NROW(x)])
        is.na(result) <- c(1:(n - 1 + NAs))
    }
    else {
        result <- .Call("runmin", x, n, PACKAGE = "TTR")
    }
    reclass(result, x)
}
WILLR_plot = function(data, n){
    
    
    
    h = Hi(data)
    l = Lo(data)
    c = Cl(data)
    hh = TTR::runMax(h, n, cumulative = FALSE)
    ll = TTR::runMin(l, n, cumulative = FALSE)
    spread = hh - ll
    willr = 100 *(hh-c)/spread
    
    df <- data.frame(Date=index(data),coredata(data), willr)
    
    colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted", "willr") 
    df$high_bound = 80
    df$low_bound = 20
    
    
    # colors column for increasing and decreasing
    for (i in 1:length(df[,1])) {
        if (df$Close[i] >= df$Open[i]) {
            df$direction[i] = 'Increasing'
        } else {
            df$direction[i] = 'Decreasing'
        }
    }
    
    i <- list(line = list(color = '#17BECF'))
    d <- list(line = list(color = '#7F7F7F'))
    
    # plot candlestick chart
    p <- df %>%
        plot_ly(x = ~Date, type="candlestick",
                open = ~Open, close = ~Close,
                high = ~High, low = ~Low, name = "OHLC",
                increasing = i, decreasing = d) %>%
        layout(yaxis = list(title = "Price"))
    
    # plot volume bar chart
    pp <- df %>%
        plot_ly(x=~Date, y=~willr, type = 'scatter', mode = 'lines', name = "Will % R",
                line = list(color = '#660000', width = 1)) %>%
        add_lines(x = ~Date, y = ~high_bound , name = "high limit",
                  line = list(color = '#386d13', width = 0.5),
                  hoverinfo = "none", inherit = F) %>%
        add_lines(x = ~Date, y = ~low_bound , name = "low limit",
                  line = list(color = '#8f2020', width = 0.5),
                  fill = 'tonexty', fillcolor="rgba(0, 78, 56,0.3)",
                  hoverinfo = "none", inherit = F) %>%
        layout(yaxis = list(title = "Will % R"))
    
    # create rangeselector buttons
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                   list(count=1,
                        label='RESET',
                        step='all'),
                   list(count=1,
                        label='1 YR',
                        step='year',
                        stepmode='backward'),
                   list(count=3,
                        label='3 MO',
                        step='month',
                        stepmode='backward'),
                   list(count=1,
                        label='1 MO',
                        step='month',
                        stepmode='backward')
               ))
    
    # subplot with shared x axis
    p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
                 shareX = TRUE, titleY = TRUE) %>%
        layout(title = "William % R",
               xaxis = list(rangeselector = rs),
               legend = list(orientation = 'h', x = 0.5, y = 1,
                             xanchor = 'center', yref = 'paper',
                             font = list(size = 10),
                             bgcolor = 'transparent'))
    
    return (p)
    
}
parabolicSAR = function(data,   init_AF = 0.02,  max_AF = 0.2){
    
    
    
    df <- data.frame(Date=index(data),coredata(data))
    
    colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    
    
    
    datasar = df
    datasar$trend=0
    datasar$sar=0.0
    datasar$real_sar=0.0
    datasar$ep=0.0
    datasar$af=0.0
    
    
    
    if (datasar[2,"Close"]>datasar[1,"Close"]) {datasar[2,"trend"]=1  } else {datasar[2,"trend"]=-1}
    if (datasar[2,"trend"]>0) { datasar[2,"sar"]=datasar[1,"High"]  } else {datasar[2,"sar"]=datasar[1,"Low"]  }
    datasar[2,"real_sar"]=datasar[2,"sar"]
    if (datasar[2,"trend"]>0) { datasar[2,"ep"]=datasar[2,"High"]  } else { datasar[2,"ep"]=datasar[2,"Low"]  }
    datasar[2,"af"]=init_AF
    
    for (i in 3:nrow(df)){
        
        temp= datasar[i-1,"sar"]+datasar[i-1,"af"]*(datasar[i-1,"ep"]-datasar[i-1,"sar"])
        if (datasar[i-1,"trend"]<0) {
            datasar[i,'sar']=max(temp,datasar[i-1,"High"],datasar[i-2,"High"])
            if (datasar[i,"sar"]<datasar[i,"High"]) {  temp = 1} else { temp = datasar[i-1,"trend"]-1 }
        } else {
            datasar[i,'sar']=min(temp,datasar[i-1,"Low"],datasar[i-2,"Low"])
            if (datasar[i,"sar"]>datasar[i,"Low"]) {  temp = -1 } else {  temp = datasar[i-1,"trend"]+1 }
        }
        datasar[i, "trend"]= temp
        
        
        if (datasar[i, "trend"]<0) {
            if (datasar[i, "trend"]!=-1) {temp=min(datasar[i, "Low"],datasar[i-1, "ep"])} else{ temp = datasar[i, "Low"]}
        } else {
            if (datasar[i, "trend"]!=1) { temp=max(datasar[i, "High"],datasar[i-1, "ep"]) } else{temp = datasar[i, "High"]}  
        }
        datasar[i, "ep"]= temp
        
        if (abs(datasar[i, "trend"])==1) {
            temp=datasar[i-1, "ep"]
            datasar[i,'af']=init_AF
        }  else{
            temp=datasar[i,"sar"]
            if ( datasar[i,"ep"]==datasar[i-1,"ep"]) {
                datasar[i,'af']=datasar[i-1,'af']
            } else {
                datasar[i,'af']=min(max_AF,datasar[i-1,'af']+init_AF)
            }
        }
        datasar[i,'real_sar']=temp
        
    }
    
    df = tail(datasar,nrow(datasar)-1)
    
    
    
    
    
    # colors column for increasing and decreasing
    for (i in 1:length(df[,1])) {
        if (df$Close[i] >= df$Open[i]) {
            df$direction[i] = 'Increasing'
        } else {
            df$direction[i] = 'Decreasing'
        }
    }
    
    
    
    
    i <- list(line = list(color = '#17BECF'))
    d <- list(line = list(color = '#7F7F7F'))
    
    # create rangeselector buttons
    rs <- list(visible = TRUE, x = 0.5, y = -0.055,
               xanchor = 'center', yref = 'paper',
               font = list(size = 9),
               buttons = list(
                   list(count=1,
                        label='RESET',
                        step='all'),
                   list(count=1,
                        label='1 YR',
                        step='year',
                        stepmode='backward'),
                   list(count=3,
                        label='3 MO',
                        step='month',
                        stepmode='backward'),
                   list(count=1,
                        label='1 MO',
                        step='month',
                        stepmode='backward')
               ))
    # plot candlestick chart
    p <- df %>%
        plot_ly(x = ~Date, type="candlestick",
                open = ~Open, close = ~Close,
                high = ~High, low = ~Low, 
                increasing = i, decreasing = d, name="Price") %>%
        add_lines(x = ~Date, y = ~real_sar , dash = 'dash',
                  line = list(color = '#DE1738', width = 0.5), 
                  name = "Parabolic SAR")%>%
        layout(title = "Parabolic SAR",
               yaxis = list(title = "Price"),
               xaxis = list(rangeselector = rs,rangeslider = list(visible = F)),
               legend = list(orientation = 'h', x = 0.5, y = 1,
                             xanchor = 'center', yref = 'paper',
                             font = list(size = 10),
                             bgcolor = 'transparent'))
    
    
    
    
    
    
    return (p)
    
}


###################
# Fonction AE
box_wisker <- function(data,value, season){
    require(lubridate)
    require("RColorBrewer")
    is_in = 0
    words = c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    for (word in words){
        
        if (word == value){
            temp = 1} 
        else {
            temp  = 0}
        is_in = is_in + temp
    }
    if(is_in == 0){
        print("Value has to be in this list : c('Date','Open','High','Low','Close','Volume','Adjusted')")} 
    else{
        
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        
        df$year = as.factor(year(df$Date))
        
        df$month = as.factor(month(df$Date, label = TRUE))
        df$month <- factor(df$month, levels=rev(levels(df$month)))
        
        df$wday <- wday(df$Date, label = TRUE)
        df$wday <- factor(df$wday, levels=rev(levels(df$wday)))
        
        df$y <- df[, value]
        
        
        if(season == "year"){
            n = nlevels(df$year)
            plot <- plot_ly(df, y = ~y, color = ~year,type = "box", colors = brewer.pal(n = n, name = "Set3"),boxpoints = "all", jitter = 0.3,
                            pointpos = -1.8) %>% 
                layout(title = "Boites à moustaches",
                       xaxis = list(title = "Années"),
                       yaxis = list (title = value))
            
        } else if ( season =="month"){
            n = nlevels(df$month)
            plot <- plot_ly(df, y = ~y, color = ~month, type = "box", colors = brewer.pal(n = n, name = "Set3"),boxpoints = "all", jitter = 0.3,
                            pointpos = -1.8)%>% 
                layout(title = "Boites à moustaches",
                       xaxis = list(title = "Mois"),
                       yaxis = list (title = value))
            
            
        }else if ( season =="week"){
            n = nlevels(df$wday)
            plot <- plot_ly(df, y = ~y, color = ~wday,type = "box", colors = brewer.pal(n = n, name = "Set3"),boxpoints = "all", jitter = 0.3,
                            pointpos = -1.8)%>% 
                layout(title = "Boites à moustaches",
                       xaxis = list(title = "Jours"),
                       yaxis = list (title = value))
            
            
        } else { plot= print("I don't understand the period you want to plot, try : 'year', 'month'or 'week'")}
        
        return(plot) }
}
seasonnal_plot <- function(data,value, season){
    
    is_in = 0
    words = c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    for (word in words){
        
        if (word == value){
            temp = 1} 
        else {
            temp  = 0}
        is_in = is_in + temp
    }
    if(is_in == 0){
        print("Value has to be in this list : c('Date','Open','High','Low','Close','Volume','Adjusted')")} 
    else{
        
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        
        df$year = as.factor(year(df$Date))
        
        df$month = as.factor(month(df$Date, label = TRUE))
        df$month <- factor(df$month, levels=rev(levels(df$month)))
        
        df$wday <- wday(df$Date, label = TRUE)
        df$wday <- factor(df$wday, levels=rev(levels(df$wday)))
        
        df$y <- df[, value]
        
        if(season == "year"){
            n = nlevels(df$year)
            plot <- plot_ly(df, y = ~y, color = ~year, type = 'scatter', mode = 'lines') %>% 
                layout(title = "Méthode à bandes",
                       xaxis = list(title = "Années"),
                       yaxis = list (title = value))
            
        } else if ( season =="month"){
            n = nlevels(df$month)
            plot <- plot_ly(df, y = ~y, color = ~month, type = 'scatter', mode = 'lines')%>% 
                layout(title = "Méthode à bandes",
                       xaxis = list(title = "Mois"),
                       yaxis = list (title = value))
            
            
        }else if ( season =="week"){
            n = nlevels(df$wday)
            plot <- plot_ly(df, y = ~y, color = ~wday, type = 'scatter', mode = 'lines')%>% 
                layout(title = "Méthode à bandes",
                       xaxis = list(title = "Semaines"),
                       yaxis = list (title = value))
            
            
        } else { plot= print("I don't understand the period you want to plot, try : 'year', 'month'or 'week'")}
        
        return(plot) }
    
    
    
}
decomposition <- function(data, value, composition, frequency = 250){
    
    is_in = 0
    words = c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    
    for (word in words){
        
        if (word == value){
            temp = 1} 
        else {
            temp  = 0}
        is_in = is_in + temp
    }
    if(is_in == 0){
        print("Value has to be in this list : c('Date','Open','High','Low','Close','Volume','Adjusted')")} 
    else{
        
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        df$year = year(df[,"Date"])
        y_ts = ts(df[, value], start=c((min(df$year)+1)-01-01), frequency = frequency)
        ggplotly(y_ts %>% decompose(composition) %>% autoplot)
        
        
    }
    
    
    
}
detect_anomalies <- function(data_forecast, real_data){
    
    forecasted = data_forecast[,c('ds','trend', 'yhat', 'yhat_lower', 'yhat_upper')]
    forecasted = cbind(forecasted[1:nrow(real_data),], real_data)
    
    forecasted$anomaly = 0
    forecasted2 <- transform(forecasted,  anomaly = ifelse(y > yhat_upper, 1, anomaly))
    forecasted2 <- transform(forecasted2,  anomaly = ifelse(y < yhat_lower, -1, anomaly))
    
    forecasted2$anomaly2<- abs(forecasted2$anomaly)
    
    forecasted2$importance = 0
    forecasted2 <- transform(forecasted2,  importance = ifelse(anomaly == 1 , (forecasted2$y - yhat_upper) / forecasted2$y, importance))
    forecasted2 <- transform(forecasted2,  importance = ifelse(anomaly == -1 , ( yhat_lower - forecasted2$y ) / forecasted2$y, importance))
    forecasted2 = mutate(forecasted2, anomaly2=anomaly2* forecasted2$y)
    
    
    
    return(forecasted2)
    
    
}
prophet_outliers <- function(data, value, mcmc_forecasting = 0){
    require(prophet)
    is_in = 0
    words = c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    
    for (word in words){
        
        if (word == value){
            temp = 1} 
        else {
            temp  = 0}
        is_in = is_in + temp
    }
    if(is_in == 0){
        print("Value has to be in this list : c('Date','Open','High','Low','Close','Volume','Adjusted')")} 
    else{
        
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("ds","Open","High","Low","Close","Volume","Adjusted") 
        df[, "y"] <- df[, value]
        ts_outlier = df[, c("ds","y")]
        m <- prophet(ts_outlier, mcmc.samples = 0)
        future <- make_future_dataframe(m, periods = 1)
        forecast <- predict(m, future)
        data_to_plot = detect_anomalies(forecast, ts_outlier)
        
        ggplotly(ggplot(data_to_plot, aes(x=ds,y=ts_outlier$y))+
                     
                     geom_line(col="Blue")+
                     geom_line(aes(y = yhat_lower, colour = 'yhat_lower'))+  
                     geom_line(aes(y = yhat_upper, colour = 'yhat_lower'))+ 
                     geom_point(aes(y= anomaly2,color = "RED", size = importance), alpha = 0.5) +
                     scale_size(range = c(0, 5))+
                     theme_bw() +
                     theme(legend.position = "none")+ 
                     ggtitle("Détection par moyenne mobile") +
                     xlab("Date") + ylab(value)) 
    }
    
}
plotMovingAverage <- function(series, value, window,  scale=1.96){
    require(Metrics)
    
    #series - dataframe with timeseries


    
    is_in = 0
    words = c("Date","Open","High","Low","Close","Volume","Adjusted") 
    for (word in words){
        
        if (word == value){
            temp = 1} 
        else {
            temp  = 0}
        is_in = is_in + temp
    }
    if(is_in == 0){
        print("Value has to be in this list : c('Date','Open','High','Low','Close','Volume','Adjusted')")} 
    else{
        
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        df[, "y"] <- df[, value]
        df$ma = SMA(df[, value], n=window)
        df_mae = slice(df, window:n())
        mae = mae(df_mae$y, df_mae$ma)
        deviation = sd(df_mae$y- df_mae$ma)
        df$lower_bond = df$ma - (mae + scale * deviation)
        df$upper_bond = df$ma  + (mae + scale * deviation)
        
        
        
        
        df$anomaly = 0
        df2 <- transform(df,  anomaly = ifelse(y > upper_bond, 1, anomaly))
        df2 <- transform(df2,  anomaly = ifelse(y < lower_bond, -1, anomaly))
        
        df2$anomaly2<- abs(df2$anomaly)
        
        df2$importance = 0
        df2 <- transform(df2,  importance = ifelse(anomaly == 1 , (df2$y - upper_bond) / df2$y, importance))
        df2 <- transform(df2,  importance = ifelse(anomaly == -1 , ( lower_bond - df2$y ) / df2$y, importance))
        df2 = mutate(df2, anomaly2=anomaly2* df2$y)
        
        
        
        
        
        
        
        
        plot=ggplotly(ggplot(df2, aes(x=Date,y=y),main)+
                     geom_line(col="Blue")+
                     geom_line(aes(y = ma), col="Green")+
                     geom_line(aes(y = lower_bond, colour = 'lower_bond'))+  
                     geom_line(aes(y = upper_bond, colour = 'lower_bond'))+ 
                     geom_point(aes(y= anomaly2,color = "Red", size = importance), alpha = 0.5) +
                     scale_size(range = c(0, 5))+
                     theme_bw()+
                     theme(legend.position = "none")+ 
                     ggtitle("Détection par moyenne mobile") +
                     xlab("Date") + ylab(value))
        return(plot)
        
    }
    
    
}
table_distrib_info <- function(series){
    
    df  <-data.frame("min",round(min(series)))
    names(df)<-c("Stat & tests","Value")
    mean = data.frame("mean",round(mean(series),3))
    names(mean)<-c("Stat & tests","Value")
    std = data.frame("std",round(sd(series),3))
    names(std)<-c("Stat & tests","Value")
    median = data.frame("median",round(median(series),3))
    names(median)<-c("Stat & tests","Value")
    q1 = data.frame("Q1",round(quantile(series, 0.25),3))
    names(q1)<-c("Stat & tests","Value")
    q3 = data.frame("Q3",round(quantile(series, 0.75),3))
    names(q3)<-c("Stat & tests","Value")
    max = data.frame("max",round(max(series),3))
    names(max)<-c("Stat & tests","Value")
    skew = data.frame("skew",round(skewness(series),3))
    names(skew)<-c("Stat & tests","Value")
    kurt = data.frame("kurt",round(kurtosis(series),3))
    names(kurt)<-c("Stat & tests","Value")
    temp_norm = jb.norm.test(series)
    jb_test = data.frame("JB test",round(temp_norm$statistic,3))
    names(jb_test)<-c("Stat & tests","Value")
    jbp = data.frame("P-val",round(temp_norm$p.value,3))
    names(jbp)<-c("Stat & tests","Value")
    #temp_norm = shapiro.test(series)
    #sw_test = data.frame("SW test",round(temp_norm$statistic,3))
    #swp = data.frame("P-val",round(temp_norm$p.value,3))
    temp_norm = lillie.test(series)
    ks_test = data.frame("KS test",round(temp_norm$statistic,3))
    names(ks_test)<-c("Stat & tests","Value")
    ksp = data.frame("P-val",round(temp_norm$p.value,3))
    names(ksp)<-c("Stat & tests","Value")
    
    df2 =  rbind(df, mean,std,median, q1,q3,max, skew,kurt, jb_test, jbp, ks_test, ksp)
    rownames(df2) <- 1:nrow(df2)

    
    return(df2)
    
    
    
}
ggplot.corr <- function(data, lag.max = 24, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE,...) {
    
    require(ggplot2)
    require(dplyr)
    require(cowplot)
    
    if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}
    
    list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
    N <- as.numeric(list.acf$n.used)
    df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
    df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
    df1$lag.acf[2] <- 0
    df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
    df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
    df1$acfstd[1] <- 0
    df1 <- select(df1, lag, acf, acfstd)
    
    list.pacf <- acf(sunspot.year, lag.max = lag.max, type = "partial", plot = FALSE)
    df2 <- data.frame(lag = list.pacf$lag,pacf = list.pacf$acf)
    df2$pacfstd <- sqrt(1/N)
    
    if(large.sample.size == TRUE) {
        plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
            geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
            geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
            geom_col(fill = "#4373B6", width = 0.7) +
            scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
            scale_y_continuous(name = element_blank(), 
                               limits = c(min(df1$acf,df2$pacf),1)) +
            ggtitle("ACF") +
            theme_bw()
        
        plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
            geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
            geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
            geom_col(fill = "#4373B6", width = 0.7) +
            scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
            scale_y_continuous(name = element_blank(),
                               limits = c(min(df1$acf,df2$pacf),1)) +
            ggtitle("PACF") +
            theme_bw()
    }
    else {
        plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
            geom_col(fill = "#4373B6", width = 0.7) +
            geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                       colour = "sandybrown",
                       linetype = "dashed") + 
            geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                       colour = "sandybrown",
                       linetype = "dashed") + 
            scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
            scale_y_continuous(name = element_blank(), 
                               limits = c(min(df1$acf,df2$pacf),1)) +
            ggtitle("ACF") +
            theme_bw()
        
        plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
            geom_col(fill = "#4373B6", width = 0.7) +
            geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                       colour = "sandybrown",
                       linetype = "dashed") + 
            geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                       colour = "sandybrown",
                       linetype = "dashed") + 
            scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
            scale_y_continuous(name = element_blank(),
                               limits = c(min(df1$acf,df2$pacf),1)) +
            ggtitle("PACF") +
            theme_bw()
    }
    cowplot::plot_grid(plot.acf, plot.pacf, nrow = numofrow)
}
get_ts <- function(data, value, frequency = 360, min="auto", max="auto"){
    is_in = 0
    words = c("Date","Open","High","Low","Close","Volume","Adjusted") 
    
    
    for (word in words){
        
        if (word == value){
            temp = 1} 
        else {
            temp  = 0}
        is_in = is_in + temp
    }
    if(is_in == 0){
        print("Value has to be in this list : c('Date','Open','High','Low','Close','Volume','Adjusted')")} 
    else{
        
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        df$year = year(df[,"Date"])
        
        if(min=="auto"){min_to_use =min(df$year)+1 } else{min_to_use = min}
        if(max=="auto"){max_to_use =max(df$year) } else{max_to_use = max}
        temp_df <- subset(df, year<= max_to_use & year >= min_to_use)
        
        y_ts = ts(temp_df[, value], start=c((min_to_use)), frequency = frequency)
        
        return(y_ts)
    }
    
    
}

    
ui <- fluidPage(shinyUI(navbarPage("Application analyse financière",
                                   theme = shinytheme("flatly"),
   
                                   
   #--------------------------
   # Définition de l'application

   tabPanel("Intro",
            includeMarkdown("./md/intro.md"),
            hr()),
   
    #--------------------------
    #Premier onglet --> avoir les données
    
    tabPanel("Données",
             
             sidebarPanel(
                 
                 helpText("Sélectionnez un titre financier a éxaminer."),
                 br(),   
                 helpText(em("(Les informations sont collectées sur Yahoo finance)")),
                 
                 textInput("symb", "Symbol", "GOOG"),
                 
                 dateRangeInput("dates", 
                                "Sélectionniez les dates : ",
                                start = "2010-01-01", end = Sys.Date()),
                 
                 br(),
                 
                 actionButton("get", "Lancer !")
                 
             ),
             dataTableOutput("tableau") %>% withSpinner(color=randomColor(1))
             
             
             ),
   
   tabPanel("Analyse technique",
       
       
       #--------------------
       # Controles analyse techniques
       
       column(width = 12,
            
            sidebarPanel(
                selectInput("plotType",
                            label = "Type de graphique",
                            choices = c('Ichimoku Kinko Hyo' = "ichimoku", 
                                        'Moving Average Convergence Divergence' = "macd", 
                                        'Bandes de Bollinger' = "bbands",
                                        'Relative strength index'= "rsi",
                                        'Williams %R'='willr',
                                        'Parabolic stop and reverse'="psar"),
                            selected = "ichimoku"),
                br(), 
                    
                #------------------
                # Arguments de ichimoku
                conditionalPanel(
                    condition = "input.plotType == 'ichimoku'",
                    sliderInput("tenkan", "Tenkan", min=1, max=30, value=9)
                ),
                
                conditionalPanel(
                    condition = "input.plotType == 'ichimoku'",
                    sliderInput("kijun", "Kijun", min=1, max=360, value=26)
                ),
                
                conditionalPanel(
                    condition = "input.plotType == 'ichimoku'",
                    sliderInput("senkou", "Senkou", min=1, max=360, value=52)
                ),
                #------------------
                # Arguments de macd
                
                conditionalPanel(
                    condition = "input.plotType == 'macd'",
                    sliderInput("ewa1", "EWA1", min=1, max=30, value=12)
                ),
                conditionalPanel(
                    condition = "input.plotType == 'macd'",
                    sliderInput("ewa2", "EWA2", min=1, max=52, value=26)
                ),
                conditionalPanel(
                    condition = "input.plotType == 'macd'",
                    sliderInput("signal", "Signal", min=1, max=30, value=9)
                ),
                
                #------------------
                # Arguments de RSI_plot
                
                conditionalPanel(
                    condition = "input.plotType == 'rsi'",
                    selectInput("rsi_ma",
                                label = "Type de moyenne mobile",
                                choices = c('Moyenne mobile' = "SMA", 
                                            'Moyenne mobile exponentielle pondérée' = "EMA"),
                                selected = "SMA")
                ),
                
                #------------------
                # Arguments de willr
                conditionalPanel(
                    condition = "input.plotType == 'willr'",
                    sliderInput("n", "Longueur de la période", min=1, max=360, value=22)
                ), 
                
                #------------------
                # Arguments de psar 
                
                conditionalPanel(
                    condition = "input.plotType == 'psar'",
                    sliderInput("af_init", "Facteur d'accélération inital", min=0.01, max=0.1, value=0.02,step = 0.01)
                ), 
                conditionalPanel(
                    condition = "input.plotType == 'psar'",
                    sliderInput("af", "Facteur d'accélération maximal", min=0.1, max=1, value=0.2, step=0.1)
                )
                
                )
            
            ),
       
       
       column( width = 12, 
            

            
            plotlyOutput("ta_graph",  height = '600px', width = 'auto') %>% withSpinner(color=randomColor(1)) )      
   
   
    ),
   tabPanel("Analyse Économétrique",fluidRow(column(width = 12, 
                                           sidebarPanel(
                                               selectInput("plotType_ae",
                                                           label = "Type de graphique :",
                                                           choices = c('Périodique' = "period",
                                                                       'Décomposition de la série' = "decompose",
                                                                       'Anomalies sur série temporelle'="outlier",
                                                                       'Information sur la série'="info_data",
                                                                       'Heatmap'="heatmap",
                                                                       'Analyse autoregressive'="AR"),
                                                           selected = "period"),
                                               
                                               selectInput("value",
                                                           label = "La série à prendre :",
                                                           choices = c('Ouverture' = "Open", 
                                                                       'Fermeture' = "Close",
                                                                       'Haut' = "High",
                                                                       'Bas' = "Low",
                                                                       'Ajustée' = "Adjusted",
                                                                       'Volume' = "Volume"),
                                                           selected = "Adjusted"),
                                               
                                               
                                               
                                               br(),
                                               
                                               #--------------------------
                                               # Les arguments de l'analyse par période
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'period'",
                                                   selectInput("period_type",
                                                               label = "Type de représentation :",
                                                               choices = c('Boite à moustache' = "box_wisker", 
                                                                           'Méthode à bandes' = "seasonnal"),
                                                               selected = "box_wisker")
                                               ),
                                               
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'period'",
                                                   selectInput("season",
                                                               label = "La période à prendre :",
                                                               choices = c('Annuelle' = "year", 
                                                                           'Mensuelle' = "month",
                                                                           'Hebdomadaire ' = "week"),
                                                               selected = "month")
                                               ),
                                               

                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'decompose'",
                                                   selectInput("decomp_type",
                                                               label = "Type de décomposition :",
                                                               choices = c('Additive' = "additive", 
                                                                           'Multiplicative' = "multiplicative"),
                                                               selected = "additive")
                                               ),
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'decompose'",
                                                   sliderInput("freq", "Frequence de la série", min=1, max=365, value=242)
                                               ), 
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'decompose'",
                                                   p(em("Les séries financières n'ont pas les week-ends, considèrer ~242 jours pour une année"))
                                               ), 
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'outlier'",
                                                   selectInput("detect",
                                                               label = "Méthode de détection :",
                                                               choices = c('Moyenne mobile' = "ma_detect", 
                                                                           'Prophet' = "prophet"),
                                                               selected = "prophet"),
                                                   conditionalPanel(
                                                       condition = "input.detect == 'ma_detect'",
                                                       sliderInput("detect_window",label = "Longueur de la fenêtre :",
                                                                   min=1,max = 60, value = 22
                                                       ))),
                                               
                                                
                                               
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'info_data'",
                                                   sliderInput("lag",label = "Nombre de retard :",
                                                               min=1,max = 360, value = 36
                                                   )
                                               
                                               ),
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'heatmap'",
                                                   selectInput("heatmap",label = "type de heatmap :",
                                                               choices = c('Mois/Année' = "year_month", 
                                                                           'Mois/Semaine' = "month_day"),
                                                               selected = "year_month")
                                                    
                                                   ),
                                               conditionalPanel(
                                                   condition = "input.plotType_ae == 'AR'",
                                                   sliderInput("lag2",label = "Nombre de retard :",
                                                               min=1,max = 360, value = 9
                                                   ),
                                                   selectInput("test_stationnarity",label = "Tests de stationnarité :",
                                                                  choices = c('ADF' = "adf", 
                                                                              'KPSS' = "kpss",
                                                                              'PP'="pp"),
                                                                  selected = "adf")
                                                  
                                               )
                                                   
                                               
                                                   
                                               )
                                               
                                           )), column(width = 12, 
                                                     
                                                     
                                                      tableOutput("ea_table"),
                                                     plotlyOutput("ea_graph",  height = '600px', width = 'auto') %>% withSpinner(color=randomColor(1)),
                                                     plotOutput("ea_plot"))
            
            
            
            #--------------
            # Prévision
            
            
            
        
            
            
            
            
            ),
   tabPanel("Prévisions")
   )
   ))



                   
                  

    
    
                  
server <- function(input, output, session) {
    
    #-------------------- 
    # Avoir les données
    
    dataInput <- reactive({
        if (input$get == 0)
            return(NULL)
        
        return(isolate({
            
            getSymbols(input$symb,src="yahoo", auto.assign = FALSE)
            
        }))
    })
    
    
    
    


    #-------------------
    # Tableau de données
    output$tableau <- renderDataTable({
        
        
        
        dates_subset <- seq(as.Date(input$dates[1]), as.Date(input$dates[2]), "day")
        data <- window(dataInput(), dates_subset)
        df <- data.frame(Date=index(data),coredata(data))
        colnames(df) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        
        
        
        datatable(df,
                  colnames=c("Date","Open","High","Low","Close","Volume","Adjusted"), 
                  rownames = F, options=list(pageLength=25,
                                             lengthMenu=list(c(25,100,-1),c("25","100","Tout")),order=list(0,"asc")))
        })

    #-------------------
    # Analyse technique
    output$ta_graph <-renderPlotly({
        dates_subset <- seq(as.Date(input$dates[1]), as.Date(input$dates[2]), "day")
        data <- window(dataInput(), dates_subset)
        
    if (input$plotType == "ichimoku") {   
        
        
        ichimoku(data, Tenkan=input$tenkan, Kijun = input$kijun, Senkou = input$senkou )} 
    else if (input$plotType == "macd"){

        MACD_plot(data, ewa1 = input$ewa1, ewa2 = input$ewa2, signal = input$signal )
    } else if (input$plotType == "bbands"){

        bbands(data )
    }else if (input$plotType == "rsi"){

        RSI_plot(data, matype = input$rsi_ma )
    }else if (input$plotType == "willr"){

        WILLR_plot(data, n = input$n )
    }else if (input$plotType == "psar"){

        parabolicSAR(data,init_AF = input$af_init, max_AF = input$af)
    }
        
        
        
    })
    #-------------------
    # Analyse économétrique
    output$ea_graph <-renderPlotly({
        
        dates_subset <- seq(as.Date(input$dates[1]), as.Date(input$dates[2]), "day")
        data <- window(dataInput(), dates_subset)
        
        
        
        
        
        if (input$plotType_ae == "period") {   
            
            if (input$period_type == "box_wisker"){
                box_wisker(data=data, value=input$value , season = input$season)}
            else if (input$period_type =="seasonnal"){
                seasonnal_plot(data=data, value=input$value , season = input$season)
            }
            } 
        else if (input$plotType_ae == "decompose"){
            
            decomposition(data, value=input$value , composition = input$decomp_type,  frequency = input$freq)        } 
        
        else if (input$plotType_ae == "outlier"){
            if(input$detect == "ma_detect"){
                plotMovingAverage(data,value = input$value, window = input$detect_window)            }
            else if (input$detect == "prophet"){
                prophet_outliers(data,input$value)
            }
        }
        
        else if (input$plotType_ae == "info_data"){
            
            data <- data.frame(Date=index(data),coredata(data))
            colnames(data) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
            
            data$y <- data[,input$value]
            
            ggplotly(ggplot(data, aes(x=Date, y=y))+
                geom_line(aes(x=Date, y=y), col=randomColor(1)))
            
            
        }else if (input$plotType == "willr"){
            
            WILLR_plot(data, n = input$n )
        }else if (input$plotType_ae == "heatmap"){
            
            
            
            data <- data.frame(Date=index(data),coredata(data))
            colnames(data) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
            
            data$y <- data[,input$value]
            data$year <- year(data$Date)
            data$day <- wday(data$Date, label=T)
            data$month <- month(data$Date, label=T)
            
            if(input$heatmap == "year_month"){
                
                
                
                yearMonth <- ddply(data, c( "year", "month" ), summarise,
                                   N    = Close
                )
                
                
                yearMonth$month <- factor(yearMonth$month, levels=rev(levels(yearMonth$month)))
                
                
                yearMonth_plot = ggplot(yearMonth, aes(year, month)) + geom_tile(aes(fill = N),colour = "white") +
                    scale_fill_gradient(low="#add8e6", high="#02064a") +  
                    guides(fill=guide_legend(title="Prix")) +
                    labs(title = "Heatmap mois/ années ",
                         x = "Années", y = "Mois") +
                    theme_bw() + theme_minimal() 
               ggplotly(yearMonth_plot)
                
            } else if (input$heatmap == "month_day"){
                
                
                yearMonth <- ddply(data, c( "day", "month" ), summarise,
                                   N    = Close
                )
                
                
                yearMonth$month <- factor(yearMonth$month, levels=rev(levels(yearMonth$month)))
                
                yearMonth_plot = ggplot(yearMonth, aes(day, month)) + geom_tile(aes(fill = N),colour = "white") +
                    scale_fill_gradient(low="#add8e6", high="#02064a") +  
                    guides(fill=guide_legend(title="Prix")) +
                    labs(title = "Heatmap jours/ années ",
                         x = "Jours", y = "Années") +
                    theme_bw() + theme_minimal() 
                ggplotly(yearMonth_plot)
                
            } 
        }
        
        
        
    })
    output$ea_plot <- renderPlot({
        
        
        if (input$plotType_ae == "info_data"){
            
            
            data <- data.frame(Date=index(data),coredata(data))
            colnames(data) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
            
            data$y <- data[,input$value]
            
            plot2 = ggplot.corr(data = data$y, lag.max = input$lag, ci= 0.95, large.sample.size = TRUE, horizontal = TRUE)
            
            plot3 = ggplot(data, aes(x=y))+
                geom_histogram(binwidth=50, colour="black", 
                               aes(y=..density.., fill=..count..))+
                geom_density(col="darkblue") + 
                scale_fill_gradient("Count", low="#add8e6", high="#02064a")+
                stat_function(fun=dnorm,
                              color="red",
                              args=list(mean=mean(data$y), 
                                        sd=sd(data$y)))
            


            

            grid.arrange(plot2,plot3, nrow=2 )
            
            
        } else if(input$plotType_ae == "AR"){
            
            data <- data.frame(Date=index(data),coredata(data))
            colnames(data) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
            
            data$y <- data[,input$value]
            
            gglagplot(data$y,lags = input$lag2)
            
        }
    })
    output$ea_table <-renderTable({
        dates_subset <- seq(as.Date(input$dates[1]), as.Date(input$dates[2]), "day")
        data <- window(dataInput(), dates_subset)
        if (input$plotType_ae == "info_data"){
            
        data <- data.frame(Date=index(data),coredata(data))
        colnames(data) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 
        
        data$y <- data[,input$value]
        table_distrib_info(data$y)
        } else if (input$plotType_ae == "AR"){
            
            data <- data.frame(Date=index(data),coredata(data))
            colnames(data) <- c("Date","Open","High","Low","Close","Volume","Adjusted") 

            ts = ts(data[, input$value], start=input$dates[1], frequency = input$freq2)
            stationary.test(ts, nlag = input$lag2, method = input$test_stationnarity)

            
            
        }
    })


}

shinyApp(ui, server)