####fixHoliは日付が固定されている祝日
####input$dateの指定された年の祝日を探しfixWeekHoliに．
##3連休以上は土日+祝日(振り替え休日)

##成人の日:1/8-14の間の月曜日
##海の日:7/15-21の間の月曜日
##敬老の日:9/15-21の間の月曜日
##体育の日:10/8-14の間の月曜日

##国民の日である9/22は敬老の日が9/21の場合：実装済
##祝日の振り替え休日：実装済
##3連休：実装済


#固定祝日
fixHoli <- c("01-01", "02-11", "03-21", "04-29", "05-03", "05-04",
             "05-05", "08-11", "09-23", "11-03", "11-23", "12-23",
             "12-30", "12-31", "01-02", "01-03",
             "08-13", "08-14", "08-15"
             )
WEEK <- c( "月曜日", "火曜日", "水曜日", "木曜日", "金曜日", "土曜日")
fixWeekHoli <- NULL
HD <- NULL 

server <- function(input, output) {
  output$dairy <- renderText({
    paste( substring(input$date,1,4), "年",
           substring(input$date,6,7), "月",
           substring(input$date,9,10), "日",
           weekdays(input$date),
    sep="")
  })

  output$holiday <- renderText({
    #成人の日　1月第2月曜日
    day <- paste( substring(input$date,1,4),"-01-08",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    daySeijin <- days[match("月曜日",weekdays(days))]
    #海の日　7月第3月曜日
    day <- paste( substring(input$date,1,4),"-07-15",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    dayUmi <- days[match("月曜日",weekdays(days))]
    #敬老の日　9月第3月曜日
    day <- paste( substring(input$date,1,4),"-09-15",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    dayKeirou <- days[match("月曜日",weekdays(days))]
    #体育の日　10月第2月曜日
    day <- paste( substring(input$date,1,4),"-10-08",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    dayTaiiku <- days[match("月曜日",weekdays(days))]
    ##国民の日_敬老の日が9/21の場合
    dayKokumin <- NULL
    if( !is.na( match( paste( substring(input$date,1,4),"-09-21",sep=""),
                       as.character( dayKeirou) ) ) )
      dayKokumin <- as.Date(paste( substring(input$date,1,4),"-09-22",sep=""))
    fixWeekHoli <- c( daySeijin, dayUmi, dayKeirou, dayTaiiku, dayKokumin)

######祝日の日付をまとめる
    a <- as.data.frame( fixWeekHoli)
    #str(a)
    fixHoli <- paste( substring(input$date, 1, 4), fixHoli,sep="-")
    bb <- as.Date(fixHoli)
    b <- as.data.frame(bb)
    #str(b)
    colnames(a) <-"Date"
    colnames(b) <-"Date"
    holiday <- rbind(a,b)
######日付に対応する曜日を追加
    wd <- NULL
    for ( i in 1:nrow(holiday)) { wd<- rbind(wd,weekdays(holiday[i,1]))}
    holiW <- cbind(holiday,wd) #holiday_weekday
    hd <- holiW #holiday_data
######祝日の振替休日
    if( !is.na( match( "日曜日", hd$wd))) { #2016年のように，祝日に日曜日がない年がある
      sunday <-  hd[grep("日曜日",hd$wd),1] #hdから日曜日の日付を見つける
      for ( j in 1:length(sunday) ) {
        i <- 0
        repeat {
          if( is.na( match( sunday[j]+i, hd[,1]))) break
          i <- i+1
        }
        hd[match(sunday[j],hd[,1]),2] <- WEEK[i]
        hd[match(sunday[j],hd[,1]),1] <- hd[match(sunday[j],hd[,1]),1] +i
      }
    }
    HD <- hd
    if(weekdays(input$date)=="日曜日" || weekdays(input$date)== "土曜日"
       || !is.na( match( input$date, hd[,1])))
      "休日" else "平日"
  })

  output$three_daysOff <- renderText({
##########holidayと同じ
    #成人の日　1月第2月曜日
    day <- paste( substring(input$date,1,4),"-01-08",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    daySeijin <- days[match("月曜日",weekdays(days))]
    #海の日　7月第3月曜日
    day <- paste( substring(input$date,1,4),"-07-15",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    dayUmi <- days[match("月曜日",weekdays(days))]
    #敬老の日　9月第3月曜日
    day <- paste( substring(input$date,1,4),"-09-15",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    dayKeirou <- days[match("月曜日",weekdays(days))]
    #体育の日　10月第2月曜日
    day <- paste( substring(input$date,1,4),"-10-08",sep="")
    days <- seq(as.Date(day), len=7, by="1 day")
    dayTaiiku <- days[match("月曜日",weekdays(days))]
    ##国民の日_敬老の日が9/21の場合
    dayKokumin <- NULL
    if( !is.na( match( paste( substring(input$date,1,4),"-09-21",sep=""),
                       as.character( dayKeirou) ) ) )
      dayKokumin <- as.Date(paste( substring(input$date,1,4),"-09-22",sep=""))
    fixWeekHoli <- c( daySeijin, dayUmi, dayKeirou, dayTaiiku, dayKokumin)
    a <- as.data.frame( fixWeekHoli)
    #str(a)
    fixHoli <- paste( substring(input$date, 1, 4), fixHoli,sep="-")
    bb <- as.Date(fixHoli)
    b <- as.data.frame(bb)
    #str(b)
    colnames(a) <-"Date"
    colnames(b) <-"Date"
    holiday <- rbind(a,b)
    wd <- NULL
    for ( i in 1:nrow(holiday)) { wd<- rbind(wd,weekdays(holiday[i,1]))}
    holiW <- cbind(holiday,wd) #holiday_weekday
    hd <- holiW #holiday_data
    if( !is.na( match( "日曜日", hd$wd))) { #2016年のように，祝日に日曜日がない年がある
      sunday <-  hd[grep("日曜日",hd$wd),1] #hdから日曜日の日付を見つける
      for ( j in 1:length(sunday) ) {
        i <- 0
        repeat {
          if( is.na( match( sunday[j]+i, hd[,1]))) break
          i <- i+1
        }
        hd[match(sunday[j],hd[,1]),2] <- WEEK[i]
        hd[match(sunday[j],hd[,1]),1] <- hd[match(sunday[j],hd[,1]),1] +i
      }
    }
##########ここまでholidayと同じ
######3連休
    monday <- NULL
    friday <- NULL
    SSM <- NULL
    FSS <- NULL
    ##祝日が月曜日と金曜日である日付を探し，3連休に対応する日付を見つける
    if( !is.na( match( "月曜日", hd$wd))) {
      monday <-  hd[grep("月曜日",hd$wd),1]
      SSM <- data.frame( monday-2, monday-1, monday) #SaturdaySundayMonday
      colnames(SSM) <- c("day1","day2","day3")
    }
    if( !is.na( match( "金曜日", hd$wd))) {
      friday <-  hd[grep("金曜日",hd$wd),1]
      FSS <- data.frame( friday+2, friday+1, friday) #FridaySaturdaySunday
      colnames(FSS) <- c("day1","day2","day3")

    }
    ##GoldenWeek
    GW <- data.frame( paste( substring(input$date,1,4),"-05-03",sep=""),
                      paste( substring(input$date,1,4),"-05-04",sep=""),
                      paste( substring(input$date,1,4),"-05-05",sep=""))
    colnames(GW) <- c("day1","day2","day3")
    threedaysOff <- rbind(FSS,SSM)
    threedaysOff <- rbind(threedaysOff,GW)
    ##SilverWeek
    SW <- NULL
    if( !is.null( dayKokumin)) {
      SW <- data.frame( paste( substring(input$date,1,4),"-09-21",sep=""),
                        paste( substring(input$date,1,4),"-09-22",sep=""),
                        paste( substring(input$date,1,4),"-09-23",sep=""))
      colnames(SW) <- c("day1","day2","day3")
      threedaysOff <- rbind(threedaysOff,SW)
    }

    tdO <-NULL #threedaysOff
    for( i in 1: nrow(threedaysOff)) {
      for( j in 1:ncol(threedaysOff)) {
        tdO <- c(tdO,as.character(threedaysOff[i,j])) #Dateだと数値化されるため
      }
    }
    ##New Year
    tdO <- c( paste( substring(input$date,1,4),"-12-30",sep=""),
              paste( substring(input$date,1,4),"-12-31",sep=""),
              paste( substring(input$date,1,4),"-01-02",sep=""),
              paste( substring(input$date,1,4),"-01-03",sep=""),
              tdO)
    ##Summer vacation
    tdO <- c( paste( substring(input$date,1,4),"-08-13",sep=""),
              paste( substring(input$date,1,4),"-08-14",sep=""),
              paste( substring(input$date,1,4),"-08-15",sep=""),
              tdO)

    utdO <- unique(tdO) #unique_tdO
    outdO <- utdO[order(utdO)] #order_utdO
    if( !is.na( match( as.character(input$date), outdO)))
      "3連休の休みです" else "3連休ではないです"
  })

}

# Create Shiny app ----
#shinyApp(ui = ui, server = server)



