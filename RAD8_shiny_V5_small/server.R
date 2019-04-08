## top ###################################
## 01/23/2019 Justin Rajendra
## app to read from RAD8 pulse ox
## Server V5

## initialize stuff
data.df <- data.frame(spo2=NULL,bpm=NULL,alarm=NULL,dateTime=NULL)
hour.df <- data.frame(spo2=NULL,bpm=NULL,alarm=NULL,dateTime=NULL)
last.alarm <- "NA"

startDateTime <- Sys.time()
spo2.mean <- spo2.sd <- NA
bpm.mean <- bpm.sd <- NA
resetCounter <- 0
rad8.df <- c()

## reset serial connection ###################################
resetSerial <- function(){
  tryCatch({
    close(con) ; closeAllConnections() ; Sys.sleep(1) ; open(con) ; Sys.sleep(1)
    sink(file.path("www",con_out),append=TRUE,split=TRUE)
  }, warning = function(w) {
    print(w)
    close(con) ; closeAllConnections() ; Sys.sleep(1) ; open(con) ; Sys.sleep(1)
    sink(file.path("www",con_out),append=TRUE,split=TRUE)
  }, error=function(err){
    print(err)
    close(con) ; closeAllConnections() ; Sys.sleep(1) ; open(con) ; Sys.sleep(1)
    sink(file.path("www",con_out),append=TRUE,split=TRUE)
  })
  cat("Resetting serial connection.\n")
  print(Sys.time())
}   ## end reset serial


## read from the rad8 and parse the serial output ###################################
getRAD8 <- function(){
  newText <- NULL
  tryCatch({
    newText <- read.serialConnection(con)
  }, warning = function(w) {
    print(w) ; resetSerial()
    tryCatch({
      newText <- read.serialConnection(con)
    }, error=function(err){
      print(err)
    })
  }, error=function(err){
    print(err) ; resetSerial()
    tryCatch({
      newText <- read.serialConnection(con)
    }, error=function(err){
      print(err)
    })
  }, finally=function(fin){
    print(fin) ; resetSerial() ; newText <- NULL
    ## write out current data
    file_name <- paste0("julian_sats_",format(Sys.time(),"%d-%m-%Y-%H-%M-%S"),".csv")
    
    tryCatch({
      write.csv(hour.df,file=file.path("www",file_name),row.names=FALSE)
      print(paste("Saving out data:",file_name))
    }, error=function(err){
      print(err) ; Sys.sleep(3)
      write.csv(hour.df,file=file.path("www",file_name),row.names=FALSE)
      print(paste("Saving out data:",file_name))
    })
    sink() ; close(con) ; stopApp()
  })
  
  # newText <- read.serialConnection(con)
  if(length(newText) > 0){
    dateTime <- substr(strsplit(newText," SN=")[[1]][1],2,18)
    dateTime <- strptime(dateTime, "%m/%d/%y %H:%M:%S")
    spo2 <- as.numeric(substr(strsplit(newText,"SPO2=")[[1]][2],1,3))
    bpm <- as.numeric(substr(strsplit(newText,"BPM=")[[1]][2],1,3))
    alarm <- substr(strsplit(newText,"ALARM=")[[1]][2],1,4)
    rad8.df <- data.frame(spo2=spo2,bpm=bpm,alarm=alarm,dateTime=dateTime)
    return(rad8.df)
  }
}

## clean up some memory  ###################################
saveANDclean <- function(){
  
  ## write out current data
  file_name <- paste0("julian_sats_",format(Sys.time(),"%d-%m-%Y-%H-%M-%S"),".csv")
  
  tryCatch({
    write.csv(hour.df,file=file.path("www",file_name),row.names=FALSE)
    print(paste("Saving out data:",file_name))
  }, error=function(err){
    print(err) ; Sys.sleep(1)
    write.csv(hour.df,file=file.path("www",file_name),row.names=FALSE)
    print(paste("Saving out data:",file_name))
  })
  
  ## do some math
  spo2.mean <<- round(mean(hour.df$spo2,na.rm=TRUE),digits=1)
  spo2.sd <<- round(sd(hour.df$spo2,na.rm=TRUE),digits=1)
  bpm.mean <<- round(mean(hour.df$bpm,na.rm=TRUE),digits=1)
  bpm.sd <<- round(sd(hour.df$bpm,na.rm=TRUE),digits=1)
  
  ## remove hour data, close serial connection, clean  memory and open connection
  hour.df <<- data.frame(spo2=NULL,bpm=NULL,alarm=NULL,dateTime=NULL)
  resetSerial()
}

## close serial connection and reopen
resetCON <- function(){
  resetCounter <<- 0 ; resetSerial()
}

## server main ####################################################
## code for server for input and output
shinyServer(function(input,output,session) {
  options(warn = -1,shiny.error = recover)
  
  ## when done, write out a bunch of stuff
  session$onSessionEnded(function() {
    close(con)
    file_name <- paste0("julian_sats_",format(Sys.time(),"%d-%m-%Y-%H-%M-%S"),".csv")
    tryCatch({
      write.csv(hour.df,file=file.path("www",file_name),row.names=FALSE)
    }, error=function(err){
      print(err) ; Sys.sleep(3)
      write.csv(hour.df,file=file.path("www",file_name),row.names=FALSE)
    })
    total.time <- difftime(Sys.time(),startDateTime,units="hours")
    print(paste(round(total.time,digits=4),"hours elapsed"))
    cat('\nAll done!\n') ; sink() ; stopApp()
  })
  
  ## Date and time ##########################################
  output$dateTimeOutput <- renderUI({
    invalidateLater(1000,session=session)
    ## get the data
    rad8.df <<- getRAD8()
    tagList(tags$sp(style="font-size:50px;color:grey;",
                    format(rad8.df$dateTime,"%b %d %Y %H:%M:%S")))
  })
  
  ## HR box ##########################################
  output$bpmOutput <- renderUI({
    invalidateLater(1000,session=session)
    if(is.null(rad8.df$bpm)){
      tagList(tags$sp("No Signal!",style="font-size:50px;color:red;"))
    } else if(is.na(rad8.df$bpm)){
      tagList(tags$sp("No Signal!",style="font-size:50px;color:red;"))
    } else {
      if(rad8.df$bpm < 65){
        last.alarm <<- paste("(HR Low)",format(rad8.df$dateTime,"%H:%M:%S"))
        tagList(tags$sp(rad8.df$bpm,style="font-size:50px;color:red;"))
      } else if(rad8.df$bpm > 180){
        last.alarm <<- paste("(HR High)",format(rad8.df$dateTime,"%H:%M:%S"))
        tagList(tags$sp(rad8.df$bpm,style="font-size:50px;color:red;"))
      } else {
        tagList(tags$sp(rad8.df$bpm,style="font-size:50px;color:grey;"))
      }
    }
  })
  
  ## alarm box  ###################################
  output$alarmOutput <- renderValueBox({
    invalidateLater(1000,session=session)
    
    if(is.null(rad8.df$alarm)){
      valueBox(
        value=tags$sp("No Signal!",
                      style="font-size:50px;color:black;"),
        subtitle=tags$sp("Alarm",style="font-size:40px;color:black;"),
        width=12,icon=icon("bell"),color="yellow")
      
    } else if(is.na(rad8.df$alarm)){
      valueBox(
        value=tags$sp("No Signal!",
                      style="font-size:50px;color:black;"),
        subtitle=tags$sp("Alarm",style="font-size:40px;color:black;"),
        width=12,icon=icon("bell"),color="yellow")
    } else {
      if(rad8.df$alarm == "0000"){
        valueBox(
          value=tags$sp("All Good",
                        style="font-size:50px;color:black;"),
          subtitle=tags$sp(last.alarm,style="font-size:40px;color:black;"),
          width=12,icon=icon("bell"),color="green")
      } else if(rad8.df$alarm == "0020"){
        valueBox(
          value=tags$sp("Muted",
                        style="font-size:50px;color:black;"),
          subtitle=tags$sp(last.alarm,style="font-size:40px;color:black;"),
          width=12,icon=icon("bell"),color="yellow")
      } else {
        valueBox(
          value=tags$sp("LOOK OUT!",
                        style="font-size:50px;color:black;"),
          subtitle=tags$sp("Alarm",style="font-size:40px;color:black;"),
          width=12,icon=icon("bell"),color="red")
      }
    }
  })
  
  ## spo2 box ##########################################
  ## stuck everything in here because I am lazy
  output$spo2Output <- renderUI({
    invalidateLater(1000,session=session)
    
    ## reset every 10 minutes?
    resetCounter <<- resetCounter + 1
    if(resetCounter > 600){ resetCON() }
    
    ## save out the hour (15 min) and clean if needed
    if(nrow(hour.df) >= 900){ saveANDclean() }
    hour.df <<- rbind(hour.df,rad8.df)
    
    ## spo2 box
    if(is.null(rad8.df$spo2)){
      tagList(tags$sp("No Signal!",style="font-size:50px;color:red;"))
    } else if(is.na(rad8.df$spo2)){
      tagList(tags$sp("No Signal!",style="font-size:50px;color:red;"))
    } else {
      if(rad8.df$spo2 >= 90){
        tagList(tags$sp(paste0(rad8.df$spo2,"%"),
                        style="font-size:50px;color:grey;"))
      } else {
        last.alarm <<- paste("(spO2 Low)",format(rad8.df$dateTime,"%H:%M:%S"))
        tagList(tags$sp(paste0(rad8.df$spo2,"%"),
                        style="font-size:50px;color:red;"))
      }
    }
  })  ## end value box messiness
  
  ## warning sound ##########################################
  output$warning_sound <- renderUI({
    invalidateLater(1000,session=session)
    
    if(input$mute_warning == "Play"){
      if(is.null(rad8.df$spo2)){
        tagList(tags$sp("No Signal!",style="font-size:50px;color:red;"))
      } else if(is.na(rad8.df$spo2)){
        tagList(tags$sp("No Signal!",style="font-size:50px;color:red;"))
      } else {
        if(rad8.df$spo2 >= 94){
          tagList(tags$sp(" ",style="font-size:50px;color:grey;"))
        } else {
          tags$audio(src="Japanese_Temple_Bell_Small.mp3",type="audio/mp3",
                     autoplay=NA,controls=NA)
        }
      }
    } else {
      tagList(tags$sp(" ",style="font-size:50px;color:grey;"))
    }
  })
  
  ## cuff stuff ##########################################
  observeEvent(input$cuffDown,{
    output$cuffDownOutput <- renderUI({
      tagList(tags$sp(style="font-size:30px;color:grey;",
                      format(rad8.df$dateTime,"%b %d %Y %H:%M:%S")))
    })
    
    cuff.df <- data.frame(cuff="Down",dateTime=rad8.df$dateTime)
    file_name <- paste0("julian_cuff_",
                        format(rad8.df$dateTime,"%d-%m-%Y-%H-%M-%S"),".tsv")
    write.table(cuff.df,file=file.path("www",file_name),row.names=FALSE,
                quote=FALSE,sep="\t")
    print(paste("Saving out data:",file_name))
  })   ## end cuff down
  
  observeEvent(input$cuffUp,{
    output$cuffUpOutput <- renderUI({
      tagList(tags$sp(style="font-size:30px;color:grey;",
                      format(rad8.df$dateTime,"%b %d %Y %H:%M:%S")))
    })
    cuff.df <- data.frame(cuff="Up",dateTime=rad8.df$dateTime)
    file_name <- paste0("julian_cuff_",
                        format(rad8.df$dateTime,"%d-%m-%Y-%H-%M-%S"),".tsv")
    write.table(cuff.df,file=file.path("www",file_name),row.names=FALSE,
                quote=FALSE,sep="\t")
    print(paste("Saving out data:",file_name))
  })   ## end cuff up
  
})   ## end server
