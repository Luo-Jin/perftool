# this is a source file which contains R functions
# that will be used in main.R
suppressMessages(library(httr))
suppressMessages(library(jsonlite))
suppressMessages(library(xml2))
suppressMessages(library(rvest))
suppressMessages(library(log4r))
suppressMessages(library(readxl))
suppressMessages(library(writexl))
#suppressMessages(library(xlsx))
suppressMessages(library(gdata))
#############################################################
##
##   functions
##
#############################################################
# change the time char into numeric
convertTosec <- function(timestr)
{
  localtime <- as.POSIXlt(timestr)
  
  l <- lapply(timestr,function(o){
    if(is.na(o))
    {
      NA
    }else
    {
      as.numeric(difftime(as.POSIXct(o),as.POSIXct("1970-01-01 00:00:00"),units = "secs"))
    }
  })
  return (unlist(l))
}

zoneTozone <- function(timestr,t1,t2)
{
  esttime <- as.numeric(difftime(as.POSIXct(timestr,t1),as.POSIXct("1970-01-01 00:00:00",t2),units="secs")) + as.POSIXct("1970-01-01 00:00:00",t2)
  format(esttime)
}

codeToMsg <- function(httpcode)
{
  msg <- list("OK"=200,"Created"=201,"No Content"=204,
              "Bad Request - Syntax or format error"=400,
              "Unauthenticated request"=401,
              "Internal Server Error"=500,
              "Service Unavailable"=503)
  return(names(msg)[msg==httpcode])
}

# collect metrics for multiple iterations
collectMetrics <- function()
{
  #if(!updateStatus())
  
  #{
  # return(FALSE)
  #}
  res <- NULL
  for (i in 1:nrow(iterations))
  {
    if(!is.na(iterations[i,"runID"]) && iterations[i,"status"]=="Finished")
    {
      mm <- extractMetrics(iterations[i,"runID"])
      if(is.logical(mm))
      {
        return(mm)
      }
      mm$cpu_min <- round(mm$cpu_min*100)
      mm$cpu_avg <- round(mm$cpu_avg*100)
      mm$cpu_max <- round(mm$cpu_max*100)
      mm$ram_used_min  <- round(mm$ram_used_min/1024/1024/1024)
      mm$ram_used_avg  <- round(mm$ram_used_avg/1024/1024/1024)
      mm$ram_used_max  <- round(mm$ram_used_max/1024/1024/1024)
      mm$ram_total     <- round(mm$ram_total_min/1024/1024/1024)
      mm$ram_total_min <- NULL
      mm$ram_total_avg <- NULL
      mm$ram_total_max <- NULL
      mm$disk_min <- round(mm$disk_min/1024/1024/1024)
      mm$disk_avg <- NULL
      mm$disk_max <- round(mm$disk_max/1024/1024/1024)
      mm$diskw_min <- round(mm$diskw_min)
      mm$diskw_avg <- round(mm$diskw_avg)
      mm$diskw_max <- round(mm$diskw_max)
      mm$diskr_min <- round(mm$diskr_min)
      mm$diskr_avg <- round(mm$diskr_avg)
      mm$diskr_max <- round(mm$diskr_max)
      mm$run_id      <-    rep(iterations[i,"runID"],nrow(mm))
     # mm$test_id     <-    rep(iterations[i,"testID"],nrow(mm))
    #  mm$instance_id <-    rep(iterations[i,"instanceID"],nrow(mm))
     # mm$start_time  <-    rep(iterations[i,"start"],nrow(mm))
      mm$desc <-    iterations[i,"desc"]
      mm$users <-   iterations[i,"con"]
      res <- rbind(res,mm)
    }
  }
  return(res)
}

collectElaspse <- function()
{
 if(!updateStatus())
 {
   return(FALSE)
 }

  res <- NULL
  for (i in 1:nrow(iterations))
  {
    if(!is.na(iterations[i,"runID"]) && iterations[i,"status"]=="Finished")
    {
      mm <- suppressMessages(extractElapse(iterations[i,"runID"]))
      mm$desc <- iterations[i,"desc"]
      mm$user <- iterations[i,"con"]
      if(is.logical(mm))
      {
        return(mm);
      }
      res <- rbind(res,mm)
    }
  }
  return(res)
}

# extract performance data from one iteration test
extractMetrics <- function(runID)
{
  start <- iterations[which(iterations$runID == runID),"start"]
  duration <- iterations[which(iterations$runID == runID),"duration"]
  start <- convertTosec(zoneTozone(start,Sys.timezone(),grafana.timezone))  + 120  #add 120 seconds delay
  end   <- start + duration
  res <- NULL  
  metr<-lapply(global.metrics,function(o){paste(grafana.endpoint,o,"&start=",start,"&end=",end,"&step=15",sep="")})
  
  for (i in 1:length(metr))
  {
    r <- GET(metr[[i]], add_headers(Authorization=grafana.key))
    if(r$status_code > 200)
    {
      cat( "\n\r",codeToMsg(r$status_code),"\n\r",sep="")
      return(FALSE)
    }
    l <- content(r,"parsed")
    if(is.null(res))
    {
      res <-  data.frame(unlist(lapply(l$data$result,function(o){o$metric["nodename"][[1]]})))
      colnames(res)<-"node"
    }
    mask<-res$node %in% unlist(lapply(l$data$result,function(o){o$metric["nodename"][[1]]}))
    res[paste(names(metr[i]),"_min",sep="")]<- unlist(lapply(l$data$result,function(o){min(as.numeric(unlist(lapply(o$values,function(o){o[2]}))))}))*mask
    res[paste(names(metr[i]),"_avg",sep="")]<- unlist(lapply(l$data$result,function(o){mean(as.numeric(unlist(lapply(o$values,function(o){o[2]}))))}))*mask
    res[paste(names(metr[i]),"_max",sep="")]<- unlist(lapply(l$data$result,function(o){max(as.numeric(unlist(lapply(o$values,function(o){o[2]}))))}))*mask
    
  }
  return(res)
}

# extract TPS and latency from test report in performance center
extractElapse <- function(runID)
{
  # read the results from xls file 
  filename <- paste("data/",runID,".xls",sep="")
  if(file.exists(filename))
  { 
    # get results from downloaded xls
    xls5 <- read_excel(filename,sheet=5)
    xls3 <- read_excel(filename,sheet=3)
  }else
  {
    #get a result collection from a run id
    r <- get_report(get_results(runID))
    if (r$status_code > 201)
    {
      cat( "\n\r",codeToMsg(r$status_code),"\n\r",sep="")
      return(FALSE)
    }
    #xls <- read.xls(filename,sheet=5) 
    xls5 <- read_excel(filename,sheet=5)
    xls3 <- read_excel(filename,sheet=3)
    
  }
  #xls$X.9<-NULL
  xls5$...2<-NULL
  col <- as.character(xls5[3,])
  #xls <- xls5[c(-1,-2,-3),]
  xls <- xls5[5,]
  colnames(xls) <- col
  xls$runID <- runID
  xls$hit   <- as.numeric(xls3$...5[4])
  # remove tmp file
  #file.remove(filename)
  return(as.data.frame(xls))
  
}

# schedule jobs in performance center
setSchedule <- function(iters=NULL)
{
 
  if(!is.null(iters))
  {
    iterations <<- rbind(iterations,iters)
  }
  
  #loop the iterations and schedule the job
  for (i in 1:nrow(iterations))
  {
    #if (is.na(iterations[i,"runID"]) && iterations[i,"status"]=="New")
    if (iterations[i,"status"]=="New")
    {
      idx <- which(!is.na(iterations[,"start"]))
      if (length(idx)>0){
        last_runtime <- convertTosec(iterations[idx,"start"] ) + ifelse(iterations[idx,"duration"]<1800,1800,iterations[idx,"duration"])
        set_runtime  <- ifelse(is.na(convertTosec(iterations[i,"start"])),convertTosec(format(Sys.time())),convertTosec(iterations[i,"start"]))
        start_time <- format(as.numeric(max(set_runtime,last_runtime,convertTosec(format(Sys.time())))) + as.POSIXct("1970-01-01 00:00:00",Sys.timezone()) + 30)
      } else
      {
        start_time <- format(as.numeric(convertTosec(format(Sys.time()))) + as.POSIXct("1970-01-01 00:00:00",Sys.timezone()) + 30)
      }
      test_name  <- paste(global.tests,"_",global.version,"_",iterations[i,"desc"],"_",iterations[i,"con"],"U",sep="")
     
      
       r <- create_timeslot(start_time,max(as.numeric(iterations[i,"duration"])/60,30),test_name,iterations[i,"con"],iterations[i,"instanceID"])
      if (r$status_code != 201) 
      {
        cat( "\n\r",codeToMsg(r$status_code),"\n\r",sep="")
        return(FALSE)
      }
        
      cs <- content(r,"text")
      cs <- gsub('\r\n','',cs)
      cs <- gsub('\"',"'",cs)
      cs <- gsub("xmlns='http://www.hp.com/PC/REST/API'","",cs)
      xml<-read_xml(cs)
      slotID   <- as.numeric(as_list(xml_find_all(xml,"/Timeslot/ID/text()")))
      status   <- as.character(xml_find_all(xml,"/Timeslot/CurrentRunState/text()"))
      iterations[i,"slotID"] <<- slotID
      iterations[i,"start"]  <<- start_time
      iterations[i,"status"] <<- status
       
 
    }
    
 
 
  }
  
  return(updateStatus())
}

# update iterations in the config file
updateStatus <- function(slotIDs=iterations[,"slotID"])
{
  # get the status from all slotID in the config file
  # r <- lapply(slotIDs,get_status)
  
  for (i in 1:length(slotIDs))
    {
    if(!is.na(slotIDs[i]))
      {
            o <- get_status(slotIDs[i])
            if (o$status_code == 200)
            {
              cs <- content(o,"text")
              cs <- gsub('\r\n','',cs)
              cs <- gsub('\"',"'",cs)
              cs <- gsub("xmlns='http://www.hp.com/PC/REST/API'","",cs)
              xml<-read_xml(cs)
              slotID   <- as.numeric(as_list(xml_find_all(xml,"/Timeslot/ID/text()")))
              start    <- as.character(as_list(xml_find_all(xml,"//StartTime/text()")))
              start    <- gsub("T"," ",start)
              start    <- gsub("\\+00","",start)
              start    <- zoneTozone(start,"Europe/London",Sys.timezone())
              start    <- format(as.POSIXct(start))  
              duration <- as.numeric(as_list(xml_find_all(xml,"//DurationInMinutes/text()")))*60
              testID   <- as.numeric(as_list(xml_find_all(xml,"//LoadTestID/text()")))
              instanceID <- as.numeric(as_list(xml_find_all(xml,"//LoadTestInstanceID/text()")))
              runID <- as.numeric(as_list(xml_find_all(xml,"//CurrentRunId/text()")))
              status <- as.character(as_list(xml_find_all(xml,"//CurrentRunState/text()")))
              
              
              iterations[which(iterations$slotID==slotID),"testID"]     <<-testID
              iterations[which(iterations$slotID==slotID),"instanceID"] <<-instanceID
              #iterations[which(iterations$slotID==slotID),"duration"]  <<-duration  # never change after first set
              iterations[which(iterations$slotID==slotID),"runID"]      <<-ifelse(length(runID)>0,runID,NA)
              iterations[which(iterations$slotID==slotID),"start"]      <<-start
              iterations[which(iterations$slotID==slotID),"status"]     <<-status
        
            }else{
              cat( "\n\r",codeToMsg(o$status_code),"\n\r",sep="")
              return(FALSE)
            }
    }
  }
  
  # update the config file
  xml_str <- "<iterations>\r\n"  
  for (i in 1:nrow(iterations))
  {
    xml_str <- paste(xml_str," <iteration>\r\n",sep="")
    for (col in colnames(iterations))
    {
      xml_str <- paste(xml_str,"  <",col,">",iterations[i,col],"</",col,">\r\n",sep="")
    }
    xml_str <- paste(xml_str," </iteration>\r\n",sep="")
  }
  xml_str <- paste(xml_str,"</iterations>",sep="")
  children <- xml_children(config)
  xml_remove(children[[7]],free=T)        # remove the iterations node from config
  xml_add_child(config,read_xml(xml_str)) # insert a new iterations node into config
  write_xml(config,"config.xml")
  
  return(TRUE)
  
}


status <-  function()
{
  if(length(perf.cookies)==0)
  {
    print("Please login first!")
    quit(save="no",status=-1)
  }
  
  if(updateStatus())
  {
    cat("\r\n")
    print(iterations)
    
  } 
}

metrics <- function(filename=NULL)
{
  df<-collectMetrics()
  if(is.logical(df))
  {
    cat("\r\n")
    cat("Collect system metrics failed, see detailed info in log file.\n\r")
    quit(save="no",status=-1)
  }
  row.names(df)<-NULL
  
  if (!is.null(filename))
  {
    print(df)
    l<-list(df)
    names(l)<-"System Metrics"
    write_xlsx(l,filename)
  }else
  {
    print(df)
  }
  
}

elapse <- function(filename=NULL)
{
  df <-collectElaspse()
  if(is.logical(df))
  {
    cat("\r\n")
    cat("Collect elapsed time failed, see detailed info in log file.\n\r")
    quit(save="no",status=-1)
  }

  row.names(df)<-NULL
   
  if (!is.null(filename))
  {
    print(df)
    l<-list(df)
    names(l)<-"Elapsed time"
    write_xlsx(l,filename)
  }else
  {
    print(df)
  }
  
  
}

schedule <- function(iters=NULL)
{

  if(!setSchedule(iters))
  {
    cat(paste("Scheddule test ",paste(names(iters),iters,sep=":",collapse = ",")," failed.\n\r"))
  }else
  {
    cat("\r\n")
    print(iterations)
  }
}


log <- function(num=10)
{
  
  if(!file.exists(log.file))
  {
    cat("\n\r")
    cat("The log file does not exist.\n\r")
  }else
  {
    f<-read.csv(log.file,sep=".",header=F)
    cat(paste("\n\r",unlist(tail(f$V1,num)),sep=""))
    cat("\n\r")
  }
}
#############################################################
##
##   performance center API 
##
#############################################################
login <- function()
{
  r <- GET(perf.auth.url, add_headers("Authorization" = paste(" Basic ",perf.cred,sep="")))
  if (r$status_code == 200){
    perf.LWSSO.cookie <<- r$cookies$value[which(r$cookies$name=="LWSSO_COOKIE_KEY")]
    perf.QCsession.cookie <<- r$cookies$value[which(r$cookies$name=="QCSession")]
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully logon to Perfomance center.",sep=""))
    
    # write cookies into config file
    xml_str <- "<cookies>\r\n" 
    for (i in 1:nrow(r$cookies))
    {
      xml_str <- paste(xml_str," <cookie>\r\n",sep="")
      xml_str <- paste(xml_str,"  <name>",r$cookies[i,"name"],"</name>\r\n",sep="")
      xml_str <- paste(xml_str,"  <value>",r$cookies[i,"value"],"</value>\r\n",sep="")
      xml_str <- paste(xml_str," </cookie>\r\n",sep="")
    }
    xml_str <- paste(xml_str," </cookies>\r\n",sep="")
    perf.cookies <<- read_xml(xml_str)
    
    if (length(xml_children(xml_children(config)[[4]]))==6)
    {
      xml_remove(xml_children(xml_children(config)[[4]])[[6]])# remove the iterations node from config
    }
    xml_add_child(xml_children(config)[[4]],perf.cookies)# insert a new iterations node into config
    write_xml(config,"config.xml")

  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to logon Perfomance center.",sep=""))
    error(global.error.logger,r)

  }
  return(r)
}

logout <-function()
{
  
  r <- GET(perf.logout.url,set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie))
  if (r$status_code == 200){
    
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully logout from Perfomance center.",sep=""))

  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to logout from Perfomance center.",sep=""))
    error(global.error.logger,r)

  }
  
  perf.cookies <<- NULL
  if (length(xml_children(xml_children(config)[[4]]))==6)
  {
    xml_remove(xml_children(xml_children(config)[[4]])[[6]])# remove the iterations node from config
  }
  write_xml(config,"config.xml")
  
  
  return(r)
}

create_timeslot <- function(startTime,duration,testName,user,instanceID)
{
  con <- user -1
  time <- zoneTozone(startTime,Sys.timezone(),perf.timezone)
  timeslotXML <- paste("<Timeslot xmlns='http://www.hp.com/PC/REST/API'>
  <StartTime>",time,"</StartTime>
  <DurationInMinutes>",duration,"</DurationInMinutes>
  <Name>",testName,"</Name>
  <Demands><ControllerDemandAutomatic/></Demands>
  <VusersNumber>0</VusersNumber>
  <VudsNumber>0</VudsNumber>
  <PostRunAction>CollateAnalyze</PostRunAction>
  <LoadTestInstanceID>",instanceID,"</LoadTestInstanceID>
  <IsTestAutostart>true</IsTestAutostart>
  </Timeslot>",sep="")
  
  r <- POST(perf.timeslot.url, add_headers("Content-Type"="application/xml"),set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie), body=timeslotXML)
  
  if (r$status_code == 201){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully schedule test in Perfomance center.",sep=""))
    
  }else
  { 
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to schedule test in Perfomance center.",sep=""))
    error(global.error.logger,r)
  }
  return(r)
}

get_status <- function(slotID)
{  
  r <- GET(paste(perf.timeslot.url,"/",slotID,sep=""), 
           add_headers("Content-Type"="application/xml","encoding"="utf-8"),
           set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie))
  if (r$status_code == 200)
  {
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully get status from slotID:",slotID," Perfomance center.",sep=""))
    return(r)
  }else
  {
    error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to get status from slotID:",slotID," from Perfomance center.",sep=""))
    return(r)
  }
  
}

get_results <- function(runID)
{
  # get result collections for a run ID
  url<-paste(perf.job.url,"/",runID,"/Results",sep="")
  r <- GET(url, add_headers("Content-Type" = "application/xml", "Accept" = "application/xml"), set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie))
  if (r$status_code == 200){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully retrieve the reuslts for runID=",runID,sep=""))
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to retrieve the reuslts for runID=",runID,sep=""))
    error(global.error.logger,r)
  }
  return(r)
}

get_report <- function(r)
{
  cs <- content(r,"text")
  cs <- gsub('\r\n','',cs)
  cs <- gsub('\"',"'",cs)
  cs <- gsub("xmlns='http://www.hp.com/PC/REST/API'","",cs)
  xmlnode <- read_xml(cs)
  runID    <- as.numeric(as_list(xml_find_all(xmlnode,"(//RunID/text())[1]")))
  resultID <- as.numeric(as_list(xml_find_all(xmlnode,"//RunResult/ID[contains(following-sibling::Name/text(),'HighLevelReport')]/text()")))
  filename <- paste("data/",runID,".xls",sep="")
  url <- paste(perf.job.url,"/",runID,"/Results/",resultID,"/data",sep="")
  r <- GET(url, set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie) )
  
  if (r$status_code == 200){
    writeBin(content(r,"raw"),filename)
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully retrieve the report for resultID=",resultID,sep=""))
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to retrieve the report for resultID=",resultID,sep=""))
    error(global.error.logger,r)
  }
  return(r)
}

get_test <- function(testID)
{
  # get test entity for a test ID
  url<-paste(perf.test.url,"/",testID,sep="")
  r <- GET(url, add_headers("Content-Type" = "application/xml", "Accept" = "application/xml"), set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie))
  if (r$status_code == 200){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully retrieve the test for ID=",testID,sep=""))
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to retrieve the test for ID=",testID,sep=""))
    error(global.error.logger,r)
  }
  return(r)
}


update_test <- function(testID,vUsers,seconds)
{
  vUsers <- as.character(vUsers)
  seconds <- as.character(seconds)
  rsp_test<-get_test(testID)
  xml <- content(rsp_test,"parsed")
  ns <- as.character(xml_ns(xml))
  cs <- xml_child(xml,6)
  xml_set_attr(cs,"xmlns",'http://www.hp.com/PC/REST/API')
  # change the number of Vusers.
  xml_set_text(xml_child(xml_child(xml_child(cs,4),1),2),vUsers)
  # set the runtime in seconds
  xml_duration <- read_xml(paste("<Seconds>",seconds,"</Seconds>",collapse = ""))
  xml_replace(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(cs,5),1),3),1),1),1),xml_duration)
  cs <- as.character(cs)
  cs <- gsub('\n','',cs)
  cs <- gsub('\"',"'",cs)
  
  
  url<-paste(perf.test.url,"/",testID,sep="")
  r <- PUT(url, add_headers("Content-Type" = "application/xml", "Accept" = "application/xml"), 
           set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie),
           body=cs)
  
  if (r$status_code == 200){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully retrieve the test for ID=",testID,sep=""))
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to retrieve the test for ID=",testID,sep=""))
    error(global.error.logger,r)
  }
  return(r)
}

#############################################################
##
##   obsolete funtions
##
#############################################################

start_job <- function(testID,instanceID,minutes)
{  
  #<TestID>820</TestID>
  #<TestInstanceID>180</TestInstanceID>
  if (length(perf.cookies)==0)
  {
    print("Please login using your credential at first !")
    return(-1)
  }
  
  jobXML <- paste('<Run xmlns="http://www.hp.com/PC/REST/API"><PostRunAction>Collate And Analyze</PostRunAction><TestID>',testID,'</TestID><TestInstanceID>',instanceID,'</TestInstanceID><TimeslotDuration><Minutes>',minutes,'</Minutes></TimeslotDuration><VudsMode>false</VudsMode></Run>',rep="")
  
  
  r <- POST(perf.job.url, add_headers("Content-Type"="application/xml"),set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie), body=jobXML)
  
  if (r$status_code == 201){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully started Job in performance center.",sep=""))
    
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to start a job in Perfomance center.",sep=""))
    error(global.error.logger,r)
    
  }
  
  #return(content(r,"parsed"))
  return(r)
}

abort_job <- function(runID)
{  
  
  
  
  r <- POST(paste(perf.job.url,"/",runID,"/abort",sep=""), add_headers("Content-Type"="application/xml"),set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie))
  
  if (r$status_code < 202){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully abort Job:",runID," in performance center.",sep=""))
    
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to abort  Job:",runID," in Perfomance center.",sep=""))
    error(global.error.logger,r)
  }
  
  return(r)
}

stopnow_job <- function(runID)
{  
  r <- POST(paste(perf.job.url,"/",runID,"/abort",sep=""), add_headers("Content-Type"="application/xml"),set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie))
  
  if (r$status_code < loooooll202){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully stop Job:",runID," immediately in performance center.",sep=""))
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to stop  Job:",runID," immediately in Perfomance center.",sep=""))
    error(global.error.logger,r)
  }
  
  return(r)
}

delete_timeslot <- function(slotID)
{
  r <- DELETE(paste(perf.timeslot.url,"/",slotID,sep=""),set_cookies("LWSSO_COOKIE_KEY"=perf.LWSSO.cookie,"QCSession"=perf.QCsession.cookie))
  if (r$status_code == 200){
    info(global.info.logger,paste(date(),", HTTP code:",r$status_code," - Successfully delete timeslot ",slotID," from Perfomance center.",sep=""))
  }else
  {
    #error(global.error.logger,paste(date(),", HTTP code:",r$status_code," - Failed to delete timeslot ",slotID," from Perfomance center.",sep=""))
    error(global.error.logger,r)
  }
  return(r)
}


