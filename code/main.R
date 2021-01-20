#############################################################
##
##   prepare environment
##
#############################################################
args <- commandArgs(trailingOnly = T)
#setwd("C:\\Users\\scnjil\\Documents\\RStudio\\project\\datacollector")
options(width=400,warn=-1)
source('code/functions.R')

#############################################################
##
##   read configuration file 
##
#############################################################
# read from the config file
config <- read_xml("config.xml")
global.status   <- "F"
global.cookie   <- NULL
global.results  <- NULL

global.tests <- xml_text(xml_find_all(config,"/test/@name"))
global.version <- xml_text(xml_find_all(config,"/test/version"))
# define performance center environment
perf.host       <- as.character(xml_find_all(config,"//perf_center/host/text()"))
perf.domain     <- as.character(xml_find_all(config,"//perf_center/domain/text()"))
perf.project    <- as.character(xml_find_all(config,"//perf_center/project/text()"))
perf.tenant     <- as.character(xml_find_all(config,"//perf_center/tenant/text()"))
perf.cred       <- as.character(xml_find_all(config,"//perf_center/credentialBase64/text()"))
perf.auth.url   <- paste("https://",perf.host,"/LoadTest/rest/authentication-point/authenticate?tenant=",perf.tenant,sep="")
perf.logout.url <- paste("https://",perf.host,"/LoadTest/rest/authentication-point/logout",sep="")
perf.job.url    <- paste("https://",perf.host,"/LoadTest/rest/domains/",perf.domain,"/projects/",perf.project,"/Runs",sep="")
perf.timeslot.url <- paste("https://",perf.host,"/LoadTest/rest/domains/",perf.domain,"/projects/",perf.project,"/timeslots",sep="")
perf.test.url <- paste("https://",perf.host,"/LoadTest/rest/domains/",perf.domain,"/projects/",perf.project,"/tests",sep="")
perf.LWSSO.cookie <- NULL
perf.QCsession.cookie <- NULL
perf.cookies    <- xml_find_all(config,"//perf_center/cookies")
if(length(perf.cookies)>0)
{
  perf.LWSSO.cookie  <- as.character(xml_find_all(config,"//perf_center/cookies/cookie/name[text()='LWSSO_COOKIE_KEY']/following-sibling::value/text()"))
  perf.QCsession.cookie  <- as.character(xml_find_all(config,"//perf_center/cookies/cookie/name[text()='QCSession']/following-sibling::value/text()"))
}
perf.timezone   <- as.character(xml_find_all(config,"//perf_center/timezone/text()"))

# define the prometheus api endpoint
grafana.host      <- as.character(xml_find_all(config,"//grafana/host/text()"))
grafana.endpoint  <- as.character(xml_find_all(config,"//grafana/endpoint/text()"))
grafana.key       <- as.character(xml_find_all(config,"//grafana/key/text()"))
grafana.timezone  <- as.character(xml_find_all(config,"//grafana/timezone/text()"))
sas.instances     <- as.character(xml_find_all(config,"//environment/instance[@role='worker']/text()"))

#define test iterations
iter.testID           <- as.numeric(unlist(as_list(xml_find_all(config,"//iteration/testID/text()"))))
iter.instanceID       <- as.numeric(unlist(as_list(xml_find_all(config,"//iteration/instanceID/text()"))))
iter.runID            <- as.numeric(unlist(lapply(as_list(xml_find_all(config,"//iteration/runID/text()")),function(o){if(o=="NA"){NA}else{o}})))
iter.slotID           <- as.numeric(unlist(lapply(as_list(xml_find_all(config,"//iteration/slotID/text()")),function(o){if(o=="NA"){NA}else{o}})))
iter.con              <- as.numeric(unlist(as_list(xml_find_all(config,"//iteration/con/text()"))))
iter.start            <- unlist(lapply(as_list(xml_find_all(config,"//iteration/start/text()")),function(o){if(o=="NA"){NA}else{o}}))
iter.status           <- unlist(lapply(as_list(xml_find_all(config,"//iteration/status/text()")),function(o){if(o=="NA"){NA}else{o}}))
iter.duration         <- as.numeric(unlist(as_list(xml_find_all(config,"//iteration/duration/text()"))))
iter.desc             <- xml_text(xml_find_all(config,"//iteration/desc/text()"))
iterations       <- data.frame(testID=iter.testID,
                               instanceID=iter.instanceID,
                               duration=iter.duration,
                               con=iter.con,
                               runID=iter.runID,
                               slotID=iter.slotID,
                               start=iter.start,
                               status=iter.status,
                               desc=iter.desc,
                               stringsAsFactors=F)

# define the metric
metrics_name <- xml_name(xml_find_all(config,"//metrics/*"))
global.metrics <-lapply(metrics_name,function(o){
  as.character(xml_find_all(config,paste("//metrics/",o,"/text()")))
})
names(global.metrics) <- metrics_name


#############################################################
##
##   log configuration
##
#############################################################
log.file <- as.character(xml_find_all(config,"//logfile/text()"))
global.error.logger <- create.logger()
global.info.logger <- create.logger()
logfile(global.info.logger) <- log.file
logfile(global.error.logger) <- log.file
level(global.info.logger) <- 'INFO'
level(global.error.logger) <- 'ERROR'

#############################################################
##
##  comand line parameters  
##
#############################################################
options <- c("status","login","logout","metrics","elapse","schedule","log")

# login to performance center
if (args[1] == "login")
{
  a<-scan();
  r<-login()
  if(r$status_code > 200)
  {
    cat("\n\r")
    cat("Login failed, please login again.")
    
  }else
  {
    r
  }
}else
{
  if(length(perf.cookies)==0)
  {
    cat("\n\r Please login first! \n\r")
    quit(save="no",status=-1)
  }
}

# get test status
if (args[1] == "status")
{
  status()
  
}

# get test metrics
if (args[1] == "metrics")
{
  
  if(is.na(args[2]))
  {
    metrics()
  }else
  {

    filename<-args[2]
    metrics(filename)
  }
  
}

# get test elapse

if (args[1] == "elapse")
{
  if(is.na(args[2]))
  {
    elapse()
  }else
  {
    
    filename<-args[2]
    elapse(filename)
  }
}

# schedule a new test 


if (args[1] == "schedule")
{
  schedule()
  
}


# check last ten lines in the log 

if (args[1] == "log")
{
  log()
  
}

# logout from performance center
if (args[1] == "logout")
{
  logout()
}
