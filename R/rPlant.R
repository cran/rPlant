# Copyright (c) 2012, University of Tennessee
# rPlant directly interacts with iplant's command-line API for the Discovery Environment (DE)

# -- AUTHENTICATION FUNCTIONS -- #
GetToken <- function(user.name, user.pwd, 
                     api=c("iplant", "cipres", "tnrs")) {
  web <- "' https://foundation.iplantc.org/auth-v1/"
  if (is.character(api)) {
    if (api == "iplant") {
      curl.string <- paste("curl -X POST -sku '", user.name, ":", user.pwd, 
                           web, sep="")
      res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
      if (res$status == "error") 
        return(res$message)  # returns if error
      else 
        return(res$result$token)  # returns with token
    }
    else 
      warning("Not yet implemented")
  }
}

RenewToken <- function(user.name, user.pwd, token, 
                       api=c("iplant", "cipres", "tnrs")) {
  web <- "' https://foundation.iplantc.org/auth-v1/renew"
  if (is.character(api)) {
    if (api == "iplant") {
      curl.string <- paste("curl -X POST -sku '", user.name, ":", 
                           user.pwd, "' -d 'token=", token, web, sep="")
     res <- suppressWarnings(fromJSON(paste(system(curl.string, intern=TRUE),sep="", collapse="")))
#     res <- fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
      res$status  # Outputs a message renewal success
    }
    else 
      warning("Not yet implemented")
  }
}
# -- END -- #


# -- FILE AND DATA FUNCTIONS -- #
UploadFile <- function(user.name, token, local.file.name, local.file.path="", file.type) {
  web <- "' https://foundation.iplantc.org/io-v1/io/"
  curl.string <- paste("curl -sku '", user.name, ":", token, 
                       "' -F 'fileToUpload=@", local.file.path, 
                       local.file.name, "' -F 'fileType=", 
                       file.type, web, user.name, sep="")

  #Automatically makes two necessary directories
  MakeDir(user.name, token, "analyses", DE.dir.path="")
  MakeDir(user.name, token, "data", DE.dir.path="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))

  #Moves the file that was just uploaded in to the rplant folder
  #MoveFile(user.name, token, local.file.name, DE.file.path="", DE.end.path="/data/")
  if (res$status == "error") 
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

RenameFile <- function(user.name, token, old.DE.file.name, new.DE.file.name, DE.file.path="") {
  web <- "' https://foundation.iplantc.org/io-v1/io/"
  curl.string <- paste("curl -sku '", user.name, ":", token, 
                       "' -X PUT -d 'newName=", 
                       new.DE.file.name, "&action=rename", web, user.name, "/", 
                       DE.file.path, "/", old.DE.file.name, sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (res$status == "error") 
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

MoveFile <- function(user.name, token, DE.file.name, DE.file.path="", DE.end.path="") {
  web <- "' https://foundation.iplantc.org/io-v1/io/"
  curl.string <- paste("curl -sku '", user.name, ":", token, 
                       "' -X PUT -d 'newPath=", user.name, "/", DE.end.path, 
                       "/", DE.file.name, "&action=move", web, user.name, 
                       "/", DE.file.path, "/", DE.file.name, sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

DeleteFile <- function(user.name, token, DE.file.name, DE.file.path) {
  web <- " https://foundation.iplantc.org/io-v1/io/"
  curl.string <- paste("curl -sku '", user.name, ":", token, "' -X DELETE", 
                       web, user.name, "/", DE.file.path, 
                       "/", DE.file.name, "/", sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

SupportFile <- function(user.name, token) {  
  # lists the supported file types -- does not work! It should. 
  web <- "' https://foundation.iplantc.org/io-v1/data/transforms/"
  curl.string <- paste("curl -X GET -sku '", user.name, ":", token, web, sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE), sep="", collapse="")))
  if(res[[1]] == "success"){
    file.types<-c()
    for(i in 1:length(res[[3]])){
      file.types<-c(file.types, res[[3]][[i]]$name)
    }
  }
  return(file.types)
}
# -- END -- #

# -- DIRECTORY FUNCTIONS -- #
ListDir <- function(user.name, token, DE.dir.path="") {
  web <- "' https://foundation.iplantc.org/io-v1/io/list/"
  curl.string <- paste("curl -sku '", user.name, ":", token, web, user.name, 
                       "/", DE.dir.path, sep="")
  tmp <- suppressWarnings(fromJSON(paste(system(curl.string, intern=TRUE),sep="", collapse="")))
  res <- matrix(, length(tmp$result), 2)
  colnames(res) <- c("name", "type")
  for (i in 1:length(tmp$result)){
    res[i, 1] <- tmp$result[[i]]$name
    res[i, 2] <- tmp$result[[i]]$type
  }
  return(res)
}

MakeDir <- function(user.name, token, DE.dir.name, DE.dir.path="") {
  web <- "https://foundation.iplantc.org/io-v1/io/"
  curl.string <- paste("curl -sku '", user.name, ":", token, 
                       "' -X PUT -d 'dirName=", DE.dir.name, "&action=mkdir' ", 
                       web, user.name, "/", DE.dir.path, sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

DeleteDir <- function(user.name, token, DE.dir.name, DE.dir.path) {
  web <- "https://foundation.iplantc.org/io-v1/io/"
  curl.string <- paste("curl -sku '", user.name, ":", token, "' -X DELETE ", 
                       web,  user.name, "/", DE.dir.path, "/", DE.dir.name, sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)  
}

# -- END -- #


# -- APPLICATION FUNCTIONS -- #
ListApps <- function(user.name, token) {
  web <- "' https://foundation.iplantc.org/apps-v1/apps/share/list"
  curl.string <- paste("curl -sku '", user.name, ":", token, web, sep="")
  tmp <- suppressWarnings(fromJSON(paste(system(curl.string, intern=TRUE), 
                          sep="", collapse="")))
  res <- matrix(, length(tmp$result))
  colnames(res) <- "Application"
  for (i in 1:length(tmp$result))
    res[i, 1] <- tmp$result[[i]]$id
  return(sort(res))
}

GetAppInfo <- function(user.name, token, application, verbose=FALSE) {
  # This needs to be cleaned up. I think the relevant info is 
        # a) inputs, 
        # b) possible input parameters, and 
        # c) outputs
  web <- "' https://foundation.iplantc.org/apps-v1/apps/share/name/"
  curl.string <- paste("curl -X GET -sku '", user.name, ":", token, web, 
                       application, sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (length(res$result[[1]]$inputs)==0){return(list(application=res$result[[1]]$id, NA))}else{
  if (verbose)
    return(res)
  else
    app.info<-c()
    for (input in sequence(length(res$result[[1]]$inputs)))
      app.info <- rbind(app.info, c("input", res$result[[1]]$inputs[[input]]$id,
                        res$result[[1]]$inputs[[input]]$semantics$fileTypes[1]))
    for (output in sequence(length(res$result[[1]]$output)))
      app.info <- rbind(app.info, c("output", res$result[[1]]$output[[output]]$id,
                        res$result[[1]]$output[[output]]$semantics$fileTypes[1])) 
                        # this seems to vary depending on the application
    colnames(app.info)<-c("kind", "id", "fileType")
    return(list(application=res$result[[1]]$id, app.info))
  }
}
# -- END -- #


# -- JOB FUNCTIONS -- #
SubmitJob <- function(user.name, token, application, DE.file.name, DE.file.path="", job.name, nprocs=1, args=c()) {
  #Automatically make analyses directory; will not overwrite if already present
  MakeDir(user.name, token, "analyses", DE.dir.path="")
  web <- "https://foundation.iplantc.org/apps-v1/job"
    
  curl.string <- paste("curl -X POST -sku '", user.name, ":", token, 
                       "' -d 'jobName=", job.name, "&softwareName=",  
                       application, "&archive=1&inputSeqs=", "/", 
                       user.name, "/", DE.file.path, "/", DE.file.name, 
                       "&processorCount=", nprocs, "&archivePath=/", 
                       user.name, "/analyses/", job.name, 
                       "&requestedTime=24:00:00&outputFormat=fasta&mode=auto", args, "' ", #need to paste in args here
                       web, sep="")
  res <- fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
  if (res$status == "success")
    cat("Job submitted. You can check the status of your job using this id:", 
        res$result$id, "\n")
  else
    cat("Error.", res$message, "\n")
    return(res$result$id)
  # also return or print citations
}

CheckJobStatus <- function(user.name, token, job.id, verbose=FALSE) {
  web <- "' https://foundation.iplantc.org/apps-v1/job/"
  curl.string <- paste("curl -X GET -sku '", user.name, ":", token, web, job.id, 
                       sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (res$status == "error") {
    print(paste("Error in job.id ", job.id, ":", res$message))
    if (verbose) 
      return(res)
  }
  else {
    if (verbose)
      return(res)
    else
      return(res$result$status)
  }
}

DeleteJob <- function(user.name, token, job.id) {
  web <- "' https://foundation.iplantc.org/apps-v1/job/"
  for (job in 1:length(job.id)) {
    curl.string <- paste("curl -X DELETE -sku '", user.name, ":", token, web, 
                         job.id[job], sep="")
    res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
    if (res$status == "success")
      print(paste("job.id", job.id[job], "was successfully deleted"))
    else
      print(paste("job.id", job.id[job], res$status, ":", res$message))
  }
}

RetrieveJob <- function(user.name, token, job.id, files, zip=TRUE) {  
  # what if file doesn't exist...make that an option with a return
  web <- "' https://foundation.iplantc.org/io-v1/io"
  fileList <- ListJobOutput(user.name, token, job.id)[[1]]  # only will work on one job.id now
  JS <- CheckJobStatus(user.name, token, job.id, verbose=T)
  if (JS$res$status == "ARCHIVING_FINISHED") {
    for (file in 1:length(files)) {
      if (files[file] %in% fileList) {  # if file exists in output then download
        curl.string <- paste("curl -X GET -sku '", user.name, ":", token, web, 
                             JS$result$archivePath, "/", files[file], " -o ", 
                             files[file], sep="")
        res <- suppressWarnings(paste(system(curl.string, intern=TRUE),sep="", collapse=""))
        print(paste("Downloaded", files[file], "to", getwd(), "directory"))
      }else{
        return(paste(files[file], "is not found within", job.id))
      }
    }
    if (zip) {
      zip(paste("job.",job.id,".zip",sep=""), files=paste(getwd(), files, sep="/"))
      for (i in c(1:length(files))){file.remove(files[i])}
    }
  }else{
    warning("Job is ", JS$res$status)
  }
}

ListJobOutput <- function(user.name, token, job.id) {
  web <- "' https://foundation.iplantc.org/apps-v1/job/"
  combRes <- vector("list", length=length(job.id))
  names(combRes) <- paste("job.id", job.id, sep="")
  for (job in 1:length(job.id)) {
    files <- c()
    JS <- CheckJobStatus(user.name, token, job.id[job], verbose=T)
    if (JS$res$status == "ARCHIVING_FINISHED") {
      curl.string <- paste("curl -X GET -sku '", user.name, ":", token, web, 
                           job.id[job], "/output/list", sep="")
      res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", 
                            collapse="")))
      print(paste("There are ", length(res$result), "output files for job", 
            job.id))
      for (i in 1:length(res$result))
        files <- append(files, res$result[[i]]$name)
    }
    else
      files <- paste("Job is ", JS$res$status)
    combRes[[job]] <- files
  }
  return(combRes)  
}

GetJobHistory <- function(user.name, token, verbose=FALSE) {
  web <- "' https://foundation.iplantc.org/apps-v1/jobs/list"
  jobList <- c()
  curl.string <- paste("curl -X GET -sku '", user.name, ":", token, web, sep="")
  res <- suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
  if (verbose) 
    return(res)
  if (length(res$result) != 0) {
    for (i in 1: length(res$result)) {
      job <- c(res$result[[i]]$id, res$result[[i]]$name, 
               res$result[[i]]$software, res$result[[i]]$status)  
      jobList <- rbind(jobList, job)
      colnames(jobList) <- c("job.id", "job.name", "application", "status")
    }  
  }
  return(jobList)
}
# -- END -- #







