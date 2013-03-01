# Copyright (c) 2012, University of Tennessee
# rPlant directly interacts with iplant's command-line API for the 
# Discovery Environment (DE)

# -- AUTHENTICATION FUNCTIONS -- #
GetToken <- function(user.name, user.pwd, 
                     api=c("iplant", "cipres", "tnrs")) {
  web <- "https://foundation.iplantc.org/auth-v1/"
  if (is.character(api)) {
    if (api == "iplant") {
      curl.call <- getCurlHandle(userpwd=paste(user.name, user.pwd, sep=":"), 
                                 httpauth=1L, ssl.verifypeer=FALSE)
      res <- suppressWarnings(fromJSON(postForm(web, curl=curl.call)))
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
  web <- "https://foundation.iplantc.org/auth-v1/renew"
  if (is.character(api)) {
    if (api == "iplant") {
      curl.call <- getCurlHandle(userpwd=paste(user.name, user.pwd, sep=":"),  
                                 httpauth=1L, ssl.verifypeer=FALSE)
      res <- suppressWarnings(fromJSON(postForm(web, curl=curl.call, 
                              token=token)))
      # Outputs a message renewal success
      res$status  
    }
    else 
      warning("Not yet implemented")
  }
}
# -- END -- #


# -- FILE AND DATA FUNCTIONS -- #
UploadFile <- function(user.name, token, local.file.name, local.file.path="", 
                       file.type) {
  web <- paste("https://foundation.iplantc.org/io-v1/io/", user.name, sep="");
  if (local.file.path == "") {
    res <- suppressWarnings(fromJSON(postForm(web, style="httppost",
                            fileToUpload=fileUpload(local.file.name), 
                            fileType=file.type, 
                            .opts=list(userpwd=paste(user.name, token, 
                            sep=":"), ssl.verifypeer=FALSE, httpauth=AUTH_BASIC,
                            useragent="R", followlocation=TRUE))))
  }
  else {
    res <- suppressWarnings(fromJSON(postForm(web, style="httppost",
                            fileToUpload=fileUpload(paste(local.file.path, 
                            local.file.name, sep="/")), fileType=file.type,
                            .opts=list(userpwd=paste(user.name, token, sep=":"), 
                            ssl.verifypeer=FALSE, httpauth=AUTH_BASIC, 
                            useragent="R", followlocation=TRUE))))
  }
  if (res$status == "error") 
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

RenameFile <- function(user.name, token, old.DE.file.name, new.DE.file.name, 
                       DE.file.path="") {
  web <- "https://foundation.iplantc.org/io-v1/io"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  content <- c()
  content[1] <- "action=rename"
  content[2] <- paste("newName=", new.DE.file.name, sep="")
  val <- charToRaw(paste(content, collapse = "&"))
  res <- suppressWarnings(fromJSON(httpPUT(paste(web, user.name, DE.file.path, 
                          old.DE.file.name, sep="/"), content=val, 
                          curl=curl.call)))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

MoveFile <- function(user.name, token, DE.file.name, DE.file.path="", 
                     DE.end.path="") {
  web <- "https://foundation.iplantc.org/io-v1/io"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  content <- c()
  content[1] <- "action=move"
  content[2] <- paste("newPath=", user.name, "/", DE.end.path, "/", 
                      DE.file.name, sep="")
  val <- charToRaw(paste(content, collapse = "&"))

  res <- suppressWarnings(fromJSON(httpPUT(paste(web, user.name, DE.file.path, 
                          DE.file.name, sep="/"), content=val,curl=curl.call)))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

DeleteFile <- function(user.name, token, DE.file.name, DE.file.path="") {
  web <- "https://foundation.iplantc.org/io-v1/io"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  if (DE.file.path == "")
    res <- suppressWarnings(fromJSON(httpDELETE(paste(web, user.name, 
                            DE.file.name, sep="/"), curl = curl.call)))
  else
    res <- suppressWarnings(fromJSON(httpDELETE(paste(web, user.name, 
                            DE.file.path, DE.file.name, sep="/"), 
                            curl=curl.call)))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

SupportFile <- function(user.name, token) {  
  web <- "https://foundation.iplantc.org/io-v1/data/transforms/"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  res <- suppressWarnings(fromJSON(getForm(paste(web, sep="/"), 
  curl=curl.call)))
  if(res[[1]] == "success") {
    file.types <- c()
    for(i in 1:length(res[[3]])) {
      file.types <- c(file.types, res[[3]][[i]]$name)
    }
  }
  return(file.types)
}
# -- END -- #

# -- DIRECTORY FUNCTIONS -- #
ListDir <- function(user.name, token, DE.dir.path="") {
  web <- "https://foundation.iplantc.org/io-v1/io/list"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  tmp <- suppressWarnings(fromJSON(getForm(paste(web, user.name,
                          DE.dir.path, sep="/"), curl=curl.call)))
  res <- matrix(, length(tmp$result), 2)
  colnames(res) <- c("name", "type")
  for (i in 1:length(tmp$result)) {
    res[i, 1] <- tmp$result[[i]]$name
    res[i, 2] <- tmp$result[[i]]$type
  }
  return(res)
}

MakeDir <- function(user.name, token, DE.dir.name, DE.dir.path="") {
  web <- "https://foundation.iplantc.org/io-v1/io"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  content <- c()
  content[1] <- "action=mkdir"
  content[2] <- paste("dirName=", DE.dir.name, sep="")
  val <- charToRaw(paste(content, collapse = "&"))
  res <- suppressWarnings(fromJSON(httpPUT(paste(web, user.name, 
                          DE.dir.path, sep="/"), content=val, 
                          curl=curl.call)))
 if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)
}

DeleteDir <- function(user.name, token, DE.dir.name, DE.dir.path="") {
  web <- "https://foundation.iplantc.org/io-v1/io"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  if (DE.dir.path == "")
    res <- suppressWarnings(fromJSON(httpDELETE(paste(web, user.name,
                            DE.dir.name, sep="/"), curl=curl.call)))
  else
    res <- suppressWarnings(fromJSON(httpDELETE(paste(web,user.name, 
                            DE.dir.path, DE.dir.name, sep="/"), 
                            curl=curl.call)))
  if (res$status == "error")
    return(paste(res$status, ":", res$message))
  else
    return(res$status)  
}

# -- END -- #


# -- APPLICATION FUNCTIONS -- #
ListApps <- function(user.name, token) {
  web <- "https://foundation.iplantc.org/apps-v1/apps/list"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, 
                             ssl.verifypeer=FALSE)
  tmp <- suppressWarnings(fromJSON(getForm(web, curl=curl.call)))
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
  web <- "https://foundation.iplantc.org/apps-v1/apps/name"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  res <- suppressWarnings(fromJSON(getForm(paste(web, application, sep="/"), 
                          curl=curl.call)))
  if (length(res$result[[1]]$inputs)==0) {
    return(list(application=res$result[[1]]$id, NA))
  } 
  else {
    if (verbose) 
      return(res)
    else {
      app.info<-c()
      for (input in sequence(length(res$result[[1]]$inputs))) {
        app.info <- rbind(app.info, c("input", res$result[[1]]$inputs[[input]]$id,
                          res$result[[1]]$inputs[[input]]$semantics$fileTypes[1]))
      }
      for (output in sequence(length(res$result[[1]]$output))) {
        app.info <- rbind(app.info, c("output", res$result[[1]]$output[[output]]$id,
                          res$result[[1]]$output[[output]]$semantics$fileTypes[1])) 
      }
      colnames(app.info)<-c("kind", "id", "fileType")
      return(list(application=res$result[[1]]$id, app.info))
    }
  }
}
# -- END -- #




# -- JOB FUNCTIONS -- #
SubmitJob <- function(user.name, token, application, DE.file.name, 
                      DE.file.path="", job.name, nprocs=1, args=c()) {
  # Automatically make analyses directory; will not overwrite if already present
  # MakeDir(user.name, token, "analyses", DE.dir.path="")
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  MakeDir(user.name, token, "analyses", DE.dir.path="")
  web <- "https://foundation.iplantc.org/apps-v1/job"
  #clean up so you don't have to repeat code
  if (is.null(args)){  
    content <- c()
    content[1] <- paste("jobName=", job.name, sep="")
    content[2] <- paste("softwareName=", application, sep="")
    content[3] <- "archive=1"
    if (DE.file.path=="") {
      content[4] <- paste("inputSeqs=", "/", user.name, "/", DE.file.name, sep="")
    } 
    else {
      content[4] <- paste("inputSeqs=", "/", user.name, "/", DE.file.path, "/", 
                          DE.file.name, sep="")
    }
    content[5] <- paste("processorCount=", nprocs, sep="")
    content[6] <- paste("archivePath=/", user.name, "/analyses/", job.name, 
                        sep="")
    content[7] <- "requestedTime=24:00:00"
    content[8] <- "outputFormat=fasta"
    content[9] <- "mode=auto"
  }
  else {
    content <- c()
    content[1] <- paste("jobName=", job.name, sep="")
    content[2] <- paste("softwareName=", application, sep="")
    content[3] <- "archive=1"
    if (DE.file.path=="") {
      content[4] <- paste("inputSeqs=", "/", user.name, "/", DE.file.name, 
                          sep="")
    } 
    else {
      content[4] <- paste("inputSeqs=", "/", user.name, "/", DE.file.path, "/", 
                          DE.file.name, sep="")
    }
    content[5] <- paste("processorCount=", nprocs, sep="")
    content[6] <- paste("archivePath=/", user.name, "/analyses/", job.name, 
                  sep="")
    content[7] <- "requestedTime=24:00:00"
    content[8] <- "outputFormat=fasta"
    content[9] <- "mode=auto"
    content[10] <- args
  }

  val <- charToRaw(paste(content, collapse = "&"))
  res <- suppressWarnings(fromJSON(getURLContent(web, curl=curl.call, infilesize=length(val), 
                          readfunction=val, upload=TRUE, customrequest="POST")))
  if (res$status == "success")
    cat("Job submitted. You can check the status of your job using this id:", 
        res$result$id, "\n")
  else
    cat("Error.", res$message, "\n")
  return(res$result$id)
  # also return or print citations
}

CheckJobStatus <- function(user.name, token, job.id, verbose=FALSE) {
  web <- "https://foundation.iplantc.org/apps-v1/job"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  res <- suppressWarnings(fromJSON(getForm(paste(web, job.id, sep="/"), 
                          curl=curl.call)))
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
  web <- "https://foundation.iplantc.org/apps-v1/job"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  for (job in 1:length(job.id)) {
    res <- suppressWarnings(fromJSON(httpDELETE(paste(web, 
                            job.id[job], sep="/"), curl = curl.call)))
    if (res$status == "error")
      return(paste(res$status, ":", res$message))
    else
      return(res$status) 
  } 
}

RetrieveJob <- function(user.name, token, job.id, files, zip=TRUE) {  
  # what if file doesn't exist...make that an option with a return
  web <- "https://foundation.iplantc.org/io-v1/io"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  # only will work on one job.id now
  invisible(capture.output(fileList <- ListJobOutput(user.name, token, 
            job.id)[[1]]))  
  JS <- CheckJobStatus(user.name, token, job.id, verbose=T)
  if (JS$res$status == "ARCHIVING_FINISHED") {
    for (file in 1:length(files)) {
      # if file exists in output then download
      if (files[file] %in% fileList) {  
        out <- suppressWarnings(getForm(paste(web, JS$result$archivePath, "/", 
                                files[file], sep=""), curl=curl.call))
        if (is.raw(out))
          out <- rawToChar(out)
        write(out, file=files[file])
        print(paste("Downloaded", files[file], "to", getwd(), "directory"))
      }
      else
        return(paste(files[file], "is not found within", job.id))
    }
    if (.Platform$OS.type=="windows") {
      zip=FALSE
      invisible(shell(paste("mkdir job_",job.id,sep="")))
      for (i in c(1:length(files))) {
        args <- c(shQuote(files[i]), shQuote(paste("job_",job.id,sep="")))
        system2("xcopy", args, stdout=FALSE)
        file.remove(files[i])
      }
    }
    if (zip) {
      zip(paste("job_",job.id,".zip",sep=""), files=paste(getwd(), files, 
          sep="/"))
      for (i in c(1:length(files))) {
        file.remove(files[i])
      }
    }
  }
  else
    warning("Job is ", JS$res$status)
}

ListJobOutput <- function(user.name, token, job.id) {
  web <- "https://foundation.iplantc.org/apps-v1/job"
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  combRes <- vector("list", length=length(job.id))
  names(combRes) <- paste("job.id", job.id, sep="")
  for (job in 1:length(job.id)) {
    files <- c()
    JS <- CheckJobStatus(user.name, token, job.id[job], verbose=T)
    if (JS$res$status == "ARCHIVING_FINISHED") {
      res <- suppressWarnings(fromJSON(getForm(paste(web, job.id[job], 
                              "output/list", sep="/"), curl=curl.call)))
      print(paste("There are ", length(res$result), "output files for job", 
            job.id))
      for (i in 1:length(res$result)) {
        files <- append(files, res$result[[i]]$name)
      }
    }
    else
      files <- paste("Job is ", JS$res$status)
    combRes[[job]] <- files
  }
  return(combRes)  
}

GetJobHistory <- function(user.name, token, verbose=FALSE) {
  web <- "https://foundation.iplantc.org/apps-v1/jobs/list"
  jobList <- c()
  curl.call <- getCurlHandle(userpwd=paste(user.name, token, sep=":"), 
                             httpauth=1L, ssl.verifypeer=FALSE)
  res <- suppressWarnings(fromJSON(getForm(web, curl=curl.call)))
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
