RunParsprotein <- function(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL) {
  if (is.null(job.name))
    job.name <- paste(user.name, "_Parsprotein_viaAPI", sep="")
  myJob<-SubmitJob(user.name, token, 
                   application="phylip-protein-parsimony-lonestar-3.69", 
                   DE.file.name=DE.file.name, DE.file.path=DE.file.path, 
                   job.name=job.name, nprocs="1")
  return(myJob)
}
