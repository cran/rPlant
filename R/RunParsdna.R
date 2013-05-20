RunParsdna <- function(user.name, token, DE.file.name, DE.file.path="", job.name=NULL, 
                       nprocs=1, version="phylip-dna-parsimony-lonestar-3.69u1") {

  App <- GetAppInfo(user.name, token, version)[[2]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(user.name,"_",version,"_viaR", sep="")

  myJob<-SubmitJob(user.name, token, application=version, 
                   DE.file.list=list(DE.file.name), DE.file.path=DE.file.path, 
                   input.list=input.list, job.name=job.name, nprocs=nprocs)

  return(myJob)
}
