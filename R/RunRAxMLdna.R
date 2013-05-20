RunRAxMLdna <- function(user.name, token, DE.file.name="", DE.file.path="", 
                        job.name=NULL, model="GTRCAT", bootstrap=NULL, 
                        algorithm="d", multipleModelFileName=NULL, 
                        numcat=25, nprocs=2, version="raxml-lonestar-7.2.8u1") {
  if (is.null(job.name)) {
    job.name <- paste(user.name, "_RAxMLdna_", model, "_viaR", sep="")
  }

  App <- GetAppInfo(user.name, token, version)[[2]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  #initialize arguments
  args <- paste("arguments=-m", model)
  #args <- append(args, c("-T", numberOfThreads))
  if (!is.null(bootstrap)) {
    args <- append(args, c("-b", bootstrap))
  }
  args <- append(args, c("-f", algorithm))
  args <- append(args, c("-p", floor(runif(1, 1, 10^6))))
  #args <- append(args, c("-#", numberOfRuns))
  args <- append(args, c("-c", numcat))
  args <- append(args, c("-s", DE.file.name))
  args <- append(args, c("-n", job.name))
  if(!is.null(multipleModelFileName)) {
    args <- append(args, c("-q", multipleModelFileName))
  }
  args <- paste(args, collapse=" ")  # make a single statement
 
  # Submit
  myJob<-SubmitJob(user.name, token, application=version, 
                   DE.file.list=list(DE.file.name), DE.file.path=DE.file.path, 
                   input.list=input.list, job.name=job.name, nprocs=nprocs)

}
