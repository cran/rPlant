RunRAxMLdna <- function(user.name, token, DE.file.name="", 
                        DE.file.path="", outputFileName=NULL, model="GTRCAT", 
                        bootstrap=NULL, algorithm="d", multipleModelFileName=NULL, 
                        numcat=25, nprocs=1) {
  if (is.null(outputFileName))
    outputFileName <- paste(user.name, "_RAxMLdna_", model, "viaAPI", sep="")

  #initialize arguments
  args <- paste("arguments=-m", model)
  #args <- append(args, c("-T", numberOfThreads))
  if (!is.null(bootstrap)) 
    args <- append(args, c("-b", bootstrap))
  args <- append(args, c("-f", algorithm))
  args <- append(args, c("-p", floor(runif(1, 1, 10^6))))
  #args <- append(args, c("-#", numberOfRuns))
  args <- append(args, c("-c", numcat))
  args <- append(args, c("-s", DE.file.name))
  args <- append(args, c("-n", outputFileName))
  if(!is.null(multipleModelFileName))
    args <- append(args, c("-q", multipleModelFileName))
  
 
  # Submit
  SubmitJob(user.name, token, "raxml-lonestar-7.2.8", DE.file.name, 
            DE.file.path, job.name=outputFileName, nprocs, paste(args, collapse=" "))
}
