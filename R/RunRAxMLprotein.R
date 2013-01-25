RunRAxMLprotein <- function(user.name, token, DE.file.name="", DE.file.path="", 
                            job.name=NULL, model="PROTCAT", bootstrap=NULL, 
                            numcat=25, nprocs=1) {
  if (is.null(job.name))
    job.name <- paste(user.name, "_RAxMLprotein_", model, "viaAPI", sep="")

  #initialize arguments
  args <- paste("arguments=-m", model)
  if (!is.null(bootstrap)) 
    args <- append(args, c("-b", bootstrap))
  args <- append(args, c("-c", numcat))
  args <- paste(args, collapse=" ")  # make a single statement
 
  # Submit
  SubmitJob(user.name, token, "raxml-lonestar-7.2.8", DE.file.name, DE.file.path, 
            job.name, nprocs, args)
}
