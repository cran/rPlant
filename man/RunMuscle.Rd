\name{RunMuscle}
\alias{RunMuscle}
\title{MUSCLE alignment}
\description{
	This function will submit a MUSCLE job to iPlant's DE
}
\usage{
        RunMuscle(user.name, token, DE.file.name, DE.file.path="", job.name=NULL,
                  nprocs=1,version="muscle-lonestar-3.8.31u2")
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{name of file to be evaluated on the Discovery Environment (DE).}
	\item{DE.file.path}{optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{the name to give the job being submitted}
	\item{nprocs}{The number of processors to be allocated to the job, default = 1}
	\item{version}{Muscle version, options include \code{muscle-lonestar-3.8.31u2}, \code{Muscle-3.8.31u1}}
	
}
\details{
	The function is a wrapper function for several other rPlant functions, but specific to MUSCLE runs.  
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
	# RunMuscle(user.name, token, DE.file.name="ex.lp.fasta", job.name="muscleJob1")

}	
