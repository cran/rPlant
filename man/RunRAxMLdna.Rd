\name{RunRAxMLdna}
\alias{RunRAxMLdna}
\title{RAxML}
\description{
	This function will submit a RAxML job to iPlant's DE
}
\usage{
RunRAxMLdna(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, model="GTRCAT", bootstrap=NULL, numcat=25, nprocs=1)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{Name of file to be evaluated on the Discovery Environment (DE).}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{model}{Substitution model.  Choices are \code{GTRCAT}, \code{GTRGAMMA}, \code{GTRCAT.GAMMA}, \code{GTRGAMMAI}, \code{GTRCAT.GAMMAI}}
	\item{bootstrap}{Random Seed Number for non-parametric bootstrapping}
	\item{numcat}{Number of distinct rate categories}
	\item{nprocs}{the number of processors to be allocated to the job, default = 1}
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
	# RunRAxMLdna(user.name, token, DE.file.name="ex.lp.fasta", DE.file.path="",  model="GTRCAT", bootstrap=NULL, numcat=25, nprocs=1)

}	
