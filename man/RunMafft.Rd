\name{RunMafft}
\alias{RunMafft}
\title{MAFFT Multiple Sequence Alignment}
\description{
	This function will submit a Mafft job to iPlant's DE
}
\usage{
RunMafft(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, version="mafft-lonestar-6.864u1")
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{name of file to be evaluated on the Discovery Environment (DE).}
	\item{DE.file.path}{optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{the name to give the job being submitted}
	\item{version}{Version 6 "mafft-lonestar-6.864"}
	
}
\details{
	The function is a wrapper function for several other rPlant functions, but specific to MAFFT runs.  
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
	# RunMafft(user.name, token, DE.file.name="ex.lp.fasta")

}	