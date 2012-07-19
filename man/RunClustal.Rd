\name{RunClustal}
\alias{RunClustal}
\title{ClustalW alignment}
\description{
	This function will submit a ClustalW job to iPlant's DE
}
\usage{
	RunClustal(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, version="ClustalW2-2.1")
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{Name of file to be evaluated on the Discovery Environment (DE)}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{version}{ClustalW version, options include \code{ClustalW2-2.1}, \code{clustalw2-lonestar-2.1}, \code{clustalw-ranger-1.0}}
}
\details{
	The function is a wrapper function to submit ClustalW alignment jobs through the iPlant api.  
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
	# RunClustal(user.name, token, DE.file.name="ex.lp.fasta", job.name="clustalw.Job1", DE.file.path="", version="ClustalW2-2.1")
}	