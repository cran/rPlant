\name{RunQuicktree}
\alias{RunQuicktree}
\title{Quicktree}
\description{
	This function will submit a Quicktree job to iPlant's DE
}
\usage{
RunQuicktree(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, version="quicktree-tree-lonestar-1.1u1")
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{Name of file to be evaluated on the Discovery Environment (DE).}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{version}{\code{quicktree-dm-lonestar-1.1} or \code{quicktree-tree-lonestar-1.1}}
}
\details{
	The function is a wrapper function for several other rPlant functions, but specific to QuickTree runs.  
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
	# RunQuicktree(user.name, token, DE.file.name="ex.lp.fasta", DE.file.path="", job.name="QTrun")
}	
