\name{RunParsprotein}
\alias{RunParsprotein}
\title{Run Phylip Parsimony}
\description{
	This function will submit a parsimony analysis job to Phylip on iPlant's DE
}
\usage{
RunParsprotein(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{Name of file to be evaluated on the Discovery Environment (DE).}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
}
\details{
	The function is a wrapper function for submitting Phylip jobs with amino acid data for a parsimony analysis.  
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
	# RunParsprotein(user.name, token, DE.file.name="your.protein.file", DE.file.path="", job.name="PhylipParsimony2")
}	
