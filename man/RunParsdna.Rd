\name{RunParsdna}
\alias{RunParsdna}
\title{Run Phylip Parsimony}
\description{
	This function will submit a parsimony analysis job to Phylip on iPlant's DE
}
\usage{
        RunParsdna(user.name, token, DE.file.name, DE.file.path="", job.name=NULL,
                   nprocs=1,version="phylip-dna-parsimony-lonestar-3.69u1")
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{Name of file to be evaluated on the Discovery Environment (DE).}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{nprocs}{The number of processors to be allocated to the job, default = 1}
	\item{version}{DNA parsimony version, options include \code{phylip-dna-parsimony-lonestar-3.69u1}, \code{phylip-dna-parsimony-lonestar-3.69u2}}
}
\details{
	The function is a wrapper function for submitting Phylip jobs with nucleotide data for a parsimony analysis.  
}
\seealso{
	\code{\link{SubmitJob}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
	# RunParsdna(user.name, token, DE.file.name="ex.lp.fasta", job.name="PhylipParsimony1")
}	
