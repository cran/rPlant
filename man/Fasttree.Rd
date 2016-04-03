\name{Fasttree}
\alias{Fasttree}
\alias{FastTree}
\alias{fasttree}
\title{FastTree Dispatcher}
\description{
	FastTree infers approximately-maximum-likelihood phylogenetic trees from alignments of nucleotide or protein sequences. See \url{http://meta.microbesonline.org/fasttree/}
}
\usage{
Fasttree(file.name, file.path="", job.name=NULL, out.name=NULL, args=NULL,
         type="DNA", model=NULL, gamma=FALSE, stat=FALSE, print.curl=FALSE, 
         shared.username=NULL, suppress.Warnings=FALSE)
}
\arguments{
	\item{file.name}{name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{out.name}{the name given to the output tree (default FastTree_OutTree.nwk)}
	\item{type}{Two options "PROTEIN" or "DNA".  This defines the type of sequences in the file, either proteins or nucleotides.}
	\item{model}{Substitution model.  For DNA the choices are \code{GTRCAT}, and \code{JCCAT} (default), the Jukes-Cantor + CAT model.  For protein the choices are \code{JTTCAT} (default), and \code{WAGCAT}.}
	\item{gamma}{(-gamma) Use this option (about 5\% slower) if you want to rescale the branch lengths and compute a Gamma20-based likelihood. Gamma likelihoods are more comparable across runs (default is FALSE).}
	\item{stat}{(-log) This allows for statistical comparisons (when gamma=TRUE) of the likelihood of different topologies.  The result file is `logfile'.}
	\item{job.name}{the name to give the job being submitted}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{args}{Optional for arguments (i.e. flags).  The Fasttree model has much additional functionality that is not fit into this wrapper function (\url{http://meta.microbesonline.org/fasttree/#Usage}), see details.  This option allows users to add anything that is not included, see details.}
}
\details{
	The input file formats that are supported are fasta format \url{http://en.wikipedia.org/wiki/FASTA_format} or interleaved phylip format \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}.

	Additional arguments, \code{args}, can be found at \url{http://meta.microbesonline.org/fasttree/}.  The args input is text with the flags and inputs for those flags in a string like on the command line.

	Fasttree outputs trees in Newick format \url{http://en.wikipedia.org/wiki/Newick_format}. The placement of the root is not biologically meaningful. The local support values are given as names for the internal nodes, and range from 0 to 1, not from 0 to 100 or 0 to 1,000. If all sequences are unique, then the tree will be fully resolved (the root will have three children and other internal nodes will have two children). If there are multiple sequences that are identical to each other, then there will be a multifurcation. Also, there are no support values for the parent nodes of redundant sequences. 
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}, \link{Validate}, \link{UploadFile}}
}
\examples{

\dontrun{data(fasta_aa.aln)}
\dontrun{write.table(fasta_aa.aln, "fasta_aa.aln", quote=FALSE, row.names=FALSE, col.names=FALSE)}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("fasta_aa.aln")}
\dontrun{myJobFaP <- Fasttree("phylip_interleaved_aa.aln", job.name="fasttreeAAphy")}
}	
