\name{ClustalW}
\alias{ClustalW}
\alias{Clustal}
\alias{clustal}
\title{ClustalW alignment}
\description{
	An approach for performing multiple alignments of large numbers of amino acid or nucleotide sequences is described. The method is based on first deriving a phylogenetic tree from a matrix of all pairwise sequence similarity scores, obtained using a fast pairwise alignment algorithm.  See details on \url{http://www.clustal.org/clustal2/}.
}
\usage{
ClustalW(file.name, file.path="", type="DNA", aln.filetype="CLUSTALW", 
         args=NULL, out.name=NULL, job.name=NULL, print.curl=FALSE,   
         shared.username=NULL, suppress.Warnings=FALSE, email=TRUE)
}
\arguments{
	\item{file.name}{Name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{type}{Two options "PROTEIN" or "DNA".  This defines the type of sequences in the file}
	\item{aln.filetype}{ClustalW does alignment of sequences, this option selects the file type of that result file.  There are seven options \code{CLUSTALW}, \code{FASTA}, \code{PHYLIP_INT}, \code{NEXUS}, \code{GCG}, \code{GDE},  and \code{PIR}}
	\item{args}{Optional for arguments (i.e. flags).  The ClustalW model has much additional functionality that is not fit into this wrapper function (\url{http://www.clustal.org/download/clustalw_help.txt}), see details.  This option allows users to add anything that is not included (.i.e. args="-ITERATION=TREE"), to iterate at each step, see details.}
	\item{out.name}{The name given to the output filename}
	\item{job.name}{The name to give the job being submitted}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{email}{By default an email will be sent to the user when the job finishes.}
}
\seealso{
	\code{\link{SubmitJob}, \link{Validate}, \link{UploadFile}}
}
\details{
	The supported input file format is the fasta format \url{http://en.wikipedia.org/wiki/FASTA_format}.

	Additional arguments, \code{args}, can be found at \url{http://www.clustal.org/download/clustalw_help.txt}.  The args input is text with the flags and inputs for those flags in a string like on the command line.

	There are seven options for output files: \code{CLUSTALW} \url{http://meme.nbcr.net/meme/doc/clustalw-format.html}, \code{FASTA} \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}, \code{PHYLIP_INT} \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}, \code{NEXUS} \url{http://en.wikipedia.org/wiki/Nexus_file}, \code{GCG} \url{http://www.genomatix.de/online_help/help/sequence_formats.html#GCG}, \code{GDE} \url{http://www.cse.unsw.edu.au/~binftools/birch/GDE/overview/GDE.file_formats.html},  and \code{PIR} \url{http://www.bioinformatics.nl/tools/crab_pir.html}.  
	
	The result file is ALWAYS `clustalw2.fa'.	
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\examples{
\dontrun{data(DNA.fasta)}
\dontrun{write.fasta(sequences = DNA.fasta, names = names(DNA.fasta), file.out = "DNA.fasta")}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("DNA.fasta", filetype="FASTA-0")}
\dontrun{ClustalW("DNA.fasta", job.name="ClustalWPHY", aln.filetype="PHYLIP_INT")}
}	
