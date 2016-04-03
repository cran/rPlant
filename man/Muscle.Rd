\name{Muscle}
\alias{Muscle}
\title{MUSCLE alignment}
\description{
	MUSCLE is a program for creating multiple alignments of amino acid or nucleotide sequences. A range of options is provided that give you the choice of optimizing accuracy, speed, or some compromise between the two.  The manual is also available here: \url{http://www.drive5.com/muscle/manual/} 
}
\usage{
Muscle(file.name, file.path="", job.name=NULL, args=NULL,
       aln.filetype="PHYLIP_INT", shared.username=NULL, out.name=NULL,
       suppress.Warnings=FALSE, print.curl=FALSE)
}
\arguments{
	\item{file.name}{name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{the name to give the job being submitted}
	\item{out.name}{name of the output file}
	\item{aln.filetype}{Muscle does alignment of sequences, this option selects the file type of that result file.  There are six options \code{PHYLIP_INT}, \code{PHYLIP_SEQ}, \code{HTML}, \code{FASTA}, \code{CLUSTALW}, \code{MSF}, see details}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{args}{Optional for arguments (i.e. flags).  The Muscle model has much additional functionality that is not fit into this wrapper function (\url{http://www.drive5.com/muscle/muscle_userguide3.8.html#_Toc260497051}), see details.  This option allows users to add anything that is not included (.i.e. args="-spscore"), which computes SP objective score, see details.}
}
\details{
	The supported input file format is the fasta format \url{http://en.wikipedia.org/wiki/FASTA_format}.

	Additional arguments, \code{args}, can be found at \url{http://www.drive5.com/muscle/muscle_userguide3.8.html#_Toc260497051}.  The args input is text with the flags and inputs for those flags in a string like on the command line.

	There are six possible alignment output files, named appropriately: \code{phylip_interleaved.aln} \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}, \code{phylip_sequential.aln} \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}, \code{html.aln} \url{http://sonnhammer.sbc.su.se/Belvu.html},\code{fasta.aln} \url{http://en.wikipedia.org/wiki/FASTA_format}, \code{clustalw.aln} \url{http://meme.nbcr.net/meme/doc/clustalw-format.html}, \code{msf.aln} \url{http://en.wikipedia.org/wiki/MSF}.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}, \link{Validate}, \link{UploadFile}}
}
\examples{
\dontrun{data(PROTEIN.fasta)}
\dontrun{write.fasta(sequences = PROTEIN.fasta, names = names(PROTEIN.fasta),
                     file.out = "PROTEIN.fasta")}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("PROTEIN.fasta", filetype="FASTA-0")}
\dontrun{Muscle("PROTEIN.fasta", aln.filetype="FASTA", job.name="muscleAAfasta")}
}	
