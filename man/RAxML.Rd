\name{RAxML}
\alias{RAxML}
\alias{RAXML}
\alias{Raxml}
\alias{raxml}
\title{RAxML}
\description{
	RAxML (Randomized Accelerated Maximum Likelihood) is a program for sequential and parallel Maximum Likelihood based inference of large phylogenetic tress.  It has originall been derived from from fastDNAml which in turn was derived from Joe Felsentein's dnaml which is part of the PHYLIP package. See \url{http://bioinformatics.oxfordjournals.org/content/suppl/2014/01/18/btu033.DC1/NewManual.pdf} for details.
}
\usage{
RAxML(file.name, file.path="", type="DNA", out.name=NULL, 
      model=NULL, bootstrap=NULL, algorithm="d", rseed=NULL,
      multipleModelFileName=NULL, args=NULL, numcat=25, nprocs=12,
      job.name=NULL, print.curl=FALSE, shared.username=NULL, 
      substitution.matrix=NULL, empirical.frequencies=FALSE,
      suppress.Warnings=FALSE, email=TRUE)
}
\arguments{
	\item{file.name}{Name of file to be evaluated on the Discovery Environment (DE), see details for supported input formats.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{out.name}{the name to give the output files}
	\item{type}{Two options "PROTEIN" or "DNA".  This defines the type of sequences in the file, either proteins or nucleotides.}
        \item{rseed}{(-p) required seed number, to replicate set this integer, otherwise it will randomly chosen}
        \item{model}{(-m) Substitution model.  For DNA the choices are \code{GTRCAT}, \code{GTRGAMMA}, \code{GTRCATI} and \code{GTRGAMMAI}.  For protein the choices are \code{PROTCAT}, \code{PROTGAMMA}, \code{PROTCATI} and \code{PROTGAMMAI}.  The details p. 10-11 of manual.}
	\item{substitution.matrix}{This is only necessary for the protein alignments.  The choices are: \code{DAYHOFF}, \code{DCMUT}, \code{JTT}, \code{MTREV}, \code{WAG}, \code{RTREV}, \code{CPREV}, \code{VT}, \code{BLOSUM62}, \code{MTMAM}, \code{LG}, \code{MTART}, \code{MTZOA}, \code{PMB}, \code{HIVB}, \code{HIVW}, \code{JTTDCMUT}, \code{FLU} and \code{GTR}, the default is \code{BLOSUM62}}
	\item{empirical.frequencies}{(F) Only used for protein alighnments, with appendix you can specify if you want to use empirical base frequencies.}
	\item{bootstrap}{(-b) Random Seed Number for non-parametric bootstrapping, details bottom of p. 7 of manual}
	\item{algorithm}{(-f) Select the type of algorithm/function you want. "d" is default, details bottom of p. 8 of manual}

	\item{multipleModelFileName}{(-q) substitution model file, details bottom of p. 7 of manual}
	\item{numcat}{(-c) Specify the number of distinct rate categories, details top of p. 13 of manual}
	\item{nprocs}{The number of processors to be allocated to the job, default = 12}
	\item{job.name}{the name to give the job being submitted}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.  Then the dir.path becomes the path to the SHARED user's shared folder.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{email}{By default an email will be sent to the user when the job finishes.}
	\item{args}{Optional for arguments (i.e. flags).  The RAxML model has much additional functionality that is not fit into this wrapper function (\url{http://bioinformatics.oxfordjournals.org/content/suppl/2014/01/18/btu033.DC1/NewManual.pdf}), see details.  This option allows users to add anything that is not included (.i.e. args="-d"), which starts the RAxML search with a random starting tree, see detals.}
}
\details{
	The input file format that is supported is the interleaved phylip format \url{http://www.bioperl.org/wiki/PHYLIP_multiple_alignment_format}.

	Additional arguments, \code{args}, can be found at \url{http://bioinformatics.oxfordjournals.org/content/suppl/2014/01/18/btu033.DC1/NewManual.pdf}.  The args input is text with the flags and inputs for those flags in a string like on the command line.

	For this application there are numerous output files.  See pg 16-17 of the manual for complete details.  RAxML outputs trees in Newick format \url{http://en.wikipedia.org/wiki/Newick_format}.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}, \link{Validate}, \link{UploadFile}}
}
\examples{
\dontrun{data(phylip_interleaved_dna.aln)}
\dontrun{write.table(phylip_interleaved_dna.aln, "phylip_interleaved_dna.aln", 
                     quote=FALSE, row.names=FALSE, col.names=FALSE)}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("phylip_interleaved_dna.aln")}
\dontrun{myJobRaxD <- RAxML("phylip_interleaved_dna.aln", job.name="raxmlDNAphy")}
}	
