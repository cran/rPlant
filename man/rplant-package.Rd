\name{rplant-package}
\alias{rplant-package}
\alias{rplant}
\alias{rPlant}
\docType{package}
\title{rplant}
\description{
	R interface to iPlant's Discovery Enviornment and the Taxonomic Name Resolution Service (TNRS)
}
\details{
	The iPlant Collaborative has developed many resources to deal with the emerging computational challenges facing biology. Users have access to many different applications for data analysis, including clustering/network analyses, QTL mapping, sequence alignments, phylogenetic tree building, and comparative methods. The 'rPlant' package provides a direct link between iPlant's API and the R environment. 'rPlant' users must be registered with iPlant and have a valid user name and password.  If you do not have an account yet, visit \url{http://user.iplantcollaborative.org/} and register.

	\tabular{ll}{
	Package: \tab rplant\cr
	Type: \tab Package\cr
	Version: \tab 3.0.0\cr
	Date: \tab 2014-6-5\cr
	License: \tab GPL (>= 2)\cr}
}
\seealso{
	\code{\link{Validate}, \link{UploadFile}, \link{Muscle}}
}
\examples{
\dontrun{data(PROTEIN.fasta)}
\dontrun{write.fasta(sequences = PROTEIN.fasta, names = names(PROTEIN.fasta), 
                     file.out = "PROTEIN.fasta")}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("PROTEIN.fasta", filetype="FASTA-0")}
\dontrun{SubmitJob(application="Muscle-3.8.32u4", file.list=list("PROTEIN.fasta"), 
                   input.list=list("stdin"), args.list=list(c("arguments", "-phyiout")),
                   job.name="MuscleSJAA")}

### Or, one can use one of the wrapper functions. ###
\dontrun{Muscle("PROTEIN.fasta", aln.filetype="PHYLIP_INT", job.name="muscleAA")}
}
