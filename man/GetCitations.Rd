\name{GetCitations}
\alias{GetCitations}
\title{DE citations}
\description{
	Retrieves citation list for variety of analyses
}
\usage{
GetCitations(application, save=FALSE, file="rPlant.bib")
}
\arguments{
	\item{application}{name of DE application}
	\item{save}{optional argument to write citations to file in Bibtex format}
	\item{file}{optional name for written file; default is "rPlant.bib"}
}
\details{
	This function draws a citation list out of the DE's application citation list.  It can be saved to .bib file that is easily read by other document preparation software.  
}
\value{
	A list of citations.
}
\examples{
	#myCitations<-GetCitations("muscle-ranger-2.0", save=FALSE, file="")
	#GetCitations("muscle-ranger-2.0", save=TRUE, file="myCitations.bib")
}
