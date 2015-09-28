\name{TNRS}
\alias{TNRS}
\alias{ResolveNames}
\alias{GetPhylotasticToken}
\alias{RetrieveTNRSNames}
\alias{CompareNames}
\title{Resolving Names via TNRS}
\description{
	The function \code{ResolveNames} interacts with the TNRS server at iPlant that uses fuzzy name matching to find standardized taxonomic plant names. \code{GetPhylotasticToken} is similar, but can be used for animal taxa as well, since it also utilizes the NCBI and ITIS databases. \code{GetPhylotasticToken} returns a unique token that can be checked online or using the \code{RetrieveTNRSNames} function. \code{CompareTNRS} will compare the original list of names to the returned TNRS names to see which names changed.
}
\usage{
ResolveNames(names, max.per.call=100, verbose=TRUE) 
GetPhylotasticToken(names, max.per.call=100, verbose=TRUE) 
RetrieveTNRSNames(names, token, source=c("iPlant_TNRS", "NCBI"),
                  match.threshold=0.5, verbose=FALSE)
CompareNames(old.names, new.names, verbose=TRUE)
}
\arguments{
	\item{names}{Vector of names to be resolved via TNRS}
	\item{max.per.call}{The number of names to submit at a time, default is 100}
	\item{verbose}{Optional screen output that displays all of the results from the api}
	\item{token}{Unique identifier from the GetPhylotasticToken function used to retrieve names}
	\item{source}{Which source to utilize to check names}
	\item{match.threshold}{Threshold to accept new name}
	\item{old.names}{Original names}
	\item{new.names}{Returned TNRS names}
}
\value{
	Vector of taxonomic names for \code{ResolveNames} and \code{RetrieveTNRSNames}. Unique identifier (=token) for \code{GetPhylotasticToken}
}
\examples{
data(DNA.fasta)
speciesNames <- names(DNA.fasta)

#Taxonomic name checking by iPlant TNRS
\dontrun{TNRSspeciesNames <- ResolveNames(speciesNames, 100, verbose=F)}

#Taxonomic name checking by Phylotastic Taxosaurus
token <- GetPhylotasticToken(speciesNames)
TNRSSpeciesNames <- RetrieveTNRSNames(speciesNames, token, "NCBI")
CompareNames(speciesNames, TNRSSpeciesNames, verbose=TRUE)

}
