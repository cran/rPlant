\name{ListApps}
\alias{ListApps}
\alias{GetAppInfo}
\title{Deployed applications}
\description{
	Functions for listing applications deployed in the iPlant software infrastructure and information about them.  GetCitations is a work in progress, currently only supports "muscle-ranger-2.0".
}
\usage{
ListApps(user.name, token)
GetAppInfo(user.name, token, application, verbose=FALSE)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{application}{name of DE application}
	\item{verbose}{optional screen output that displays all of the results from the api, default = FALSE}
}
\value{
	A list of applications deployed in the DE.
}
\examples{
	# ListApps("User", token)
	# GetAppInfo("User", token, "muscle-ranger-2.0", verbose=T)
}
