\name{ListApps}
\alias{ListApps}
\alias{GetAppInfo}
\title{Deployed applications}
\description{
	Functions for listing applications deployed in the iPlant software infrastructure and information about them.  GetCitations is a work in progress, currently only supports "muscle-ranger-2.0".
}
\usage{
ListApps(user.name, token)
GetAppInfo(user.name, token, application, verbose=FALSE, print.curl=FALSE)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{application}{name of DE application}
	\item{verbose}{optional screen output that displays all of the results from the api, default = FALSE}
	\item{print.curl}{Prints the Curl Statement that can be used in the terminal}
}
\value{
	A list of applications deployed in the DE.  GetAppInfo, for verbose=F gives critical information on the Application is used in the SubmitJob function.  Let's use the application "velveth-1.2.07u1" as an example.  The matrix outputted for verbose=F gives three columns of information.  The first column, labeled 'kind', tells the "input" and sometimes the "output" from the application.  The second column, labeled 'id', give the name of the "input", or the name of the output.  For example, \code{GetAppInfo(user.name, token, "velveth-1.2.07u1")[[2]]}, the 'kind' column states there are six inputs for this app, and the 'id' column the names of those inputs are "reads5","reads3", etc.  The last column in the matrix is 'fileType'.  This is important, because if the wrong fileType is inputted into the function, then the function will not work.
}
\examples{
	# ListApps("User", token)
	# GetAppInfo("User", token, "muscle-ranger-2.0", verbose=T)
}
