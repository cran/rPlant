\name{ListApps}
\alias{ListApps}
\alias{GetAppInfo}
\title{Deployed applications}
\description{
	Functions for listing applications deployed in the iPlant software infrastructure and information about them.
}
\usage{
ListApps(description=FALSE, print.curl=FALSE, suppress.Warnings = FALSE)
GetAppInfo(application, return.json=FALSE, print.curl=FALSE)
}
\arguments{
	\item{application}{name of DE application}
	\item{description}{when the description=TRUE, then the \code{ListApps} function will include a brief description of the app, default = FALSE}
	\item{return.json}{optional screen output that displays all of the results from the api, default = FALSE}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\value{
	The function \code{ListApps} returns a list of sorted applications and a short description.  The applications are all verified to be public applications and they are the newest version.  All of the public applications in that list can be used in the \code{SubmitJob} function.  The \code{GetAppInfo} function gives critical information on the \code{application} that is needed in the \code{SubmitJob} function.   A list of information is outputted.  The first element gives a short description of the application.  The second element in the list gives basic information on the application including is it a public application and if it is the newest version.  Both are important information.  If the application is a private application it can only be run by the person who submitted the application to the Agave API, and clearly you want to run the newest version of the public application.

	The third element in the list the matrix outputted gives four columns of information.  The first column, labeled '\code{kind}, tells the "\code{input}", sometimes the "\code{output}" and "\code{parameters}" from the application.  The second column, labeled \code{id}, give the name of the "\code{input}", etc.  For example, \code{GetAppInfo("velveth-1.2.07u1")$Information}, the 'kind' column states there are six inputs for this app, and the 'id' column the names of those inputs are "\code{reads5}","\code{reads3}", etc.  There are also eight parameters for the app, the paramters are \code{format2}, etc.  The third column in the matrix is '\code{fileType/value}.  For the \code{input} this tells the file type which is important because if the wrong \code{fileType} is inputted into the function, then the function will not work.  For the \code{parameters} the third column contains the type of input necessary for the parameters, common ones are string, boolean, etc.  The last column gives brief details on each input.
}
\seealso{
	\code{\link{SubmitJob}}
}
\examples{
	\dontrun{ListApps()}
	\dontrun{GetAppInfo("Muscle-3.8.32u4")}
	\dontrun{GetAppInfo("velveth-1.2.07u1")}
}
