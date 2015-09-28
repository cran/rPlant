\name{Validate}
\alias{Validate}
\title{Authentication functions}
\description{
	This basic function authenticates users.  
}
\usage{
Validate(user, pwd, api="agave", print.curl=FALSE)
}
\arguments{
	\item{user}{iPlant Discovery Environment username}
	\item{pwd}{iPlant Discovery Environment password}
	\item{api}{The API to be interfaced with.  The only supported API is "agave"}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
}
\details{
All that is necessary for the \code{Validate} function is for the user to provide the username and password combination; then it will verify that they are a valid pair for iPlant.  To get a valid pair of credentials go to \url{http://user.iplantcollaborative.org/} and register.

Sometimes the API will have trouble validating.  If this is the case, you will receive the following message from 'rPlant': "API Error, please retry". This only seems to occur in a new R session. If this occurs, please try the validation a second time.  If it does not work the second time, then there may be a problem with your account.  
}
\value{
	The \code{Validate} function will create a new R environment that stores variables that communicate automatically with the API.  These include, an access token (foundation) or an API consumer key and secret (agave), the expiration, the user name and password, and a few other variables. To see the entire list of environmental variables, see \code{ls(rplant.env)}. 
	
	The function will print the error "Authentication failed" if if fails. Sometimes the API will require a second validation, in which case 'rPlant' will report an error asking you to retry.  If it is successful, nothing will print to screen, however you can check that validation occurred by checking rplant.env$consumer_key and rplant.env$consumer_secret.
	
	Validation will have to occur at every new R session.  If a session times out and expires, you do not have to renew validation. If the rplant.env is an object found within the workspace, your token will be auto-renewed by using one of 'rPlant's' functions (ie. \code{SubmitJob}, \code{CheckJobStatus}, etc.). 
}
\examples{
\dontrun{Validate("username", "password")}
}
