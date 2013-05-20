\name{GetToken}
\alias{GetToken}
\alias{RenewToken}
\title{Authentication functions}
\description{
	Authenticates users and provides tokens to be used in place of unencrypted passwords
}
\usage{
GetToken(user.name, user.pwd, api=c("iplant", "cipres", "tnrs"), print.curl=FALSE)
RenewToken(user.name, user.pwd, token, api=c("iplant", "cipres", "tnrs"),
    print.curl=FALSE)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{user.pwd}{iPlant Discovery Environment password}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{api}{the API to interfaced with, currently only the iPlant API is supported}
	\item{print.curl}{Prints the Curl Statement that can be used in the terminal}
}
\details{
	The authentication functions will issue and renew an iPlant token.  Tokens expire after 2 hours unless renewed.  
}
\value{
	A token used in place of an unencrypted password.
}
\examples{
	#token<-GetToken("User", "SecretPassword", api="iplant")
	#RenewToken("User", "SecretPassword", token, api="iplant")
}
