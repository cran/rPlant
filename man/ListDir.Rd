\name{ListDir}
\alias{ListDir}
\alias{MakeDir}
\alias{DeleteDir}
\title{Directory management}
\description{
	Functions for listing the contents of a directory, making new directories, or deleting entire directories in the iPlant infrastructure
}
\usage{
ListDir(user.name, token, DE.dir.path="", print.curl=FALSE)
MakeDir(user.name, token, DE.dir.name, DE.dir.path="", print.curl=FALSE)
DeleteDir(user.name, token, DE.dir.name, DE.dir.path="", print.curl=FALSE)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{DE.dir.path}{optional path to a user's sub directory on the Discovery Environment (DE); default path is empty, which leads to the home directory}
	\item{DE.dir.name}{name of subdirectory to be modified on the DE in the users home iPlant directory.}
	\item{print.curl}{Prints the Curl Statement that can be used in the terminal}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\seealso{
	\code{\link{UploadFile}}
}
\examples{
# Makes the subdirectory named "new" in the users rplant directory
# MakeDir("User", token, DE.dir.name="new", DE.dir.path="/data/")
             
# Lists the contents of a user's subdirectory "new"
# ListDir("User", token, DE.dir.path="/data/new/")

# Lists the contents of a user's subdirectory "rplant"
# ListDir("User", token, DE.dir.path="/data/")

# Deletes the subdirectory "new"
# DeleteDir("User", token, DE.dir.name="new", DE.dir.path="/data/")
}

