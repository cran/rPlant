\name{UploadFile}
\alias{UploadFile}
\alias{RenameFile}
\alias{MoveFile}
\alias{DeleteFile}
\alias{SupportFile}
\title{File management}
\description{
	Functions for uploading and manipulating files in the iPlant infrastructure
}
\usage{
UploadFile(user.name, token, local.file.name, local.file.path, file.type)
RenameFile(user.name, token, old.DE.file.name, new.DE.file.name, DE.file.path)
MoveFile(user.name, token, DE.file.name, DE.file.path, DE.end.path)
DeleteFile(user.name, token, DE.file.name, DE.file.path)
SupportFile(user.name, token)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{a validation tool used in place of unencrypted passwords}
	\item{local.file.name}{Name of local file on user's computer.  This file cannot be an object in the R workspace.}
	\item{DE.file.name}{Name of file to be modified on the Discovery Environment (DE).}
	\item{file.type}{format of file that is to be uploaded. Currently 34 file formats supported} 
	\item{old.DE.file.name}{the name of the file on DE to be renamed}
	\item{new.DE.file.name}{the new name of the file on DE to be renamed} 
	\item{local.file.path}{optional path to a sub directory where local file on user's computer is located (include ENTIRE path); default path is empty which leads to a users home directory}
	\item{DE.file.path}{optional path to a sub directory where file on DE is located; default path is empty which leads to a users home directory}
	\item{DE.end.path}{optional path to destination sub directory where file on DE is moved to; default path is empty which leads to a users home directory}
}
\details{
	The function SupportFile provides a list of the supported file types that can be uploaded onto iPlant.
}
\seealso{
	\code{\link{SupportFile}}
	\code{\link{ListDir}}
}
\value{
	Text listing action as either being a "success" or "error". If an error, then a message stating the error should also be reported.  
}
\examples{
# Write .fasta file to home directory
# data(ex.lp.fasta)
# write.fasta(sequences=ex.lp.fasta,names=names(ex.lp.fasta),file.out="ex.lp.fasta") 
         
# Upload a file to the DE
# UploadFile("User", token, local.file.name="ex.lp.fasta", file.type="FASTA-0")
# UploadFile("User", token, local.file.name="ex.lp.fasta", local.file.path="/path/", file.type="FASTA-0")
     
# Rename a file 
# RenameFile("User", token, old.DE.file.name="ex.lp.fasta", new.DE.file.name="lp.fasta", DE.file.path="/data/")
             
# Move a file from the subdirectory rplant to the home directory
# MoveFile("User", token, DE.file.name="lp.fasta", DE.file.path="/data/", DE.end.path="")
     
# Delete a file in the home directory
# DeleteFile("User", token, DE.file.name="lp.fasta", DE.file.path="")

# Lists file types that are supported
# SupportFile("User", token)  
}	
