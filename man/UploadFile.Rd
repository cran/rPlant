\name{UploadFile}
\alias{UploadFile}
\alias{ShareFile}
\alias{PermissionsFile}
\alias{RenameFile}
\alias{MoveFile}
\alias{CopyFile}
\alias{DeleteFile}
\alias{SupportFile}
\title{File management}
\description{
	Functions for uploading and manipulating files in the iPlant infrastructure
}
\usage{
UploadFile(local.file.name, local.file.path="", filetype=NULL,
           print.curl=FALSE, suppress.Warnings=FALSE)
ShareFile(file.name, file.path="", shared.username, read=TRUE, 
          execute=TRUE, write=TRUE, print.curl=FALSE, suppress.Warnings=FALSE)
PermissionsFile(file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE)
RenameFile(file.name, new.file.name, file.path="", print.curl=FALSE,
           suppress.Warnings=FALSE)
CopyFile(file.name, file.path="", end.path="", print.curl=FALSE,
         suppress.Warnings=FALSE)
MoveFile(file.name, file.path="", end.path="", print.curl=FALSE,
         suppress.Warnings=FALSE)
DeleteFile(file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE)
SupportFile(print.curl=FALSE, suppress.Warnings=FALSE) 
}
\arguments{
	\item{local.file.name}{Name of local file on user's computer.  This file cannot be an object in the R workspace.}
	\item{local.file.path}{optional path to a sub directory where local file on user's computer is located (include ENTIRE path); default path is empty which leads to a users home directory}
	\item{filetype}{format of file that is to be uploaded. Currently 34 file formats supported.  A filetype is not required to upload the file, but some applications require that your data set have a file type assigned to it.}
	\item{file.name}{Name of file to be modified on the iPlant servers.}
	\item{new.file.name}{the new name of the file on the iPlant server to be renamed} 
	\item{file.path}{optional path to a sub directory where file on the iPlant server is located; default path is empty which leads to a users home directory}
	\item{end.path}{Path to destination sub directory where file on DE is moved to; default path is empty which leads to a users home directory}
	\item{read}{Gives read permissions for shared file}
	\item{write}{Gives write permissions for shared file}
	\item{execute}{Gives execute permissions for shared file}
	\item{shared.username}{With iPlant you have the ability to share files with other users, their username is the value for the shared.username}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	The \code{SupportFile} function provides a list of the supported file types that can be uploaded onto the iPlant servers.  The \code{UploadFile} function uploads a file, \code{local.file.name}, that is on your computer (i.e. local directory), in the directory \code{local.file.path}.  If the \code{local.file.path} is not specified your working directory is the default path (use \code{getwd()} to find that).  The \code{filetype} is NOT required, but if you do use it then use the \code{SupportFile} function to view the 34 file formats that are supported (i.e. for a fasta file use \code{FASTA-0}).  iPlant offers sharing of files between two (or more) iPlant users, the \code{ShareFile} makes it so.  Once sharing is done a user can use \code{PermissionsFile} on any file to find who the file is shared with.  The \code{RenameFile} function renames the \code{file.name} in \code{file.path} (on the iPlant servers), to the \code{new.file.name}.  The \code{CopyFile} function copies a \code{file.name} in the \code{file.path} to the \code{end.path}.  The \code{MoveFile} function moves a \code{file.name} in the \code{file.path} to the \code{end.path}.  The \code{DeleteFile} function  deletes the \code{file.name} in the \code{file.path}.
}
\seealso{
	\code{\link{ListDir}}
}
\value{
	If an error, then a message stating the error should also be reported.  
}
\examples{
# Write .fasta file to home directory
\dontrun{data(DNA.fasta)}
\dontrun{write.fasta(sequences = DNA.fasta, names = names(DNA.fasta), 
                     file.out = "DNA.fasta")}
\dontrun{UploadFile(local.file.name="DNA.fasta", local.file.path="path/to/dir", 
                    filetype="FASTA-0")}
         
# Upload a file to the DE
\dontrun{data(geno_test.tfam)}
\dontrun{UploadFile(local.file.name="geno_test.tfam")}

# Shares the file named "DNA.fasta" with the iPlant user "dude"
\dontrun{ShareFile(file.name="DNA.fasta", shared.username="dude")}

# Checks permissions on file "DNA.fasta"
\dontrun{PermissionsFile(file.name="DNA.fasta")}

# Rename a file 
\dontrun{RenameFile(file.name="DNA.fasta", new.file.name="lp.fasta")}

# Copy a file from the subdirectory 'rPlant' to the home directory
\dontrun{CopyFile(file.name="lp.fasta", file.path="", end.path="data")}
             
# Move a file from the subdirectory 'rPlant' to the home directory
\dontrun{MoveFile(file.name="lp.fasta", file.path="", end.path="data")}
     
# Delete a file in the home directory
\dontrun{DeleteFile(file.name="lp.fasta", DE.file.path="data")}

# Lists file types that are supported
\dontrun{SupportFile()}
}
