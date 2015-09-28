\name{ListDir}
\alias{ListDir}
\alias{CopyDir}
\alias{MakeDir}
\alias{ShareDir}
\alias{PermissionsDir}
\alias{RenameDir}
\alias{MoveDir}
\alias{DeleteDir}
\title{Directory management}
\description{
	Functions for listing the contents of a directory, making new directories, or deleting entire directories in the iPlant infrastructure
}
\usage{
ListDir(dir.name, dir.path="", print.curl=FALSE, shared.username=NULL,
        suppress.Warnings=FALSE, show.hidden=FALSE)
ShareDir(dir.name, dir.path="", shared.username, read=TRUE, execute=TRUE,
         write=TRUE, print.curl=FALSE, suppress.Warnings=FALSE) 
PermissionsDir(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE)
RenameDir(dir.name, new.dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE)
CopyDir(dir.name, dir.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE)
MoveDir(dir.name, dir.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE)
MakeDir(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE)
DeleteDir(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE)
}
\arguments{
	\item{dir.path}{optional path to a user's sub directory on the iPlant servers; default path is empty, which leads to the home directory}
	\item{dir.name}{name of subdirectory to be modified on the iPlant server in the \code{dir.path}}
	\item{new.dir.name}{the new name of the directory on the iPlant server to be renamed} 
	\item{end.path}{Path to destination sub directory where directory on the iPlant server is moved to; default path is empty which leads to a users home directory}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share files with other users, their username is the value for the shared.username, see details.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{read}{Gives read permissions for shared directory}
	\item{write}{Gives write permissions for shared directory}
	\item{execute}{Gives execute permissions for shared directory}
	\item{show.hidden}{Option to display hidden files (ie, ".svn", ".Rdata")}
}
\details{
	The \code{ListDir} function lists the files in the given directory, \code{dir.path}.  If listing shared folders then the \code{dir.path} becomes the path to the SHARED users shared folder.  iPlant offers sharing of files between two (or more) iPlant users, the \code{ShareDir} makes it so.  Once sharing is done a user can use \code{PermissionsDir} on any directory to find who the folder is shared with.  The \code{RenameDir} function renames the \code{dir.name} in \code{dir.path} (on the iPlant servers), to the \code{new.dir.name}.  The \code{CopyDir} function moves a \code{dir.name} in the \code{dir.path} to the \code{end.path}.  The \code{MoveDir} function moves a \code{dir.name} in the \code{dir.path} to the \code{end.path}  .The \code{MakeDir} function makes a directory \code{dir.name} in the directory \code{dir.path}.  The \code{DeleteDir} function deletes the directory \code{dir.name} in the directory \code{dir.path}.
}
\value{
	If an error, then a message stating the error should also be reported.  
}
\seealso{
	\code{\link{UploadFile}}
}
\examples{
# Makes the subdirectory named "new" in the users 'rPlant' directory
\dontrun{MakeDir(dir.name="new", dir.path="data")}

# Shares the subdirectory named "new" with the iPlant user "dude"
\dontrun{ShareDir(dir.name="new", dir.path="data", shared.username="dude")}

# Checks permissions folder "new"
\dontrun{PermissionsDir(dir.name="new", dir.path="data")}

# Lists the contents of a user's subdirectory "new"
\dontrun{ListDir(dir.name="new". dir.path="data")}

# Rename the directory
\dontrun{RenameDir(dir.name="new", dir.path="data", new.dir.name="newest")}
 
 # Copy a subdirectory `newest' from the directory `data' to the home directory
\dontrun{CopyDir(dir.name="newest", dir.path="data", end.path="")}
            
# Move a subdirectory `newest' from the directory `data' to the home directory
\dontrun{MoveDir(dir.name="newest", dir.path="data", end.path="")}

# Deletes the subdirectory "newest"
\dontrun{DeleteDir(dir.name="newest", dir.path="")}
}

