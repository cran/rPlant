\name{SubmitJob}
\alias{SubmitJob}
\alias{CheckJobStatus}
\alias{DeleteJob}
\alias{RetrieveJob}
\alias{ListJobOutput}
\alias{GetJobHistory}

\title{Executing analytical applications}
\description{
	Functions for executing and managing analytical applications deployed in the iPlant infrastructure
}
\usage{
SubmitJob(user.name, token, application,  DE.file.path="", DE.file.list,
          input.list, job.name, nprocs=1, args=NULL, print.curl=FALSE)
CheckJobStatus(user.name, token, job.id, verbose=FALSE, print.curl=FALSE)
DeleteJob(user.name, token, job.id, print.curl=FALSE)
RetrieveJob(user.name, token, job.id, files, zip, print.curl=FALSE)
ListJobOutput(user.name, token, job.id, print.curl=FALSE)
GetJobHistory(user.name, token, verbose=FALSE, print.curl=FALSE)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{application}{Name of DE application}
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{DE.file.list}{A list of input files, many functions only have one input file, but some have multiple input files, in that case input all those files in list form.  This list is the same length as input.list, and input.list[[1]] corresponds with DE.file.list[[1]].  See details for more information}
	\item{job.name}{The name to give the job being submitted}
	\item{nprocs}{The number of processors to be allocated to the job, default = 1}
	\item{args}{Optional for arguments (i.e. flags)}
	\item{job.id}{The unique ID number given to a submitted job} 
	\item{input.list}{A list of the name of inputs.  See details for more information.}
	\item{verbose}{Optional screen output that displays all of the results from the api, default = FALSE}
	\item{files}{Names of output files to download, can be one or many}
	\item{zip}{Zip job files together, default is TRUE}
	\item{print.curl}{Prints the Curl Statement that can be used in the terminal}
}
\details{
	Application must match an application name directly out of the DE, to ensure, you can check using \code{ListApps}. The job.id is returned after submitting, and it is used to check the status or download results. It can also be retrieved using \code{GetJobHistory}. For \code{input.list} use the \code{GetAppInfo} function, the 'kind' column verifies if "input" or "output".  What goes in the \code{input.list} is only the name in the 'id' column when the 'kind' column is "input".  For example, when the application is "muscle-lonestar-3.8.31u2", from the \code{GetAppInfo(user.name, token, "muscle-lonestar-3.8.31u2")[[2]]}, we get the \code{input.list=list("stdin")}.  For the application "velveth-1.2.07u1", again using \code{GetAppInfo(user.name, token, "velveth-1.2.07u1")[[2]]}, the \code{input.list=list("reads1", "reads2", "reads3", "reads4", "reads5", "reads6")}.  A couple notes, the \code{input.list} can be shorter than the the number of inputs, for example, using the "velveth-1.2.07u1" application, the input list could be \code{input.list=list("reads1", "reads2", "reads3")}.  Also, the \code{DE.file.list} is the same length as \code{input.list}, and in this example, lets say \code{DE.file.list=list("filer", "filea", "filez")}, then "filer" corresponds with the "read1" input.
}
\value{
	A job ID is provided for jobs submitted via job.submit.
}
\seealso{
	\code{\link{ListApps}}
}
\examples{
# Upload a file to the DE
# UploadFile("User", token, local.file.name="ex.lp.fasta", file.type="FASTA-0")

# Submit a MUSCLE job using landplant data, which will return a job.id
# myJob <- SubmitJob("User", application="muscle-lonestar-3.8.31u2",
#                    DE.file.list=list("ex.lp.fasta"), input.list=list("stdin")
#                    DE.file.path="/data/", job.name="MUSCLE", nprocs=1)

# Check the status of any job
# CheckJobStatus("User", token, myJob, verbose=F)
             
# Lists and output files a job has created
# ListJobOutput("User", token, myJob)  
             
# Download output files
# RetrieveJob("User", token, myJob, ListJobOutput("User", token, myJob)[[1]])
     
# View job history
# GetJobHistory("User", token, verbose=FALSE)

# Delete Job
# DeleteJob("User", token, myJob)
}
