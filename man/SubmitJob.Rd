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
SubmitJob(user.name, token, application, DE.file.name, DE.file.path="", job.name, nprocs=1, args=c())
CheckJobStatus(user.name, token, job.id, verbose=FALSE)
DeleteJob(user.name, token, job.id)
RetrieveJob(user.name, token, job.id, files, zip)
ListJobOutput(user.name, token, job.id)
GetJobHistory(user.name, token, verbose=FALSE)
}
\arguments{
	\item{user.name}{iPlant Discovery Environment user name}
	\item{token}{A validation tool used in place of unencrypted passwords}
	\item{DE.file.name}{Name of file to be evaluated on the Discovery Environment (DE).}
	\item{application}{Name of DE application}
	\item{job.name}{The name to give the job being submitted}
	\item{nprocs}{The number of processors to be allocated to the job, default = 1}
	\item{args}{Optional for arguments (i.e. flags)}
	\item{job.id}{The unique ID number given to a submitted job} 
	\item{DE.file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{verbose}{Optional screen output that displays all of the results from the api, default = FALSE}
	\item{files}{Names of output files to download, can be one or many}
	\item{zip}{Zip job files together, default is TRUE}
}
\details{
	Application must match an application name directly out of the DE, to ensure, you can check using \code{ListApps}. The job.id is returned after submitting, and it is used to check the status or download results. It can also be retrieved using \code{GetJobHistory}.
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
# myJob <- SubmitJob("User", application="muscle-ranger-2.0", DE.file.name="ex.lp.fasta", DE.file.path="/data/", job.name="MUSCLE", nprocs="1")

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
