\name{SubmitJob}
\alias{SubmitJob}
\alias{Wait}
\alias{CheckJobStatus}
\alias{DeleteJob}
\alias{KillJob}
\alias{RetrieveJob}
\alias{ListJobOutput}
\alias{GetJobHistory}

\title{Executing analytical applications}
\description{
	Functions for executing and managing analytical applications deployed in the iPlant infrastructure
}
\usage{
SubmitJob(application, file.path="", file.list=NULL, input.list, 
          args.list=NULL, job.name, nprocs=1, private.APP=FALSE, 
          suppress.Warnings=FALSE,  shared.username=NULL,
          print.curl=FALSE, email=TRUE)
Wait(job.id, minWaitsec, maxWaitsec, print=FALSE)
CheckJobStatus(job.id, history = FALSE, print.curl = FALSE)
KillJob(job.id, print.curl=FALSE)
ListJobOutput(job.id, print.curl=FALSE, print.total=TRUE)
RetrieveJob(job.id, file.vec=NULL, print.curl=FALSE, verbose=FALSE)
GetJobHistory(return.json=FALSE, print.curl=FALSE)
DeleteJob(job.id, print.curl=FALSE, ALL=FALSE) 
}
\arguments{
	\item{application}{Name of DE application.  Use the \code{ListApps()} function for a list of eligible applications.  To run your own private application use \code{private.APP =TRUE} and \code{suppress.Warnings=TRUE}.}
	\item{file.path}{Optional path to a user's subdirectory on the DE; the default path is empty, which leads to the home directory.}
	\item{file.list}{A list of input files, many functions only have one input file, but some have multiple input files. These should be organized as a list. The file.list and input.list should correspond. See details for more information.}
	\item{job.name}{The name to give the job being submitted.}
	\item{nprocs}{The number of processors to be allocated to the job, default = 1.}
	\item{private.APP}{Optional argument for submitting a job on your own private application, default is FALSE}
	\item{job.id}{The unique ID number given to a submitted job.}
	\item{input.list}{A list of type of input that is specific to the application. See details for more information.}
	\item{args.list}{A list of input options available for the application. These are usually the flagging options in command line invocations. See details for more information.}
	\item{return.json}{Optional screen output that displays all of the results from the api, default = FALSE.}
	\item{file.vec}{Names of output files to download, can be one or many. If left NULL, all the files in the job output will download.}
	\item{minWaitsec}{A range of times (in seconds) must be entered for the \code{Wait} function.  This entry is the minimum time (in seconds) of that range.}
	\item{maxWaitsec}{A range of times (in seconds) must be entered for the \code{Wait} function.  This entry is the maximum time (in seconds) of that range.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer.}
	\item{print.total}{Option only for the \code{ListJobOuput} function this option will print the total number of files in the folder.}
	\item{print}{Only for the \code{Wait} function, when \code{print=TRUE}, it simply prints the status when the job is complete.}
	\item{verbose}{For the \code{RetrieveJob} function this option will print the names of the files as they are downloaded.}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.  If the application you are running is a private application have suppress.Warnings=TRUE.}
	\item{email}{This option is only on the \code{SubmitJob} function.  By default an email will be sent to the user when the job finishes.}
	\item{ALL}{This option is only on the \code{DeleteJob} function.  If ALL=TRUE then all jobs in the job history will be deleted.}
	\item{history}{This option is only on the \code{CheckJobStatus} function.  If TRUE, then will show entire history of job.}
}
\details{
	The function \code{SubmitJob}, takes inputs and arguments and submits a job on the Agave API. The \code{SubmitJob} function will run the \code{application} with the file inputs \code{file.list} that are in the directory \code{file.path}. The files within \code{file.list} need to match the expected file types for the application (defined in \code{input.list} argument). The appropriate options for the \code{application} need to be outlined in \code{input.list} and potentially \code{args.list}. The \code{SubmitJob} function outputs the \code{job.id} and the \code{job name}.  With that \code{job.id} you can run \code{CheckJobStatus(job.id)} to check the status of your job, and the \code{job name} can be used in workflows. The stages for \code{CheckJobStatus} are: 

\tabular{l}{
	\code{PENDING}            \cr 
	\code{STAGING_INPUTS}     \cr 
	\code{CLEANING_UP}        \cr 
	\code{ARCHIVING}          \cr 
	\code{STAGING_JOB}        \cr 
	\code{FINISHED}           \cr 
	\code{KILLED}             \cr 
	\code{FAILED}             \cr 
	\code{STOPPED}            \cr 
	\code{RUNNING}            \cr 
	\code{PAUSED}             \cr 
	\code{QUEUED}             \cr 
	\code{SUBMITTING}         \cr 
	\code{STAGED}             \cr 
	\code{PROCESSING_INPUTS}  \cr 
	\code{ARCHIVING_FINISHED} \cr 
	\code{ARCHIVING_FAILED}  
}

When it is finished it will read either \code{ARCHIVING_FINISHED} or \code{FINISHED}, unless it failed.  Use the \code{KillJob} function to terminate a running job. Use the \code{Wait} function to wait until job is finished. Be cautious using the \code{Wait} function, because it will lock up the workspace until the job is finished. When the job is finished then use the \code{ListJobOutput} function to see all of the files in your job.  The number of output files varies by \code{application}.  The \code{RetrieveJob} function takes the \code{job.id} and the \code{file.vec} as input, and downloads the specified files in the \code{file.vec}. The files will be downloaded to your current working directory (\code{getwd()}). The \code{file.vec} contains the file names that you want to download. This vector is a subset of the output from \code{ListJobOutput}. The \code{DeleteJob} function then deletes the job and the correponding output folder that was generated from running the job. Using the option \code{DeleteJob(ALL=TRUE)} will delete all jobs in a user's job history. The \code{GetJobHistory} function displays all jobs in your history that have not been deleted.

For the \code{SubmitJob} function the \code{application} must match an application name that is in the output from the \code{ListApps} function. For the \code{input.list} use the \code{GetAppInfo} function, the 'kind' column verifies if "input" or "output".  What goes in the \code{input.list} is only the name in the 'id' column when the 'kind' column is "input".  For example, when the application is "muscle-lonestar-3.8.31u2", we can use \code{GetAppInfo("muscle-lonestar-3.8.31u2")$Information} to determine that the application is expecting "stdin" as its first input file (\code{input.list=list("stdin")}). For the application "velveth-1.2.07u1", \code{GetAppInfo("velveth-1.2.07u1")$Information}, tells us that the application will expect six input files, which should be in the order: \code{input.list=list("reads1", "reads2", "reads3", "reads4", "reads5", "reads6")}. 
	
A few things to note: 1) depending on the application, the \code{input.list} can be shorter than the the number of inputs, for example, using the "velveth-1.2.07u1" application, the input list could be \code{input.list=list("reads1", "reads2", "reads3")}; 2) the \code{file.list} should always be the same length as \code{input.list}; 3) for \code{args.list} use \code{GetAppInfo} function, when the 'kind' column is 'parameters', those are the inputs for \code{args.list}.  For \code{velveth-1.2.07u1} the \code{args.list} is as follows, \code{list(c("format1", value), c("kmer", value), c("Output", value))}. The list can be as long as the number of options.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{ListApps}, \link{Validate}, \link{UploadFile}}
}
\examples{
\dontrun{data(DNA.fasta)}
\dontrun{write.fasta(sequences = DNA.fasta, names = names(DNA.fasta), file.out = "DNA.fasta")}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("DNA.fasta", filetype="FASTA-0")}

# Submit a MUSCLE job using the provided data in the package.  The job will return
# a job id and job name
\dontrun{myJob <- SubmitJob(application="Muscle-3.8.32u4", file.list=list("DNA.fasta"),
                            input.list=list("stdin"), args.list=list(c("arguments", 
                            "-phyiout")), job.name="muscleDNA")}

# Check the status of any job
\dontrun{CheckJobStatus(myJob$id)}
             
# Lists and output files a job has created
\dontrun{ListJobOutput(myJob$id)}

# Might want to kill job if incorrect running
\dontrun{KillJob(myJob$id)}
# Need to wait for job to be done 
\dontrun{Wait(myJob$id, 5, 1800, print=TRUE)}
 
# Download output files
\dontrun{RetrieveJob(myJob$id, ListJobOutput(myJob$id, print.total=FALSE))}
     
# View job history
\dontrun{GetJobHistory()}

# Delete Job
\dontrun{DeleteJob(myJob$id)}
\dontrun{DeleteJob(ALL=TRUE)}
}
