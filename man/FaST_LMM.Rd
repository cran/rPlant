\name{FaST_LMM}
\alias{FaST_LMM}
\title{FaST-LMM-2.07}
\description{	FaST-LMM (Factored Spectrally Transformed Linear Mixed Models) is a program for performing genome-wide association studies (GWAS) on large data sets.  FaST-LMM is described more fully at \url{http://www.nature.com/nmeth/journal/v8/n10/abs/nmeth.1681.html}, and also at \url{http://fastlmm.codeplex.com/}
}
\usage{
FaST_LMM(input.file.list="", ALL.file.path="", print.curl=FALSE,
         sim.file.list=NULL, pheno.file.name=NULL, mpheno=1,
         args=NULL, covar.file.name=NULL, job.name=NULL, 
         shared.username=NULL, suppress.Warnings=FALSE,
         out.basename=NULL, email=TRUE) 
}
\arguments{
	\item{input.file.list}{(required) Names of files, in a list format, to be evaluated on the iPlant servers.  There are only three possible input groups for file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{ALL.file.path}{Optional path to a user's subdirectory on the iPlant servers; default path is empty, which leads to the home directory.  All files specified including optional files must be in this directory.}
	\item{job.name}{The name to give the job being submitted}
	\item{out.basename}{The base name for the output files (not including extension).}
	\item{sim.file.list}{(optional) A list containing the names of genetic similarity files.  These are used to determine the genetic similarities between individuals. The may be different from the input.file.list but MUST have the same formats as the input.file.list.}
	\item{pheno.file.name}{(optional) The full file name of the PLINK alternative phenotype file. This includes at least three columns, family ID, individual ID, and phenotype value, and may have any number of phenotype columns. Missing value as default is -9 but it can be changed.}
	\item{mpheno}{(optional) This value MUST be entered if a phenotype file is provided.  This value is the index for phenotype in -pheno file to process, starting at 1 for the first phenotype column.  Default is 1}
	\item{covar.file.name}{(optional) Must have at least three tab-delimited columns: family ID, individual ID, and covariate value, and may have any number of covariate values. The same missing value signifier from the phenotype file must be used. Note: the file should not have a header row.}
	\item{args}{Optional for arguments (i.e. flags).  The FaST-LMM model has much additional functionality that is not fit into this wrapper function (\url{http://fastlmm.codeplex.com/}), see manual.  This option allows users to add anything that is not included (.i.e. args="-Ftest"), to automatically invoke the F-test, see details.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{email}{By default an email will be sent to the user when the job finishes.}
}
\details{
	The inputs for \code{input.file.list} are to be used only in three very strict groups.  Group 1: TFAM, TPED, these are the PLINK transposed filesets.  Group 2: MAP, PED, these are the PLINK regular filesets.  Group 3: BED, BIM, FAM, these are the PLINK binary filesets.

	Additional arguments, \code{args}, can be found at \url{http://fastlmm.codeplex.com/}, see the manual.  The args input is text with the flags and inputs for those flags in a string like on the command line.

Not all information on the FaST-LMM model is here, see the FaST-LMM website \url{http://fastlmm.codeplex.com/}, or the FaST-LMM manual for more information.
}
\value{
	A list containing the job id and the job name is provided for jobs submitted.  If an error, then a message stating the error should also be reported.
}
\seealso{
	\code{\link{SubmitJob}, \link{Validate}, \link{UploadFile}}
}
\examples{
\dontrun{data(geno_test.tfam)}
\dontrun{data(geno_test.tped)}
\dontrun{write.table(geno_test.tfam, file = "geno_test.tfam", row.names=FALSE, 
                     col.names=FALSE, quote=FALSE, sep="\t")}
\dontrun{write.table(geno_test.tped, file = "geno_test.tped", row.names=FALSE, 
                     col.names=FALSE, quote=FALSE, sep="\t")}
\dontrun{Validate("username","password")}
\dontrun{UploadFile("geno_test.tfam")}
\dontrun{UploadFile("geno_test.tped")}
\dontrun{FaST_LMM(input.file.list=list("geno_test.tfam","geno_test.tped"))}
}	
