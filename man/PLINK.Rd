\name{PLINK}
\alias{PLINK}
\title{PLINK-1.07}
\description{
	PLINK is an open-source whole genome association analysis toolset, designed to perform a range of basic, large-scale analyses in a computationally efficient manner, check \url{https://pods.iplantcollaborative.org/wiki/display/DEapps/PLINK} for details.
}
\usage{
PLINK(file.list="", file.path="", job.name=NULL, out.basename=NULL,
      assoc.method="Standard", no.sex=TRUE, args=NULL,
      print.curl=FALSE, multi.adjust=TRUE, shared.username=NULL, 
      suppress.Warnings=FALSE)
}
\arguments{
	\item{file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{out.basename}{The base name for the output files (not including extension).}
	\item{assoc.method}{PLINKs association methods.  Choices are outlined on the PLINK below, see details}
	\item{no.sex}{The sex column (5) is all zeroes (No sex field) (default is TRUE)}
	\item{multi.adjust}{Adjustment for multiple testing (recommended).  A file of adjust significance values that correct for all tests performed and other metrics will be created (default is TRUE).}
	\item{args}{Optional for arguments (i.e. flags).  The PLINK model has so much additional functionality that it cannot all be fit into this wrapper function (\url{http://pngu.mgh.harvard.edu/~purcell/plink/reference.shtml#options}).  This option allows users to add anything that is not included (.i.e. args="--silent"), to suppress output to console, see details.}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	The inputs for \code{file.list} are to be used only in three very strict groups.  Group 1: TFAM, TPED, these are the PLINK transposed filesets.  Group 2: MAP, PED, these are the PLINK regular filesets.  Group 3: BED, BIM, FAM, these are the PLINK binary filesets.

	`assoc.method' lets the user choose the quantitative method.  It isn't recommended, but if NONE is selected, that means no method will be done, one can use this to enter something into the command line.  All possible arguments to be entered by user for `assoc.method' are outlined below, if a different argument is entered the application will fail.

       \tabular{rll}{
         "assoc.method" \tab "explanation"                                    \cr
         Standard       \tab Standard case/control association analysis       \cr
         QTM            \tab Quantitative trait means                         \cr
         GxE            \tab Quantitative trait GxE                           \cr
         Fisher         \tab Fisher's exact test                              \cr
         AlternateModel \tab Alternate/Full model testing                     \cr
         Breslow-Day    \tab Test of homogeneity -- Breslow-Day homogeneity of odds ratio test \cr
         Homog          \tab Test of homogeneity -- partitioning chi-square homogeneity of odds ratio test \cr
         Linear         \tab Linear model testing                             \cr
         Logistic       \tab Logistic model testing                           \cr
         NONE           \tab No Association Method, use if using command line \cr
       } 

	Additional arguments, \code{args}, can be found at \url{http://pngu.mgh.harvard.edu/~purcell/plink/reference.shtml#options}.  The args input is text with the flags and inputs for those flags in a string like on the command line.

	There are many output files possible, \url{http://pngu.mgh.harvard.edu/~purcell/plink/reference.shtml#output}
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
\dontrun{PLINK(file.list=list("geno_test.tfam","geno_test.tped"),
               assoc.method="Standard", print.curl=TRUE)}
}	
