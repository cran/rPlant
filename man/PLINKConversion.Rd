\name{PLINKConversion}
\alias{PLINKConversion}
\title{PLINKConversion}
\description{
	PLINK is an open-source whole genome association analysis toolset, designed to perform a range of basic, large-scale analyses in a computationally efficient manner, check \url{http://pngu.mgh.harvard.edu/~purcell/plink/} for details. This function converts the standard PLINK file formats (Regular (ped/map), Transposed (tped/tfam), and Binary (bed/bim/fam)) to various other PLINK file formats.
}
\usage{
PLINKConversion(file.list="", file.path="", output.type="--recode",
                job.name=NULL, shared.username=NULL, print.curl=FALSE,
                 suppress.Warnings=FALSE, out.basename=NULL, email=TRUE)
}
\arguments{
	\item{file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{output.type}{PLINKs conversion methods.  Choices are outlined on the PLINK webpage, see details for more information.}
	\item{out.basename}{The base name for the output files (not including extension).}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
	\item{email}{By default an email will be sent to the user when the job finishes.}
}
\details{
	The inputs for \code{file.list} are to be used only in three very strict groups.  Group 1: TFAM, TPED, these are the PLINK transposed filesets.  Group 2: MAP, PED, these are the PLINK regular filesets.  Group 3: BED, BIM, FAM, these are the PLINK binary filesets.

	`output.type' gives the user a lot of flexibility by allowing you to type in the proper argument.  All possible arguments for `output.type' are outlined below, if a different argument is entered the application will fail.

       \tabular{rll}{
              \tab "output.type"      \tab "explanation"                                \cr
         [,1] \tab --make-bed         \tab Make .bed, .fam and .bim                     \cr
         [,2] \tab --recode           \tab Output new .ped and .map files               \cr
         [,3] \tab --recode12         \tab As above, with 1/2 allele coding             \cr
         [,4] \tab --recode-rlist     \tab List individuals with minor allele genotypes \cr
         [,5] \tab --recode-lgen      \tab Output data in long LGEN format              \cr
         [,6] \tab --recodeHV         \tab As above, with Haploview .info file          \cr
         [,7] \tab --recode-fastphase \tab Ouput fastphase format file                  \cr
         [,8] \tab --recode-bimbam    \tab Ouput bimbam format file                     \cr
         [,9] \tab --recode-structure \tab Ouput structure format file                  \cr
         [,10] \tab --recodeA         \tab Raw data file with additive coding           \cr
         [,11] \tab --recodeAD        \tab Raw data file with additive/dominance coding \cr
         [,12] \tab --recode --transpose \tab Transposed Filests (.tfam/.tped)          \cr
       } 

See the PLINK website for more information. \url{http://pngu.mgh.harvard.edu/~purcell/plink}
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
\dontrun{PLINKConversion(file.list=list(geno_test.tfam, geno_test.tped), output.type="--recode")}
}	
