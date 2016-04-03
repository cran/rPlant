\name{PLINKConversion}
\alias{PLINKConversion}
\title{PLINKConversion}
\description{
	PLINK is an open-source whole genome association analysis toolset, designed to perform a range of basic, large-scale analyses in a computationally efficient manner, check \url{https://pods.iplantcollaborative.org/wiki/display/DEapps/PLINK+Conversion} for details. This function converts the standard PLINK file formats (Regular (ped/map), Transposed (tped/tfam), and Binary (bed/bim/fam)) to various other PLINK file formats.
}
\usage{
PLINKConversion(file.list="", file.path="", output.type="PED/MAP", job.name=NULL,
                shared.username=NULL, print.curl=FALSE, suppress.Warnings=FALSE,
                out.basename=NULL)
}
\arguments{
	\item{file.list}{Names of files, in a list format, to be evaluated on the Discovery Environment (DE).  There are only three possible input groups for file.list, regular fileset (.map/.ped),  transposed fileset (.tfam/.tped), and binary fileset (.bed/.bim/.fam).}
	\item{file.path}{Optional path to a user's subdirectory on the DE; default path is empty, which leads to the home directory}
	\item{job.name}{The name to give the job being submitted}
	\item{output.type}{PLINKs conversion output file types.  Choices are outlined below, see details.}
	\item{out.basename}{The base name for the output files (not including extension).}
	\item{print.curl}{Prints the curl statement that can be used in the terminal, if curl is installed on your computer}
	\item{shared.username}{With iPlant you have the ability to share folders with other users.  If someone has shared a folder with you and you want to run a job with them, enter their username for this input.}
	\item{suppress.Warnings}{This will turn off the warnings, will speed up run time.  Use with caution, if the inputs are incorrect they will not be caught.}
}
\details{
	The inputs for \code{file.list} are to be used only in three very strict groups.  Group 1: TFAM, TPED, these are the PLINK transposed filesets.  Group 2: MAP, PED, these are the PLINK regular filesets.  Group 3: BED, BIM, FAM, these are the PLINK binary filesets.

	`output.type' gives the user allows the user to pick what file type to convert the data set to.  All possible arguments to be entered by user for `output.type' are outlined below, if a different argument is entered the application will fail.

       \tabular{rll}{
         "output.type" \tab "explanation"                                      \cr
         PED/MAP       \tab Output new .ped and .map files                     \cr
         TFAM/TPED     \tab Transposed Filests (.tfam/.tped)                   \cr
         BED/FAM/BIM   \tab Make .bed, .fam and .bim                           \cr
         LGEN          \tab Output data in long LGEN format                    \cr
         RAWa          \tab Raw data file with additive coding                 \cr
         RAWad         \tab Raw data file with additive/dominance coding       \cr
         PED/MAP-1/2   \tab Regular fileset (.map/.ped) with 1/2 allele coding \cr
         FASTPHASE     \tab Ouput fastphase format file                        \cr
         BIMBAM        \tab Ouput bimbam format file                           \cr
         STRUCTURE     \tab Ouput structure format file                        \cr
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
\dontrun{PLINKConversion(file.list=list("geno_test.tfam", "geno_test.tped"), 
                     output.type="BED/FAM/BIM")}
}	
