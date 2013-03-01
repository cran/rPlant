#NOT in Google Style 

GetCitations <- function(application, save=FALSE, file="rPlant.bib") {
  # Returns citations for programs and analyses
  # So we are going to make the library of citations.  I would suggest we make a bibtex entry alongside these entries, someone might want a different citation method.
  library(knitcitations)
  citations<-c()
  #MUSCLE citations
  if (application == "muscle-ranger-2.0" || application == "Muscle-3.8.31" || 
      application == "muscle-lonestar-3.8.31" || application == "muscle-trestles-2.0") {
  	citations <- append(citations, ref("10.1093/nar/gkh340"))
  	citations <- append(citations, ref("10.1186/1471-2105-5-113"))
  }
  #RAxML citations
  if (application == "raxml-lonestar-7.2.8") {
  	citations <- append(citations, ref("10.1093/bioinformatics/btl446"))
  	citations <- append(citations, ref("10.1093/bioinformatics/bti191"))
  }
  #Phylip citations
  if (application == "phylip-dna-parsimony-lonestar-3.69" || 
      application == "phylip-protein-parsimony-lonestar-3.69") {
    citations <- append(citations, bibentry(bibtype="misc", title = "PHYLIP (Phylogeny Inference Package) version 3.6", author=c(person("Felsenstein", "Joe", role="aut")), year="2005", note="Distributed by the author. Department of Genome Sciences, University of Washington, Seattle.", url="http://evolution.genetics.washington.edu/phylip/programs.html"))
    citations <- append(citations, bibentry(bibtype="Article", title = "PHYLIP - Phylogeny Inference Package (Version 3.2)", author=c(person("Felsenstein", "Joe", role="aut")), year="1989", journal="Cladistics", number="5", pages="164-166"))
  }
  #FastTree citations
  if (application == "fasttree-ranger-2.1.4") {
    citations <- append(citations, ref("10.1093/molbev/msp077"))
    citations <- append(citations, ref("10.1371/journal.pone.0009490"))
  }
  #ClustalW citations
  if (application == "clustalw-ranger-1.0" || application == "ClustalW2-2.1" || 
      application == "clustalw2-lonestar-2.1") {
    citations <- append(citations, ref("10.1093/bioinformatics/btm404"))
  }
  #MAFFT citations
  if (application == "mafft-lonestar-6.864") {
    citations <- append(citations, ref("10.1007/978-1-59745-251-9_3"))
    citations <- append(citations, ref("10.1093/nar/gkf436"))
    citations <- append(citations, ref("10.1093/nar/gki198"))
  }
  #QuickTree citations
  if (application == "quicktree-dm-lonestar-1.1" || application == "quicktree-tree-lonestar-1.1") {
    citations <- append(citations, ref("10.1093/bioinformatics/18.11.1546"))
  }

  if(save)
    write.bibtex(citations, file=file, append=T) 
  else  
    return(citations)
}
