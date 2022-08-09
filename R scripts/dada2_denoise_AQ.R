library("devtools")
library ('dada2')

#learn error rate illumina
path <- ("C:/Users/Alexandria Quinlan/Desktop/LYKdemultiplexScript/October/trnLF_demultiplex/trimmed")

fnFs <- sort(list.files(path, pattern="_r1.fq", full.names = TRUE))
fnRs <- sort(list.files(path, pattern="_r2.fq", full.names = TRUE))
#errF <- learnErrors("trim_trnLF_L5675_br11_0_F4121_br05_0_r1.fq", multithread=FALSE)
#errR <- learnErrors("trim_trnLF_L5675_br11_0_F4121_br05_0_r2.fq", multithread=FALSE)

errF <- learnErrors(fnFs, multithread=TRUE)
errR <- learnErrors(fnRs, multithread=TRUE)

numbers = c("01", "02", "03", "04", "05", "06", "07", "08", "09", 10:99)

rbcLN = c("fVGF", "rECL")
rbcLC = c("fNYG", "rVVG")
trnLF = c("L5675", "F4121")
trnL = c("L0725", "L7556")

AP<-data.frame(trnLF)

setwd("C:/Users/Alexandria Quinlan/Desktop/LYKdemultiplexScript/October/trnLF_demultiplex/trimmed")
multiplex <- read.table(
  "multiplex_clean.txt",
  sep="\t", header = TRUE)

#1:nrow(AP) to loop all regions
for (a in 1){
colnames(AP[a]) -> region
AP[,a][1] -> Fp
AP[,a][2] -> Rp
multiplex[!multiplex[,AP[,a][1]] %in% "",] -> amplicon
miss = c()
seqtable = c()
mergfail = c()
for (s in 1:nrow(amplicon)){
  r1 = paste0("filtered_trim_",region, "_", Fp, "_", amplicon[s,Fp],"_0_", Rp, "_", amplicon[s,Rp], "_0_r1.fq")

  r2 = paste0("filtered_trim_",region, "_", Fp, "_", amplicon[s,Fp],"_0_", Rp, "_", amplicon[s,Rp], "_0_r2.fq")
  filename = paste(amplicon[s,"Databaseno..name_on_the_tube."], amplicon[s,"Collection_no."], amplicon[s,"Genus"], amplicon[s,"X.Infra.Species"], ".fas", sep = "_")
  r0 = paste0(amplicon[s,Fp],"_", amplicon[s,Rp])
  filename = paste(amplicon[s,1], amplicon[s,2], amplicon[s,3], amplicon[s,4], ".fas", sep = "_")
  readfilename = paste(amplicon[s,"Databaseno..name_on_the_tube."], amplicon[s,"Collection_no."], amplicon[s,"Genus"], amplicon[s,"X.Infra.Species"], ".fas", sep = "_")
  seqname = paste(amplicon[s,"Genus"], amplicon[s,"X.Infra.Species"], amplicon[s,"Collection_no."], amplicon[s,"Databaseno..name_on_the_tube."], sep = "_")
  header = paste0(">",seqname)
  if (purrr::has_element(list.files(),r1)==TRUE){
    dadaFs <- dada(r1, err=errF, multithread=FALSE)
    dadaRs <- dada(r2, err=errR, multithread=FALSE)
    mergers <- try(mergePairs(dadaFs, r1, dadaRs, r2, verbose=F))
    if (inherits(mergers, "try-error")){
      print(r1)
      print(r2)
      next
    }
    if (nrow(mergers)>0){
       max(c(dadaFs[["clustering"]][["abundance"]], dadaRs[["clustering"]][["abundance"]]))->clustermax
       paste0(rep(header, length(mergers$abundance)), "_", numbers[1:length(mergers$abundance)], rep("_", length(mergers$abundance)), sprintf(mergers$abundance/clustermax, fmt = '%#.3f'), rep("_abundance_", length(mergers$abundance)), mergers$abundance)->merglist
       cbind(merglist,mergers$sequence,mergers$abundance)-> fas
       fas[order(as.numeric(fas[,3]), decreasing = TRUE)]
       write.table(fas[,1:2], file = paste0("denoice/",filename), append = FALSE, sep = "\n", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
       write.table(fas[1,1:2], file = paste0("denoice_best/",filename), append = FALSE, sep = "\n", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
    }
    if (nrow(mergers)==0){
       cbind(r1, r2, header)->fail
       rbind(mergfail, fail)-> mergfail
    }
  paste0(rep(header, length(dadaFs[["clustering"]][["abundance"]])), "_", numbers[1:length(dadaFs[["clustering"]][["abundance"]])], rep("_r1_", length(dadaFs[["clustering"]][["abundance"]])), sprintf(dadaFs[["clustering"]][["abundance"]]/sum(dadaFs[["clustering"]][["abundance"]]), fmt = '%#.3f'), rep("_abundance_", length(dadaFs[["clustering"]][["abundance"]])), dadaFs[["clustering"]][["abundance"]])->r1list
  cbind(r1list,dadaFs[["clustering"]][["sequence"]],dadaFs[["clustering"]][["abundance"]])-> r1fas
  r1fas[order(as.numeric(r1fas[,3]), decreasing = TRUE)]
  write.table(r1fas[,1:2], file = paste0("denoice/r1/",readfilename), append = FALSE, sep = "\n", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  write.table(r1fas[1,1:2], file = paste0("denoice_best/r1/",readfilename), append = FALSE, sep = "\n", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  paste0(rep(header, length(dadaRs[["clustering"]][["abundance"]])), "_", numbers[1:length(dadaRs[["clustering"]][["abundance"]])], rep("_r2_", length(dadaRs[["clustering"]][["abundance"]])), sprintf(dadaRs[["clustering"]][["abundance"]]/sum(dadaRs[["clustering"]][["abundance"]]), fmt = '%#.3f'), rep("_abundance_", length(dadaRs[["clustering"]][["abundance"]])), dadaRs[["clustering"]][["abundance"]])->r2list
  cbind(r2list,dadaRs[["clustering"]][["sequence"]],dadaRs[["clustering"]][["abundance"]])-> r2fas
  r2fas[order(as.numeric(r2fas[,3]), decreasing = TRUE)]
  write.table(r2fas[,1:2], file = paste0("denoice/r2/",readfilename), append = FALSE, sep = "\n", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  write.table(r2fas[1,1:2], file = paste0("denoice_best/r2/",readfilename), append = FALSE, sep = "\n", quote = FALSE,
              row.names = FALSE, col.names = FALSE)
  
  if (nrow(mergers)==0){
    cbind(r1, r2, header)->fail
    rbind(mergfail, fail)-> mergfail
    write.table(r1fas[,1:2], file = paste0("denoice/nonmerged/r1/",filename), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    write.table(r1fas[1,1:2], file = paste0("denoice_best/nonmerged/r1/",filename), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    write.table(r1fas[,1:2], file = paste0("denoice/nonmerged/r1/",r0), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    write.table(r1fas[1,1:2], file = paste0("denoice_best/nonmerged/r1/",r0), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    write.table(r2fas[,1:2], file = paste0("denoice/nonmerged/r2/",filename), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    write.table(r2fas[1,1:2], file = paste0("denoice_best/nonmerged/r2/",filename), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    write.table(r2fas[,1:2], file = paste0("denoice/nonmerged/r2/",r0), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
    write.table(r2fas[1,1:2], file = paste0("denoice_best/nonmerged/r2/",r0), append = FALSE, sep = "\n", quote = FALSE,
                row.names = FALSE, col.names = FALSE)
  }
  cbind(rep(r1, length(dadaFs[["clustering"]][["abundance"]])),r1list)-> seqtable01
  cbind(rep(r2, length(dadaRs[["clustering"]][["abundance"]])),r2list)-> seqtable02
  rbind(seqtable, seqtable01, seqtable02)-> seqtable
  }
  if (purrr::has_element(list.files(),r1)==FALSE){
    cbind(r1, header)->mis
    rbind(miss, mis)-> miss
  }
}
write.table(miss, file = paste0("denoice/missing_samples.txt"), append = FALSE, sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = FALSE)
write.table(seqtable, file = paste0("denoice/sequence_table.txt"), append = FALSE, sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = FALSE)
write.table(mergfail, file = paste0("denoice/merg_fail.txt"), append = FALSE, sep = "\t", quote = FALSE,
            row.names = FALSE, col.names = FALSE)
}