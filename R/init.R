# Markdown

  library(knitr)
  library(rmdformats)


# tidyr libraries

  library(stringr)
  library(dplyr)
  library(tidyr)
  library(tibble)

# For tables

  library(kableExtra)

# Import export library

library(rio)

## Global options

  options(max.print="75",
          knitr.table.format = "HTML")
  opts_chunk$set(echo=TRUE,
                 cache=TRUE,
                 prompt=FALSE,
                 tidy=FALSE,
                 comment=NA,
                 message=FALSE,
                 warning=FALSE,
                 eval = TRUE,
                 hold = TRUE)
  opts_knit$set(width=75)


## Constants
  
  # * Bootstrap min value for class = 90 
  # * At least 10 reads
  
  
  taxo_level = "class"
  boot_level = "class_boot"
  taxo_name = "Bacillariophyta"
  
  cultures_file_name = "../import/metapr2_he492_cultures_ASVs.xlsx"
  metabarcodes_file_name = "../import/metapr2_he492_metabarcodes_ASVs.xlsx"
  
  df_he492_file_name = "../export/metapr2_he492_reads.xlsx"
  fasta_he492_file_name = "../export/metapr2_he492_asvs.xlsx"
  samples_with_taxa_file_name = "../export/metapr2_samples_with_diatoms.xlsx"
  samples_without_taxa_file_name = "../export/metapr2_samples_without_diatoms.xlsx"