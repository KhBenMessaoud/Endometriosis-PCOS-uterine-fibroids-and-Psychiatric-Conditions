#!/bin/bash

export project=`dx pwd`


# PLINK command using q-score-range
plink_PRS="plink2 --bfile ukb22418_all_UF_F \
  --score UF_hg19_new.txt.gz 2 3 6 header \
  --q-score-range range_list.txt SNP.pvalue.UF \
  --extract clump_UF.SNP \
  --out prs_scores_UF"

# DNAnexus job
dx run swiss-army-knife \
  -iin="${project}data/ukb22418_all_UF_F.bim" \
  -iin="${project}data/ukb22418_all_UF_F.bed" \
  -iin="${project}data/ukb22418_all_UF_F.fam" \
  -iin="${project}UF_hg19_new.txt.gz" \
  -iin="${project}range_list.txt" \
  -iin="${project}SNP.pvalue.UF" \
  -iin="${project}clump_UF.SNP" \
  -icmd="${plink_PRS}" \
  --tag="PRS_UF_qrange" \
  --instance-type "mem2_ssd2_v2_x16" \
  --priority "high" \
  --destination="${project}PRS" \
  --brief --yes

