#!/bin/bash

export project=`dx pwd`


# PLINK command using q-score-range
plink_PRS="plink2 --bfile ukb22418_all_EM_F \
  --score EM_clean.txt.gz 5 4 9 header \
  --q-score-range range_list.txt SNP.pvalue.EM min \
  --extract clump_EM.SNP \
  --out prs_scores_EM"

# DNAnexus job
dx run swiss-army-knife \
  -iin="${project}data/ukb22418_all_EM_F.bim" \
  -iin="${project}data/ukb22418_all_EM_F.bed" \
  -iin="${project}data/ukb22418_all_EM_F.fam" \
  -iin="${project}EM_clean.txt.gz" \
  -iin="${project}range_list.txt" \
  -iin="${project}SNP.pvalue.EM" \
  -iin="${project}clump_EM.SNP" \
  -icmd="${plink_PRS}" \
  --tag="PRS_EM_qrange" \
  --instance-type "mem2_ssd2_v2_x16" \
  --priority "high" \
  --destination="${project}PRS" \
  --brief --yes

