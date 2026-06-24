#!/bin/bash

export project=`dx pwd`


# PLINK command using q-score-range
plink_PRS="plink2 --bfile ukb22418_all_PCOS_F \
  --score PCOS.tsv.gz 9 3 5 header \
  --q-score-range range_list.txt SNP.pvalue.PCOS min \
  --extract clump_PCOS.SNP \
  --out prs_scores_PCOS"

# DNAnexus job
dx run swiss-army-knife \
  -iin="${project}data/ukb22418_all_PCOS_F.bim" \
  -iin="${project}data/ukb22418_all_PCOS_F.bed" \
  -iin="${project}data/ukb22418_all_PCOS_F.fam" \
  -iin="${project}PCOS.tsv.gz" \
  -iin="${project}range_list.txt" \
  -iin="${project}SNP.pvalue.PCOS" \
  -iin="${project}clump_PCOS.SNP" \
  -icmd="${plink_PRS}" \
  --tag="PRS_PCOS_qrange" \
  --instance-type "mem2_ssd2_v2_x16" \
  --priority "high" \
  --destination="${project}PRS" \
  --brief --yes
