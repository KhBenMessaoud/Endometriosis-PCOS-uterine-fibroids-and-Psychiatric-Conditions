#!/bin/bash

export project=`dx pwd`

plink_clump="plink2 --bfile ukb22418_all_EM \
  --clump EM_clean.txt.gz \
    --clump-p1 1 \
    --clump-r2 0.1 \
    --clump-kb 250 \
    --clump-snp-field rsids \
    --clump-field pval \
  --out clump_EM"

# Submit the DNAnexus job
dx run swiss-army-knife \
  -iin="${project}data/ukb22418_all_EM.bim" \
  -iin="${project}data/ukb22418_all_EM.bed" \
  -iin="${project}data/ukb22418_all_EM.fam" \
  -iin="${project}EM_clean.txt.gz" \
  -icmd="${plink_clump}" \
  --tag="clump_EM" \
  --instance-type "mem2_ssd2_v2_x16" \
  --priority "high" \
  --destination="${project}" \
  --brief --yes

