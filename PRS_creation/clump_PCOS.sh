#!/bin/bash

export project=`dx pwd`

plink_clump="plink2 --bfile ukb22418_all_PCOS \
  --clump PCOS.tsv.gz \
    --clump-p1 1 \
    --clump-r2 0.1 \
    --clump-kb 250 \
    --clump-snp-field rs_id \
    --clump-field p_value \
  --out clump_PCOS"

# Submit the DNAnexus job
dx run swiss-army-knife \
  -iin="${project}data/ukb22418_all_PCOS.bim" \
  -iin="${project}data/ukb22418_all_PCOS.bed" \
  -iin="${project}data/ukb22418_all_PCOS.fam" \
  -iin="${project}PCOS.tsv.gz" \
  -icmd="${plink_clump}" \
  --tag="clump_PCOS" \
  --instance-type "mem2_ssd2_v2_x16" \
  --priority "high" \
  --destination="${project}" \
  --brief --yes

