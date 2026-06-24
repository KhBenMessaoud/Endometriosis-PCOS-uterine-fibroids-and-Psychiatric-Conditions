#!/bin/bash

export project=`dx pwd`

plink_clump="plink2 --bfile ukb22418_all_UF \
  --clump UF_hg19_new.txt.gz \
    --clump-p1 1 \
    --clump-r2 0.1 \
    --clump-kb 250 \
    --clump-snp-field SNP \
    --clump-field P \
  --out clump_UF"

# Submit the DNAnexus job
dx run swiss-army-knife \
  -iin="${project}data/ukb22418_all_UF.bim" \
  -iin="${project}data/ukb22418_all_UF.bed" \
  -iin="${project}data/ukb22418_all_UF.fam" \
  -iin="${project}UF_hg19_new.txt.gz" \
  -icmd="${plink_clump}" \
  --tag="clump_UF" \
  --instance-type "mem2_ssd2_v2_x16" \
  --priority "high" \
  --destination="${project}" \
  --brief --yes

