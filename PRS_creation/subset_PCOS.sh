#!/bin/bash

imp_file_dir="/Bulk/Genotype_Results/Genotype_calls"

data_file_dir="/data"
data_field="ukb22418"
export project=`dx pwd`

for i in {1..22}; do
    run_plink_subset="plink2 --bfile ukb22418_c${i}_b0_v2 \
    --extract snplist_PCOS.txt \
    --maf 0.01 --geno 0.1 --mind 0.1 --hwe 1e-6 \
    --make-bed --out ukb22418_c${i}_subset_qc_PCOS"

    dx run swiss-army-knife -iin="${imp_file_dir}/${data_field}_c${i}_b0_v2.bim" \
    -iin="${imp_file_dir}/${data_field}_c${i}_b0_v2.bed" \
    -iin="${imp_file_dir}/${data_field}_c${i}_b0_v2.fam" \
    -iin="${project}snplist_PCOS.txt" \
     -icmd="${run_plink_subset}" --tag="subset" --instance-type "mem2_ssd2_v2_x16" --priority "high"\
     --destination="${project}data/" --brief --yes
done
