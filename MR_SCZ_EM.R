# ===== Two-sample MR: Schizophrenia -> Endometriosis =====
library(data.table)
library(TwoSampleMR)
library(ieugwasr)
library(tibble)
setDTthreads(4)
options(timeout = 600)
OPENGWAS_JWT=your_token_here

# ---- Paths & output dir ----
exposure_path <- "/gpfs/scratch/asgelz01/MR/SCZ_hg19.txt.gz"   
outcome_path  <- "/gpfs/scratch/asgelz01/MR/EM_dedup.tsv"     

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
outdir <- file.path("/gpfs/scratch/asgelz01/MR/results", paste0("SCZ_to_EM_6_", ts))
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

logf <- file.path(outdir, "log.txt")
cat(sprintf("[%s] Start. Writing to: %s\n", Sys.time(), outdir), file = logf, append = TRUE)

# ---- Read exposure (Schizophrenia) ----
# If SCZ file uses 'rsid' instead of 'SNP', change snp_col accordingly.
exposure_dat <- read_exposure_data(
  filename = exposure_path,
  sep = "\t",
  snp_col = "SNP",
  beta_col = "beta",
  se_col = "standard_error",
  effect_allele_col = "effect_allele",
  other_allele_col = "other_allele",
  eaf_col = "effect_allele_frequency",
  pval_col = "p_value",
  samplesize_col = "N"
)
exposure_dat$exposure <- "Schizophrenia"

# ---- LD clumping (local PLINK reference) ----
clumped <- ieugwasr::ld_clump_local(
  tibble(rsid = exposure_dat$SNP, pval = exposure_dat$pval.exposure),
  bfile = "/gpfs/scratch/asgelz01/MR/EUR",
  plink_bin = "/gpfs/share/apps/plink/1.9/plink",
  clump_r2 = 0.001,
  clump_kb = 10000,
  clump_p = 5e-6
)
head(clumped)
exposure_dat <- exposure_dat[exposure_dat$SNP %in% clumped$rsid, , drop = FALSE]

# ---- Read outcome (Endometriosis) ----
outcome_dat <- read_outcome_data(
  snps = exposure_dat$SNP,
  filename = outcome_path,
  sep = "\t",
  snp_col = "rsids",
  beta_col = "beta",
  se_col = "sebeta",
  effect_allele_col = "alt",
  other_allele_col = "ref",
  eaf_col = "af_alt",
  pval_col = "pval"
)
outcome_dat$outcome <- "Endometriosis"

# ---- Harmonize ----
dat <- harmonise_data(exposure_dat, outcome_dat, action = 2)

# ---- MR Analysis ----
mr_res <- mr(
  dat,
  method_list = c("mr_ivw", "mr_egger_regression", "mr_weighted_median", "mr_weighted_mode")
)
print(mr_res)
fwrite(mr_res, file.path(outdir, "mr_results.tsv"), sep = "\t")

# ---- Sensitivity ----
het    <- mr_heterogeneity(dat);     fwrite(het,    file.path(outdir, "heterogeneity.tsv"), sep = "\t")
pleio  <- mr_pleiotropy_test(dat);   fwrite(pleio,  file.path(outdir, "pleiotropy.tsv"),    sep = "\t")
leave1 <- mr_leaveoneout(dat);       fwrite(leave1, file.path(outdir, "leaveoneout.tsv"),   sep = "\t")
single <- mr_singlesnp(dat);         fwrite(single, file.path(outdir, "single_snp.tsv"),    sep = "\t")

# ---- Plots ----
pdf(file.path(outdir, "scatter.pdf"), width = 7, height = 6)
print(mr_scatter_plot(mr_res, dat)[[1]]); dev.off()

pdf(file.path(outdir, "leaveoneout.pdf"), width = 7, height = 8)
print(mr_leaveoneout_plot(leave1)[[1]]); dev.off()

pdf(file.path(outdir, "forest_leaveoneout.pdf"), width = 7, height = 10)
print(mr_forest_plot(leave1)[[1]]); dev.off()

pdf(file.path(outdir, "funnel.pdf"), width = 7, height = 6)
print(mr_funnel_plot(single)[[1]]); dev.off()

# ---- Save R objects + session info ----
saveRDS(list(dat=dat, mr_res=mr_res, het=het, pleio=pleio, leave1=leave1, single=single),
        file.path(outdir, "objects.rds"))
writeLines(capture.output(sessionInfo()), file.path(outdir, "sessionInfo.txt"))

cat(sprintf("[%s] Done. Results in %s\n", Sys.time(), outdir), file = logf, append = TRUE)