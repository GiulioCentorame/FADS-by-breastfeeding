# Analysis pipeline for *No evidence of interaction between FADS2 genotype and breastfeeding on cognitive or other traits in the UK Biobank*

Snakemake workflow for analysis and plots for the manuscript on the gene-by-environment interaction between *FADS2* and breastfeeding on cognitive traits, cardiometabolic measures, and number of offspring

## Requirements

This workflow assumes the following software is installed and available:
- `snakemake 7.x`
- `ldbird`
- `plink 2.0`
- `SQLite 3.42`
- `pandoc`
- `renv` (optional)

Environment modules are provided for most software when running on UQ's HPC Bunya.

## Setup

- Clone the repository
- Set the paths within `config/config.yaml`
- (If not running using `conda` environments) restore installed packages in R using `renv::restore()`
- Run the workflow
