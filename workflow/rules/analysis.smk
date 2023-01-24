rule merge_phenotypes_and_variants:
    input:
        phenotypes = f"{TEMP_DIR}/phenotypes/vars_subset.rds",
        variants = f"{TEMP_DIR}/variants/variants.raw",
        SBP = f"{TEMP_DIR}/phenotypes/sbp.tsv",
        DBP = f"{TEMP_DIR}/phenotypes/dbp.tsv"
    output:
        merged = f"{TEMP_DIR}/merged/data.tsv"
    conda:
        "../envs/clean_phenotypes.yaml"
    script:
        "../scripts/merge_data.R"
