rule merge_phenotypes_and_variants:
    input:
        phenotypes = f"{TEMP_DIR}/phenotypes/phenotypes_clean.tsv",
        variants = f"{TEMP_DIR}/variants/variants.raw"
    output:
        merged = f"{TEMP_DIR}/merged/data.tsv"
    conda:
        "../envs/clean_phenotypes.yaml"
    script:
        "../scripts/merge_data.R"
