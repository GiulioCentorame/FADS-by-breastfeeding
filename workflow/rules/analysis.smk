rule merge_phenotypes_and_variants:
    input:
        phenotypes = f"{TEMP_DIR}/phenotypes/vars_subset.rds",
        variants = f"{TEMP_DIR}/variants/variants.raw",
        SBP = f"{TEMP_DIR}/phenotypes/sbp.tsv",
        DBP = f"{TEMP_DIR}/phenotypes/dbp.tsv"
    output:
        merged = f"{TEMP_DIR}/merged/data.rds"
    envmodules:
        "r/4.2.1-foss-2021a"
    conda:
        "../envs/clean_phenotypes.yaml"
    script:
        "../scripts/merge_data.R"

rule clean_phenotypes:
    input:
        data = f"{TEMP_DIR}/merged/data.rds"
    output:
        output = f"{TEMP_DIR}/clean/data.RData"
    envmodules:
        "r/4.2.1-foss-2021a"
    script:
        "../scripts/clean_phenotypes.R"

rule fit_models:
    input:
        data = f"{TEMP_DIR}/clean/data.RData"
    output:
        output = f"{TEMP_DIR}/clean/model_summaries.RData"
    threads: 96
    resources:
        mem_mb=500000
    envmodules:
        "r/4.2.1-foss-2021a"
    script:
        "../scripts/fit_models.R"
