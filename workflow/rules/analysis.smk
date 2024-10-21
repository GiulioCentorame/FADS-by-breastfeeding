rule merge_phenotypes_and_variants:
    input:
        phenotypes = f"{TEMP_DIR}/phenotypes/{{ancestry_group}}/subset.rds",
        variants = f"{TEMP_DIR}/variants/variants.raw",
        SBP = f"{TEMP_DIR}/phenotypes/sbp.tsv",
        DBP = f"{TEMP_DIR}/phenotypes/dbp.tsv"
    output:
        merged = f"{TEMP_DIR}/merged/{{ancestry_group}}/data.rds"
    envmodules:
        "r/4.4.1"
    conda:
        "../envs/clean_phenotypes.yaml"
    script:
        "../scripts/merge_data.R"

rule clean_SBP:
    input:
        f"{config.get('basket_path')}/ukb39610.csv"
    output:
        f"{TEMP_DIR}/phenotypes/sbp.tsv"
    threads: 96
    resources:
        mem_mb=1000000,
        disk_mb=4000,
        time_min=400
    envmodules:
        "r/4.1.0-foss-2021a"
    script:
        "../scripts/SBP.R"

rule clean_DBP:
    input:
        f"{config.get('basket_path')}/ukb39610.csv"
    output:
        f"{TEMP_DIR}/phenotypes/dbp.tsv"
    threads: 96
    resources:
        mem_mb=1000000,
        disk_mb=4000,
        time_min=400
    envmodules:
        "r/4.1.0-foss-2021a"
    script:
        "../scripts/DBP.R"

rule clean_phenotypes:
    input:
        data = f"{TEMP_DIR}/merged/{{ancestry_group}}/data.rds"
    output:
        output = f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData",
    envmodules:
        "r/4.1.0-foss-2021a"
    script:
        "../scripts/clean_phenotypes.R"

rule fit_models_additive:
    input:
        data = f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData"
    output:
        output = f"{TEMP_DIR}/clean/{{ancestry_group}}/model_summaries_additive.RData"
    threads: 96
    resources:
        mem_mb=100000,
    envmodules:
        "r/4.1.0-foss-2021a"
    params:
        recessive = False,
    script:
        "../scripts/fit_models.R"

rule fit_models_recessive:
    input:
        data = f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData"
    output:
        output = f"{TEMP_DIR}/clean/{{ancestry_group}}/model_summaries_recessive.RData"
    threads: 96
    resources:
        mem_mb=100000,
    envmodules:
        "r/4.1.0-foss-2021a"
    params:
        recessive = True,
    script:
        "../scripts/fit_models.R"

# Main analysis without trimming

rule clean_phenotypes_no_trim:
    input:
        data = f"{TEMP_DIR}/merged/{{ancestry_group}}/data.rds"
    output:
        output = f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models_no_trim.RData",
    envmodules:
        "r/4.1.0-foss-2021a"
    script:
        "../scripts/clean_phenotypes_no_trim.R"

rule fit_models_additive_no_trimming:
    input:
        data = f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models_no_trim.RData"
    output:
        output = f"{TEMP_DIR}/clean/{{ancestry_group}}/model_summaries_additive_no_trim.RData"
    threads: 96
    resources:
        mem_mb=100000,
    envmodules:
        "r/4.1.0-foss-2021a"
    params:
        recessive = False,
    script:
        "../scripts/fit_models.R"
