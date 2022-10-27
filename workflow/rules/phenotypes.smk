rule convert_basket:
    # Generate an R file with all the phenotypes in the basket
    input:
        multiext(f"{config.get('basket_path')}/{config.get('basket_filename))}",
                 ".r", ".tab", ".html")
    output:
        protected(f"{TEMP_DIR}/phenotypes/ukb.rda")
    conda:
        "envs/ukbtools.yaml"
    script:
        "../scripts/convert_basket.R"

rule extract_phenotype_variables:
    # Extract the variables of interest in the UKB
    input:
        f"{TEMP_DIR}/phenotypes/ukb.rda"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes_raw.rda"
    conda:
        "envs/data.table.yaml"
    script:
        "../scripts/extract_phenotypes.R"

rule clean_phenotypes:
    input:
        f"{TEMP_DIR}/phenotypes/phenotypes_raw.rda"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes_clean.rda"
    conda:
        "envs/clean_phenotypes.yaml"
    script:
        "../scripts/clean_phenotypes.R"
