rule strip_comments:
    input:
        "config/variables.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    shell:
        "grep -v '#' {input} > {output}"

rule extract_phenotype_variables:
    # Extract the variables of interest in the UKB
    input:
        f"{config.get('basket_path')}/{config.get('basket_filename')}.tab",
        f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes_raw.tsv"
    conda:
        "../envs/fmrib-unpack.yaml"
    log:
        "logs/extract_phenotype_variables.log"
    shell:
        """
        fmrib_unpack \
        --variable {input[1]}\
        {output} \
        {input[0]}
        """

rule clean_phenotypes:
    input:
        f"{TEMP_DIR}/phenotypes/phenotypes_raw.rda"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes_clean.rda"
    conda:
        "../envs/clean_phenotypes.yaml"
    script:
        "../scripts/clean_phenotypes.R"
