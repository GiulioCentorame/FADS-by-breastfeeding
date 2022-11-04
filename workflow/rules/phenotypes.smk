rule strip_comments:
    input:
        "../config/variables.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    shell:
        "grep -v '#' {input} > {output}"

rule extract_phenotype_variables:
    # Extract the variables of interest in the UKB
    input:
        tab_files = expand(f"{config.get('basket_path')}/{{filename}}.tab",
                           filename = config.get("basket_filename")),
        list_phenotypes = f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes_raw.tsv"
    threads: 16
    resources:
        mem_mb=10000
    conda:
        "../envs/fmrib-unpack.yaml"
    log:
        "logs/extract_phenotype_variables.log"
    shell:
        """
        fmrib_unpack \
        --variable {input.list_phenotypes}\
        {output} \
        {input.tab_files}
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
