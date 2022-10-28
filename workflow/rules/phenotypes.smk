rule extract_phenotype_variables:
    # Extract the variables of interest in the UKB
    input:
        f"{config.get('basket_path')}/{config.get('basket_filename')}.tab"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes_raw.tsv"
    conda:
        "../envs/fmrib-unpack.yaml"
    shell:
        """
        fmrib_unpack \
        --variable 34 \
        {output} \
        {input}
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
