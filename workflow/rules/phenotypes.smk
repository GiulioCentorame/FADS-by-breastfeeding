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
        tab_files = expand(f"{config.get('basket_path')}/{{filename}}.tab",
                           filename = config.get("basket_filename")),
        list_phenotypes = f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes_raw.tsv"
    threads: 16
    resources:
        mem_mb=10000,
        time_min=400
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

rule extract_variable_levels:
    # Extract only the parts of the original UKB script that involve our variables
    input:
        original_scripts = expand(f"{config.get('basket_path')}/{{filename}}.r",
                           filename = config.get("basket_filename")),
        list_phenotypes = f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    output:
        derived_script = f"{TEMP_DIR}/phenotypes/variable_levels.R"
    shell:
        """
        cat {input.original_scripts} | \
        grep -Fwf <(sed 's/^/f./' {input.list_phenotypes}) > \
        {output.derived_script}
        """

rule clean_phenotypes:
    input:
        data = f"{TEMP_DIR}/phenotypes/phenotypes_raw.tsv",
        derived_script = f"{TEMP_DIR}/phenotypes/variable_levels.R"
    output:
        clean_data = f"{TEMP_DIR}/phenotypes/phenotypes_clean.tsv"
    conda:
        "../envs/clean_phenotypes.yaml"
    script:
        "../scripts/clean_phenotypes.R"
