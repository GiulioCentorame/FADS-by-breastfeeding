rule strip_comments:
    input:
        "config/variables.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    shell:
        "grep -v '#' {input} > {output}"

rule extract_phenotypes_variables:
    # Extract the variables of interest in the UKB
    input:
        tab_files = expand(f"{config.get('basket_path')}/{{filename}}.tab",
                           filename = config.get("basket_filename")),
        html_files = expand(f"{config.get('basket_path')}/{{filename}}.html",
                           filename = config.get("basket_filename")),
        R_files = expand(f"{config.get('basket_path')}/{{filename}}.r",
                           filename = config.get("basket_filename")),
        phenotypes = f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    output:
        f"{TEMP_DIR}/phenotypes/ukb_parquet/{{parquet_names}}.parquet"
    threads: 96
    params:
        output_path= f"{TEMP_DIR}/phenotypes/ukb_parquet/"
    resources:
        mem_mb=100000,
        time_min=400
    script:
        "../scripts/convert_basket.R"

rule select_and_clean_phenotypes:
    input:
        data = f"{TEMP_DIR}/phenotypes/phenotypes_raw.tsv",
        derived_script = "workflow/scripts/levels.R",
        withdrawals = config.get("withdrawals"),
        std_exclusions = config.get("std_exclusions")
    output:
        clean_data = f"{TEMP_DIR}/phenotypes/phenotypes_clean.tsv"
    conda:
        "../envs/clean_phenotypes.yaml"
    script:
        "../scripts/clean_phenotypes.R"
