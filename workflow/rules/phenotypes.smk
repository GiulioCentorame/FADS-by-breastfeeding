def phenotypes_parquets(wildcards):
    checkpoint_output = checkpoints.create_ukb_parquets.get(**wildcards).output[0]
    return expand(f"{TEMP_DIR}/phenotypes/ukb_parquet/{{parquet}}.parquet",
                  parquet = glob_wildcard(os.path.join(checkpoint_output, "{parquet}.parquet".parquet)))

rule strip_comments:
    input:
        "config/variables.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    shell:
        "grep -v '#' {input} > {output}"

checkpoint create_ukb_parquets:
    # Extract the variables of interest in the UKB
    input:
        tab_files = expand(f"{config.get('basket_path')}/{{filename}}.tab",
                           filename = config.get("basket_filename")),
        html_files = expand(f"{config.get('basket_path')}/{{filename}}.html",
                           filename = config.get("basket_filename")),
        R_files = expand(f"{config.get('basket_path')}/{{filename}}.r",
                           filename = config.get("basket_filename")),
    output:
        directory(f"{TEMP_DIR}/phenotypes/ukb_parquet/")
    envmodules:
        "r/4.2.1-foss-2021a"
    conda:
        "../envs/r.yaml"
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
        data =  phenotypes_parquets,
        derived_script = "workflow/scripts/levels.R",
        withdrawals = config.get("withdrawals"),
        std_exclusions = config.get("std_exclusions")
    output:
        clean_data = f"{TEMP_DIR}/phenotypes/phenotypes_clean.tsv"
    params:
        output_path= f"{TEMP_DIR}/phenotypes/ukb_parquet/"
    envmodules:
        "r/4.2.1-foss-2021a"
    conda:
        "../envs/r.yaml"
    script:
        "../scripts/clean_phenotypes.R"
