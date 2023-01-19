rule strip_comments:
    input:
        "config/variables.txt"
    output:
        f"{TEMP_DIR}/phenotypes/phenotypes.txt"
    shell:
        "grep -v '#' {input} > {output}"

rule create_phenotypes_file:
    # Extract the variables of interest in the UKB
    input:
        tab_files = expand(f"{config.get('basket_path')}/{{filename}}.tab",
                           filename = config.get("basket_filename")),
        html_files = expand(f"{config.get('basket_path')}/{{filename}}.html",
                           filename = config.get("basket_filename")),
        R_files = expand(f"{config.get('basket_path')}/{{filename}}.r",
                           filename = config.get("basket_filename")),
    output:
        rda_file = f"{TEMP_DIR}/phenotypes/all_phenotypes.rda"
    envmodules:
        "r/4.2.1-foss-2021a"
    # conda:
    #     "../envs/r.yaml"
    threads: 96
    resources:
        mem_mb=1000000,
        time_min=400
    script:
        "../scripts/convert_basket.R"

rule select_and_clean_phenotypes:
    input:
        data = f"{TEMP_DIR}/phenotypes/all_phenotypes.rda",
        derived_script = "workflow/scripts/levels.R",
        withdrawals = config.get("withdrawals"),
        std_exclusions = config.get("std_exclusions")
    output:
        clean_data = f"{TEMP_DIR}/phenotypes/phenotypes_clean.tsv"
    envmodules:
        "r/4.2.1-foss-2021a"
    # conda:
    #     "../envs/r.yaml"
    script:
        "../scripts/clean_phenotypes.R"
