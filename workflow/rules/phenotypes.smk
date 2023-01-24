rule strip_comments:
    input:
        "config/variables.txt"
    output:
        f"{TEMP_DIR}/phenotypes/field_ids.txt"
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
        rds_file = temp(f"{TEMP_DIR}/phenotypes/all_phenotypes.rds")
    envmodules:
        "r/4.2.1-foss-2021a"
    # conda:
    #     "../envs/r.yaml"
    threads: 96
    resources:
        mem_mb=1000000,
        disk_mb=4000,
        time_min=400
    script:
        "../scripts/convert_basket.R"

rule select_phenotypes_and_participants:
    input:
        data = f"{TEMP_DIR}/phenotypes/all_phenotypes.rds",
        field_ids = f"{TEMP_DIR}/phenotypes/field_ids.txt",
        withdrawals = config.get("withdrawals"),
        std_exclusions = config.get("std_exclusions"),
        related_individuals = config.get("related_individuals")
    output:
        filtered_data = f"{TEMP_DIR}/phenotypes/vars_subset.rds"
    envmodules:
        "r/4.2.1-foss-2021a"
    # conda:
    #     "../envs/r.yaml"
    threads: 96
    resources:
        mem_mb=500000
    script:
        "../scripts/subset_phenotypes.R"

rule clean_SBP:
    input:
        f"{config.get('basket_path')}/ukb39610.csv"
    output:
        f"{TEMP_DIR}/phenotypes/sbp.tsv"
    script:
        "../scripts/SBP.R"

rule clean_DBP:
    input:
        f"{config.get('basket_path')}/ukb39610.csv"
    output:
        f"{TEMP_DIR}/phenotypes/dbp.tsv"
    script:
        "../scripts/DBP.R"
