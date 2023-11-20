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
        rds_file = f"{TEMP_DIR}/phenotypes/all_phenotypes.rds"
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

rule subset_participants_white_british:
    input:
        data = f"{TEMP_DIR}/phenotypes/all_phenotypes.rds",
        field_ids = f"{TEMP_DIR}/phenotypes/field_ids.txt",
        withdrawals = config.get("withdrawals"),
        std_exclusions = config.get("std_exclusions"),
        related_individuals = config.get("related_individuals"),
        nonwhitebritish = config.get("nonwhitebritish")
    output:
        filtered_data = f"{TEMP_DIR}/phenotypes/white_british/subset.rds"
    envmodules:
        "r/4.2.1-foss-2022a"
    # conda:
    #     "../envs/r.yaml"
    threads: 96
    resources:
        mem_mb=500000
    script:
        "../scripts/subset_phenotypes_white_british.R"

rule subset_participants_any_ancestry:
    input:
        data = f"{TEMP_DIR}/phenotypes/all_phenotypes.rds",
        field_ids = f"{TEMP_DIR}/phenotypes/field_ids.txt",
        withdrawals = config.get("withdrawals"),
        std_exclusions = config.get("std_exclusions"),
        related_individuals = config.get("related_individuals")
    output:
        filtered_data = f"{TEMP_DIR}/phenotypes/any_ancestry/subset.rds"
    envmodules:
        "r/4.2.1-foss-2022a"
    # conda:
    #     "../envs/r.yaml"
    threads: 96
    resources:
        mem_mb=500000
    script:
        "../scripts/subset_phenotypes_any_ancestry.R"
