rule extract_FADS_variants_allelic_dosage:
    # Extract the FADS variants by rsid
    # NOTE plink2 --export A counts allele from REF, not ALT
    input:
        bgen = config.get("chromosome_11_bgen"),
        bgi = f"{config.get('chromosome_11_bgen')}.bgi",
        sample = config.get("sample_file")
    output:
        variants = f"{TEMP_DIR}/variants/variants.raw"
    conda:
        "../envs/plink2.yaml"
    params:
        output_prefix = f"{TEMP_DIR}/variants/variants"
    threads: 36
    resources:
    # HACK find a less memory-intensive way to get this
    # Potentially bgenix + plink2?
        mem_mb = 500000
    shell:
        """
        plink2 \
        --bgen {input.bgen} ref-first \
        --export A \
        --sample {input.sample} \
        --snps rs174575, rs1535, rs174583 \
        --out {params.output_prefix}
        """

rule create_exclusions_list:
    input:
        config.get("withdrawals"),
        config.get("std_exclusions"),
        config.get("related_individuals"),
        config.get("nonwhitebritish")
    output:
        f"{TEMP_DIR}/phenotypes/exclusions.txt"
    shell:
        "cat {input} > {output}"

rule extract_data_bgen:
    input:
        bgen = config.get("chromosome_11_bgen"),
        bgen_index = config.get("chromosome_11_bgen")+".bgi",
        sample = config.get("sample_file"),
        exclusions = f"{TEMP_DIR}/phenotypes/exclusions.txt"
    output:
        filtered_bgen = f"{TEMP_DIR}/variants/filtered.bgen",
        filtered_sample = f"{TEMP_DIR}/variants/filtered.sample"
    shell:
        """
        bgenix \
        -g {input.bgen} \
        -incl-rsids rs174575 rs1535 rs174583 | \
        qctool \
        -g - \
        -s {input.sample} \
        -filetype bgen \
        -excl-samples {input.exclusions} \
        -og {output.filtered_bgen} \
        -os {output.filtered_sample}
        """

# TODO rewrite with restricted sample only
rule compute_FADS_variants_summary_stats:
    input:
        bgen = f"{TEMP_DIR}/variants/filtered.bgen",
        sample = f"{TEMP_DIR}/variants/filtered.sample"
    output:
        snpstats = f"{TEMP_DIR}/variants/variants.snp-stats"
    conda:
        # TODO add qctool to bioconda
        "../envs/snpstats.yaml"
    threads: 36
    shell:
        """
        qctool \
        -g {input.bgen} \
        -s {input.sample} \
        -snp-stats \
        -osnp {output.snpstats}
        """

# TODO rewrite with ldbird
rule calculate_ld:
    input:
        bgen = f"{TEMP_DIR}/variants/filtered.bgen",
        sample = f"{TEMP_DIR}/variants/filtered.sample"
    output:
        sqlite = f"{TEMP_DIR}/ld/ld.sqlite"
    envmodules:
        "sqlite/3.42.0-gcccore-12.3.0"
    params:
        sqlite_query = "workflow/scripts/ld_table.sqlite"
    shell:
        # The first call runs LDBIRD to compute LD
        # The second call runs a sqlite script to get a
        # table with all the pairs of SNPs
        # (from https://www.well.ox.ac.uk/~gav/ldbird/documentation/getting_started.html)
        # HACK this is a single step since sqlite is a pain to run in batch mode with
        # parameters, it's easier to just modify the file in place
        """
        ldbird \
        -g1 {input.bgen} \
        -s {input.sample} \
        -o {output.sqlite}

        sqlite3 -batch {output.sqlite} < {params.sqlite_query}
        """

rule get_ld_table:
    input:
        sqlite = f"{TEMP_DIR}/ld/ld.sqlite"
    output:
        ld_table = f"{TEMP_DIR}/ld/ld_table.tsv"
    envmodules:
        "sqlite/3.42.0-gcccore-12.3.0"
    shell:
        """
        sqlite3 \
        -column \
        -header \
        {input.sqlite} \
        ".mode tabs" "SELECT * FROM MyRView" > {output.ld_table}
        """
