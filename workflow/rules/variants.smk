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

rule compute_FADS_variants_summary_stats:
    input:
        bgen = config.get("chromosome_11_bgen"),
        bgi = f"{config.get('chromosome_11_bgen')}.bgi",
    output:
        snpstats = f"{TEMP_DIR}/variants/variants.snp-stats"
    conda:
        # TODO add qctool to bioconda
        "../envs/snpstats.yaml"
    threads: 36
    shell:
        """
        bgenix \
        -g {input.bgen} \
        -incl-rsids rs174575 rs1535 rs174583 | \
        qctool \
        -g - \
        -filetype bgen \
        -snp-stats \
        -osnp {output.snpstats}
        """

rule get_ld:
    input:
        bgen = config.get("chromosome_11_bgen"),
        sample = config.get("sample_file"),
        withdrawals = config.get("withdrawals"),
        std_exclusions = config.get("std_exclusions"),
        related_individuals = config.get("related_individuals"),
        nonwhitebritish = config.get("nonwhitebritish")
    output:
        output_file = f"{TEMP_DIR}/variants/ld.vcor"
    threads: 36
    resources:
        mem_mb=500000
    params:
        output_prefix = lambda wildcards, input: op.splitext(output["output_file"])[0]
    shell:
        """
        plink2 \
        --bgen {input.bgen} ref-first \
        --sample {input.sample} \
        --ld-snps rs1535,rs174575,rs174583 \
        --remove {input.withdrawals} {input.std_exclusions} {input.related_individuals} {input.nonwhitebritish} \
        --r2-unphased \
        --out {params.output_prefix}
        """