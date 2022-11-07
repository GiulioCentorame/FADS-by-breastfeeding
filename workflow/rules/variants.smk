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
        mem_mb = 50000
    shell:
        """
        plink2 \
        --bgen {input.bgen} ref-first \
        --export A \
        --sample {input.sample} \
        --snps rs174575, rs1535, rs174583 \
        --out {params.output_prefix}
        """
