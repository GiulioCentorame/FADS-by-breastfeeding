rule extract_FADS_variants_allelic_dosage:
    # Extract the FADS variants by rsid
    # NOTE plink2 --export A counts allele from REF, not ALT
    input:
        bgen = config.get("chromosome_11_bgen"),
        bgi = f"{config.get('chromosome_11_bgen')}.bgi"
    output:
        variants = f"{TEMP_DIR}/variants/variants.raw"
    conda:
        "../envs/plink2.yaml"
    params:
        output_prefix = f"{TEMP_DIR}/variants/variants"
    shell:
        """
        plink2 \
        --bgen {input.bgen} ref-first \
        --export A \
        --snps rs174575, rs1535, rs174583 \
        --out {params.output_prefix}
        """
