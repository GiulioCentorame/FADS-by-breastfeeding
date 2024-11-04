rule download_Savage_2018:
    output:
        "resources/sumstats/SavageJansen_2018_intelligence_metaanalysis.txt"
    params:
        url = "https://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/GCST006001-GCST007000/GCST006250/sumstats/SavageJansen_2018_intelligence_metaanalysis.txt"
    shell:
        "wget {params.url} -O {output}"
