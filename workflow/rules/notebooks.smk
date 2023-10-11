rule render_descriptive_analysis:
    input:
        f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData",
    output:
        "results/notebooks/{ancestry_group}/descriptives.html"
    envmodules:
        "r/4.2.1-foss-2021a"
    script:
        "../notebooks/descriptives.Rmd"

rule render_phenotypic_associations:
    input:
        f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData",
    output:
        "results/notebooks/{ancestry_group}/phenotypic_analysis.html"
    envmodules:
        "r/4.2.1-foss-2021a"
    script:
        "../notebooks/phenotypic_analysis.Rmd"


rule render_direction_effect_notebook:
    output:
        "results/notebooks/direction_effect.html"
    envmodules:
        "r/4.2.1-foss-2021a"
    script:
        "../notebooks/direction_effect.Rmd"

rule render_paper_plots:
    input:
       f"{TEMP_DIR}/clean/white_british/model_summaries_additive.RData",
       f"{TEMP_DIR}/clean/any_ancestry/model_summaries_additive.RData",
       f"{TEMP_DIR}/clean/white_british/model_summaries_recessive.RData",
    output:
        "results/notebooks/plots_paper.html"
    envmodules:
        "r/4.2.1-foss-2021a"
    script:
        "../notebooks/plots_paper.Rmd"


rule render_histogram_plots:
    input:
        f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData",
    output:
        "results/notebooks/{ancestry_group}/histograms.html"
    envmodules:
        "r/4.2.1-foss-2021a"
    script:
        "../notebooks/histograms.Rmd"