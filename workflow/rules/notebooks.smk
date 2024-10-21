## Rule to render notebooks for tables

rule render_descriptive_analysis:
    input:
        f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData",
    output:
        "results/notebooks/tables/{ancestry_group}/descriptives.html"
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    script:
        "../notebooks/tables/descriptives.Rmd"

rule render_phenotypic_associations:
    input:
        f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData",
    output:
        "results/notebooks/tables/{ancestry_group}/phenotypic_analysis.html"
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    resources:
        mem_mb=50000
    script:
        "../notebooks/tables/phenotypic_association_analysis.Rmd"

rule render_direction_effect_notebook:
    output:
        "results/notebooks/tables/direction_effect.html"
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    script:
        "../notebooks/tables/direction_effect.Rmd"

rule render_regression_coefficients_notebook:
    input:
        expand(
            f"{TEMP_DIR}/clean/{{ancestry_group}}/model_summaries_additive.RData",
            ancestry_group = ancestry_group
        ),
        expand(
            f"{TEMP_DIR}/clean/{{ancestry_group}}/model_summaries_recessive.RData",
            ancestry_group = ancestry_group
        ),
    output:
        "results/notebooks/tables/regression_coefficients.html"
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    script:
        "../notebooks/tables/regression_coefficients.Rmd"

## Rule to render notebooks for figures

rule render_phenotypic_analyses_plots:
    input:
        f"{TEMP_DIR}/clean/white_british/data_for_models.RData",
    output:
        "results/notebooks/figures/phenotypic_forest_plot.html"
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    resources:
        mem_mb = 50000
    script:
        "../notebooks/figures/phenotypic_forest_plot.Rmd"

rule render_paper_plots:
    input:
       f"{TEMP_DIR}/clean/white_british/model_summaries_additive.RData",
       f"{TEMP_DIR}/clean/any_ancestry/model_summaries_additive.RData",
       f"{TEMP_DIR}/clean/white_british/data_for_models.RData",
    output:
        "results/notebooks/figures/plots_main_results.html"
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    script:
        "../notebooks/figures/plots_main_results.Rmd"

rule render_histogram_plots:
    input:
        f"{TEMP_DIR}/clean/{{ancestry_group}}/data_for_models.RData",
    output:
        "results/notebooks/figures/{ancestry_group}/histograms.html"
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    script:
        "../notebooks/figures/histograms.Rmd"

rule render_regional_plot:
    input:
        "resources/sumstats/SavageJansen_2018_intelligence_metaanalysis.txt"
    output:
        "results/notebooks/figures/regional_plot_FADS2.html"
    params:
        LDlink_token = config.get("LDlink_token")
    envmodules:
        "r/4.1.0-foss-2021a",
        "pandoc/3.1.2"
    script:
        "../notebooks/figures/regional_plot_FADS2.Rmd"
