# Effects of harvesting and prescribed burns in global forest ecosystems
This repository includes the code and data associated with a manuscript, "Impacts of harvesting and prescribed burning on forest soil carbon dynamics: A global meta-analysis". 

**Article**: Ono, M., & Noormets, A. (2025). Impacts of Harvesting and Prescribed Burning on Forest Soil Carbon Dynamics: A Global Meta-Analysis. Forests, 16(10), 1555. https://doi.org/10.3390/f16101555

**Data & Codes**: Ono, M. (2025). Data and Software from: Impacts of harvesting and prescribed burning on forest soil dynamics: A global meta-analysis (Version 1.1.0) (Computer software). https://doi.org/10.5281/zenodo.17290818 <br><br>

#### Contents
| File title | Description |
| --- | --- |
| Ln_RR_raw.csv | Data file with original values for control (C_) and treatment (T_), and log-transformed response ratios |
| Abbreviation.xlsx | A list of abbreviations used across the project |
| meta_analysis_v2.Rmd, meta_analysis_v1.Rmd (OLD) |	Main file to calculate response ratios. A bit messy with earlier attempts for other methods|
| categorical_comparison.Rmd, cat_comparison_biogeochem.R | Calculating the changes under each subcategory for SR components under cat_comparison_graph.Rmd) and other biogeochemical variables (cat_comparison_biogeochem.R) |
|cat_comparison_graph.Rmd | Creating graphs for categorical comparisons above |
|model_selection.Rmd | Making a ranking of variables to estimate SR components |
| Site_location.Rmd | Visualizing study sites in a global map for Fig. 1 |
| Time_since.Rmd | Estimating and visualizing how many years it takes till response ratios become 1 |
| Funnel_plot.Rmd	| Making a funnel plot (Fig. S1) to show that the sampled data are not biased |
