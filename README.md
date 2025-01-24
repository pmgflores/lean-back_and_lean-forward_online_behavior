# Lean-back and Lean-forward Online Behaviors

## Overview
This repository contains the data, code, and figures for the research article:

Flores, P. M. & Hilbert, M. (2023). Lean-back and lean-forward online behaviors: The role of emotions in passive versus proactive information diffusion of social media content. *Computers in Human Behavior, 147*, 107841. https://doi.org/10.1016/j.chb.2023.107841

## Repository Structure
```
├── data/                   # Input datasets
├── scripts/               
│   ├── functions/         # R function files (.R)
│   └── rmd/               # R Markdown files (.Rmd) and .md files with results
├── renv/                  # R environment management
├── .Rprofile             # R profile configuration
├── lean[...].Rproj       # R project file
└── renv.lock             # Package version lockfile
```

## Repository Contents
* `data/`: Datasets for all six case studies (JB, JJ, LA, NZ, N20, N21)
* `scripts/functions/`: R function files for analysis
* `scripts/rmd/Supplemental_Material_1.Rmd`: R Markdown source file for sentiment and emotion analysis
* `scripts/rmd/Supplemental_Material_2.Rmd`: R Markdown source file for dynamic emotion analysis
* `renv.lock`: Package dependencies and versions used in the analysis

## Getting Started
1. Clone this repository
2. Open the project in RStudio
3. Install package management dependencies:
```r
install.packages("renv", repos="https://cloud.r-project.org")
renv::restore()
```
4. Open and run either `Supplemental_Material_1.Rmd` or `Supplemental_Material_2.Rmd` to replicate the analysis
   * The RMD files allow full replication of the analysis

## Dependencies
This project uses `renv` for package management to ensure reproducibility. All necessary packages and their versions are specified in `renv.lock` and will be automatically installed when running `renv::restore()`.

## Required Packages
Key packages used in this analysis:
* `here`: File path management
* `knitr`: Document generation
* `ggplot2`: Data visualization
* `dplyr`: Data manipulation
* `psych`: Psychological data analysis
* `effectsize`: Effect size calculations
* `ggpubr`: Publication-ready plots
* `lavaan`: Structural equation modeling
* `reshape2`: Data reshaping
* And others (full list managed via `renv`)

## Citation
If you use this code or data in your research, please cite:

```bibtex
@article{flores2023lean,
  title={Lean-back and lean-forward online behaviors: The role of emotions in passive versus proactive information diffusion of social media content},
  author={Flores, P. M. and Hilbert, M.},
  journal={Computers in Human Behavior},
  volume={147},
  pages={107841},
  year={2023},
  publisher={Elsevier},
  doi={10.1016/j.chb.2023.107841}
}
```

## Contact
For questions about the code or paper, please contact the corresponding author Pablo M. Flores (https://pablomflores.com).

## License
This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png

You are free to:
- Share: copy and redistribute the material in any medium or format
- Adapt: remix, transform, and build upon the material for any purpose, even commercially

Under the following terms:
- Attribution: You must give appropriate credit, provide a link to the license, and indicate if changes were made.