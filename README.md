# The Status of Women in Preindustrial Societies - a Reanalysis

# Data sources

This projects uses data of the Standard Cross-Cultural Sample, accessible on [D-place](https://d-place.org/contributions/SCCS). The raw data is used in these scripts is directly downloadable [on Zenodo](https://zenodo.org/records/17601873). 

It consists in four tables:
- `societies.csv`: description of the societies of the Standard Cross-Cultural Sample (SCCS).
- `variables.csv`: description of the variables of the Standard Cross-Cultural Sample.
- `codes.csv`: description of the values taken by the variables.
- `sccs.csv`: actual values of variables measured on societies of the Standard Cross-Cultural Sample.
The data used for the present analysis is a subset of the SCCS, containing the measurements made by Whyte in 1978.

We additionally used height data reported in a supplementary table by Gleeson and Kushnick (2018).  

# Scripts

`00_runall.r` sequentially runs all the analyses used in the article. This includes:
- `01_` data import
- `02_` multivariate analyses of the response variables
- `03_` analyses o the relationship with putative predictors
- `04_` relation of women's status variables with sexual size dimorphism
- `05_` tables shown in the article and its appendix
- `07_` figures shown in the article and its appendix

For reproducibility, consult the `sessionInfo.txt` file at the root of this directory (which is the output of the `sessionInfo()` R function).

# References for the data:

Whyte, M. K. (1978). Cross-cultural codes dealing with the relative status of women. Ethnology, 17(2), 211–237.

Gleeson, B. T., & Kushnick, G. (2018). Female status, food security, and stature sexual dimorphism: Testing mate choice as a mechanism in human self‐domestication. American Journal of Physical Anthropology. https://doi.org/10.1002/ajpa.23642

