# mabt-experiments

This repository contains the R code (simulation experiments, figures, real-data examples, OpenML benchmark study) accompanying the paper:

Pascal Rink & Werner Brannath (2025). Post-selection confidence bounds for prediction performance. Machine Learning, Volume 114, 82, Springer Nature. URL: [https://link.springer.com/article/10.1007/s10994-024-06632-w](https://link.springer.com/article/10.1007/s10994-024-06632-w)

Status: Archived (research artifact).

Further development continues in:
[https://github.com/pascalrink/prediction-performance-ci](https://github.com/pascalrink/prediction-performance-ci)

The simulations we conducted are extensive, so we uploaded results to (a) the model estimation and selection process, and (b) the estimated confidence bounds from those models. Regarding (a) we only uploaded the results to the first ten runs of the lasso simulations. Please find the full set of lasso model estimation and selection results [here](https://nc.uni-bremen.de/index.php/s/dj23Sp3kXdYJWGm). To replicate the results in the publication, replace the '1-savefiles' directory from GitHub with the unpacked ZIP file. The uploaded results to (b) however are completely uploaded here at GitHub. For the random forest simulations there are no intermediate results, so they need to run from scratch. 

Also, the full results for the conditional coverage probability simulation in 6-additions/conditional-coverage can be found [here](https://nc.uni-bremen.de/index.php/s/dj23Sp3kXdYJWGm). The results in this GitHub repo only include those simulation runs that are relevant for the comparison and plots discussed in the manuscript (due to file size of the full results).
