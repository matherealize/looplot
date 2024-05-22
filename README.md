# looplot: An R package for creating nested loop plots

Generate nested loop plots to display results from controlled experiments and 
statistical simulation studies. Implements the visualisation presented in 
Ruecker, G. and Schwarzer, G. (2014): *Presenting simulation results in a nested loop plot.*
BMC Medical Research Methodology 14(1) <doi:10.1186/1471-2288-14-129>.

## Installation
``` r
# install.packages("devtools")
devtools::install_github("matherealize/looplot")

# to also build vignettes locally use
# devtools::install_github("matherealize/looplot", build_vignettes = TRUE)
```

In case you encounter an http 401 error during installation, this is not due
to the package itself, but due to rate limits set by Github. Please refer to 
[these steps from the remotes Github repository](https://github.com/r-lib/remotes/issues/330#issuecomment-578474009) 
to resolve the issue.

## Usage
Please take a look at the package vignettes:

- [Introduction to nested loop plots and basic workflow](https://matherealize.github.io/looplot_demo.html)
- [Annotated gallery of examples and advanced functionality](https://matherealize.github.io/looplot_gallery.html)

## Contact

For feedback and issues when working with the package please create a new
[Issue here at Github](https://github.com/matherealize/looplot/issues) or 
send [me an email](mailto:michael.kammer@meduniwien.ac.at).
