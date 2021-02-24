
## Sobol-MDA variable importance for random forests
C.Benard

sobolMDA implements the Sobol-MDA algorithm: 
a variable importance measure for random forests, which provides the proportion of output explained
variance lost when a given variable is removed from the data. The Sobol-MDA
fixes the biais of Breiman's MDA when input variables are dependent, and is 
based on projections of the tree partitions of the forest. 

sobolMDA is a fork from the project ranger, a fast random forest implementation (https://github.com/imbs-hl/ranger).

### Installation

To recompile from source, run the following R command:
`install.package("release/sobolMDA_0.1.0.tar.gz")`

Pre-compiled binaries for windows are available in the release folder.

### References
* Wright, M. N. & Ziegler, A. (2017). ranger: A fast implementation of random forests for high dimensional data in C++ and R. J Stat Softw 77:1-17. https://doi.org/10.18637/jss.v077.i01.

