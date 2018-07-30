## **cgraph**: Computational Graphs
Allows to create, evaluate, and differentiate computational graphs in R. A computational graph is a graph representation of a multivariate function decomposed by its (elementary) operations. Nodes in the graph represent arrays while edges represent dependencies among the arrays. An advantage of expressing a function as a computational graph is that this enables to differentiate the function by automatic differentiation. The 'cgraph' package supports various functions including basic arithmetic, trigonometry functions, and linear algebra functions. It differentiates computational graphs by reverse automatic differentiation. The flexible architecture of the package makes it applicable to solve a variety of problems including local sensitivity analysis, gradient-based optimization, and machine learning.

## Install

You can install cgraph by running one of the following commands in the R command-line.

### Stable Version

The latest stable version can be installed from CRAN by:

```{r eval = F}
  install.packages("cgraph")
```

### Development Version

The latest development version can be installed from Github by:

```{r eval = F}
  library(devtools)

  install_github("triepels/cgraph")
```

Notice, the development version may contain code that is experimental and not thoroughly tested. If you find a bug, please report it [here](https://github.com/triepels/cgraph/issues).

### More Information
Please visit the package [website](https://cgraph.org).
