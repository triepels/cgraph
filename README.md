# Description

The cgraph package allows to create, evaluate, and differentiate computational graphs in R. A computational graph is a graph representation of a multivariate function decomposed by its (elementary) operations. Nodes in the graph represent arrays while edges represent dependencies among the arrays. One of the advantages of expressing a function as a computational graph is that this enables to differentiate the function by automatic differentiation. The cgraph package supports various functions including basic arithmetic, trigonometry functions, and linear algebra functions. It differentiates computational graphs by reverse automatic differentiation. The flexible architecture of the package makes it applicable to solve a variety of problems including local sensitivity analysis, gradient-based optimization, and machine learning.

# Install

You can install cgraph by running one of the following commands in the R command-line:

## Binary

```{r eval = F}
    install.packages("https://cgraph.org/releases/cgraph_0.8.16.zip", repos = NULL)
```

## Source

```{r eval = F}
  install.packages("https://cgraph.org/releases/cgraph_0.8.16.tar.gz", repos = NULL, type = "source")
```

Please note that this an early development version which may contain bugs or behave unexpectedly. If you find a bug, please report it [here](https://github.com/triepels/cgraph/issues). For more information about the cgraph package please visit [https://www.cgraph.org/](https://www.cgraph.org/).
