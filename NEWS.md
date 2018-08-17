cgraph 2.0.2
----------------------------------------------------------------

Bug fixes:

* Nodes with a fixed value are now properly printed to the R console.
* Function `approx.grad` no longer changes the parent environment of argument `values` when the argument is an environment.
* Function `run`, `gradients`, and `approx.grad` no longer cause an infinite loop when member `values` of a `cgraph` object is supplied to the `values` argument of the functions.
* Fixed protection bug in function `gradients` which caused the R session to crash unexpectedly.
* Function `approx.grad` now checks whether node `x` and `y` evaluate to a numeric vector or array.

cgraph 2.0.1
----------------------------------------------------------------

Bug fixes:

* Function `run` and `gradients` no longer change the parent environment of argument `values` when the argument is an environment.
* Function `name` no longer generates a name that indicates the node type for performance reasons. Instead, it simply generates the name 'node' followed by the number of nodes that have been added to the graph.
* Fixed several protection bugs in the C-API thanks to `rchk`.

cgraph 2.0.0
----------------------------------------------------------------

Comments:

* The license of the package has changed from GPL-3 to Apache License 2.0.

Features:

* Added hyperbolic trigonometry operations `sinh` and `cosh`.
* Added inverse hyperbolic trigonometry operations `asinh` and `acosh`.
* The elements of member `nodes` of a `cgraph` object are now named to allow more convenient access to the symbols of the nodes in a graph.
* You can now instruct the active graph to no longer automatically evaluate a node when it is printed to the console by setting `options(cg.autorun = FALSE)`.

Documentation:

* Removed collate field in package description as it is no longer needed.

Bug fixes:

* Improved error handling. An error is now raised when an invalid object is provided to the methods of a `cgraph` object instead of trying to coerce the object to a valid object.
* Function `opr` now ignores argument `grads` when its elements are not properly named.
* Function `run` now checks whether each node evaluates to a numeric vector or array.
* Function `gradients` now checks whether the node supplied to argument `name` evaluates to a numeric vector or array.
* Function `get.parms` and `add.parms` now correctly call the underlying C-API.

cgraph 1.0.1
----------------------------------------------------------------

Comments:

* CRAN Review on the cgraph 1.0.0 submission.

Documentation:

* Improved the package title and description.
* Added RGraphviz to suggests.
* Added examples to the documentation of the most important methods of a `cgraph` object and their corresponding wrapper functions.

cgraph 1.0.0
----------------------------------------------------------------

Comments:

* CRAN Submission.
