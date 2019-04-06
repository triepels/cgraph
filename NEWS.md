cgraph 4.0.2
----------------------------------------------------------------

Comments:

* The default name of a node is now `v` followed by the node id.
* The classes for the node types (i.e. constant, parameter, input, and operator) are removed. Instead, the node type is stored as an integer attribute in a `cg_node` object for efficiency reasons.
* Argument `index` of function `cg_graph_gradients` can now also be `NULL` to differentiate nodes element-wise. This is now also the default behavior.

Bug fixes:

* Fixed memory leak when performing a forward pass and backward pass using function `cg_graph_run` and `cg_graph_gradients` respectively.
* Function `bsum` now prints a correct error message when argument `block_size` is not a numerical scalar.

cgraph 4.0.1
----------------------------------------------------------------

Bug fixes:

* Fixed an bug that caused an error when installing `cgraph` on R versions before 3.5.
* Function `cg_session_set_graph` no longer prints `NULL` to the console when the active graph is changed. 
* Operator `cg_sigmoid` now works correctly when argument `x` is a logical or integer vector or array.

cgraph 4.0.0
----------------------------------------------------------------

Comments:

* The C-API has been completely reworked.
* The R6 class `cgraph` is removed. To create a computational graph, use function `cg_graph` instead.
* Method `get_parms` and `add_parms` are no longer available.
* Method `active` is removed. The active graph can now be retrieved and changed by function `cg_session_graph` and `cg_session_set_graph` respectively.
* Method `adj_mat` is removed.
* S3 overloaded method `print` is removed.
* Function `const`, `input`, `parm`, and `opr` have been renamed to `cg_constant`, `cg_input`, `cg_parameter`, and `cg_operator` respectively.
* Function `val` and `set` are removed. The value of a constant or parameter node can be retrieved or changed directly by calling `x$value` where `x` is the environment of a `cg_node` object.
* Function `run` and `gradients` have been renamed to `cg_graph_run` and `cg_graph_gradients` respectively.

Features:

* The function that is called by an operator is no longer stored as a symbol in the environment of the operator. Instead, each function now has its own global `cg_function` object which can be linked to an operator. This significantly reduces the size of a `cg_graph` object.

Documentation:

* Package `R6` has been removed from the `Imports` section in the package description.
* Package `Rgraphviz` has been removed from the `Suggests` section in the package description.

Bug fixes:

* Fixed a bug that caused a node to be evaluated to its corresponding `cg_node` environment rather than its value when calling `cg_graph_run` and `cg_graph_gradients`.
* Fixed a bug that caused a segfault when calling function `cg_graph_run` and `cg_graph_gradients` on a `cg_graph` object which contains nodes with an invalid id.

cgraph 3.0.1
----------------------------------------------------------------

Bug fixes:

* Fixed a bug that caused S3 methods called by operators to dispatch incorrectly on R versions before 3.5.

cgraph 3.0.0
----------------------------------------------------------------

Comments:

* Large parts of the C-API have been reworked.
* Some naming conventions have changed. Individual names in the name of a function are now separated with an underscore (_) instead of a dot (.). For example, operator `cg.matmul` is now named `cg_matmul`. The same rule applies to class names. For example, class `cg.node` is now named `cg_node`.
* Function `name` is removed. Names for nodes are now generated internally in the C-API.
* Function `approx.gradients` is removed. A simliar function is still available as `cgraph:::approx_grad` in the package namespace. However, it should be noted that this function is not well-optimized and should only be used for testing purposes.
* Calling `print` on a `cg_node` object no longer prints the value of the node. Use function `val` to evaluate a node.

Features:

* The initialization method of a cgraph object has a new argument `library` which can be used to specify which function library the graph uses.
* The calls of functions and their corresponding gradient functions are now build at run-time. This potentially allows operators to accept a variable number of arguments.
* Added function `val` and `set` to retrieve or set the value of a node respectively.
* A node can now also be named 'grad'. 'grad' is no longer a reserved word.
* Operator `cg_mean` now calls the base `mean` function.
* Operator `cg_crossprod` and `cg_tcrossprod` now allow argument `y` to be missing (similarly as the corresponding base functions).
* Argument `dim` of operator `cg_reshape` now expects an `cg_node` object instead of an integer scalar or vector that holds the new array dimensions.

Bug fixes:

* Fixed a bug that caused a segfault when using cgraph in RStudio on Red Hat Enterprise Linux.

cgraph 2.0.3
----------------------------------------------------------------

Comments:

* The S3 overloaded methods of prefix operators have been marked as deprecated and will be removed in the next major release. Only infix operators like `+` or `-` will have S3 overloaded methods.

Bug fixes:

* Operator `sigmoid` now correctly handles numerical underflow.
* Operator `cg.pmax` and `cg.pmin` are now correctly differentiated when argument `x` and `y` are both arrays.

Features:

* Added logical operators `!`, `==`, `!=`, `<`, `>`, `<=`, and `>=`.

cgraph 2.0.2
----------------------------------------------------------------

Documentation:

* The documentation of function `const`, `input`, `parm`, and `opr` now correctly states that argument `name` must be a character scalar (not a symbol).

Bug fixes:

* Nodes that are given a value upon creation are now properly printed to the R console.
* Function `run`, `gradients`, and `approx.grad` no longer cause an infinite loop when member `values` of a `cgraph` object is supplied to the `values` argument of the functions.
* Fixed protection bug in function `gradients` which caused the R session to crash unexpectedly.
* Function `approx.grad` now checks whether node `x` and `y` evaluate to a numeric vector or array.
* Function `approx.grad` no longer changes the parent environment of argument `values` when the argument is an environment.

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
