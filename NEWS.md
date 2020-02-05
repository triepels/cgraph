cgraph 6.0.1
----------------------------------------------------------------

New Features:

* Added operator `cg_rowmeans` and `cg_colmeans`.

Bug Fixes:

* Global symbols in the C-API are now correctly defined.

cgraph 6.0.0
----------------------------------------------------------------

New Features:

* Added operator `cg_length`, `cg_dim`, `cg_nrow`, and `cg_ncol`.
* Added function `cg_graph_get` to retrieve a node by name from a graph.
* Nodes can now be subsetted at run-time. Subsetting can be performed by calling the new operators `cg_subset1` or `cg_subset2`, or the corresponding overloaded S3 methods `[` and `[[` respectively, on a `cg_node` object.
* Function `cg_graph` has a new argument `eager` which can be used to enable or disable eager evaluation. Eager evaluation can also be enabled or disabled on an existing graph by changing data member `eager` of a `cg_graph` object.
* Function `cg_graph_forward` and `cg_graph_backward` now allow argument `target` to be the name of the target node, in which case a linear search is performed to retrieve the node by name from the graph prior to a forward or backward pass. Please note that this search can be expensive for large graphs.

Comments:

* The name of a node no longer has to be unique.
* Function `cg_graph_run` and `cg_graph_gradients` have been removed.
* Data member `nodes` of a `cgraph` object is no longer named for performance reasons. Use function `cg_graph_get` to retrieve a node.
* The performance of function `cg_operator` has significantly improved. Graphs can now be build considerbly faster.

cgraph 5.0.1
----------------------------------------------------------------

Bug Fixes:

* Fixed a bug in function `cg_graph_backward` which sometimes caused a computational graph to be traversed in an incorrect order and compute incorrect gradients.

cgraph 5.0.0
----------------------------------------------------------------

New Features:

* Added operator `cg_square`.
* Operators are now evaluated eagerly. This means that, when an operator is added to a computational graph, it is immediately evaluated (if possible). Eager evaluation makes it easier to debug a computational graph and enables the user to change its control flow at run-time.
* Added function `cg_graph_forward` and `cg_graph_backward` to perform a forward pass and backward pass respectively. These functions are similar to function `cg_graph_run` and `cg_graph_gradients` but do not use environments to store the values and derivatives of nodes. Instead, the value and derivative of a node are stored 'locally' at each node and can be retrieved by data member `value` and `grad` respectively.

Comments:

* Function `cg_graph_run` and `cg_graph_gradients` are deprecated and will be removed in the next major release. Use function `cg_graph_forward` and `cg_graph_backward` to perform a forward pass and backward pass instead.

Bug Fixes:

* Fixed several memory leaks and protection bugs.

cgraph 4.0.3
----------------------------------------------------------------

Bug Fixes:

* Fixed several protection bugs in the C-API thanks to `rchk`.

cgraph 4.0.2
----------------------------------------------------------------

New Features:

* Argument `index` of function `cg_graph_gradients` can now also be `NULL`, in which case the nodes in a graph are differented element-wise. This is now also the default behavior.

Comments:

* The default name of a node is now `v` followed by the node id.
* The S3 classes of the node types (i.e. constant, parameter, input, and operator) are removed for performance reasons. The type of a node can now be retrieved by data member `type` of a `cg_node` object.

Bug Fixes:

* Fixed memory leak in function `cg_graph_run` and `cg_graph_gradients`.
* Function `bsum` now prints a correct error message when argument `block_size` is not a numerical scalar.

cgraph 4.0.1
----------------------------------------------------------------

Bug Fixes:

* Fixed an bug that caused an error when installing `cgraph` on R versions before 3.5.
* Function `cg_session_set_graph` no longer prints `NULL` to the console when the active graph is changed. 
* Operator `cg_sigmoid` now works correctly when argument `x` is a logical or integer vector or array.

cgraph 4.0.0
----------------------------------------------------------------

Comments:

* The C-API has been completely reworked.
* Operators `cg_reshape`, `!`, `==`, `!=`, `<`, `>`, `<=`, and `>=` are removed.
* R6 class `cgraph` is removed. Use function `cg_graph` to create a new graph. The active graph can now be retrieved and changed by function `cg_session_graph` and `cg_session_set_graph` respectively.
* Function `const`, `input`, `parm`, and `opr` have been renamed to `cg_constant`, `cg_input`, `cg_parameter`, and `cg_operator` respectively.
* Function `val` and `set` are removed. The value of a constant or parameter can now be retrieved or changed by data member `value` of a `cg_node` object.
* Function `run` and `gradients` have been renamed to `cg_graph_run` and `cg_graph_gradients` respectively.

New Features:

* The function that is called by an operator is no longer stored as a symbol in the environment of the operator. Instead, each function now has its own global `cg_function` object which can be linked to an operator. This significantly reduces the size of a `cg_graph` object.

Bug Fixes:

* Fixed a bug that caused a node to be evaluated to its corresponding `cg_node` environment rather than its value when calling `cg_graph_run` and `cg_graph_gradients`.
* Fixed a bug that caused a segfault when calling function `cg_graph_run` and `cg_graph_gradients` on a `cg_graph` object which contains nodes with an invalid id.

cgraph 3.0.1
----------------------------------------------------------------

Bug Fixes:

* Fixed a bug that caused overloaded S3 methods called by operators to dispatch incorrectly on R versions before 3.5.

cgraph 3.0.0
----------------------------------------------------------------

Comments:

* Large parts of the C-API have been reworked.
* Some naming conventions have changed. Individual names in the name of a function are now separated with an underscore (_) instead of a dot (.). For example, operator `cg.matmul` is now named `cg_matmul`. The same rule applies to class names. For example, class `cg.node` is now named `cg_node`.
* Function `name` is removed. Names for nodes are now generated internally in the C-API.
* Function `approx.gradients` is no longer exported but still avaiable in the package namespace. Please note that this function is not well-optimized and should only be used for testing purposes.
* Calling `print` on a `cg_node` object no longer prints the value of the node. Use function `val` to evaluate a node.

New Features:

* The initialization method of a cgraph object has a new argument `library` which can be used to specify the function library that is used by the graph.
* The calls of functions and their corresponding gradient functions are now build at run-time.
* Added function `val` and `set` to retrieve or set the value of a node respectively.
* A node can now also be named 'grad'. 'grad' is no longer a reserved word.
* Operator `cg_mean` now calls the base `mean` function.
* Operator `cg_crossprod` and `cg_tcrossprod` now allow argument `y` to be missing (similarly as the corresponding base functions).
* Argument `dim` of operator `cg_reshape` now expects an `cg_node` object instead of an integer scalar or vector that holds the new array dimensions.

Bug Fixes:

* Fixed a bug that caused a segfault when using cgraph in RStudio on Red Hat Enterprise Linux.

cgraph 2.0.3
----------------------------------------------------------------

Comments:

* The S3 overloaded methods of prefix operators have been marked as deprecated and will be removed in the next major release. Only infix operators like `+` or `-` will have S3 overloaded methods.

Bug Fixes:

* Operator `sigmoid` now correctly handles numerical underflow.
* Operator `cg.pmax` and `cg.pmin` are now correctly differentiated when argument `x` and `y` are arrays.

New Features:

* Added logical operators `!`, `==`, `!=`, `<`, `>`, `<=`, and `>=`.

cgraph 2.0.2
----------------------------------------------------------------

Bug Fixes:

* Nodes that are given a value upon creation are now properly printed to the R console.
* Function `run`, `gradients`, and `approx.grad` no longer cause an infinite loop when member `values` of a `cgraph` object is supplied to the `values` argument of the functions.
* Fixed protection bug in function `gradients` which caused the R session to crash unexpectedly.
* Function `approx.grad` now checks whether node `x` and `y` evaluate to a numeric vector or array.
* Function `approx.grad` no longer changes the parent environment of argument `values` when the argument is an environment.

cgraph 2.0.1
----------------------------------------------------------------

Bug Fixes:

* Function `run` and `gradients` no longer change the parent environment of argument `values` when the argument is an environment.
* Function `name` no longer generates a name that indicates the node type for performance reasons. Instead, it simply generates the name 'node' followed by the number of nodes that have been added to the graph.
* Fixed several protection bugs in the C-API thanks to `rchk`.

cgraph 2.0.0
----------------------------------------------------------------

Comments:

* The license of the package has changed from GPL-3 to Apache License 2.0.

New Features:

* Added hyperbolic trigonometry operations `sinh` and `cosh`.
* Added inverse hyperbolic trigonometry operations `asinh` and `acosh`.
* Data member `nodes` of a `cgraph` object is now named to allow more convenient access to the symbols of the nodes in a graph.
* You can now instruct the active graph to no longer automatically evaluate a node when it is printed to the console by setting `options(cg.autorun = FALSE)`.

Bug Fixes:

* An error is now raised when an invalid object is provided to the methods of a `cgraph` object instead of coercing the object to a valid object.
* Function `opr` now ignores argument `grads` when its elements are not properly named.
* Function `run` now checks whether each node evaluates to a numeric vector or array.
* Function `gradients` now checks whether the node supplied to argument `name` evaluates to a numeric vector or array.
* Function `get.parms` and `add.parms` now correctly call the underlying C-API.

cgraph 1.0.1
----------------------------------------------------------------

Comments:

* CRAN Review on the cgraph 1.0.0 submission.

cgraph 1.0.0
----------------------------------------------------------------

Comments:

* CRAN Submission.
