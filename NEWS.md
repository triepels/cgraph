cgraph 1.0.2
----------------------------------------------------------------

Comments:

* The license of the package has changed from GPL-3 to Apache License 2.0.

Features:

* Added hyperbolic trigonometry operations `sinh` and `cosh`.
* Added inverse hyperbolic trigonometry operations `asinh` and `acosh`.
* You can now instruct the active graph to no longer automatically evaluate a node when it is printed to the console by setting `options(cg.autorun = FALSE)`.

Bug Fixes:

* Improved several error messages.
* Method `opr` now ignores the gradients provided to argument `grads` when its elements are not named.
* Function `address` now correctly shows the address of an pointer on Windows machines.
* Function `get.parms` and `add.parms` now correctly call the underlying C-code.

cgraph 1.0.1
----------------------------------------------------------------

Comments:

* CRAN Review on the cgraph 1.0.0 submission.

Documentation:

* Improved the package title and description.
* Added RGraphviz to suggests.
* Added examples to the documentation of the most important methods of a `cgraph` object and their corresponding wrapper functions.

Bug Fixes:

* Function `address` now correctly calls the underlying C-code. 

cgraph 1.0.0
----------------------------------------------------------------

Comments:

* CRAN Submission.
