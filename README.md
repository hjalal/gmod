gmod 

Grammar of Modelling is a new approach to streamline the development of Markov models and decision trees for Medical Decision Making and Health Economic Evaluations. 

The approach is build on the concept of using a generic cycle tree that gets applied to all the states.  

The gmod R package is based on the framework for Grammar of Modeling. 

To install gmod, use the following command in R: 

install_github("hjalal/gmod", build_vignettes = TRUE, force = TRUE)

This will allow the Vignettes to be built.

For a visual interface for generating gmod syntax interactively and visualize the syntax, you can use Decision Twigs available at https://dashlab.ca/projects/decision_twig/ 

To get started with the package, review the vignettes using the following commond in R:

browseVignettes(package="gmod")

There are several examples in the Vignettes for building decision trees and markov models. The example graudally become more complex.  

Both Decision Twigs and gmod are provided as-is and come with absolutely no warranty.
