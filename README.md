# SmartRoot pipeline
* Author: Beatriz Moreno
* Institute: INRA

This pipeline has been designed to visualize maize lateral root length data exported with the [SmartRoot ImageJ plugin](https://smartroot.github.io/).

## 1.Rstructuration of single root system SmartRoot files
Script location: `SmartRoot_pipeline/src/SmartRoot_pipeline/1.Rstructuration/SRfiles_to_Rstructured_onePlant.R`

In a first step, basic features are computed for individual roots of single root system image series, such as their root ages or daily root elongation rates. 

Input files are SmartRoot CSV datasets **Global Root Data**, providing information about individual roots, and **Root Nodes**, providing information about individual nodes; both located under the `data/SR_files` directory. The information of individual root nodes is used to compute a basal root diameter as the median diameter of the 3 most basal nodes, and an apical root diameter for which the 3 most distant nodes are used. The date and time associated to each image acquisition must be provided in a separate CSV file, located under the `data/Auxiliary_files` directory. 

A main output file containing complete information about individual root growth and root diameters is generated at the end of the Rstructuration script and saved under the `data/Rstructured_files` directory. In parallel, a short summary CSV file providing key root system properties for each image acquisition is generated and saved into `data/Summary_files`.

## 2.RPlots 
Script location: `SmartRoot_pipeline/src/SmartRoot_pipeline/2.Rplots/Rplots.R`

The above mentioned script allows to generate interesting graphics about root growth and/or morphology in a customisable way, and export them into PDF files. Here is the list of currently available graphical representations:
* Individual *root length* as function of the *date of acquisition*, grouped by distance from the seed into 5 cm primary root sections
* Individual *root relative elongation rate (RER)* as function of *root age*,  grouped by distance from the seed into 5 cm primary root sections
* Individual *root relative elongation rate (RER)* at logaritmic scale as function of *root age*,  grouped by distance from the seed into 5 cm primary root sections 
* Individual *root elongation rate (ER)* as function of *root age*,  grouped by distance from the seed into 5 cm primary root sections
* Individual *root length* as function of *seed distance* at the last date of acquisition
* An histogram of *root diameter* to see differences in the distribution of apical relative to basal root diameters 
* Individual *apical root diameter* and *ER* as function of *root length*, in groups of N roots (N to be specified by the user)
* Individual *apical root diameter* and *ER* as function of *root age*, in groups of N roots (N to be specified by the user)





