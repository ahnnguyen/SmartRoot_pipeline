# SmartRoot pipeline
* Author: Beatriz Moreno
* Institute: INRA
This pipeline has been designed to visualize maize lateral root length data exported with the [SmartRoot ImageJ plugin](https://smartroot.github.io/).

## 1.Rstructuration of single root system SmartRoot files
In a first step, basic features are computed for individual roots of single root system image series, such as their root ages or daily root elongation rates. 

Input files are SmartRoot datasets **Global Root Data**, providing information about individual roots, and **Root Nodes**, providing information about individual nodes; both located under the `data/SR_files` directory. The information of individual root nodes is used to compute a basal root diameter as the median diameter of the 3 most basal nodes, and an apical root diameter for which the 3 most distant nodes are used. The date and time associated to each image acquisition must be provided in a separate .csv file, located under the `data/Auxiliary_files` directory. 

A first output file containing complete information about individual root growth and root diameters is generated at the end of the Rstructuration script and saved under the `data/Rstructured_files` directory. In parallel, a short summary file is generated and saved `data/Summary_files` providing key root system properties for each acquisition image.

## 2.RPlots from single root system Rstructured files
O,ce










