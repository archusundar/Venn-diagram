GeneSetViz: Interactive Visualization of Differentially Expressed Genes with Venn Diagrams and UpSet Plots

### Shiny package to get the Venn Diagram from a list of files (Differentially expressed genes between different conditions/samples)

This shiny app uses VennDiagram to create Venn diagram for upto file gene list.
For gene list more than 5, R-package named upSetR is used to create Upset plot instead of Venn Diagram used to deal with more than 5 sets.
This shiny app requires libraries such as shiny, VennDiagram, dplyr, gridExtra, dplyr and UpSetR
