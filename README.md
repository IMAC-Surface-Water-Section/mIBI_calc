This is an RShiny app for calculating mIBI scores using IEPA data. The original calculator was created by Hillary Marler. The current version and shiny app, which fixed double counting issues and incorporated additional taxonomic ranks, was created by Yaal Dryer.

The filter_hierarchy function is used for identifying redundant levels of taxonomic resolution and either removing or reallocating the relevant abundance information. using methods derived from Meredith et al., 2019. The default method is RPKC-S, but DPAC-S (my preferred method), or APTC-S can be used as well.
