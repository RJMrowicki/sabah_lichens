# sabah_lichens
# Analyse data

# run MDS analysis (on log10(x+1)-transformed data):
mds(log10(dd_lichens_taxa[, c(lichen_taxa, "dummy")] + 1))
