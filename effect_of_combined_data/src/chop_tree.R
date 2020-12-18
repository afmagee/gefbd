library(ape)

calsoy <- read.tree("effect_of_combined_data/data/calsoy_as_gonio.equal.tre")

extant <- drop.fossil(calsoy,tol=1e-5)
write.tree(extant,"effect_of_combined_data/data/calsoy_as_gonio.equal.extant.tre")

extinct <- drop.tip(calsoy,extant$tip.label)
write.tree(extinct,"effect_of_combined_data/data/calsoy_as_gonio.equal.extinct.tre")

