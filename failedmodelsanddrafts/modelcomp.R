mcmtmod <- readRDS("female_beginMCMT.rds")
matmod <- readRDS("female_beginMAT.rds")
bffpmod <- readRDS("female_beginbFFP.rds")
latmod <- readRDS("female_beginLatitude.rds")
base <- readRDS("female_begin.rds")
latprovmod <- readRDS("female_beginLatitudeProv.rds")
emtmod <- readRDS("female_beginEMT.rds")
bigmod <-readRDS("female_beginbig.rds")
genmod <- readRDS("female_beginGen.rds")
genmodnoprov <- readRDS("female_beginGennoprov.rds")
monodepprov <- readRDS("female_beginMonodepprov.rds")
monobig <- readRDS("female_beginbigGen.rds")
# monobig_noprov <- readRDS("female_beginbigGennp.rds")
# monoparorchprov <- readRDS("female_beginmonoparorchprov.rds")
# monoparorchprovmin <- readRDS("female_beginmonoparorchprovmin.rds")
# parorchprov <- readRDS("female_beginparorchprov.rds")
# notree <- readRDS("female_beginnotree.rds")
# noprov <- readRDS("female_beginnoprov.rds")
noprovnoorch <- readRDS("female_beginnoprovnoorch.rds")
#noprovnogen <- readRDS("female_beginnoprovnogen.rds")
gv <- readRDS("female_begingv.rds")

summary(bffpmod)
summary(matmod)
summary(mcmtmod)
summary(latmod)
summary(base)
summary(latprovmod)
summary(emtmod)
summary(bigmod)
summary(genmod)
summary(genmodnoprov)
summary(monodepprov)
summary(monobig)
summary(monobig_noprov)
#summary(monoparorchprov)
#summary(monoparorchprovmin)
#summary(parorchprov)
#summary(notree)
#summary(noprov)
#summary(noprovnogen)
summary(noprovnoorch)
summary(gv)

plot(conditional_effects(mcmtmod))
plot(conditional_effects(bffpmod))
plot(conditional_effects(matmod))
plot(conditional_effects(latmod))
plot(conditional_effects(emtmod))
plot(conditional_effects(monodepprov))
plot(conditional_effects(noprovnoorch))

plot(conditional_effects(latprovmod), conditions = plotConditions)
plot(conditional_effects(genmodnoprov))
plot(conditional_effects(monobig))
plot(conditional_effects(monobig_noprov))
plot(conditional_effects(monoparorchprov))
plot(conditional_effects(gv))

loomat <- readRDS("model_dev/loo_mat.rds")
loobffp <- readRDS("model_dev/loo_bffp.rds")
loomcmt <- readRDS("model_dev/loo_mcmt.rds")
loobase <- readRDS("model_dev/loo_base.rds")
loolat <- readRDS("model_dev/loo_lat.rds")
loolatprov <- readRDS("model_dev/loo_latprov.rds") #fbfitlatprov - backgroundLatitudeProv.R
looemt <- readRDS("model_dev/loo_emt.rds")
loobig <- readRDS("model_dev/loo_big.rds") #fbfitbig - backgroundbig.R
loogennoprov <- readRDS("model_dev/loo_latgennoprov.rds") #fbfitgennoprov - monotonic.R
loogen <- readRDS("model_dev/loo_latgen.rds") #fbfitgen - monotonicplainprov.R
#loomonodep <- readRDS("model_dev/loo_latMonodepprov.rds") #fbfitMonodepprov ambiguous
#loobiggen <- readRDS("model_dev/loo_latbiggen.rds") #fbfitbiggen
loobiggennp <- readRDS("model_dev/loo_latbiggennp.rds") #fbfitbiggennp - monotonicbiggen_noprov.R
loonoprovnoorch <- readRDS("model_dev/loo_noprovnoorch.rds") #fbfitnoprovnoorch - fbfitnoprovnoorch
brms::loo_compare(loomat, loobffp, loomcmt, loobase, loolat, loolatprov, looemt, loobig, loogennoprov, loogen, loobiggennp, loonoprovnoorch)



