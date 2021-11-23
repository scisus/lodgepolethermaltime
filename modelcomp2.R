offset <- readRDS("female_beginoffset.rds")
offset_geo <- readRDS("female_beginoffset_geo.rds")
offset_source <- readRDS("female_beginoffset_source.rds")

summary(offset)
summary(offset_geo)
summary(offset_source)

loo_offset <- readRDS("model_dev/loo_offset.rds")
loo_offset_geo <- readRDS("model_dev/loo_offset_geo.rds")
loo_offset_source <- readRDS("model_dev/loo_offset_source.rds")

loo_compare(loo_offset, loo_offset_geo, loo_offset_source)
