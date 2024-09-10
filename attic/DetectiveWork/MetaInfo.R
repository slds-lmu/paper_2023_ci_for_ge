source(here("shiny_app_old/setup.R"))

data_names = list(
  "45570" = "higgs",
  "45689" = "adult",
  "45704" = "covertype",
  "45654" = "bates_classif_20",
  "45665" = "colon",
  "45668" = "bates_classif_100",
  "45669" = "breast",
  "45672" = "prostate",
  "45693" = "electricity",
  "45692" = "diamonds",
  "45694" = "physiochemical_protein",
  "45655" = "bates_regr_20",
  "45666" = "friedman1",
  "45667" = "bates_regr_100",
  "45670" = "chen_10_null",
  "45671" = "chen_10",
  "45695" = "sgemm_gpu",
  "45696" = "video_transcoding"
)

load(here("64plots/Data/Suspicious_DGPS.RData"))

Suspicious_DGPS_ID <- Suspicious_DGPS %>%
  group_by(size,inducer,target,loss) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()
CoverageSus <- Suspicious_DGPS_ID[which(Suspicious_DGPS_ID$reason=="coverage< cutoff (value) for solid methods" ),c(1,3,5)]%>% distinct()

never_sus <- setdiff(DGPS,c(setdiff(levels(as.factor(Suspicious_DGPS$name)),"none"),"chen_10_null"))

sometimes_sus <- unique(unlist(CoverageSus$name))






