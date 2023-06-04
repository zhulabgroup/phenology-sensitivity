library(tidyverse)
library(rnpn)


df_phenophase<- rnpn::npn_phenophases()
leaf_id <- df_phenophase %>% filter(pheno_class_id==1) %>% pull(phenophase_id)
flower_id <- df_phenophase %>% filter(pheno_class_id==7) %>% pull(phenophase_id)

leaf_metric <- rnpn::npn_download_individual_phenometrics(request_source ="YS",
                                                          years = 2022,
                                                          phenophase_ids = leaf_id,
                                                          individual_ids = "232884",
                                                          additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty"))
leaf_metric

leaf_status <- rnpn::npn_download_status_data(request_source ="YS",
                                              years = 2022,
                                              phenophase_ids = leaf_id,
                                              species_ids = 62,
                                              station_ids = 37695,
                                              # individual_ids = "232884",
                                              additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty"))
leaf_status %>%  filter( individual_id == "232884") %>% select (day_of_year, phenophase_status, phenophase_description, intensity_value)%>% arrange(day_of_year) %>% View()


flower_metric <- rnpn::npn_download_individual_phenometrics(request_source ="YS",
                                                            years = 2022,
                                                            phenophase_ids = flower_id,
                                                            individual_ids = "232884",
                                                            additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty"))
flower_metric


flower_status <- rnpn::npn_download_status_data(request_source ="YS",
                                                years = 2022,
                                                phenophase_ids = flower_id,
                                                species_ids = 62,
                                                station_ids = 37695,
                                                # individual_ids = "232884",
                                                additional_fields = c("ObservedBy_Person_ID","Observed_Status_Conflict_Flag","multiple_firsty"))
flower_status %>%  filter( individual_id == "232884") %>% select (day_of_year, phenophase_status, phenophase_description, intensity_value)%>% arrange(day_of_year) %>% View()

# https://rdrr.io/cran/rnpn/f/inst/doc/VIII_data_cleaning.Rmd
