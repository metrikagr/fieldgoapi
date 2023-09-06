library(tidyverse)
library(jsonlite)
library(fieldgoapi)
library(httr)


test_that("multiplication works", {



    token <- yaml::read_yaml(file = "config/config.yml")$token
    #get server URL
    serverURL <- yaml::read_yaml(file = "config/config.yml")$serverURL


    obj_studyInfo <- FgStudyInfo$new(serverURL = serverURL,
                                     token=token,
                                     version=NULL)

    dfr_studyinfo <- obj_studyInfo$fg_get_studyinfo_studyId(studyDbId = 1,
                                                            format = "list")$data %>%
                                    fieldgoapi::list_to_dataframe()

    testthat::is_equivalent_to(2, nrow(dfr_studyinfo))

})
