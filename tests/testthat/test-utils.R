library(assertthat)
library(tidyverse)



test_that("Test list_to_dataframe for studyinfo with NA values", {

    # Set seed for reproducibility
    set.seed(123)

    # Create the first list named "status" with two values
    status <- list(first_status = "active", second_status = "inactive")

    # Create the second list with 10 values
    values <- list(value1 = "a", value2 = "b", value3 = "c", value4 = "d", value5 = "e",
                   value6 = "f", value7 = "g", value8 = "h", value9 = "i", value10 = "j")

    # Randomly select 3 positions in the second list to replace with NA
    na_positions <- sample(1:10, 3)

    for(pos in na_positions) {
        values[[pos]] <- NA
    }

    # Combine the two named lists into a single named list
    combined_list <- list(Status = status, Data = values)
    out <- fieldgoapi::list_to_dataframe(combined_list$Data)
    out_noNA <- out$Values %>% assertthat::noNA()

    testthat::expect_equal(10, nrow(out))
    testthat::expect_false(out_noNA)

})


test_that("Test list_to_dataframe for studyinfo with NA", {

    # Set seed for reproducibility
    set.seed(132)

    # Create the first list named "status" with two values
    status <- list(first_status = "active", second_status = "inactive")

    # Create the second list with 10 values
    values <- list(value1 = "a", value2 = "b", value3 = "c", value4 = "d", value5 = "e",
                   value6 = "f", value7 = "g", value8 = "h", value9 = "i", value10 = "j")

    # Randomly select 3 positions in the second list to replace with NA
    na_positions <- sample(1:10, 3)

    for(pos in na_positions) {
        values[[pos]] <- NA
    }


    # Combine the two named lists into a single named list
    combined_list <- list(Status = status, Data = values)
    out <- fieldgoapi::list_to_dataframe(combined_list$Data)
    #out_noNA <- out$Values %>% assertthat::noNA()
    testthat::expect_equal(10, nrow(out))
    testthat::expect_true(!assertthat::noNA(out$Values))
    testthat::expect_equal(class(out), "data.frame")

})


test_that("Test fill_missing_values for studyinfo with complete values", {

    # Set seed for reproducibility
    set.seed(123)

    # Create the first list named "status" with two values
    status <- list(first_status = "active", second_status = "inactive")

    # Create the second list with 10 values
    values <- list(value1 = "a", value2 = "b", value3 = "c", value4 = "d", value5 = "e",
                   value6 = "f", value7 = "g", value8 = "h", value9 = "i", value10 = "j")

    # Combine the two named lists into a single named list
    combined_list <- list(Status = status, Data = values)
    out <- fieldgoapi::fill_missing_values(combined_list$Data)

    testthat::expect_type(object = out, "list")
    testthat::expect_equal(0, sum(out==""))
    testthat::expect_true(noNA(out))
})


