#' @include checkers.R utils.R
#'
#' Study-FieldBook Class
#'
#' @description subclass for representing the field book information of studies. Each study may have 1 or more field books inside. It inherits from FieldGo client class
#' all the basic parameters and methods.
#'
#' @title FgStudyFbook
#'
#' @docType class
#'
#' @format R6 class
#'
#' @field user specifies the user name, defaults to NULL
#' @field password specifies the password, defaults to NULL
#' @field user specifies the user name, defaults to NULL
#' @field password specifies the password, defaults to NULL
#' @field authentication specifies the password, defaults to NULL
#' @field token specifies the password, defaults to NULL
#' @field user_agent specifies the password, defaults to NULL
#' @field serverURL specifies the password, defaults to
#'                  http://research.cip.cgiar.org/agrofims/api/dev/
#' @field version specifies the version, defaults to 0051
#' @field endPoint specifies the end point value,   tableName/method.php
#' @importFrom R6 R6Class
#'
#' @export
#'
FgStudyFbook <- R6::R6Class(

    classname = "FgStudyFbook",
    inherit = FieldGoClient,

    public = list(

        #siteId = NA, #siteId
        initialize = function(user=NULL, password=NULL, authentication=NULL, token=NULL, user_agent=NULL,
                              serverURL, version ){
            super$initialize(user, password, authentication, token, user_agent, serverURL, version)#, endPoint)
        },

        fg_get_metafbook_studyId = function(studyDbId, format, ...){}#,#in BRAPI is location

    )
)


#' @title Get experiment field book metadata from an study
#' @description retrieve metadata of field books that belongs to a specific study, through study database Id (\code{studyDbId})
#' @field ... argument inherents by FieldGoClient
#' @field studyDbId character study ID
#' @field format support in three data structures: json, list and data.frames
#' @importFrom R6 R6Class
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble rownames_to_column
#' @author Omar Benites
#' @export
#'
FgStudyFbook$set(which = "public", name = "fg_get_metafbook_studyId",
                function( studyDbId =NULL,
                          format=c("json","list","data.frame"),
                          ... ){

                    #end Point
                    super$endPoint <- "study/"

                    #sub endPoint for fieldbook metadata
                    sub_endPoint <- "/fieldbook"

                    #Study database Id
                    queryParams <- studyDbId

                    #create basic URL
                    url  <- paste0(self$serverURL, self$version, super$endPoint)

                    #Add aditional sub_endPoint to basic URL
                    url  <- paste0(url, queryParams, sub_endPoint)
                    print(url)

                    #Iterate over exp_site_id to extract experiment descript from each site in the study
                    #Call the api and get a response
                    res <- self$call_api(
                        url = url,
                        method = "GET",
                        #queryParams = queryParams, #, #TODO
                        #headerParams = headerParams, #TODO
                        #body = body,
                        ...
                    )

                    #Pass the content as text and then encoding as UTF-8
                    cont <- httr::content(res, as = "text", encoding = "UTF-8")

                    #Extract the data itself (without success attribute)
                    #cont <- cont$data

                    #Object structure
                    if(format=="json"){
                        out <-  cont
                    } else if(format=="list"){
                        out <- jsonlite::fromJSON(cont,simplifyVector = "vector")
                    } else if(format=="data.frame") {
                        #TODO : convert correctly to data.frame
                        out <- as_data_frame_fgo(cont)
                        #get only "data" list
                    }

                    return(out)

                },
                overwrite = TRUE)

