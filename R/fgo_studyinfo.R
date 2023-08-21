#' @include checkers.R utils.R
#'
#' Study-Information Class
#'
#' @description subclass for representing the study information data. It inherits from FieldGo client class
#' all the basic parameters and methods.
#'
#' @title StudyInfo
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
FgStudyInfo <- R6::R6Class(

    classname = "FgStudyInfo",
    inherit = FieldGoClient,
    # private = list(
    #   super$endPoint = "/study-info/getAll?id="
    # ),

    public = list(

        #siteId = NA, #siteId
        initialize = function(user=NULL, password=NULL, authentication=NULL, token=NULL, user_agent=NULL,
                              serverURL, version ){
            super$initialize(user, password, authentication, token, user_agent, serverURL, version)#, endPoint)
        },

        fg_get_studyinfo_studyId = function(studyDbId, format, ...){}, #,#in BRAPI is location
        fg_get_addstudyinfo_studyId = function(studyDbId, format, ...){}#,#in BRAPI is location
    )
)


#' @title  Get study information by studyID (studyDbId)
#' @description retrieve data from databases with FieldGo API standard
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
FgStudyInfo$set(which = "public", name = "fg_get_studyinfo_studyId",
                function( studyDbId =NULL,
                          format=c("json","list","data.frame"),
                          ... ){

                    super$endPoint <- "study/"
                    queryParams <- studyDbId

                    url  <- paste0(self$serverURL, self$version, super$endPoint) #everything before the URL

                    #GET parameters for retrieving data ----------------------------
                    url  <- paste0(url, queryParams)
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


###

#' @title  Get additional study information by studyID (studyDbId)
#' @description retrieve additional data from studies with the FieldGo API standard
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
FgStudyInfo$set(which = "public", name = "fg_get_addstudyinfo_studyId",
                function( studyDbId =NULL,
                          format=c("json","list","data.frame"),
                          ... ){

                    #end Point
                    super$endPoint <- "study/"

                    #sub endPoint for additional study information
                    sub_endPoint <- "/description-item"

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

