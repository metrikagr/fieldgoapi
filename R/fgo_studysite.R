#' @include checkers.R utils.R
#'
#' Study-Site Class
#'
#' @description subclass for representing the study-site data. It inherits from FieldGo client class
#' all the basic parameters and methods.
#'
#' @title StudySite
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
FgStudySite <- R6::R6Class(

    classname = "FgSiteInfo",
    inherit = FieldGoClient,
    # private = list(
    #   super$endPoint = "/stu-site/getAll?id="
    # ),

    public = list(

        #siteId = NA, #siteId
        initialize = function(user=NULL, password=NULL, authentication=NULL, token=NULL, user_agent=NULL,
                              serverURL, version ){
            super$initialize(user, password, authentication, token, user_agent, serverURL, version)#, endPoint)
        },

        fg_get_siteinfo_siteId = function(siteDbId, format, ...){}#,#in BRAPI is location

    )
)


#' @title  Get experiment site information by siteID (siteDbId)
#' @description retrieve data from databases with FieldGo API standard
#' @field ... argument inherents by AgaAPIClient
#' @field siteDbId character site ID
#' @field format support in three data structures: json, list and data.frames
#' @importFrom R6 R6Class
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @importFrom tibble rownames_to_column
#' @author Omar Benites
#' @export
#'
FgStudySite$set(which = "public", name = "fg_get_siteinfo_siteId",
              function( siteDbId =NULL,
                        format=c("json","list","data.frame"),
                        ... ){

                  super$endPoint <- "/site/get?id="
                  url  <- paste0(self$serverURL, self$version, super$endPoint) #everything before the URL
                  print(url)

                  #GET parameters for retrieving data ----------------------------
                  #headerParams <- character()
                  queryParams <- list(id = siteDbId)

                  #Iterate over exp_site_id to extract experiment descript from each site in the study

                  res <- self$call_api(
                      url = url,
                      method = "GET",
                      queryParams = queryParams, #, #TODO
                      #headerParams = headerParams, #TODO
                      #body = body,
                      ...
                  )
                  cont <- httr::content(res, as = "text", encoding = "UTF-8")
                  #Object structure
                  if(format=="json"){
                      out <-  cont
                  } else if(format=="list"){
                      out <- jsonlite::fromJSON(cont,simplifyVector = "vector")
                  } else if(format=="data.frame") {
                      #TODO : convert correctly to data.frame
                      out <- as.data.frame(cont)
                  }

                  return(out)

              },
              overwrite = TRUE)
