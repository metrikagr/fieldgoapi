#' Wrapper for site information get method
#'
#' @description wrapper function for \code{objetc$fg_get_siteinfo_siteId} from \code{FgSiteInfo} objects.
#' @param ... inherents arguments from FieldGoClient class and FgSiteInfo
#' @param siteDbId character agronomic study identifier from MetrikaTools database
#' @param format data format: json, list and data.frame
#' @param serverURL URL of the metrikatools server
#' @param version version of the call. By default version \code{0212}.
#' @param ... additional parameters
#' @author Omar Benites
#' @examples \dontrun{
#' #Get experimental details by studyId (agronomic study ID)
#' out <- fg_get_siteinfo_siteId(siteDbId = 21,
#'                                  format = "data.frame",
#'                                  serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
#'                                  version ="/0212/r")
#' }
#' @export
#'

fg_get_siteinfo_siteId = function(format=c("json","list","data.frame"),
                                  serverURL = "https://research.cip.cgiar.org/agrofims/api/dev",
                                  version ="/0212/r",
                                  ...){

        format <- match.arg(format)
        sc <- FgSiteInfo$new(serverURL = serverURL, version = version)
        out <- sc$fg_get_siteinfo_siteId(siteDbId = studyDbId,
                                            format = format,
                                            ...
        )

        return(out)

    }

