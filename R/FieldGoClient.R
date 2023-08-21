library(R6)

FieldGoClient <- R6Class(
    "FieldGoClient",

    # Private attribute
    private = list(
        endPoint = NULL,
        token = NULL
    ),

    # Public attributes
    public = list(
        user = NULL,
        password = NULL,
        authentication = NULL,
        #token = NULL,
        user_agent = NULL,
        serverURL = NULL,
        version = NULL,

        # Initialization method
        initialize = function(
        user, password, authentication,
        token, user_agent, serverURL, version
        ) {
            private$endPoint <- paste0(serverURL) #, "/", version)
            self$user <- user
            self$password <- password
            self$authentication <- authentication
            #self$token <- token
            private$token <- token
            self$user_agent <- user_agent
            self$serverURL <- serverURL
            self$version <- version
        },

        # Print method
        print = function() {
            cat("User:", self$user, "\n",
                "Authentication:", self$authentication, "\n",
                #"Token:", self$token, "\n",
                "User Agent:", self$user_agent, "\n",
                "Server URL:", self$serverURL, "\n",
                "Version:", self$version, "\n"
            )
        },

        # Call API method
        call_api = function(url, method, queryParams, headerParams, body, ...) {
            # This is a placeholder. You should replace this with actual API call code.
            cat("Calling API", api_name, "with parameters", parameters, "\n")
        }
    )
)

#' FieldGoClient method for http methods GET and POST
#'
#' @field url base URL composed by the composition of the server URL, api version and end point
#' @field method GET and POST methods
#' @field queryParams parameters to retrieve information
#' @field headerParams parameter values
#' @field body corpues of the request
#' @field ... inherents argument from the class
#' @importFrom curl has_internet
#' @author Omar Benites
#' @export

FieldGoClient$set(which = "public", "call_api", function(url, method, queryParams=NULL,
                                                       headerParams=NULL, body, ...) {


    if(curl::has_internet()){

        headers <- httr::add_headers(c("Authorization" = paste("Bearer", private$token)))
        ua <- NULL #httr::user_agent(self$userAgent) #TODO ENABLE WHEN have user and password
        if (method == "GET") {

            res <- GET(url, headers)
            #get the content of the response because all the requests have status 200
            cont <- httr::content(res, as = "text", encoding = "UTF-8")
            #get the content and extrac the "sucess" status which is "TRUE" or "FALSE"
            cont <- jsonlite::fromJSON(cont,simplifyVector = "vector")

            #If sucess is TRUE then
            txt <- ifelse(isTRUE(cont$success),
                          yes="The request was fulfilled successfully",
                          no=paste0("There was a possible issue in your request with an status ",res$message))
            print(txt)

            ###############
            # res <- httr::GET(url,
            #                  #query = queryParams, #when use ?ID=NUMBER
            #                  #user_agent = NULL, #ua, #TODO ENABLE WHEN have user and password
            #                  headers = headers    #headers, ...   #TODO ENABLE WHEN have user and password
            # )
            #txt <- ifelse(res$status == 200, "The request was fulfilled successfully with status 200",
            #              paste0("There was a possible issue in your request with an status ",res$status))
            ###############

            class(res) <- c("FieldGoClient","response")
            return(res)

        }
        else if (method == "POST") {
            httr::POST(url,
                       #query = queryParams,
                       #ua,
                       headers = headers, body = body, encode = "json", ...
            )
        }
        else {
            stop(paste("http method must be `GET` or `POST`."))
        }
    } else {
        stop(paste("No internet connection or quite unstable to connect to the server. Check your connection."))
    }


}, overwrite = TRUE)


