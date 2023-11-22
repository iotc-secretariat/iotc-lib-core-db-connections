SERVER_CACHE      = new.env(hash = TRUE)
CREDENTIALS_CACHE = new.env(hash = TRUE)

#'The constants holding the name of the IOTC_MASTER database
#'@export
IOTC_MASTER    = "IOTC_MASTER"

#'The constants holding the name of the IOTDB database
#'@export
IOTDB          = "IOTDB"

#'The constants holding the name of the IOTCStatistics database
#'@export
IOTCSTATISTICS = "IOTCStatistics"

#'The constants holding the name of the IOTCVessels (RAV) database
#'@export
IOTCVESSELS    = "IOTCVessels"

#'The constants holding the name of the WP_CE_raised database
#'@export
WP_CE_RAISED   = "WP_CE_raised"

#'The constants holding the name of the ROS database
#'@export
ROS            = "ROS_2.3.0"

#'The constants holding the name of the ROS_analysis database
#'@export
ROS_ANALYSIS   = "ROS_2.2.1_analysis"

#'The constants holding the name of the 3_BU database
#'@export
BUOY_DATA      = "3BU_DATA"

#'The constants holding the name of the main IOTC DB Server
#'@export
SERVER_DEFAULT = "IOTCS09"

#' Sets the 'debug' mode for all DB connections
#' @export
db_debug_connections = function(mode = NA) {
  if(is.na(mode)) {
    debug = as.logical(Sys.getenv("IOTC_LIBS_CONNECTIONS_DEBUG"))

    return(!is.na(debug) & debug)
  } else {
    Sys.setenv(IOTC_LIBS_CONNECTIONS_DEBUG = mode)
  }
}

#'Sets the default username / password for a given database.
#'@param database the database identifier
#'@param username the username
#'@param password the password
#'@export
set_credentials_for_db = function(database, username, password) {
  cache_set(CREDENTIALS_CACHE, database, list(USERNAME = username, PASSWORD = password), hash_key = FALSE)
}

#'Retrieves the default username / password for a given database.
#'@param database the database identifier
#'@return the stored credentials for the database (if any)
#'@export
get_credentials_for_db = function(database) {
  if(!are_credentials_for_db_available(database)) return(NA)

  return(cache_get(CREDENTIALS_CACHE, database, hash_key = FALSE))
}

#'Retrieves the default username for a given database.
#'@param database the database identifier
#'@return the stored username for the database (if any) or \code{NA}
#'@export
get_username_for_db = function(database) {
  if(are_credentials_for_db_available(database)) return(get_credentials_for_db(database)$USERNAME)

  return(NA)
}

#'Retrieves the default password for a given database.
#'@param database the database identifier
#'@return the stored password for the database (if any) or \code{NA}
#'@export
get_password_for_db = function(database) {
  if(are_credentials_for_db_available(database)) return(get_credentials_for_db(database)$PASSWORD)

  return(NA)
}

#'Checks that stored  username / password exist for a given database.
#'@param database the database identifier
#'@return \code{TRUE} if stored credentials exist for the given database
#'@export
are_credentials_for_db_available = function(database) {
  return(is_available(cache_get(CREDENTIALS_CACHE, database, hash_key = FALSE)))
}

#' Sets the default server name / IP address for all ODBC request to standard IOTC databases
#'
#' @param server A server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @export
set_default_db_server = function(server = SERVER_DEFAULT) {
  cache_set(SERVER_CACHE, "SERVER", server, hash_key = FALSE)
}

#' Gets the default server name / IP address for all ODBC request to standard IOTC databases
#'
#' @export
get_default_db_server = function() {
  if(!cache_contains(SERVER_CACHE, "SERVER")) {
    global_default = Sys.getenv("DEFAULT_IOTC_DB_SERVER")

    if(is.na(global_default) |
       is.null(global_default) |
       global_default == "") global_default = SERVER_DEFAULT

    return(global_default)
  }

  return (cache_get(SERVER_CACHE, "SERVER", hash_key = FALSE))
}

#' Connects to a SQL Server database on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The database name
#' @param username The user name
#' @param password The password
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{database} on \code{server}
#' @examples
#' DB_CONNECT_TO("localhost", "IOTDB")
#' DB_CONNECT_TO("IOTDB")
#' @export
DB_CONNECT_TO = function(server = get_default_db_server(), database, username = NA, password = NA, database_encoding = "CP1252", client_charset = "UTF-8") {
  DEBUG = db_debug_connections()

  is_trusted = ( is.na(username) | username == "") & ( is.na(password) | password == "" )

  if(is_trusted) {
    if(DEBUG) l_info(paste("Connecting to", database, "on", server, "using current user credentials"))

    return (dbConnect(odbc(),
                      Driver = "SQL Server",
                      Server = server,
                      database = database,
                      Trusted_Connection = "true",
                      encoding = database_encoding,
                      clientcharset = client_charset))
  } else {
    if(DEBUG) l_info(paste("Connecting to", database, "on", server, "using", username, "as username"))

    return (dbConnect(odbc(),
                      Driver = "SQL Server",
                      Server = server,
                      database = database,
                      UID = username,
                      PWD = password,
                      encoding = database_encoding,
                      clientcharset = client_charset))
  }
}

#' Connects to an instance of \code{IOTC_MASTER} on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "IOTC master" database name (defaults to \code{\link{IOTC_MASTER}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{IOTC_MASTER} on \code{server}
#' @export
DB_IOTC_MASTER = function(server = get_default_db_server(), database = IOTC_MASTER, username = get_username_for_db(IOTDB), password = get_password_for_db(IOTDB), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(connect_to(server, IOTC_MASTER, username, password, database_encoding, client_charset))
}

#' Connects to an instance of \code{IOTDB} on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "IOTC DB" database name (defaults to \code{\link{IOTDB}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{IOTDB} on \code{server}
#' @export
DB_IOTDB = function(server = get_default_db_server(), database = IOTDB, username = get_username_for_db(IOTDB), password = get_password_for_db(IOTDB), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(connect_to(server, IOTDB, username, password, database_encoding, client_charset))
}

#' Connects to an instance of \code{\link{IOTCSTATISTICS}} on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "IOTC statistics" database name (defaults to \code{\link{IOTCSTATISTICS}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{\link{IOTCSTATISTICS}} on \code{server}
#' @export
DB_IOTCSTATISTICS = function(server = get_default_db_server(), database = IOTCSTATISTICS, username = get_username_for_db(IOTCSTATISTICS), password = get_password_for_db(IOTCSTATISTICS), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(connect_to(server, IOTCSTATISTICS, username, password))
}

#' Connects to an instance of \code{\link{WP_CE_RAISED}} on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "WP raised catches" database name (defaults to \code{\link{WP_CE_RAISED}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{\link{WP_CE_RAISED}} on \code{server}
#' @export
DB_WP_CE_RAISED = function(server = get_default_db_server(), database = WP_CE_RAISED, username = get_username_for_db(WP_CE_RAISED), password = get_password_for_db(WP_CE_RAISED), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(connect_to(server, WP_CE_RAISED, username, password))
}

#' Connects to an instance of \code{\link{IOTCVESSELS}} (containing the RAV and the AVL) on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "RAV" database name (defaults to \code{\link{RAV}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{\link{IOTCVESSELS}} on \code{server}
#' @export
DB_RAV = function(server = get_default_db_server(), database = "RAV", username = get_username_for_db(IOTCVESSELS), password = get_password_for_db(IOTCVESSELS), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(connect_to(server, IOTCVESSELS, username, password))
}

#' Connects to an instance of \code{\code{ROS}} on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "ROS" database name (defaults to \code{\link{ROS}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{\link{ROS}} on \code{server}
#' @export
DB_ROS = function(server = get_default_db_server(), database = ROS, username = get_username_for_db(ROS), password = get_password_for_db(ROS), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(??connect_to(server, ROS, username, password))
}

#' Connects to an instance of \code{\link{ROS_ANALYSIS}} on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "ROS analysis" database name (defaults to \code{\link{ROS_ANALYSIS}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{\link{ROS_ANALYSIS}} on \code{server}
#' @export
DB_ROS_ANALYSIS = function(server = get_default_db_server(), database = ROS_ANALYSIS, username = get_username_for_db(ROS_ANALYSIS), password = get_password_for_db(ROS_ANALYSIS), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(connect_to(server, ROS_ANALYSIS, username, password))
}

#' Connects to an instance of \code{\link{BUOY_DATA}} on a given server machine using a trusted connection
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "Buoy positions" database name (defaults to \code{\link{BUOY_DATA}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param database_encoding The encoding set for the specific database (defaults to CP1252)
#' @param client_charset The character set used by the client (defaults to UTF-8)
#' @return An ODBC connection to \code{\link{BUOY_DATA}} on \code{server}
#' @export
DB_BUOYS = function(server = get_default_db_server(), database = BUOY_DATA, username = get_username_for_db(BUOY_DATA), password = get_password_for_db(BUOY_DATA), database_encoding = "CP1252", client_charset = "UTF-8") {
  return(connect_to(server, BUOY_DATA, username, password))
}

connect_to = function(server = get_default_db_server(), database, username = NA, password = NA, database_encoding = "CP1252", client_charset = "UTF-8") {
  return (DB_CONNECT_TO(server, database, username, password, database_encoding = database_encoding, client_charset = client_charset))
}
