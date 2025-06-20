% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iotc_core_db_connections_main.R
\name{DB_WP_CE_RAISED}
\alias{DB_WP_CE_RAISED}
\title{Connects to an instance of \code{\link{WP_CE_RAISED}} on a given server machine using a trusted connection}
\usage{
DB_WP_CE_RAISED(
  server = get_default_db_server(),
  database = WP_CE_RAISED,
  username = get_username_for_db(WP_CE_RAISED),
  password = get_password_for_db(WP_CE_RAISED),
  database_encoding = "CP1252",
  client_charset = "UTF-8"
)
}
\arguments{
\item{server}{The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})}

\item{database}{The "WP raised catches" database name (defaults to \code{\link{WP_CE_RAISED}})}

\item{username}{The username (defaults to the standard one for this specific DB)}

\item{password}{The password (defaults to the standard one for this specific DB)}

\item{database_encoding}{The encoding set for the specific database (defaults to CP1252)}

\item{client_charset}{The character set used by the client (defaults to UTF-8)}
}
\value{
An ODBC connection to \code{\link{WP_CE_RAISED}} on \code{server}
}
\description{
Connects to an instance of \code{\link{WP_CE_RAISED}} on a given server machine using a trusted connection
}
