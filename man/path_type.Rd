% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_assignment.R
\name{path_type}
\alias{path_type}
\title{Get the path type.}
\usage{
path_type(path, auth = NULL)
}
\arguments{
\item{path}{Character element of url or local search path.}

\item{auth}{an auth request class with options \code{httpauth} and \code{userpwd}.}
}
\value{
\code{path_type} object with c("path_local", "path_http", "path_error")
}
\description{
Check the path type.
}
\keyword{internal}
