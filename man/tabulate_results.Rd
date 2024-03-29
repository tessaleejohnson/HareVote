% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vote_funs.R
\name{tabulate_results}
\alias{tabulate_results}
\title{tabulate_results}
\usage{
tabulate_results(.data)
}
\arguments{
\item{.data}{Dataframe or tibble. Nominees in columns; rankings in rows.}
}
\value{
A tibble with two columns: \code{name}, containing the name of the
nominee, and \code{pct}, containing the proportion of first-choice votes
the corresponding nominee received.
}
\description{
Tabulate proportion of first-choice votes for each nominee.
}
