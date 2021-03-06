% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_common.R
\name{make_common}
\alias{make_common}
\title{Make Data - Commonsize}
\usage{
make_common(df, pos, split_cols = NULL)
}
\arguments{
\item{df}{dataframe}

\item{pos}{numeric}

\item{split_cols}{character vector specifying the columns to split by.
defaults to NULL, which results in no split.}
}
\value{
dataframe
}
\description{
This is a recursive function.
If the split columns are present the function will use them to split the
the dataframe into a list of dataframes, then cycle through each element
and apply the function.
}
