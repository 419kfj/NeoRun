\name{NeoRun}
\alias{NeoRun}
\title{NeoRun backup data conversion}
\usage{
fname <- "20170801-20170801.csv"
.res <- NeoRun_data(fname)
.res$base
.res$Lap
.res$Graph
}
\description{
set fname as backup file
Run NeoRun_data and get the results.
.res$base :: basic information of the event
.res$Lap :: Get Lap data table
.res$Graph :: Graph plot related table
.res$GPS :: on the check. might have bug.
}
\examples{
fname <- "20170801-20170801.csv"
.res <- NeoRun_data(fname)
.res$base
.res$Lap
.res$Graph
}
