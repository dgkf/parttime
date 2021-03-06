% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_partial_time_ops.R
\name{Ops.partial_time}
\alias{Ops.partial_time}
\title{Handler for Ops generics for partial_time objects}
\usage{
\method{Ops}{partial_time}(e1, e2)
}
\arguments{
\item{e1}{objects}

\item{e2}{objects}
}
\description{
Handler for Ops generics for partial_time objects
}
\examples{
                                       # when assume_tz "GMT"         when assume_tz NA
                                       # ---------------------------  ---------------------------
                                       # raw    possibly  definitely   raw   possibly  definitely  
                                       # -----  --------- -----------  ----- --------- -----------
parttime(1998) < parttime(1999)        # TRUE   TRUE      TRUE         NA    TRUE      FALSE
parttime(1998) < parttime(1997)        # FALSE  FALSE     FALSE        NA    TRUE      FALSE
parttime(1999) < parttime(1999)        # NA     TRUE      FALSE        NA    TRUE      FALSE
parttime(1998) < parttime(1999, 1, 3)  # TRUE   TRUE      TRUE         TRUE  TRUE      TRUE

}
