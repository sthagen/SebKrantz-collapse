\name{list-processing}
\alias{list-processing}
\title{List Processing}

\description{
\code{collapse} provides the following set of functions to work with lists of R objects:
\itemize{
\item \bold{Identification:}\itemize{
\item \code{\link{is.regular}} checks whether an R object is either atomic or a list. A (nested) list composed of regular objects at each level of the list-tree is unlistable to an atomic vector, checked by \code{\link{is.unlistable}}.
\item \code{\link{ldepth}} determines the level of nesting of the list (i.e. the maximum number of nodes of the list-tree).
\item \code{\link{has_elem}} searches elements in a list using element names, regular expressions applied to element names, or a function applied to the elements, and returns \code{TRUE} if any matches were found.
}

\item \bold{Subsetting:} \itemize{
\item \code{\link{atomic_elem}} examines the top-level of a list and returns a sublist with the atomic elements. Conversely \code{\link{list_elem}} returns the sublist of elements which are themselves lists or list-like objects.
 \item \code{\link{reg_elem}} and \code{\link{irreg_elem}} are recursive versions of the former. \code{\link{reg_elem}} extracts the regular part of the list-tree (leading to atomic elements in the final nodes), while \code{\link{irreg_elem}} extracts the 'irregular' part of the list tree leading to non-atomic elements in the final nodes. (\emph{Tipp}: try calling both on an \code{lm} object). Naturally for all lists \code{l}, \code{is.unlistable(reg_elem(l))} evaluates to \code{TRUE}...
\item \code{\link{get_elem}} extracts elements from a list using element names, regular expressions applied to element names, a function applied to the elements, or element-indices used to subset the lowest-level sub-lists. by default the result is presented as a simplified list containing all matching elements. With the \code{keep.tree} option however \code{\link{get_elem}} can also be used to subset lists i.e. maintain the full tree but cut off non-matching branches.
}

\item \bold{Apply Functions:} \code{\link{rapply2d}} is a recursive version of \code{base::lapply} with two key differences to \code{base::rapply}: (1) Data frames are considered as atomic objects, not as (sub-)lists, and (2) the result is not simplified.

\item \bold{Unlisting / Row-Binding:} \code{\link{unlist2d}} efficiently unlists unlistable lists in 2-dimensions and creates a \code{data.frame} (or \code{data.table}) representation of the list (unlike \code{base::unlist} which returns an atomic vector). This is done by recursively flattening and row-binding R objects in the list with (using \code{data.table::rbindlist}) while creating identifier columns for each level of the list-tree and (optionally) saving the row-names of the objects in a separate column. \code{\link{unlist2d}} can thus also be understood as a recursive generalization of \code{do.call(rbind, l)}, for lists of vectors, data.frames, arrays or heterogenous objects.


}
}
\section{Table of Functions}{
  \tabular{lll}{\emph{ Function} \Sexpr{"\u200B"} \Sexpr{"\u200B"}  \tab \emph{ Description }  \cr
  \code{\link{is.regular}} \tab \code{function(x) is.atomic(x) || is.list(x)} \cr
  \code{\link{is.unlistable}} \tab checks if list is unlistable \cr
  \code{\link{ldepth}} \tab level of nesting / maximum depth of list-tree \cr
  \code{\link{has_elem}} \tab checks if list contains a certain element \cr
  \code{\link{get_elem}} \tab subset list / extract certain elements \cr
  \code{\link{get_elem}} \tab subset list / extract certain elements \cr
  \code{\link{reg_elem}} \tab subset / extract regular part of list \cr
  \code{\link{irreg_elem}} \tab subset / extract non-regular part of list \cr
  \code{\link{atomic_elem}} \tab top-level subset atomic elements \cr
  \code{\link{list_elem}} \tab top-level subset list/list-like elements \cr
  \code{\link{rapply2d}} \tab recursively apply functions to lists of data objects \cr
  \code{\link{unlist2d}} \tab recursively unlist/row-bind lists of data objects in 2D, to \code{data.frame} or \code{data.table} \cr
}
}
\seealso{
\link[=collapse-documentation]{Collapse Overview}
}


