library(Rcpp)
sourceCpp("R/C++/mrtl_type_dispatch_final.cpp", rebuild = TRUE)
sourceCpp("R/C++/qFqG.cpp", rebuild = TRUE) # https://gallery.rcpp.org/articles/fast-factor-generation/
qF <- function(x, ordered = TRUE) {
  if(is.factor(x)) return(x)
  qFCpp(x, ordered)
}


# todo: what about cimpilation using compile::cmpfun??
# todo: what if g is a list of factors ?? -> general convern for all functions !!
# todo: parallelism using mcapply !!

# also what about split apply combining other data stricture i.e. factors, date and time ... -> try mode !!
# -> need fplit and unlist (original) to account for factors. Note that fsplit does not deal with date and time ... but unlist can't handle those either... but nobody aggregates dates anyway...


fsplit <- function(x, f) {
  if (is.null(attr(x, "class"))) 
    return(.Internal(split(x, f)))
  lf <- levels(f)
  y <- vector("list", length(lf))
  names(y) <- lf
  ind <- .Internal(split(seq_along(x), f))
  for (k in lf) y[[k]] <- x[ind[[k]]]
  y
}

# Faster version of BY:
BY <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE) {
  UseMethod("BY", X)
}

BY.default <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE, expand.wide = FALSE) { # what about ... in those other internal calls ???. 
  if(!is.atomic(X)) stop("X needs to be an atomic vector") # redundant ?? 
  if(!is.factor(g)) if(all(class(g) == "GRP")) g <- as.factor.GRP(g) else if(is.list(g)) # multiple comparisons with class ?? 
                    g <- interaction(lapply(g, qF)) else g <- qF(g) 
    res <- lapply(fsplit(X, g), FUN, ...)
    if(simplify) {
      if(expand.wide) {
        res <- do.call(rbind, res) 
        if(!use.g.names) dimnames(res) <- list(NULL, dimnames(res)[[2]])
      } else {
        if(use.g.names) {
          res <- unlist(res, recursive = FALSE)
          if(length(res) == length(X) && typeof(res) == typeof(X)) {
            ax <- attributes(X)
            if(!is.null(ax)) {
              ax[["names"]] <- names(res)
              attributes(res) <- ax
            }
          }
        } else {
        ll <- length(res)
        nr1 <- names(res[[1]]) # good solution ?? 
        res <- unlist(res, recursive = FALSE, use.names = FALSE)          
        if(length(res) == length(X) && typeof(res) == typeof(X))
          attributes(res) <- attributes(X) else if(length(res) != ll) {
            if(!is.null(nr1) && length(res) == length(nr1)*ll) # additional check
            names(res) <- rep(nr1, ll)
          }
        }
      }
    } else if(!use.g.names) names(res) <- NULL
    res
}

# do something about the attributes -> either lapply with attribute copy or splitfun with attribute copy !!
# is typeof enough to warrant attribute copy ?? or also equal length needed ??
BY.data.frame <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE, expand.wide = FALSE) {
  if(!is.factor(g)) if(all(class(g) == "GRP")) g <- as.factor.GRP(g) else if(is.list(g)) # multiple comparisons with class ?? 
                      g <- interaction(lapply(g, qF)) else g <- qF(g) 

    if(simplify) {
      ax <- attributes(X)
      if(expand.wide) {
        splitfun <- function(x) mctl(do.call(rbind, lapply(fsplit(x, g), FUN, ...)), names = TRUE)
        res <- unlist(lapply(X, splitfun), recursive = FALSE, use.names = TRUE)
        ax[["row.names"]] <- if(use.g.names && !any(ax[["class"]] == "data.table")) levels(g) else .set_row_names(length(res[[1]])) # faster than nlevels ??
        ax[["names"]] <- names(res)
      } else {
        if(use.g.names && !any(ax[["class"]] == "data.table")) {
          res <- vector("list", length(X))
          res[[1]] <- unlist(lapply(fsplit(X[[1]], g), FUN, ...), FALSE, TRUE) 
          ax[["row.names"]] <- names(res[[1]]) 
          names(res[[1]]) <- NULL  # length(X[[1]])
          if(length(res[[1]]) == nrow(X) && typeof(res[[1]]) == typeof(X[[1]])) { # safe ?? or rather use function below ??
            attributes(res[[1]]) <- attributes(X[[1]])
            splitfun <- function(x) duplicate_attributes(unlist(lapply(fsplit(x, g), FUN, ...), FALSE, FALSE), x) 
          } else splitfun <- function(x) unlist(lapply(fsplit(x, g), FUN, ...), FALSE, FALSE)
          res[-1] <- lapply(X[-1], splitfun) # internal ??
        } else {
          nrx <- nrow(X) # length(X[[1]]) because I'm using this in collapse on unclassed objects !!
          splitfun <- function(x) {
            out <- unlist(lapply(fsplit(x, g), FUN, ...), FALSE, FALSE)
            if(length(out) == nrx && typeof(out) == typeof(x)) attributes(out) <- attributes(x) # good ??
            out
          }
          res <- lapply(X, splitfun) # internal ??
          if(length(res[[1]]) != nrx) ax[["row.names"]] <- .set_row_names(length(res[[1]]))
        }
      }
      attributes(res) <- ax
      res
    } else {
      if(expand.wide) lapply(X, function(x) do.call(rbind, lapply(fsplit(x, g), FUN, ...))) else {
       if(use.g.names) lapply(X, function(x) lapply(fsplit(x, g), FUN, ...)) else
        lapply(X, function(x) setNames(lapply(fsplit(x, g), FUN, ...), NULL))
      }
    }
}

# todo: create mcapply -> faster ?? -> NOPE !!!
BY.matrix <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE, expand.wide = FALSE) { # add row.names !!
  if(!is.factor(g)) if(all(class(g) == "GRP")) g <- as.factor.GRP(g) else if(is.list(g)) 
                    g <- interaction(lapply(g, qF)) else g <- qF(g) 
                    
      if(expand.wide) {
        splitfun <- if(use.g.names) function(x) do.call(rbind, lapply(fsplit(x, g), FUN, ...)) else 
                    function(x) do.call(rbind, setNames(lapply(fsplit(x, g), FUN, ...), NULL))
        res <- lapply(mctl(X, names = TRUE), splitfun) # not simplifying here could be nice !!!
        if(!simplify) return(res)
        nr <- names(res)
        res <- do.call(cbind, res)
        dimnames(res)[[2]] <- paste(rep(nr, each = ncol(res)/length(nr)), dimnames(res)[[2]], sep = ".") # best ??
        res
      } else {
      if(simplify) {
        splitfun <- function(x, un = FALSE) unlist(lapply(fsplit(x, g), FUN, ...), FALSE, un)
        if(use.g.names) {
          res <- vector("list", ncol(X))
          X <- mctl(X, names = TRUE)
          res[[1]] <- splitfun(X[[1]], un = TRUE) # rewrite all in C++ ??
          dnr <- list(names(res[[1]]), names(X))
          res[-1] <- lapply(X[-1], splitfun) # internal ??
          setDimnames(do.call(cbind, res), dnr) # what about attribute copy ??
        } else {
          res <- do.call(cbind, lapply(mctl(X, names = TRUE), splitfun)) # internal ??
          if(nrow(res) == nrow(X) && typeof(res) == typeof(X)) attributes(res) <- atributes(X) # or dimnames ?? ??
          res
        }
      } else {
        if(use.g.names) lapply(mctl(X, names = TRUE), function(x) lapply(fsplit(x, g), FUN, ...)) else
          lapply(mctl(X, names = TRUE), function(x) setNames(lapply(fsplit(x, g), FUN, ...), NULL))
      }
    }
}


# Previous Versions !!

# minimal version : Slightly faster but less secure and versatile than the above!! 
# BY.default <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE, expand.wide = FALSE) { # what about ... in those other internal calls ???. 
#   if(!is.atomic(X)) stop("X needs to be an atomic vector") # redundant ?? 
#   if(!is.factor(g)) if(class(g) == "GRP") g <- as.factor.GRP(g) else if(is.list(g)) # multiple comparisons with class ?? 
#     g <- interaction(lapply(g, qF)) else g <- qF(g) 
#     res <- lapply(.Internal(split(X, g)), FUN, ...)
#     if(simplify) {
#       if(expand.wide) {
#         res <- do.call(rbind, res) 
#         if(!use.g.names) dimnames(res) <- list(NULL, dimnames(res)[[2]])
#       } else {
#         ll <- length(res) # levs <- levels(g) # what if unused levels ??? 
#         nr1 <- names(res[[1]]) # good solution ?? 
#         res <- .Internal(unlist(res, FALSE, FALSE))
#         if(length(res) == ll) {
#           if(use.g.names) names(res) <- levels(g)  # what to copy ?? 
#         } else if(length(res) == length(X))
#           attributes(res) <- attributes(X) else # what to copy ??
#             names(res) <- if(use.g.names) 
#               paste(rep(levels(g), each = length(nr1)), nr1, sep = ".") else 
#                 rep(nr1, ll)
#       }
#     } else if(!use.g.names) names(res) <- NULL
#     res
# }


# BY.default <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE) { # what about ... in those other internal calls ???. 
#   if(!is.factor(g)) if(class(g) == "GRP") g <- as.factor.GRP(g) else if(is.list(g)) g <- interaction(lapply(g, qF)) else # is.list(g) && all(vapply(g, is.factor, TRUE, USE.NAMES = FALSE))
#     g <- qF(g) # stop("g must be a factor, GRP object or list of factors")
#   if(simplify) {
#     # This vapply does not work, because you could have a function like NOBS applied to character but returning integer !!
#     # if(use.g.names) vapply(.Internal(split(X, g)), FUN, X[1], ...) else vapply(.Internal(split(X, g)), FUN, X[1], ..., USE.NAMES = FALSE)
#     ax <- attributes(X)
#     if(use.g.names) {
#       if(is.null(ax) || all(names(ax) == "names")) .Internal(unlist(lapply(.Internal(split(X, g)), FUN, ...), FALSE, TRUE)) else {
#         res <- .Internal(unlist(lapply(.Internal(split(X, g)), FUN, ...), FALSE, TRUE)) 
#         ax[["names"]] <- names(res) # best ?? 
#         attributes(res) <- ax
#         res
#       }
#     } else { # best ?? 
#       if(is.null(ax) || is.null(ax[["names"]])) .Internal(unlist(lapply(.Internal(split(X, g)), FUN, ...), FALSE, FALSE)) else {
#         res <- .Internal(unlist(lapply(.Internal(split(X, g)), FUN, ...), FALSE, FALSE)) 
#         if(length(ax[["names"]]) != length(res))  ax[["names"]] <- NULL
#         attributes(res) <- ax
#         res
#       }
#     }
#   } else {
#     if(use.g.names) lapply(.Internal(split(X, g)), FUN, ...) else setNames(lapply(.Internal(split(X, g)), FUN, ...), NULL)
#   }
# }

# good ????
# fastest ??? best ?? 
# res1 <- res[[1]] # Old implementation !!
# dim(res) <- c(length(levs),length(res1))
# dimnames(res) <- list(levs, names(res1))

# } else {
#   res <- lapply(.Internal(split(X, g)), FUN, ...) # faster than unlist ?? 
#   if(length(res) == length(X)) { # speed up ???
#     attributes(res) <- attributes(X)
#     # ax <- attributes(X)
#     # if(!is.null(ax)) {
#     #   ax[["names"]] <- names(res)
#     #   attributes(res) <- ax
#     # }
#   }
# }

# } else { # best ?? 
#   ll <- nlevels(g)
#   if(expand.wide) {
#     res <- lapply(.Internal(split(X, g)), FUN, ...)
#     res1 <- res[[1]]
#     res <- .Internal(unlist(res, FALSE, FALSE)) # faster than unlist ?? 
#     if(length(res) == length(X)) 
#       attributes(res) <- attributes(X) else if(length(res) != ll) { # what attributes to copy ?? 
#         dim(res) <- c(ll,length(res1))
#         dimnames(res) <- list(NULL, names(res1))
#       }
#   } else {
#     res <- .Internal(unlist(lapply(.Internal(split(X, g)), FUN, ...), FALSE, FALSE))
#     if(length(res) == length(X)) attributes(res) <- attributes(X)
#   }
# }


# Latest version: Just one bug:
# BY.data.frame <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE, expand.wide = FALSE) {
#   if(!is.factor(g)) if(all(class(g) == "GRP")) g <- as.factor.GRP(g) else if(is.list(g)) # multiple comparisons with class ?? 
#     g <- interaction(lapply(g, qF)) else g <- qF(g) 
#     
#     if(simplify) {
#       ax <- attributes(X)
#       if(expand.wide) {
#         splitfun <- function(x) mctl(do.call(rbind, lapply(fsplit(x, g), FUN, ...)), names = TRUE)
#         res <- unlist(lapply(X, splitfun), recursive = FALSE, use.names = TRUE)
#         ax[["row.names"]] <- if(use.g.names && !any(ax[["class"]] == "data.table")) levels(g) else .set_row_names(length(res[[1]])) # faster than nlevels ??
#         ax[["names"]] <- names(res)
#       } else {
#         nrx <- nrow(X)
#         splitfun <- function(x, un = FALSE) {
#           out <- unlist(lapply(fsplit(x, g), FUN, ...), FALSE, un)
#           if(length(out) == nrx) attributes(out) <- attributes(x) # good ??
#           out
#         }
#         if(use.g.names && !any(ax[["class"]] == "data.table")) {
#           res <- vector("list", length(X))
#           res[[1]] <- splitfun(X[[1]], un = TRUE)
#           ax[["row.names"]] <- names(res[[1]]) # problem here, because you assigned attributes in splitfun, so names are NULL !! 
#           names(res[[1]]) <- NULL
#           res[-1] <- lapply(X[-1], splitfun) # internal ??
#         } else {
#           res <- lapply(X, splitfun) # internal ??
#           if(length(res[[1]]) != nrow(X)) ax[["row.names"]] <- .set_row_names(length(res[[1]]))
#         }
#       }
#       attributes(res) <- ax
#       res
#     } else {
#       if(expand.wide) lapply(X, function(x) do.call(rbind, lapply(fsplit(x, g), FUN, ...))) else {
#         if(use.g.names) lapply(X, function(x) lapply(fsplit(x, g), FUN, ...)) else
#           lapply(X, function(x) setNames(lapply(fsplit(x, g), FUN, ...), NULL))
#       }
#     }
# }



# # do something about the attributes -> either lapply with attribute copy or splitfun with attribute copy !!
# # BY.data.frame2 is also pretty good !!!, juts less parsimonious !!
# BY.data.frame2 <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE, expand.wide = FALSE) {
#   if(!is.factor(g)) if(class(g) == "GRP") g <- as.factor.GRP(g) else if(is.list(g)) # multiple comparisons with class ?? 
#     g <- interaction(lapply(g, qF)) else g <- qF(g) 
#     # what about ... argument ?? 
#     if(simplify) {
#       ax <- attributes(X)
#       if(expand.wide) {
#         # faster than using splitfun ??
#         res <- lapply(X, function(x) mctl(do.call(rbind, lapply(.Internal(split(x, g)), FUN, ...)), names = TRUE)) 
#         res <- .Internal(unlist(res, FALSE, TRUE))
#         ax[["row.names"]] <- if(use.g.names && !any(ax[["class"]] == "data.table")) levels(g) else .set_row_names(length(res[[1]])) # faster than nlevels ??
#         ax[["names"]] <- names(res)
#       } else {
#         lx <- length(X)
#         if(use.g.names && !any(ax[["class"]] == "data.table")) {
#           res <- vector("list", lx)
#           res[[1]] <- .Internal(unlist(lapply(.Internal(split(X[[1]], g)), FUN, ...), FALSE, TRUE)) 
#           ax[["row.names"]] <- names(res[[1]]) # better splitfun else build ... call every time ??
#           res[-1] <- lapply(X[-1], function(x) .Internal(unlist(lapply(.Internal(split(x, g)), FUN, ...), FALSE, FALSE)))
#         } else {
#           res <- lapply(X, function(x) .Internal(unlist(lapply(.Internal(split(x, g)), FUN, ...), FALSE, FALSE)))
#           if(length(res[[1]]) != nrow(X)) ax[["row.names"]] <- .set_row_names(length(res[[1]]))
#         }
#       }
#       attributes(res) <- ax
#     } else {
#       res <- if(use.g.names) lapply(X, function(x) lapply(.Internal(split(x, g)), FUN, ...)) else
#         lapply(X, function(x) setNames(lapply(.Internal(split(x, g)), FUN, ...), NULL))
#     }
#     res
# }
# 
# BD3 <- compiler::cmpfun(BY.data.frame2)
# 
# # do something about the attributes -> either lapply with attribute copy or splitfun with attribute copy !!
# BY.data.frame <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE) {
#   if(!is.factor(g)) if(class(g) == "GRP") g <- as.factor.GRP(g) else if(is.list(g)) 
#     g <- interaction(lapply(g, qF)) else g <- qF(g) 
#     if(simplify) {
#       splitfun <- function(x) unlist(lapply(.Internal(split(x, g)), FUN, ...), FALSE, FALSE) # do lapply with copy attributes ??
#       # splitfun <- function(x) { # perhaps better ????
#       #   res <- lapply(.Internal(split(x, g)), FUN, ...)
#       #   if(length(res[[1]]) == 1)
#       
#       res <- lapply(X, splitfun) # faster than lapply ?? -> .Internal does not work here !! -> fast, 0.62 sec NWDI sum with na.rm !!
#       ax <- attributes(X)
#       if(use.g.names) {
#         levs <- levels(g)
#         ll <- length(levs)
#         lr <- length(res[[1]])
#         if(lr == ll) {
#           ax[["row.names"]] <- if(any(ax[["class"]] == "data.table")) .set_row_names(ll) else levs
#         } else if(lr != nrow(X)) {
#           nam <- names(FUN(X[[1]][1], ...)) # best ?? what if different columns give different responses ?? rbind would be better .. 
#           trafun <- function(x) mctl(matrix(x, nrow = ll, byrow = TRUE)) # best ?? what about do.call rbind ?? -> still no list ... but stil better because multiple functions ... 
#           res <- unlist(lapply(res, trafun), FALSE, FALSE) # fast ?? 
#           ax[["names"]] <- paste(rep(ax[["names"]], each = length(nam)), nam, sep = ".")
#           ax[["row.names"]] <- if(any(ax[["class"]] == "data.table")) .set_row_names(ll) else levs
#         }
#         attributes(res) <- ax
#       } else {
#         stop("error")
#       }
#       res
#     } else { # best ?? -> much slower than if not using split.data.frame...
#       # if(use.g.names) lapply(split(X, g), dapply, FUN, ...) else setNames(lapply(split(X, g), dapply, FUN, ...), NULL)
#       if(use.g.names) lapply(X, lapply, split(x, g), FUN, ...) else 
#         lapply(X, function(x) setNames(lapply(.Internal(split(x, g)), FUN, ...), NULL))
#     }
# }


# # split.data.frame or data.table is extremely slow !!
# BY.data.frame <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE) {
#   if(!is.factor(g)) if(class(g) == "GRP") g <- as.factor.GRP(g) else if(is.list(g)) g <- interaction(lapply(g, qF)) else # is.list(g) && all(vapply(g, is.factor, TRUE, USE.NAMES = FALSE))
#     g <- qF(g) # stop("g must be a factor, GRP object or list of factors")
#   if(simplify) {
#     # This vapply does not work, because you could have a function like NOBS applied to character but returning integer !!
#     # splitfun <- function(x) vapply(.Internal(split(x, g)), FUN, x[1], ..., USE.NAMES = FALSE) 
#     splitfun <- function(x) .Internal(unlist(lapply(.Internal(split(x, g)), FUN, ...), FALSE, FALSE)) # do lapply with copy attributes ??
#     res <- .Internal(lapply(X, splitfun))
#     # res <- .Call(data.table:::Crbindlist, rapply(split(X, g), FUN, how = "list", ...), FALSE, FALSE, NULL) # what if use.names = TRUE??
#     ax <- attributes(X)
#     if(use.g.names) {
#       if(length(res[[1]]) == nlevels(g)) 
#         ax[["row.names"]] <- if(any(ax[["class"]] == "data.table")) .set_row_names(length(res[[1]])) else attr(g, "levels")
#     } else if(length(res[[1]]) != length(X[[1]])) ax[["row.names"]] <- .set_row_names(length(X[[1]]))
#     attributes(res) <- ax
#     res
#   } else { # best ?? -> much slower than if not using split.data.frame...
#     # if(use.g.names) lapply(split(X, g), dapply, FUN, ...) else setNames(lapply(split(X, g), dapply, FUN, ...), NULL)
#     if(use.g.names) lapply(X, lapply, split(x, g), FUN, ...) else 
#       lapply(X, function(x) setNames(lapply(.Internal(split(x, g)), FUN, ...), NULL))
#   }
# }


# # Note: create matrix apply in Cpp ?? -> google it !!
# BY.matrix <- function(X, g, FUN, ..., simplify = TRUE, use.g.names = TRUE) { # add row.names !!
#   if(!is.factor(g)) if(class(g) == "GRP") g <- as.factor.GRP(g) else if(is.list(g)) 
#     g <- interaction(lapply(g, qF)) else g <- qF(g) 
#     if(simplify) {
#       splitfun <- function(x) .Internal(unlist(lapply(.Internal(split(x, g)), FUN, ...), FALSE, FALSE)) # chek speed !!
#       res <- do.call(cbind, lapply(mctl(X), splitfun)) # internal here is a lot slower 
#       dnx <- dimnames(X)
#       if(use.g.names) { # faster way ??
#         if(nrow(res) == nlevels(g)) dnx[[1]] <- attr(g, "levels")
#       } else if(nrow(res) != nrow(X)) dnx[[1]] <- NULL
#       dimnames(res) <- dnx
#       res
#     } else { # what does this mean ?? 
#       if(use.g.names) lapply(mctl(X), function(x) lapply(.Internal(split(x, g)), FUN, ...)) else 
#         lapply(mctl(X), function(x) setNames(lapply(.Internal(split(x, g)), FUN, ...), NULL))
#     }
# }