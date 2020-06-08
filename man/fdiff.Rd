\name{fdiff}
\alias{fdiff}
\alias{fdiff.default}
\alias{fdiff.matrix}
\alias{fdiff.data.frame}
\alias{fdiff.list}
\alias{fdiff.pseries}
\alias{fdiff.pdata.frame}
\alias{fdiff.grouped_df}
\alias{D}
\alias{D.default}
\alias{D.matrix}
\alias{D.data.frame}
\alias{D.list}
\alias{D.pseries}
\alias{D.pdata.frame}
\alias{D.grouped_df}
\alias{Dlog}
\alias{Dlog.default}
\alias{Dlog.matrix}
\alias{Dlog.data.frame}
\alias{Dlog.list}
\alias{Dlog.pseries}
\alias{Dlog.pdata.frame}
\alias{Dlog.grouped_df}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ % Lagged and Iterated
Fast (Quasi-, Log-) Differences for Time-Series and Panel Data
}
\description{
\code{fdiff} is a S3 generic to compute (sequences of) suitably lagged / leaded and iterated differences, quasi-differences, log-differences or quasi-log-differences. The difference and log-difference operators \code{D} and \code{Dlog} also exists as parsimonious wrappers around \code{fdiff}. Apart from being more parsimonious, they provide a bit more flexibility than \code{fdiff} when applied to data frames.

}
\usage{
  fdiff(x, n = 1, diff = 1, \dots)
      D(x, n = 1, diff = 1, \dots)
   Dlog(x, n = 1, diff = 1, \dots)
% ## Default S3 methods:
\method{fdiff}{default}(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, rho = 1,
      stubs = TRUE, \dots)
\method{D}{default}(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1,
  stubs = TRUE, \dots)
\method{Dlog}{default}(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1, stubs = TRUE, \dots)
%## S3 methods for class 'matrix'
\method{fdiff}{matrix}(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, rho = 1,
      stubs = TRUE, \dots)
\method{D}{matrix}(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1,
  stubs = TRUE, \dots)
\method{Dlog}{matrix}(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, rho = 1, stubs = TRUE, \dots)
%## S3 methods for class 'data.frame'
\method{fdiff}{data.frame}(x, n = 1, diff = 1, g = NULL, t = NULL, fill = NA, logdiff = FALSE, rho = 1,
      stubs = TRUE, \dots)
\method{D}{data.frame}(x, n = 1, diff = 1, by = NULL, t = NULL, cols = is.numeric,
  fill = NA, rho = 1, stubs = TRUE, keep.ids = TRUE, \dots)
\method{Dlog}{data.frame}(x, n = 1, diff = 1, by = NULL, t = NULL, cols = is.numeric,
     fill = NA, rho = 1, stubs = TRUE, keep.ids = TRUE, \dots)

# Methods for compatibility with plm:
%## S3 methods for class 'pseries'
\method{fdiff}{pseries}(x, n = 1, diff = 1, fill = NA, logdiff = FALSE, rho = 1, stubs = TRUE, \dots)
\method{D}{pseries}(x, n = 1, diff = 1, fill = NA, rho = 1, stubs = TRUE, \dots)
\method{Dlog}{pseries}(x, n = 1, diff = 1, fill = NA, rho = 1, stubs = TRUE, \dots)
%## S3 methods for class 'pdata.frame'
\method{fdiff}{pdata.frame}(x, n = 1, diff = 1, fill = NA, logdiff = FALSE, rho = 1, stubs = TRUE, \dots)
\method{D}{pdata.frame}(x, n = 1, diff = 1, cols = is.numeric, fill = NA, rho = 1, stubs = TRUE,
  keep.ids = TRUE, \dots)
\method{Dlog}{pdata.frame}(x, n = 1, diff = 1, cols = is.numeric, fill = NA, rho = 1, stubs = TRUE,
     keep.ids = TRUE, \dots)

# Methods for compatibility with dplyr:
%## S3 methods for class 'grouped_df'
\method{fdiff}{grouped_df}(x, n = 1, diff = 1, t = NULL, fill = NA, logdiff = FALSE, rho = 1, stubs = TRUE,
      keep.ids = TRUE, \dots)
\method{D}{grouped_df}(x, n = 1, diff = 1, t = NULL, fill = NA, rho = 1, stubs = TRUE,
  keep.ids = TRUE, \dots)
\method{Dlog}{grouped_df}(x, n = 1, diff = 1, t = NULL, fill = NA, rho = 1, stubs = TRUE,
     keep.ids = TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector, matrix, data.frame, panel-series (\code{plm::pseries}), panel-data.frame (\code{plm::pdata.frame}) or grouped tibble (\code{dplyr::grouped_df}).}
  \item{n}{a integer vector indicating the number of lags or leads.}
  \item{diff}{a vector of integers > 1 indicating the order of differencing / log-differencing.}
  \item{g}{a factor, \code{\link{GRP}} object, atomic vector (internally converted to factor) or a list of vectors / factors (internally converted to a \code{\link{GRP}} object) used to group \code{x}.}
  \item{by}{\emph{data.frame method}: Same as \code{g}, but also allows one- or two-sided formulas i.e. \code{~ group1} or \code{var1 + var2 ~ group1 + group2}. See Examples.}
  \item{t}{same input as \code{g}, to indicate the time-variable. For safe computation of differences on unordered time-series and panels. \emph{Notes}: data.frame method also allows name, index or one-sided formula i.e. \code{~time}. grouped_df method also allows lazy-evaluation i.e. \code{time} (no quotes).}
  \item{cols}{\emph{data.frame method}: Select columns to difference using a function, column names or indices. Default: All numeric variables. \emph{Note}: \code{cols} is ignored if a two-sided formula is passed to \code{by}.}
  \item{fill}{value to insert when vectors are shifted. Default is \code{NA}. }
  \item{logdiff}{logical. \code{TRUE} Computes log-differences instead. See Details.}
  \item{rho}{double. Autocorrelation parameter. Set to a value between 0 and 1 for quasi-differencing. However any numeric value can be supplied. }
  \item{stubs}{logical. \code{TRUE} will rename all differenced columns by adding prefixes "L\code{n}D\code{diff}." / "F\code{n}D\code{diff}." for differences "L\code{n}Dlog\code{diff}." / "F\code{n}Dlog\code{diff}." for log-differences and replacing "D" / "Dlog" with "QD" / "QDlog" for quasi-differences. }
  \item{keep.ids}{\emph{data.frame / pdata.frame / grouped_df methods}: Logical. Drop all panel-identifiers from the output (which includes all variables passed to \code{by} or \code{t}). \emph{Note}: For panel-data.frame's and grouped tibbles identifiers are dropped, but the 'index' / 'groups' attributes are kept.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
By default, \code{fdiff/D/Dlog} return \code{x} with all columns differenced / log-differenced. Differences are computed as \code{repeat(diff) x[i] - rho*x[i-n]}, and log-differences as \code{repeat(diff) log(x[i]) - rho*log(x[i-n])}. If \code{rho < 1}, this becomes quasi- (or partial) differencing, which is a technique suggested by Cochrane and Orcutt (1949) to deal with serial correlation in regression models, where \code{rho} is typically estimated by running a regression of the model residuals on the lagged residuals. Setting \code{diff = 2} returns differences of differences etc... and setting \code{n = 2} returns simple differences computed by subtracting twice-lagged \code{x} from \code{x}. It is also possible to compute forward differences by passing negative \code{n} values. \code{n} also supports arbitrary vectors of integers (lags), and \code{diff} supports positive sequences of integers (differences):

If more than one value is passed to \code{n} and/or \code{diff}, the data is expanded-wide as follows: If \code{x} is an atomic vector or time-series, a (time-series) matrix is returned with columns ordered first by lag, then by difference. If \code{x} is a matrix or data.frame, each column is expanded in like manor such that the output has \code{ncol(x)*length(n)*length(diff)} columns ordered first by column name, then by lag, then by difference.

With groups/panel-identifiers supplied to \code{g/by}, \code{fdiff/D/Dlog} efficiently compute panel-differences. If \code{t} is left empty, the data needs to be ordered such that all values belonging to a group are consecutive and in the right order. It is not necessary that the groups themselves occur in the right order. If time-variable(s) are supplied to \code{t}, the panel is fully identified and differences can be securely computed even if the data is completely unordered.

\code{fdiff/D/Dlog} supports balanced panels and unbalanced panels where various individuals are observed for different time-sequences (both start, end and duration of observation can differ for each individual), but does not natively support irregularly spaced time-series and panels. For computational details and efficiency considerations see the help page for \code{\link{flag}}. A work-around for differencing irregular panels is easily achieved with the help of \code{\link{seqid}}.

It is also possible to compute differences on unordered vectors / time-series (thus utilizing \code{t} but leaving \code{g/by} empty).

The methods applying to \emph{plm} objects (panel-series and panel-data.frames) automatically utilize the panel-identifiers attached to these objects and thus securely compute fully identified panel-differences. If these objects have > 2 panel-identifiers attached to them, the last identifier is assumed to be the time-variable, and the others are taken as grouping-variables and interacted.
}
\value{
\code{x} differenced \code{diff} times using lags \code{n} of itself. Quasi and log-differences are toggled by the \code{rho} and \code{logdiff} arguments or the \code{Dlog} operators. Computations can be grouped by \code{g/by} and/or ordered by \code{t}. See Details and Examples.
}
\references{
Cochrane, D.; Orcutt, G. H. (1949). Application of Least Squares Regression to Relationships Containing Auto-Correlated Error Terms. \emph{Journal of the American Statistical Association}. 44 (245): 32-61.
}
% \author{
%%  ~~who you are~~
% }
% \note{
%%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[=flag]{flag/L/F}}, \code{\link[=fgrowth]{fgrowth/G}}, \link[=A7-time-series-panel-series]{Time-Series and Panel-Series}, \link[=collapse-documentation]{Collapse Overview}
}
\examples{
## Simple Time-Series: AirPassengers
D(AirPassengers)                      # 1st difference, same as fdiff(AirPassengers)
D(AirPassengers,-1)                   # forward difference
Dlog(AirPassengers)                   # log-difference
D(AirPassengers,1,2)                  # second difference
Dlog(AirPassengers,1,2)               # second log-difference
D(AirPassengers,12)                   # seasonal difference (data is monthly)
D(AirPassengers,                      # quasi-difference, See a better example below
  rho = pwcor(AirPassengers, L(AirPassengers)))                   #

D(AirPassengers,-2:2,1:3)             # sequence of leaded/lagged and iterated differences

# let's do some visual analysis
plot(AirPassengers)                   # plot the series - seasonal pattern is evident
plot(stl(AirPassengers, "periodic"))  # Seasonal decomposition
plot(D(AirPassengers,c(1,12),1:2))    # plotting ordinary and seasonal first and second differences
plot(stl(window(D(AirPassengers,12),  # Taking seasonal differences removes most seasonal variation
                1950), "periodic"))


## Time-Series Matrix of 4 EU Stock Market Indicators, recorded 260 days per year
plot(D(EuStockMarkets, c(0, 260)))                      # Plot series and annual differnces
mod <- lm(DAX ~., L(EuStockMarkets, c(0, 260)))         # Regressing the DAX on its annual lag
summary(mod)                                            # and the levels and annual lags others
r <- residuals(mod)                                     # Obtain residuals
pwcor(r, L(r))                                          # Residual Autocorrelation
fFtest(r, L(r))                                         # F-test of residual autocorrelation
                                                        # (better use lmtest::bgtest)
modCO <- lm(QD1.DAX ~., D(L(EuStockMarkets, c(0, 260)), # Cochrane-Orcutt (1949) estimation
                        rho = pwcor(r, L(r))))
summary(modCO)
rCO <- residuals(modCO)
fFtest(rCO, L(rCO))                                     # No more autocorrelation

## World Development Panel Data
head(fdiff(num_vars(wlddev), 1, 1,                      # Computes differences of numeric variables
             wlddev$country, wlddev$year))              # fdiff requires external inputs...
head(D(wlddev, 1, 1, ~country, ~year))                  # Differences of numeric variables
head(D(wlddev, 1, 1, ~country))                         # Without t: Works because data is ordered
head(D(wlddev, 1, 1, PCGDP + LIFEEX ~ country, ~year))  # Difference of GDP & Life Expectancy
head(D(wlddev, 0:1, 1, ~ country, ~year, cols = 9:10))  # Same, also retaining original series
head(D(wlddev, 0:1, 1, ~ country, ~year, 9:10,          # Dropping id columns
       keep.ids = FALSE))

# Dynamic Panel-Data Models:
summary(lm(D(PCGDP,1,1,iso3c,year) ~                    # Diff. GDP regressed on it's lagged level
             L(PCGDP,1,iso3c,year) +                    # and the difference of Life Expanctancy
             D(LIFEEX,1,1,iso3c,year), data = wlddev))

g = qF(wlddev$country)                                  # Omitting t and precomputing g allows for
summary(lm(D(PCGDP,1,1,g) ~ L(PCGDP,1,g) +              # a bit more parsimonious specification
                            D(LIFEEX,1,1,g), wlddev))

summary(lm(D1.PCGDP ~.,                                 # Now adding level and lagged level of
L(D(wlddev,0:1,1, ~ country, ~year,9:10),0:1,           # LIFEEX and lagged differences rates
  ~ country, ~year, keep.ids = FALSE)[-1]))

## Using plm can make things easier, but avoid attaching or 'with' calls:
pwlddev <- plm::pdata.frame(wlddev, index = c("country","year"))
head(D(pwlddev, 0:1, 1, 9:10))                          # Again differences of LIFEEX and PCGDP
PCGDP <- pwlddev$PCGDP                                  # A panel-Series of GDP per Capita
D(PCGDP)                                                # Differencing the panel series.
summary(lm(D1.PCGDP ~.,                                 # Running the dynamic model again ->
           data = L(D(pwlddev,0:1,1,9:10),0:1,          # code becomes a bit simpler
                    keep.ids = FALSE)[-1]))

# One could be tempted to also do something like this, but THIS DOES NOT WORK!!!:
# lm drops the attributes (-> with(pwlddev, PCGDP) drops attr. so D.default and L.matrix are used)
summary(lm(D(PCGDP) ~ L(D(PCGDP,0:1)) + L(D(LIFEEX,0:1),0:1), pwlddev))

# To make it work, one needs to create pseries (note: attach(pwlddev) also won't work)
LIFEEX <- pwlddev$LIFEEX
summary(lm(D(PCGDP) ~ L(D(PCGDP,0:1)) + L(D(LIFEEX,0:1),0:1))) # THIS WORKS !!

## Using dplyr:
library(dplyr)
wlddev \%>\% group_by(country) \%>\%
             select(PCGDP,LIFEEX) \%>\% fdiff(0:1,1:2)       # Adding a first and second difference
wlddev \%>\% group_by(country) \%>\%
             select(year,PCGDP,LIFEEX) \%>\% D(0:1,1:2,year) # Also using t (safer)
wlddev \%>\% group_by(country) \%>\%                           # Ddropping id's
             select(year,PCGDP,LIFEEX) \%>\% D(0:1,1:2,year, keep.ids = FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip} % __ONLY ONE__ keyword per line % use one of  RShowDoc("KEYWORDS")
\keyword{ts}