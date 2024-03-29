% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting-formats.R
\name{gitbook_clinDataReview_report}
\alias{gitbook_clinDataReview_report}
\title{Clinical data format for bookdown report.}
\usage{
gitbook_clinDataReview_report(
  logo = NULL,
  logoHeight = "60px",
  split_by = "section+number",
  config = list(sharing = NULL, toc = list(collapse = "section")),
  extra_dependencies = NULL,
  css = NULL,
  ...
)
}
\arguments{
\item{logo}{String, path to the logo. No logo is printed by default.}

\item{logoHeight}{String, indicating the logo height; 60px height by default.}

\item{split_by}{String, how the reports should be split,
(see help of the \link[bookdown]{gitbook} function)}

\item{config}{List with config parameters,
by default: no sharing and collapsed by section.
(see help of the \link[bookdown]{gitbook} function)}

\item{extra_dependencies}{NULL by default}

\item{css}{String, path to the css.}

\item{...}{Extra parameters passed to the
\link[bookdown]{gitbook} function.}
}
\value{
R Markdown output format to pass to \code{\link[bookdown]{render_book}}.
}
\description{
This function is only meant to set sensitive
defaults for gitbook.\cr
\code{\link[bookdown]{gitbook}} can be used instead.
}
\seealso{
Other clinical data reporting: 
\code{\link{checkReportTitles}()},
\code{\link{forceParams}()},
\code{\link{getMdHeader}()},
\code{\link{getParamsFromConfig}()},
\code{\link{html_clinDataReview_report}()},
\code{\link{knitPrintClinDataReview}()},
\code{\link{postProcessReport}()},
\code{\link{render_clinDataReviewReport}()}
}
\author{
Laure Cougnaud
}
\concept{clinical data reporting}
