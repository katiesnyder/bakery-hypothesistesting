\name{chiSquareTest}
\alias{chiSquareTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-Square Test
%%  ~~function to do ... ~~
}
\description{
This function runs a two category chi-squared test of independence.

Chi-squared tests are used to analyze a frequency table formed by two categorical variables. It evaluates whether there is a significant association between the categories of the two variables.

}
\usage{
chiSquareTest(catA, catAPop, catB, catBPop)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{catA}{
positive whole number; mumber of defects observed in category A. Default is defined variable category.a.defect
}

  \item{catAPop}{
positive whole number; total population of category A, default is defined variable category.a.population
}

  \item{catB}{
positive whole number;total population of category B, default is defined variable category.b.defect
}

  \item{catBPop}{
positive whole number; Number of defects observed in category A, default is defined variable category.b.population
}
}
\details{
Note all arguments above need to be positive whole numbers (not decimals). If data is initiatlly provided as a decimal or percentage (i.e. 2.5 percent defects out of 1000), please convert to whole numbers before inputting to function (i.e. 25 defects out of 1000).
}
\value{
Function will return a data frame of data provided, and output of Peason's Chi-Squared test.

	Pearson's Chi-squared test with Yates' continuity correction

data:  chiInput;
X-squared = 25.274, df = 1, p-value = 4.974e-07

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Katie Snyder, Data Science Architect @ Wegmans; katie.snyder@wegmans.com
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## If all inputs have been defined as written in input section, no arguments are required.
chiSquareTest()

## If you wish to supply differently defined inputs, updates can be made here.
chiSquareTest(catA = 25,           #category.a.defect
              catAPop = 1000,      #category.a.population
              catB = 75,           #category.b.defect
              catBPop = 1000)      #category.b.population


## The function is currently defined as
chiSquareTest = function(catA = category.a.defect, catAPop = category.a.population, catB = category.b.defect, catBPop = category.b.population){

  chiObserved =
    bind_cols(
      bind_cols(defectState = c("Defect", "Not Defect", "Total")),
      bind_cols(categoryA = c(catA, (catAPop - catA), catAPop)),
      bind_cols(categoryB = c(catB, (catBPop - catB), catBPop))  ) %>%
    mutate(total = categoryA + categoryB)

  print("Input for chi-square test: ")
  print(as_tibble(chiObserved))

  chiInput =
    chiObserved %>%
    filter(defectState != "Total") %>%
    select(categoryA, categoryB)

  chisq.test(chiInput)

}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
