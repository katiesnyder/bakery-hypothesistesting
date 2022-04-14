\name{oneSampleZTest}
\alias{oneSampleZTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{One Sample Z Test
%%  ~~function to do ... ~~
}
\description{
This function runs a one-proportion or one-sample z-test.

A one-sample z-test is used to compare an observed population to a theoretical one, when there are only two categories.

}
\usage{
oneSampleZTest(numDefect, numObserv, expDef = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{numDefect}{
positive whole number; mumber of defects observed
}
  \item{numObserv}{
positive whole number; mumber of total observations
}
  \item{expDef}{
decimal; expected proportion of defects. Default is .50; random chance with two equally weighted options (i.e., a coin flip). Technically optional but highly recommended a value be specified.
}
}
\details{
A value for expDef is highly recommended if the expected percentage of defects is different than 50 percent (random chance with two equally weighted options).

The function is already equipped to handle tests where the sample size is small. If the number of observations is less than 30, the function will run a binom.test. If the number of observations is larger than 30, it will run a prop.test.
}
\value{
Function returns outputs of a one-sample proportions (z) test.

1-sample proportions test with continuity correction

data:  numDefect out of numObserv, null probability expDef

X-squared = 900.6, df = 1, p-value < 2.2e-16

alternative hypothesis: true p is not equal to 0.5

95 percent confidence interval:
 0.01658466 0.03723503

sample estimates:
    p
0.025

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Katie Snyder, Data Science Architect @ Wegmans; katie.snyder@wegmans.com
}
\note{
Note that, by default, the function prop.test() used the Yates continuity correction, which is really important if either the expected successes or failures is < 5.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# If you are supplying an expected proportion of defects and are using inputs defined as written in input section:
oneSampleZTest(number.of.defects, number.of.observations, expDef = expected.proportion.of.defects)

# If you are NOT supplying an expected proportion of defects and are using inputs defined as written in input section:
# and wish to use the default of 0.5
oneSampleZTest(number.of.defects, number.of.observations)

# If you wish to supply differently defined inputs, updates can be made here.
oneSampleZTest(numDefect = 25,                #number.of.defects
                numObserv = 1000,             #number.of.observations
                expDef = 0.1)                 #expected.proportion.of.defects



## The function is currently defined as
oneSampleZTest = function(numDefect = number.of.defects, numObserv = number.of.observations, expDef = 0.5){

  if (numObserv > 30) {

    prop.test(x = numDefect,
              n = numObserv,
              p = expDef
    )

  } else {

    binom.test(x = numDefect,
               n = numObserv,
               p = expDef
    )

  }
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
