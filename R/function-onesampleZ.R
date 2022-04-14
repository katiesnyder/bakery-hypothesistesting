#------------------------------------------------------------------
#     FUNCTION: One-Sample Z-Test
#------------------------------------------------------------------

# This function runs a one-proportion or one-sample z-test.
# A one-sample z-test is used to compare an observed
# population to a theoretical one, when there are only
# two categories.

# The function takes two required inputs and one optional input.
#       - Number of defects observed (whole number)
#       - Number of total observations (whole number)
#       - Expected proportion of defects *(optional)*
#           - If no value for expected proportion of defects
#             is provided, the default will be used (50%; a coin flip)


#if number of observations is greater than 30, run a prop text
#if number of observations is less than 30, run a binom test

#if a value has been entered for the expected proportion of defects, use that value for p
#if a value has not been entered, run the same test with the default value of 0.5 (50%)

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
