
#------------------------------------------------------------------
#     FUNCTION: Chi Square Test
#------------------------------------------------------------------

# This function runs a two category chi-squared test of independence.
# Chi-squared tests are used to analyze a frequency table formed by two
# categorical variables. It evaluates whether there is a significant
# association between the categories of the two variables.

# This function takes four required inputs:
#       - Number of defects observed in category A
#       - Total population of category A
#       - Number of defects observed in category B
#       - Total population in category B

# All function inputs are only positive whole numbers, NOT percentages or proportions.




chiSquareTest = function(catA = category.a.defect, catAPop = category.a.population, catB = category.b.defect, catBPop = category.b.population){


  #load required packages
  packages = c("dplyr", "tibble", "magrittr")


  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  )


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
