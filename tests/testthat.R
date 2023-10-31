#Copyright © 2016 RTE Réseau de transport d’électricité

  # CRAN limite CPU usage
data.table::setDTthreads(2)

library(testthat)
library(antaresEditObject)

test_check("antaresEditObject")
