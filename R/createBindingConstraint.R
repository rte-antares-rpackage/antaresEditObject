#' Create a Binding Constraint
#'
#' @param name The name for the binding constraint
#' @param id An id
#' @param values Values used by the constraint.
#'  It contains one line per time step and three columns "less", "greater" and "equal".
#' @param enabled Logical, is the constraint enabled ?
#' @param timeStep Time step the constraint applies to : \code{hourly}, \code{daily} or \code{weekly}
#' @param operator Type of constraint: equality, inequality on one side or both sides
#' @param coefficients Elements containing the coefficients used by the constraint
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath} 
#'
#' @return An upddated list containing various information about the simulation.
#' @export
#'
#' @examples
#' # createBindingConstraint()
createBindingConstraint <- function(name, id = tolower(name),
                                    values = NULL,
                                    enabled = TRUE,
                                    timeStep = c("hourly", "daily", "weekly"),
                                    operator = c("both", "equal"),
                                    coefficients = NULL,
                                    opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  timeStep <- match.arg(arg = timeStep)
  operator <- match.arg(arg = operator)
  
  
  if (!is.null(values)) {
    if (ncol(values) != 3 & is.null(colnames(values))) 
      stop("'values' must have 3 columns or must be named")
    
    if (!is.null(colnames(values))) {
      if (any(c("greater", "less", "equal") %in% colnames(values))) {
        stop("'value' must have at least one colum named 'greater' or 'less' or 'equal")
      }
      
      var_to_add <- c("greater", "less", "equal")[!c("greater", "less", "equal") %in% colnames(values)]
      names(var_to_add) <- var_to_add
      
      values <- do.call("cbind", c(list(values), lapply(var_to_add, function(x) 0)))
      values <- values[, c("greater", "less", "equal")]
    }
  } else {
    values <- character(0)
  }
  
  
  path <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(path, stringsAsFactors = FALSE)
  
  
  #
  
}

# 
# x <- matrix(1:6, ncol = 2)
# colnames(x) <- c("upper", "lower")
# x
# 
# xx <- character(0)
# names(xx) <- xx
# lapply(xx, function(x) 0)
# 
# do.call("cbind", c(list(x), lapply(xx, function(x) 0)))
# 

