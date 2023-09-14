#'
#' @param scans String
#' @param solutions String
#' @param partial Boolean
#'
#'
#' @export
#'
#'
evaluate <-
  function(scans,
           solutions,
           partial = TRUE,
           negative = FALSE,
           rule = "false",
           ...) {
    # scoring rules (from exams_eval()):
    #
    # Jede richtige Antwort gibt 1/ncorrect Punkte
    #
    # "false" = -1/sum(!type$correct),   1/nfalsch Punkte Abzug
    # "false2" = -1/pmax(sum(!type$correct), 2), wie oben, aber mind. wie 2 falsche AW
    # "true" = -1/sum(type$correct),
    # "all" = -1,
    # "none" = 0)

    if (rule != "simple") {
      eval_fun <-
        exams_eval(partial = partial,
                   negative = negative,
                   rule = rule)
    } else {
      eval_fun <- eval_simple()
    }

    exams::nops_eval(
      register = NULL,
      solutions = solutions,
      scans = scans,
      eval = eval_fun
    )
  }
