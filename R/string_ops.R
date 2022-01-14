#' Parse text input to numeric vector
#'
#' Generic to parse text into numeric vectors. This was initially designed
#' for a robust interpretation of text input in teal modules.
#'
#' @param str `vector` to extract numeric from.
#' @details The function is intended to extract any numeric from a character
#'   string, factor levels, boolean and return a vector of numeric.
#'
#' @md
#'
#' @return As vector of numeric if directly parsed from `numeric` or boolean.
#'   A list of numeric if parsed from a character string, each character string
#'   associated with an list item.
#'
#' @examples
#' dta <- list(
#'   character = c("text10,20.5letter30.!", "!-.40$$-50e5[", NA),
#'   factor    = factor(c("]+60e-6, 7.7%%8L", "%90sep.100\"1L", NA_character_)),
#'   numeric   = c(1, -5e+2, NA),
#'   logical   = c(TRUE, FALSE, NA)
#' )
#' lapply(dta, teal.modules.clinical:::as_num)
#' @keywords internal
as_num <- function(str) { # nolint # nousage
  UseMethod("as_num")
}

#' @export
#' @rdname as_num
as_num.default <- function(str) { # nolint # nousage
  stop("No default implementation for `as_num.default`.")
}

#' @export
#' @rdname as_num
as_num.character <- function(str) { # nolint # nousage

  y <- regmatches(
    x = str,
    m = gregexpr(
      "[-+]?(\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?",
      str,
      perl = TRUE
    )
  )

  y <- lapply(y, as.numeric)
  y[unlist(lapply(y, length) == 0)] <- NA

  return(y)
}

#' @export
#' @rdname as_num
as_num.numeric <- function(str) { # nolint # nousage
  return(str)
}

#' @export
#' @rdname as_num
as_num.factor <- function(str) { # nolint # nousage
  y <- as.character(str)
  y <- as_num(y)
  return(y)
}

#' @export
#' @rdname as_num
as_num.logical <- function(str) { # nolint # nousage
  y <- as.numeric(str)
  return(y)
}
