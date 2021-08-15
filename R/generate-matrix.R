#' Generate a forces matrix
#'
#' Create a forces matrix based on mathematical expressions.
#' Taking x and y, evaluating force z.
#'
#' @param exp Mat expression.
#' @param size Number of rows/columns to create.
#' @param scale Whether to apply scaling.
#'
#' @export
#'
#' @return  A size x size matrix, with values equal to evaluated expression
#' @examples
#' generate_matrix(2 * sin(x)^2 + sin(y)^2, 400)
generate_matrix <- function(exp, size = 80, scale = FALSE) {
    half_size <- size / 2
    exp <- rlang::enexpr(exp)

    series_up <- scale(-half_size:(half_size - 1))
    series_down <- scale((half_size - 1):(-half_size))

    matrix <- cross2(series_up, series_down) %>%
        map_dbl(~ {
            x <- .x[[1]]
            y <- .x[[2]]
            z <- rlang::eval_bare(exp)
            return(z)
        }) %>%
        matrix(nrow = size)

    if (scale) {
        matrix <- (matrix - mean(matrix)) / diff(range(matrix))
    }
    return(matrix)
}