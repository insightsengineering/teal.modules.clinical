# nocov start
roxygen_decorators_param <- function(module_name) {
  paste(
    sep = " ",
    lifecycle::badge("experimental"),
    " (`list` of `teal_transform_module`, named `list` of `teal_transform_module` or",
    "`NULL`) optional, if not `NULL`, decorator for tables or plots included in the module.",
    "When a named list of `teal_transform_module`, the decorators are applied to the",
    "respective output objects.\n\n",
    "Otherwise, the decorators are applied to all objects, which is equivalent as using the name `default`.\n\n",
    sprintf("See section \"Decorating `%s`\"", module_name),
    "below for more details."
  )
}
# nocov start
