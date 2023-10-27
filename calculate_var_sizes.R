
calculate_object_size <- function(x) {
  message(glue("Calculating size for object {x}"))

  size <- x |>
    get() |>
    obj_size()

  return(size)
}


var_sizes_tbl <- ls() |>
  enframe(name = NULL, value = "object_name") |>
  mutate(
    object_size = map_dbl(
      object_name, calculate_object_size,
      .progress = FALSE
      ),
    object_size_mb = object_size / (1024 * 1024)
    )

var_sizes_tbl |> glimpse()
