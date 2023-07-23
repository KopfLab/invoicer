# users =====

authenticate_user <- function(users, user_id) {
  user <- users |>
    dplyr::filter(user_id == !!user_id)
  if(nrow(user) < 1L) abort(paste0("user does not exist: ", user_id))
  if(nrow(user) > 1L) abort(paste0("user_id is not unique: ", user_id))
  user <- user |> as.list()
  return(user)
}
