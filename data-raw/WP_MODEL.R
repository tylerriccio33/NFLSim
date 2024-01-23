## code to prepare `WP_MODEL` dataset goes here

butched_wp_model <- butcher::butcher(fastrmodels::wp_model)
GET_PREDS_WP_PARTIAL <- purrr::partial(fget_preds_wp, model = !!butched_wp_model)

usethis::use_data(GET_PREDS_WP_PARTIAL, overwrite = TRUE)
