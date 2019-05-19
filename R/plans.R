

# WORKFLOW PLAN -----------------------------------------------------------

#' @title Get the Workflow Plan
#' @description The drake \code{\link[drake:drake_plan]{plan}}s for creating
#'   the project's workflow.
#' @return a \code{\link[drake:drake_plan]{plan} tibble}

#' @rdname workflow_plan
#' @export
get_workflow_plan <- function(){

  options(drake_make_menu = FALSE)

  workflow_plan <- drake::bind_plans(
    get_data_source_plan(),  # this only needs to be included when a data source is added or changed
    get_data_cache_plan()
  )

  return(workflow_plan)
}


# DATA PLANS --------------------------------------------------------------

#' @title Get the Data Plans
#' @description Drake workflow \code{\link[drake:drake_plan]{plan}}s for creating
#'   the project's data objects.
#' @return a \code{\link[drake:drake_plan]{plan} tibble}

#' @rdname data_plan
#' @export
get_data_source_plan <- function(){

  options(drake_make_menu = FALSE)

  prep_plan <- drake_plan(
    tax_status_prep_status = target(command = prepare_tax_status(path = file_out("extdata/source/tax_status.txt")),
                                    trigger = trigger(mode = "blacklist", condition = TRUE))
  )

  upload_plan <- drake_plan(
    has_osf_access = osf_check_write_access(osf_id = "pvu6f"),
    tax_status_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                         project_id = "pvu6f",
                                                         file_id = "qyb3h",
                                                         path = file_in("extdata/source/tax_status.txt"),
                                                         osf_dirpath = "data/raw-data"),
                                    trigger = trigger(mode = "blacklist", condition = TRUE))
  )

  data_source_plan <- drake::bind_plans(prep_plan, upload_plan)

  return(data_source_plan)

}

#' @rdname data_plan
#' @export
get_data_cache_plan <- function(){

  options(drake_make_menu = FALSE)

  download_plan <- drake::drake_plan(
    lookup_filepath = target(command = osf_download_file(osf_id = "jmuzh", path = file_out("extdata/osf/EXTR_LookUp.csv")),
                             trigger = trigger(change = osf_get_file_version(osf_id = "jmuzh"))),
    tax_status_filepath = target(command = osf_download_file(osf_id = "qyb3h", path = file_out("extdata/osf/tax_status.txt")),
                             trigger = trigger(change = osf_get_file_version(osf_id = "qyb3h")))
  )

  ready_plan <- drake::drake_plan(
    lookup = make_lookup(path = file_in("extdata/osf/EXTR_LookUp.csv")),
    tax_status = make_tax_status(path = file_in("extdata/osf/tax_status.txt"))
  )

  data_cache_plan <- drake::bind_plans(download_plan, ready_plan)

  return(data_cache_plan)


}



