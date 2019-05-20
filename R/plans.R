

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
                                    trigger = trigger(mode = "blacklist", condition = FALSE)),
    tax_reason_prep_status = target(command = prepare_tax_reason(path = file_out("extdata/source/tax_reason.txt")),
                                    trigger = trigger(mode = "blacklist", condition = FALSE)),
    prop_type_prep_status = target(command = prepare_prop_type(path = file_out("extdata/source/prop_type.txt")),
                                   trigger = trigger(mode = "blacklist", condition = FALSE)),
    present_use_recode_prep_status = target(command = prepare_present_use_recode(path = file_out("extdata/source/present_use_recode.csv")),
                                            trigger = trigger(mode = "blacklist", condition = FALSE))
  )

  upload_plan <- drake_plan(
    has_osf_access = osf_check_write_access(osf_id = "pvu6f"),
    tax_status_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                           project_id = "pvu6f",
                                                           file_id = "qyb3h",
                                                           path = file_in("extdata/source/tax_status.txt"),
                                                           osf_dirpath = "data/raw-data"),
                                      trigger = trigger(mode = "blacklist", condition = FALSE)),
    tax_reason_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                           project_id = "pvu6f",
                                                           file_id = "5ma4t",
                                                           path = file_in("extdata/source/tax_reason.txt"),
                                                           osf_dirpath = "data/raw-data"),
                                      trigger = trigger(mode = "blacklist", condition = FALSE)),
    prop_type_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                          project_id = "pvu6f",
                                                          file_id = "26r89",
                                                          path = file_in("extdata/source/prop_type.txt"),
                                                          osf_dirpath = "data/raw-data"),
                                     trigger = trigger(mode = "blacklist", condition = FALSE)),
    present_use_recode_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                   project_id = "pvu6f",
                                                                   file_id = "4rjk6",
                                                                   path = file_in("extdata/source/present_use_recode.csv"),
                                                                   osf_dirpath = "data/raw-data"),
                                              trigger = trigger(mode = "blacklist", condition = FALSE))
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
                                 trigger = trigger(change = osf_get_file_version(osf_id = "qyb3h"))),
    tax_reason_filepath = target(command = osf_download_file(osf_id = "5ma4t", path = file_out("extdata/osf/tax_reason.txt")),
                                 trigger = trigger(change = osf_get_file_version(osf_id = "5ma4t"))),
    parcel_metadata_table_filepath = target(command = osf_download_file(osf_id = "he2kf", path = file_out("extdata/osf/KC_Parcel_Metadata_Table.csv")),
                                            trigger = trigger(change = osf_get_file_version(osf_id = "he2kf"))),
    prop_type_filepath = target(command = osf_download_file(osf_id = "26r89", path = file_out("extdata/osf/prop_type.txt")),
                                trigger = trigger(change = osf_get_file_version(osf_id = "26r89"))),
    present_use_recode_filepath = target(command = osf_download_file(osf_id = "4rjk6", path = file_out("extdata/osf/present_use_recode.csv")),
                                         trigger = trigger(change = osf_get_file_version(osf_id = "4rjk6")))
  )

  ready_plan <- drake::drake_plan(
    lookup = make_lookup(path = file_in("extdata/osf/EXTR_LookUp.csv")),
    tax_status = make_tax_status(path = file_in("extdata/osf/tax_status.txt")),
    tax_reason = make_tax_reason(path = file_in("extdata/osf/tax_reason.txt")),
    parcel_metadata_table = make_parcel_metadata_table(path = file_in("extdata/osf/KC_Parcel_Metadata_Table.csv")),
    prop_type = make_prop_type(path = file_in("extdata/osf/prop_type.txt")),
    present_use_recode = make_present_use_recode(path = file_in("extdata/osf/present_use_recode.csv"))
  )

  data_cache_plan <- drake::bind_plans(download_plan, ready_plan)

  return(data_cache_plan)


}



