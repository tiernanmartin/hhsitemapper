

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
    get_data_cache_plan(),
    get_parcel_plan(),
    get_building_plan(),
    get_development_assumptions_plan(),
    get_owner_plan(),
    get_suitability_criteria_plan(),
    get_suitability_plan(),
    get_utilization_criteria_plan(),
    get_utilization_plan(),
    get_official_names_plan(),
    get_filter_plan()
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

  options(drake_make_menu = FALSE,
          tigris_class = 'sf')

  prep_plan <- drake_plan(
    tax_status_prep_status = target(command = prepare_tax_status(path = file_out("extdata/source/tax_status.txt")),
                                    trigger = trigger(mode = "blacklist", condition = FALSE)),
    tax_reason_prep_status = target(command = prepare_tax_reason(path = file_out("extdata/source/tax_reason.txt")),
                                    trigger = trigger(mode = "blacklist", condition = FALSE)),
    prop_type_prep_status = target(command = prepare_prop_type(path = file_out("extdata/source/prop_type.txt")),
                                   trigger = trigger(mode = "blacklist", condition = FALSE)),
    present_use_recode_prep_status = target(command = prepare_present_use_recode(path = file_out("extdata/source/present_use_recode.csv")),
                                            trigger = trigger(mode = "blacklist", condition = FALSE)),
    name_recode_key_prep_status = target(command = prepare_name_recode_key(path = file_out("extdata/source/name_recode_key.rda")),
                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    public_owner_name_category_key_prep_status = target(command = prepare_public_owner_name_category_key(path = file_out("extdata/source/public_owner_name_category_key.rda")),
                                                        trigger = trigger(mode = "blacklist", condition = FALSE)),
    other_exempt_owner_name_category_key_prep_status = target(command = prepare_other_exempt_owner_name_category_key(path = file_out("extdata/source/other_exempt_owner_name_category_key.csv")),
                                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    owner_antijoin_names_prep_status = target(command = prepare_owner_antijoin_names(path = file_out("extdata/source/owner_antijoin_names.csv")),
                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    parcel_addr_prep_status = target(command = prepare_parcel_addr(path = file_out("extdata/source/parcel_addr.csv")),
                                     trigger = trigger(mode = "blacklist", condition = FALSE)),
    king_county_prep_status = target(command = prepare_king_county(path = file_out("extdata/source/king_county.gpkg")),
                                     trigger = trigger(mode = "blacklist", condition = FALSE)),
    wa_major_waterbodies_prep_status = target(command = prepare_wa_major_waterbodies(path = file_out("extdata/source/ECY_WAT_NHDWAMajor.zip")),
                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    uga_prep_status = target(command = prepare_uga(path = file_out("extdata/source/urban_growth_SHP.zip")),
                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    zoning_prep_status = target(command = prepare_zoning(path = file_out("extdata/source/zoning_kc_consol_20_SHP.zip")),
                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    kc_city_prep_status = target(command = prepare_kc_city(path = file_out("extdata/source/city_kc_SHP.zip")),
                                 trigger = trigger(mode = "blacklist", condition = FALSE)),
    census_tracts_prep_status = target(command = prepare_census_tracts(path = file_out("extdata/source/census_tracts.gpkg")),
                                       trigger = trigger(mode = "blacklist", condition = FALSE)),
    zcta_prep_status = target(command = prepare_zcta(path = file_out("extdata/source/zcta.gpkg")),
                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    census_place_prep_status = target(command = prepare_census_place(path = file_out("extdata/source/census_place.gpkg")),
                                      trigger = trigger(mode = "blacklist", condition = FALSE)),
    school_districts_prep_status = target(command = prepare_school_districts(path = file_out("extdata/source/schdst_SHP.zip")),
                                          trigger = trigger(mode = "blacklist", condition = FALSE)),
    leg_districts_prep_status = target(command = prepare_leg_districts(path = file_out("extdata/source/legdst_SHP.zip")),
                                       trigger = trigger(mode = "blacklist", condition = FALSE)),
    kc_council_districts_prep_status = target(command = prepare_kc_council_districts(path = file_out("extdata/source/kccdst_SHP.zip")),
                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    seattle_council_districts_prep_status = target(command = prepare_seattle_council_districts(path = file_out("extdata/source/sccdst_SHP.zip")),
                                                   trigger = trigger(mode = "blacklist", condition = FALSE)),
    bus_stops_metro_prep_status = target(command = prepare_bus_stops_metro(path = file_out("extdata/source/transitstop_SHP.zip")),
                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    transit_stops_osm_prep_status = target(command = prepare_transit_stops_osm(path = file_out("extdata/source/transit_stops_osm.gpkg")),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    play_spaces_osm_prep_status = target(command = prepare_play_spaces_osm(path = file_out("extdata/source/play_spaces_osm.gpkg")),
                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    mj_businesses_prep_status = target(command = prepare_mj_businesses(path = file_out("extdata/source/MarijuanaApplicants.xls")),
                                       trigger = trigger(mode = "blacklist", condition = TRUE)),
    other_suitability_characteristics_prep_status = target(command = prepare_other_suitability_characteristics(path = file_out("extdata/source/other_suitability_characteristics.rda")),
                                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    seattle_dev_cap_prep_status = target(command = prepare_seattle_dev_cap(path = file_out("extdata/source/seattle_dev_cap.csv")),
                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    future_lightrail_prep_status = target(command = prepare_future_lightrail(path = file_out("extdata/source/future_lightrail.gpkg")),
                                          trigger = trigger(mode = "blacklist", condition = FALSE)),
    brownfield_sites_prep_status = target(command = prepare_brownfield_sites(path = file_out("extdata/source/brownfield_sites.csv")),
                                          trigger = trigger(mode = "blacklist", condition = FALSE)),
    contaminated_sites_prep_status = target(command = prepare_contaminated_sites(path = file_out("extdata/source/contaminated_sites.csv")),
                                            trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_seattle_prep_status = target(command = prepare_official_names_seattle(path = file_out("extdata/source/official_names_seattle.csv")),
                                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_kc_prep_status = target(command = prepare_official_names_kc(path = file_out("extdata/source/official_names_kc.csv")),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_wa_prep_status = target(command = prepare_official_names_wa(path = file_out("extdata/source/official_names_wa.csv")),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_us_prep_status = target(command = prepare_official_names_us(path = file_out("extdata/source/official_names_us.csv")),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_places_prep_status = target(command = prepare_official_names_places(path = file_out("extdata/source/official_names_places.csv")),
                                               trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_tribes_prep_status = target(command = prepare_official_names_tribes(path = file_out("extdata/source/official_names_tribes.csv")),
                                               trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_housing_authorities_prep_status = target(command = prepare_official_names_housing_authorities(path = file_out("extdata/source/official_names_housing_authorities.csv")),
                                                            trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_special_purpose_districts_prep_status = target(command = prepare_official_names_special_purpose_districts(path = file_out("extdata/source/official_names_special_purpose_districts.csv")),
                                                                  trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_higher_ed_providers_prep_status = target(command = prepare_official_names_higher_ed_providers(path = file_out("extdata/source/official_names_higher_ed_providers.csv")),
                                                            trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_hospitals_prep_status = target(command = prepare_official_names_hospitals(path = file_out("extdata/source/official_names_hospitals.csv")),
                                                  trigger = trigger(mode = "blacklist", condition = FALSE)),
    development_assumptions_zoning_prep_status = target(command = prepare_development_assumptions_zoning(path = file_out("extdata/source/development_assumptions_zoning.csv")),
                                                        trigger = trigger(mode = "blacklist", condition = FALSE)),
    nmtc_prep_status = target(command = prepare_nmtc(path = file_out("extdata/source/NMTC-2011-2015-LIC-Nov2-2017-4pm.xlsx")),
                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    dda_prep_status = target(command = prepare_dda(path = file_out("extdata/source/DDA2019M.PDF")),
                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    qct_prep_status = target(command = prepare_qct(path = file_out("extdata/source/QCT2019dbf.zip")),
                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    oz_prep_status = target(command = prepare_oz(path = file_out("extdata/source/Designated_QOZs_12-14-18.xlsx")),
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
                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    name_recode_key_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                project_id = "pvu6f",
                                                                file_id = "4azv5",
                                                                path = file_in("extdata/source/name_recode_key.rda"),
                                                                osf_dirpath = "data/raw-data"),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    public_owner_name_category_key_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                               project_id = "pvu6f",
                                                                               file_id = "8az7h",
                                                                               path = file_in("extdata/source/public_owner_name_category_key.rda"),
                                                                               osf_dirpath = "data/raw-data"),
                                                          trigger = trigger(mode = "blacklist", condition = FALSE)),
    other_exempt_owner_name_category_key_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                                     project_id = "pvu6f",
                                                                                     file_id = "2k3dp",
                                                                                     path = file_in("extdata/source/other_exempt_owner_name_category_key.csv"),
                                                                                     osf_dirpath = "data/raw-data"),
                                                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    owner_antijoin_names_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                     project_id = "pvu6f",
                                                                     file_id =  "t7zpe",
                                                                     path = file_in("extdata/source/owner_antijoin_names.csv"),
                                                                     osf_dirpath = "data/raw-data"),
                                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    parcel_addr_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                            project_id = "pvu6f",
                                                            file_id =  "x9zwq",
                                                            path = file_in("extdata/source/parcel_addr.csv"),
                                                            osf_dirpath = "data/raw-data/king-county-assessor-data"),
                                       trigger = trigger(mode = "blacklist", condition = FALSE)),
    king_county_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                            project_id = "pvu6f",
                                                            file_id =  "jqsym",
                                                            path = file_in("extdata/source/king_county.gpkg"),
                                                            osf_dirpath = "data/raw-data/"),
                                       trigger = trigger(mode = "blacklist", condition = FALSE)),
    wa_major_waterbodies_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                     project_id = "pvu6f",
                                                                     file_id =  "p4yg5",
                                                                     path = file_in("extdata/source/ECY_WAT_NHDWAMajor.zip"),
                                                                     osf_dirpath = "data/raw-data/"),
                                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    uga_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                    project_id = "pvu6f",
                                                    file_id =  "vnsfw",
                                                    path = file_in("extdata/source/urban_growth_SHP.zip"),
                                                    osf_dirpath = "data/raw-data/"),
                               trigger = trigger(mode = "blacklist", condition = FALSE)),
    zoning_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                       project_id = "pvu6f",
                                                       file_id =  "5hjxa",
                                                       path = file_in("extdata/source/zoning_kc_consol_20_SHP.zip"),
                                                       osf_dirpath = "data/raw-data/"),
                                  trigger = trigger(mode = "blacklist", condition = FALSE)),
    kc_city_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                        project_id = "pvu6f",
                                                        file_id =  "8zfd4",
                                                        path = file_in("extdata/source/city_kc_SHP.zip"),
                                                        osf_dirpath = "data/raw-data/"),
                                   trigger = trigger(mode = "blacklist", condition = FALSE)),
    census_tracts_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                              project_id = "pvu6f",
                                                              file_id =  "tq4b9",
                                                              path = file_in("extdata/source/census_tracts.gpkg"),
                                                              osf_dirpath = "data/raw-data/"),
                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    zcta_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                     project_id = "pvu6f",
                                                     file_id =  "n5rhf",
                                                     path = file_in("extdata/source/zcta.gpkg"),
                                                     osf_dirpath = "data/raw-data/"),
                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    census_place_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                             project_id = "pvu6f",
                                                             file_id =  "w69pb",
                                                             path = file_in("extdata/source/census_place.gpkg"),
                                                             osf_dirpath = "data/raw-data/"),
                                        trigger = trigger(mode = "blacklist", condition = FALSE)),
    school_districts_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                 project_id = "pvu6f",
                                                                 file_id =  "8svty",
                                                                 path = file_in("extdata/source/schdst_SHP.zip"),
                                                                 osf_dirpath = "data/raw-data/"),
                                            trigger = trigger(mode = "blacklist", condition = FALSE)),
    leg_districts_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                              project_id = "pvu6f",
                                                              file_id =  "v4xk7",
                                                              path = file_in("extdata/source/legdst_SHP.zip"),
                                                              osf_dirpath = "data/raw-data/"),
                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    kc_council_districts_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                     project_id = "pvu6f",
                                                                     file_id =  "82c7m",
                                                                     path = file_in("extdata/source/kccdst_SHP.zip"),
                                                                     osf_dirpath = "data/raw-data/"),
                                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    seattle_council_districts_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                          project_id = "pvu6f",
                                                                          file_id =  "cj27h",
                                                                          path = file_in("extdata/source/sccdst_SHP.zip"),
                                                                          osf_dirpath = "data/raw-data/"),
                                                     trigger = trigger(mode = "blacklist", condition = FALSE)),
    bus_stops_metro_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                project_id = "pvu6f",
                                                                file_id =  "w9ap3",
                                                                path = file_in("extdata/source/transitstop_SHP.zip"),
                                                                osf_dirpath = "data/raw-data/"),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    transit_stops_osm_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                  project_id = "pvu6f",
                                                                  file_id =  "zk826",
                                                                  path = file_in("extdata/source/transit_stops_osm.gpkg"),
                                                                  osf_dirpath = "data/raw-data/"),
                                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    play_spaces_osm_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                project_id = "pvu6f",
                                                                file_id =  "4prez",
                                                                path = file_in("extdata/source/play_spaces_osm.gpkg"),
                                                                osf_dirpath = "data/raw-data/"),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    mj_businesses_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                              project_id = "pvu6f",
                                                              file_id =  "3m5eh",
                                                              path = file_in("extdata/source/MarijuanaApplicants.xls"),
                                                              osf_dirpath = "data/raw-data/"),
                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    other_suitability_characteristics_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                                  project_id = "pvu6f",
                                                                                  file_id = "wub8k",
                                                                                  path = file_in("extdata/source/other_suitability_characteristics.rda"),
                                                                                  osf_dirpath = "data/raw-data"),
                                                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    seattle_dev_cap_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                project_id = "pvu6f",
                                                                file_id = "vby7h",
                                                                path = file_in("extdata/source/seattle_dev_cap.csv"),
                                                                osf_dirpath = "data/raw-data"),
                                           trigger = trigger(mode = "blacklist", condition = FALSE)),
    affordable_housing_properties_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                              project_id = "pvu6f",
                                                                              file_id = "2e9sb",
                                                                              path = file_in("extdata/source/NHPD Properties Only Export.xlsx"),
                                                                              osf_dirpath = "data/raw-data"),
                                                         trigger = trigger(mode = "blacklist", condition = FALSE)),
    future_lightrail_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                 project_id = "pvu6f",
                                                                 file_id = "zkphb",
                                                                 path = file_in("extdata/source/future_lightrail.gpkg"),
                                                                 osf_dirpath = "data/raw-data"),
                                            trigger = trigger(mode = "blacklist", condition = FALSE)),
    brownfield_sites_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                 project_id = "pvu6f",
                                                                 file_id = "8dpeq",
                                                                 path = file_in("extdata/source/brownfield_sites.csv"),
                                                                 osf_dirpath = "data/raw-data"),
                                            trigger = trigger(mode = "blacklist", condition = FALSE)),
    contaminated_sites_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                   project_id = "pvu6f",
                                                                   file_id = "a5mgq",
                                                                   path = file_in("extdata/source/contaminated_sites.csv"),
                                                                   osf_dirpath = "data/raw-data"),
                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_seattle_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                       project_id = "pvu6f",
                                                                       file_id = "3ygke",
                                                                       path = file_in("extdata/source/official_names_seattle.csv"),
                                                                       osf_dirpath = "data/raw-data/official-names"),
                                                  trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_kc_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                  project_id = "pvu6f",
                                                                  file_id = "psv3b",
                                                                  path = file_in("extdata/source/official_names_kc.csv"),
                                                                  osf_dirpath = "data/raw-data/official-names"),
                                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_wa_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                  project_id = "pvu6f",
                                                                  file_id = "c47ka",
                                                                  path = file_in("extdata/source/official_names_wa.csv"),
                                                                  osf_dirpath = "data/raw-data/official-names"),
                                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_us_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                  project_id = "pvu6f",
                                                                  file_id = "un7zk",
                                                                  path = file_in("extdata/source/official_names_us.csv"),
                                                                  osf_dirpath = "data/raw-data/official-names"),
                                             trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_places_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                      project_id = "pvu6f",
                                                                      file_id = "ntjdx",
                                                                      path = file_in("extdata/source/official_names_places.csv"),
                                                                      osf_dirpath = "data/raw-data/official-names"),
                                                 trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_tribes_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                      project_id = "pvu6f",
                                                                      file_id = "bw3st",
                                                                      path = file_in("extdata/source/official_names_tribes.csv"),
                                                                      osf_dirpath = "data/raw-data/official-names"),
                                                 trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_housing_authorities_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                                   project_id = "pvu6f",
                                                                                   file_id = "2nwjh",
                                                                                   path = file_in("extdata/source/official_names_housing_authorities.csv"),
                                                                                   osf_dirpath = "data/raw-data/official-names"),
                                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_special_purpose_districts_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                                         project_id = "pvu6f",
                                                                                         file_id = "6dp8t",
                                                                                         path = file_in("extdata/source/official_names_special_purpose_districts.csv"),
                                                                                         osf_dirpath = "data/raw-data/official-names"),
                                                                    trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_higher_ed_providers_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                                   project_id = "pvu6f",
                                                                                   file_id = "cj7s3",
                                                                                   path = file_in("extdata/source/official_names_higher_ed_providers.csv"),
                                                                                   osf_dirpath = "data/raw-data/official-names"),
                                                              trigger = trigger(mode = "blacklist", condition = FALSE)),
    official_names_hospitals_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                         project_id = "pvu6f",
                                                                         file_id = "mcrb4",
                                                                         path = file_in("extdata/source/official_names_hospitals.csv"),
                                                                         osf_dirpath = "data/raw-data/official-names"),
                                                    trigger = trigger(mode = "blacklist", condition = FALSE)),
    development_assumptions_zoning_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                                               project_id = "pvu6f",
                                                                               file_id = "ynhd6",
                                                                               path = file_in("extdata/source/development_assumptions_zoning.csv"),
                                                                               osf_dirpath = "data/raw-data"),
                                                          trigger = trigger(mode = "blacklist", condition = FALSE)),
    nmtc_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                     project_id = "pvu6f",
                                                     file_id = "e9kyw",
                                                     path = file_in("extdata/source/NMTC-2011-2015-LIC-Nov2-2017-4pm.xlsx"),
                                                     osf_dirpath = "data/raw-data"),
                                trigger = trigger(mode = "blacklist", condition = FALSE)),
    dda_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                    project_id = "pvu6f",
                                                    file_id = "vn5dj",
                                                    path = file_in("extdata/source/DDA2019M.PDF"),
                                                    osf_dirpath = "data/raw-data"),
                               trigger = trigger(mode = "blacklist", condition = FALSE)),
    qct_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                    project_id = "pvu6f",
                                                    file_id = "az49w",
                                                    path = file_in("extdata/source/QCT2019dbf.zip"),
                                                    osf_dirpath = "data/raw-data"),
                               trigger = trigger(mode = "blacklist", condition = FALSE)),
    oz_upload_status = target(osf_upload_or_update(has_osf_access = has_osf_access,
                                                   project_id = "pvu6f",
                                                   file_id = "4gxr9",
                                                   path = file_in("extdata/source/Designated_QOZs_12-14-18.xlsx"),
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
                                         trigger = trigger(change = osf_get_file_version(osf_id = "4rjk6"))),
    name_recode_key_filepath = target(command = osf_download_file(osf_id = "4azv5", path = file_out("extdata/osf/name_recode_key.rda")),
                                      trigger = trigger(change = osf_get_file_version(osf_id = "4azv5"))),
    public_owner_name_category_key_filepath = target(command = osf_download_file(osf_id = "8az7h", path = file_out("extdata/osf/public_owner_name_category_key.rda")),
                                                     trigger = trigger(change = osf_get_file_version(osf_id = "8az7h"))),
    other_exempt_owner_name_category_key_filepath = target(command = osf_download_file(osf_id = "2k3dp", path = file_out("extdata/osf/other_exempt_owner_name_category_key.csv")),
                                                           trigger = trigger(change = osf_get_file_version(osf_id = "2k3dp"))),
    owner_antijoin_names_filepath = target(command = osf_download_file(osf_id = "t7zpe", path = file_out("extdata/osf/owner_antijoin_names.csv")),
                                           trigger = trigger(change = osf_get_file_version(osf_id = "t7zpe"))),
    pub_parcel_filepath = target(command = osf_download_file(osf_id = "5f7bd", path = file_out("extdata/osf/pub_parcel.csv")),
                                 trigger = trigger(change = osf_get_file_version(osf_id = "5f7bd"))),
    env_restrictions_filepath = target(command = osf_download_file(osf_id = "c54dv", path = file_out("extdata/osf/env_restrictions.csv")),
                                       trigger = trigger(change = osf_get_file_version(osf_id = "c54dv"))),
    parcel_addr_filepath = target(command = osf_download_file(osf_id = "x9zwq", path = file_out("extdata/osf/parcel_addr.csv")),
                                  trigger = trigger(change = osf_get_file_version(osf_id = "x9zwq"))),
    # add parcel_df,
    # add parcel_sf_poly,
    building_residential_filepath = target(command = osf_download_file(osf_id = "356ph", path = file_out("extdata/osf/EXTR_ResBldg.csv")),
                                           trigger = trigger(change = osf_get_file_version(osf_id = "356ph"))),
    building_apartment_filepath = target(command = osf_download_file(osf_id = "5bj39", path = file_out("extdata/osf/EXTR_AptComplex.csv")),
                                         trigger = trigger(change = osf_get_file_version(osf_id = "5bj39"))),
    building_condo_filepath = target(command = osf_download_file(osf_id = "98hrb", path = file_out("extdata/osf/EXTR_CondoComplex.csv")),
                                     trigger = trigger(change = osf_get_file_version(osf_id = "98hrb"))),
    building_commercial_filepath = target(command = osf_download_file(osf_id = "hmq9z", path = file_out("extdata/osf/EXTR_CommBldg.csv")),
                                          trigger = trigger(change = osf_get_file_version(osf_id = "hmq9z"))),
    king_county_filepath = target(command = osf_download_file(osf_id = "jqsym", path = file_out("extdata/osf/king_county.gpkg")),
                                  trigger = trigger(change = osf_get_file_version(osf_id = "jqsym"))),
    wa_major_waterbodies_filepath = target(command = osf_download_file(osf_id = "p4yg5", path = file_out("extdata/osf/ECY_WAT_NHDWAMajor.zip")),
                                           trigger = trigger(change = osf_get_file_version(osf_id = "p4yg5"))),
    uga_filepath = target(command = osf_download_file(osf_id = "vnsfw", path = file_out("extdata/osf/urban_growth_SHP.zip")),
                          trigger = trigger(change = osf_get_file_version(osf_id = "vnsfw"))),
    zoning_filepath = target(command = osf_download_file(osf_id = "5hjxa", path = file_out("extdata/osf/zoning_kc_consol_20_SHP.zip")),
                             trigger = trigger(change = osf_get_file_version(osf_id = "5hjxa"))),
    kc_city_filepath = target(command = osf_download_file(osf_id = "8zfd4", path = file_out("extdata/osf/city_kc_SHP.zip")),
                              trigger = trigger(change = osf_get_file_version(osf_id = "8zfd4"))),
    census_tracts_filepath = target(command = osf_download_file(osf_id = "tq4b9", path = file_out("extdata/osf/census_tracts.gpkg")),
                                    trigger = trigger(change = osf_get_file_version(osf_id = "tq4b9"))),
    zcta_filepath = target(command = osf_download_file(osf_id = "n5rhf", path = file_out("extdata/osf/zcta.gpkg")),
                           trigger = trigger(change = osf_get_file_version(osf_id = "n5rhf"))),
    census_place_filepath = target(command = osf_download_file(osf_id = "w69pb", path = file_out("extdata/osf/census_place.gpkg")),
                                   trigger = trigger(change = osf_get_file_version(osf_id = "w69pb"))),
    school_districts_filepath = target(command = osf_download_file(osf_id = "8svty", path = file_out("extdata/osf/schdst_SHP.zip")),
                                       trigger = trigger(change = osf_get_file_version(osf_id = "8svty"))),
    leg_districts_filepath = target(command = osf_download_file(osf_id = "v4xk7", path = file_out("extdata/osf/legdst_SHP.zip")),
                                    trigger = trigger(change = osf_get_file_version(osf_id = "v4xk7"))),
    kc_council_districts_filepath = target(command = osf_download_file(osf_id = "82c7m", path = file_out("extdata/osf/kccdst_SHP.zip")),
                                           trigger = trigger(change = osf_get_file_version(osf_id = "82c7m"))),
    seattle_council_districts_filepath = target(command = osf_download_file(osf_id = "cj27h", path = file_out("extdata/osf/sccdst_SHP.zip")),
                                                trigger = trigger(change = osf_get_file_version(osf_id = "cj27h"))),
    bus_stops_metro_filepath = target(command = osf_download_file(osf_id = "w9ap3", path = file_out("extdata/osf/transitstop_SHP.zip")),
                                      trigger = trigger(change = osf_get_file_version(osf_id = "w9ap3"))),
    transit_stops_osm_filepath = target(command = osf_download_file(osf_id = "zk826", path = file_out("extdata/osf/transit_stops_osm.gpkg")),
                                        trigger = trigger(change = osf_get_file_version(osf_id = "zk826"))),
    play_spaces_osm_filepath = target(command = osf_download_file(osf_id = "4prez", path = file_out("extdata/osf/play_spaces_osm.gpkg")),
                                      trigger = trigger(change = osf_get_file_version(osf_id = "4prez"))),
    mj_businesses_filepath = target(command = osf_download_file(osf_id = "3m5eh", path = file_out("extdata/osf/MarijuanaApplicants.xls")),
                                    trigger = trigger(change = osf_get_file_version(osf_id = "3m5eh"))),
    other_suitability_characteristics_filepath = target(command = osf_download_file(osf_id = "wub8k", path = file_out("extdata/osf/other_suitability_characteristics.rda")),
                                                        trigger = trigger(change = osf_get_file_version(osf_id = "wub8k"))),
    seattle_dev_cap_filepath = target(command = osf_download_file(osf_id = "vby7h", path = file_out("extdata/osf/seattle_dev_cap.csv")),
                                      trigger = trigger(change = osf_get_file_version(osf_id = "vby7h"))),
    affordable_housing_properties_filepath = target(command = osf_download_file(osf_id = "2e9sb", path = file_out("extdata/osf/NHPD Properties Only Export.xlsx")),
                                                    trigger = trigger(change = osf_get_file_version(osf_id = "2e9sb"))),
    future_lightrail_filepath = target(command = osf_download_file(osf_id = "zkphb", path = file_out("extdata/osf/future_lightrail.gpkg")),
                                       trigger = trigger(change = osf_get_file_version(osf_id = "zkphb"))),
    brownfield_sites_filepath = target(command = osf_download_file(osf_id = "8dpeq", path = file_out("extdata/osf/brownfield_sites.csv")),
                                       trigger = trigger(change = osf_get_file_version(osf_id = "8dpeq"))),
    contaminated_sites_filepath = target(command = osf_download_file(osf_id = "a5mgq", path = file_out("extdata/osf/contaminated_sites.csv")),
                                         trigger = trigger(change = osf_get_file_version(osf_id = "a5mgq"))),
    official_names_seattle_filepath = target(command = osf_download_file(osf_id = "3ygke", path = file_out("extdata/osf/official_names_seattle.csv")),
                                             trigger = trigger(change = osf_get_file_version(osf_id = "3ygke"))),
    official_names_kc_filepath = target(command = osf_download_file(osf_id = "psv3b", path = file_out("extdata/osf/official_names_kc.csv")),
                                        trigger = trigger(change = osf_get_file_version(osf_id = "psv3b"))),
    official_names_wa_filepath = target(command = osf_download_file(osf_id = "c47ka", path = file_out("extdata/osf/official_names_wa.csv")),
                                        trigger = trigger(change = osf_get_file_version(osf_id = "c47ka"))),
    official_names_us_filepath = target(command = osf_download_file(osf_id = "un7zk", path = file_out("extdata/osf/official_names_us.csv")),
                                        trigger = trigger(change = osf_get_file_version(osf_id = "un7zk"))),
    official_names_places_filepath = target(command = osf_download_file(osf_id = "ntjdx", path = file_out("extdata/osf/official_names_places.csv")),
                                            trigger = trigger(change = osf_get_file_version(osf_id = "ntjdx"))),
    official_names_tribes_filepath = target(command = osf_download_file(osf_id = "bw3st", path = file_out("extdata/osf/official_names_tribes.csv")),
                                            trigger = trigger(change = osf_get_file_version(osf_id = "bw3st"))),
    official_names_housing_authorities_filepath = target(command = osf_download_file(osf_id = "2nwjh", path = file_out("extdata/osf/official_names_housing_authorities.csv")),
                                                         trigger = trigger(change = osf_get_file_version(osf_id = "2nwjh"))),
    official_names_special_purpose_districts_filepath = target(command = osf_download_file(osf_id = "6dp8t", path = file_out("extdata/osf/official_names_special_purpose_districts.csv")),
                                                               trigger = trigger(change = osf_get_file_version(osf_id = "6dp8t"))),
    official_names_higher_ed_providers_filepath = target(command = osf_download_file(osf_id = "cj7s3", path = file_out("extdata/osf/official_names_higher_ed_providers.csv")),
                                                         trigger = trigger(change = osf_get_file_version(osf_id = "cj7s3"))),
    official_names_hospitals_filepath = target(command = osf_download_file(osf_id = "mcrb4", path = file_out("extdata/osf/official_names_hospitals.csv")),
                                               trigger = trigger(change = osf_get_file_version(osf_id = "mcrb4"))),
    development_assumptions_zoning_filepath = target(command = osf_download_file(osf_id = "ynhd6", path = file_out("extdata/osf/development_assumptions_zoning.csv")),
                                                     trigger = trigger(change = osf_get_file_version(osf_id = "ynhd6"))),
    nmtc_filepath = target(command = osf_download_file(osf_id = "e9kyw", path = file_out("extdata/osf/NMTC-2011-2015-LIC-Nov2-2017-4pm.xlsx")),
                           trigger = trigger(change = osf_get_file_version(osf_id = "e9kyw"))),
    dda_filepath = target(command = osf_download_file(osf_id = "vn5dj", path = file_out("extdata/osf/DDA2019M.PDF")),
                          trigger = trigger(change = osf_get_file_version(osf_id = "vn5dj"))),
    qct_filepath = target(command = osf_download_file(osf_id = "az49w", path = file_out("extdata/osf/QCT2019dbf.zip")),
                          trigger = trigger(change = osf_get_file_version(osf_id = "az49w"))),
    oz_filepath = target(command = osf_download_file(osf_id = "4gxr9", path = file_out("extdata/osf/Designated_QOZs_12-14-18.xlsx")),
                         trigger = trigger(change = osf_get_file_version(osf_id = "4gxr9")))
  )

  ready_plan <- drake::drake_plan(
    lookup = make_lookup(path = file_in("extdata/osf/EXTR_LookUp.csv")),
    tax_status = make_tax_status(path = file_in("extdata/osf/tax_status.txt")),
    tax_reason = make_tax_reason(path = file_in("extdata/osf/tax_reason.txt")),
    parcel_metadata_table = make_parcel_metadata_table(path = file_in("extdata/osf/KC_Parcel_Metadata_Table.csv")),
    prop_type = make_prop_type(path = file_in("extdata/osf/prop_type.txt")),
    present_use_recode = make_present_use_recode(path = file_in("extdata/osf/present_use_recode.csv")),
    parcel_lookup = make_parcel_lookup(parcel_metadata_table, lookup, present_use_recode),
    name_recode_key = make_name_recode_key(path = file_in("extdata/osf/name_recode_key.rda")),
    public_owner_name_category_key = make_public_owner_name_category_key(path = file_in("extdata/osf/public_owner_name_category_key.rda")),
    other_exempt_owner_name_category_key = make_other_exempt_owner_name_category_key(path = file_in("extdata/osf/other_exempt_owner_name_category_key.csv")),
    owner_antijoin_names = make_owner_antijoin_names(path = file_in("extdata/osf/owner_antijoin_names.csv")),
    pub_parcel = make_pub_parcel(path = file_in("extdata/osf/pub_parcel.csv")),
    acct = make_acct(path = file_in("extdata/source/Real_Property_Account_Extract_2010515.csv")),
    env_restrictions = make_env_restrictions(path = file_in("extdata/osf/env_restrictions.csv")),
    parcel_addr_ready = make_parcel_addr_ready(path = file_in("extdata/osf/parcel_addr.csv")),
    parcel_df = make_parcel_df(path = file_in("extdata/osf/EXTR_Parcel.csv")),
    parcel_sf_poly = make_parcel_sf_poly(path = file_in("extdata/osf/parcel_sf_poly.gpkg")),
    parcel_sf = make_parcel_sf(parcel_sf_poly),
    parcel_sf_ready = make_parcel_sf_ready(parcel_sf),
    parcel_df_ready =  make_parcel_df_ready(parcel_lookup, prop_type, pub_parcel, parcel_df),
    parcel_acct_ready = make_parcel_acct_ready(acct, tax_status, tax_reason),
    parcel_env_ready = make_parcel_env_ready(env_restrictions),
    building_residential = make_building_residential(path = file_in("extdata/osf/EXTR_ResBldg.csv")),
    building_apartment = make_building_apartment(path = file_in("extdata/osf/EXTR_AptComplex.csv")),
    building_condo = make_building_condo(path = file_in("extdata/osf/EXTR_CondoComplex.csv")),
    building_commercial = make_building_commercial(path = file_in("extdata/osf/EXTR_CommBldg.csv")),
    king_county = make_king_county(path = file_in("extdata/osf/king_county.gpkg")),
    wa_major_waterbodies = make_wa_major_waterbodies(path = file_in("extdata/osf/ECY_WAT_NHDWAMajor.zip")),
    kc_waterbodies = make_kc_waterbodies(wa_major_waterbodies, king_county),
    uga = make_uga(path = file_in("extdata/osf/urban_growth_SHP.zip"), king_county),
    zoning = make_zoning(path = file_in("extdata/osf/zoning_kc_consol_20_SHP.zip")),
    kc_city = make_kc_city(path = file_in("extdata/osf/city_kc_SHP.zip")),
    census_tracts = make_census_tracts(path = file_in("extdata/osf/census_tracts.gpkg")),
    zcta = make_zcta(path = file_in("extdata/osf/zcta.gpkg"), king_county),
    census_place = make_census_place(path = file_in("extdata/osf/census_place.gpkg")),
    school_districts = make_school_districts(path = file_in("extdata/osf/schdst_SHP.zip")),
    leg_districts = make_leg_districts(path = file_in("extdata/osf/legdst_SHP.zip")),
    kc_council_districts = make_kc_council_districts(path = file_in("extdata/osf/kccdst_SHP.zip")),
    seattle_council_districts = make_seattle_council_districts(path = file_in("extdata/osf/sccdst_SHP.zip")),
    bus_stops_metro = make_bus_stops_metro(path = file_in("extdata/osf/transitstop_SHP.zip")),
    transit_stops_osm = make_transit_stops_osm(path = file_in("extdata/osf/transit_stops_osm.gpkg")),
    play_spaces_osm = make_play_spaces_osm(path = file_in("extdata/osf/play_spaces_osm.gpkg")),
    mj_businesses = make_mj_businesses(path = file_in("extdata/osf/MarijuanaApplicants.xls")),
    el_facilities = make_el_facilities("extdata/source/FutureWise EC Facility Data Request.xlsx"),
    other_suitability_characteristics = make_other_suitability_characteristics(path = file_in("extdata/osf/other_suitability_characteristics.rda")),
    seattle_dev_cap = make_seattle_dev_cap(path = file_in("extdata/osf/seattle_dev_cap.csv")),
    affordable_housing_properties = make_affordable_housing_properties(path = file_in("extdata/osf/NHPD Properties Only Export.xlsx")),
    affordable_housing_subsidies = make_affordable_housing_subsidies(path = "extdata/source/All Subsidies.xlsx",zcta),
    future_lightrail = make_future_lightrail(path = file_in("extdata/osf/future_lightrail.gpkg")),
    brownfield_sites = make_brownfield_sites(path = file_in("extdata/osf/brownfield_sites.csv")),
    contaminated_sites = make_contaminated_sites(path = file_in("extdata/osf/contaminated_sites.csv")),
    official_names_seattle = make_official_names_seattle(path = file_in("extdata/osf/official_names_seattle.csv")),
    official_names_kc = make_official_names_kc(path = file_in("extdata/osf/official_names_kc.csv")),
    official_names_wa = make_official_names_wa(path = file_in("extdata/osf/official_names_wa.csv")),
    official_names_us = make_official_names_us(path = file_in("extdata/osf/official_names_us.csv")),
    official_names_places = make_official_names_places(path = file_in("extdata/osf/official_names_places.csv")),
    official_names_tribes = make_official_names_tribes(path = file_in("extdata/osf/official_names_tribes.csv")),
    official_names_housing_authorities = make_official_names_housing_authorities(path = file_in("extdata/osf/official_names_housing_authorities.csv")),
    official_names_regional_transit_authorities = make_official_names_regional_transit_authorities(),
    official_names_special_purpose_districts = make_official_names_special_purpose_districts(path = file_in("extdata/osf/official_names_special_purpose_districts.csv")),
    official_names_school_districts = make_official_names_school_districts(official_names_special_purpose_districts),
    official_names_higher_ed_providers = make_official_names_higher_ed_providers(path = file_in("extdata/osf/official_names_higher_ed_providers.csv")),
    official_names_hospitals = make_official_names_hospitals(path = file_in("extdata/osf/official_names_hospitals.csv")),
    development_assumptions_zoning = make_development_assumptions_zoning(path = file_in("extdata/osf/development_assumptions_zoning.csv")),
    nmtc = make_nmtc(path = file_in("extdata/osf/NMTC-2011-2015-LIC-Nov2-2017-4pm.xlsx")),
    dda = make_dda(path = file_in("extdata/osf/DDA2019M.PDF")),
    qct = make_qct(path = file_in("extdata/osf/QCT2019dbf.zip")),
    oz = make_oz(path = file_in("extdata/osf/Designated_QOZs_12-14-18.xlsx"))
  )

  data_cache_plan <- drake::bind_plans(download_plan, ready_plan)

  return(data_cache_plan)


}


# SUITABILITY AND UTILIZATION PLANS ---------------------------------------

get_parcel_plan <- function(){

  options(drake_make_menu = FALSE)

  parcel_plan <- drake::drake_plan(
    parcel_ready = make_parcel_ready(parcel_addr_ready,
                                     parcel_env_ready,
                                     parcel_acct_ready,
                                     parcel_sf_ready,
                                     parcel_df_ready)
  )

  return(parcel_plan)

}

get_building_plan <- function(){

  options(drake_make_menu = FALSE)

  building_plan <- drake::drake_plan(
    building_template = make_building_template(),
    building = make_building(building_template, building_residential, building_apartment, building_condo, building_commercial)
  )

  return(building_plan)


}

get_development_assumptions_plan <- function(){

  options(drake_make_menu = FALSE)

  development_assumptions_plan <- drake::drake_plan(
    city_block_sqft = make_city_block_sqft(),
    city_block_acre = make_city_block_acre(city_block_sqft),
    lot_types = make_lot_types(),
    lot_size_breaks = make_lot_size_breaks(city_block_acre),
    lot_development_parameters = make_lot_development_parameters(),
    development_assumptions_lot = make_development_assumptions_lot(lot_types, lot_development_parameters, development_assumptions_zoning)
  )

  return(development_assumptions_plan)
}

get_suitability_criteria_plan <- function(){

  options(drake_make_menu = FALSE)

  suitability_criteria_plan <- drake::drake_plan(
    criteria_tax_exempt = make_criteria_tax_exempt(),
    criteria_max_water_overlap_pct = make_criteria_max_water_overlap_pct(),
    criteria_within_uga = make_criteria_within_uga(),
    criteria_developable_zoning = make_criteria_developable_zoning(development_assumptions_zoning),
    criteria_undevelopable_present_use = make_criteria_undevelopable_present_use(),
    criteria_lot_size = make_criteria_lot_size(lot_size_breaks),
    criteria_area_ratio = make_criteria_area_ratio(),
    criteria_steep_vacant = make_criteria_steep_vacant(),
    criteria_unbuildable = make_criteria_unbuildable(),
    criteria_other = make_criteria_other(),
    suitability_criteria = make_suitability_criteria(criteria_tax_exempt,
                                                     criteria_max_water_overlap_pct,
                                                     criteria_within_uga,
                                                     criteria_developable_zoning,
                                                     criteria_undevelopable_present_use,
                                                     criteria_lot_size,
                                                     criteria_area_ratio,
                                                     criteria_steep_vacant,
                                                     criteria_unbuildable,
                                                     criteria_other)
  )

  return(suitability_criteria_plan)

}

get_suitability_plan <- function(){

  options(drake_make_menu = FALSE)

  suitability_plan <- drake::drake_plan(
    suitability_tax_exempt = make_suitability_tax_exempt(parcel_ready),
    suitability_water_overlap = make_suitability_water_overlap(parcel_sf_ready, kc_waterbodies, king_county),
    suitability_within_uga = make_suitability_within_uga(parcel_sf_ready, uga),
    suitability_developable_zoning = make_suitability_developable_zoning(parcel_sf_ready, zoning),
    suitability_present_use = make_suitability_present_use(parcel_ready),
    suitability_lot_size = make_suitability_lot_size(parcel_sf_ready, lot_size_breaks),
    suitability_parcel_area_ratio = make_suitability_parcel_area_ratio(parcel_sf_ready),
    suitability_steep_vacant = make_suitability_steep_vacant(parcel_ready),
    suitability_unbuildable = make_suitability_unbuildable(parcel_ready),
    suitability_other = make_suitability_other(parcel_ready, other_suitability_characteristics),
    suitability = make_suitability(parcel_ready,
                                   suitability_criteria,
                                   suitability_tax_exempt,
                                   suitability_water_overlap,
                                   suitability_within_uga,
                                   suitability_developable_zoning,
                                   suitability_present_use,
                                   suitability_lot_size,
                                   suitability_parcel_area_ratio,
                                   suitability_steep_vacant,
                                   suitability_unbuildable,
                                   suitability_other)
  )

  return(suitability_plan)
}

get_utilization_criteria_plan <- function(){

  options(drake_make_menu = FALSE)

  utilization_criteria_plan <- drake::drake_plan(
    utilization_criteria_lot_size = make_utilization_criteria_lot_size(city_block_sqft, lot_types),
    utilization_criteria_ratio = make_utilization_criteria_ratio(),
    utilization_criteria_ratio_bins = make_utilization_criteria_ratio_bins(),
    utilization_criteria = make_utilization_criteria(utilization_criteria_lot_size,
                                                     utilization_criteria_ratio,
                                                     utilization_criteria_ratio_bins)
  )

  return(utilization_criteria_plan)
}

get_utilization_plan <- function(){

  options(drake_make_menu = FALSE)

  utilization_plan <- drake::drake_plan(
    seattle_utilization_ratio = make_seattle_utilization_ratio(parcel_sf_ready, seattle_dev_cap),
    utilization_present = make_utilization_present(parcel_ready, building),
    utilization_lot_size = make_utilization_lot_size(parcel_ready, utilization_criteria),
    utilization_potential = make_utilization_potential(suitability, development_assumptions_lot, utilization_lot_size)
  )

  return(utilization_plan)
}


# FILTERS AND HELPERS PLANS -----------------------------------------------

get_official_names_plan <- function(){

  options(drake_make_menu = FALSE)

  official_names_plan <- drake::drake_plan(
    official_names = make_official_names(official_names_seattle,
                                         official_names_kc,
                                         official_names_wa,
                                         official_names_us,
                                         official_names_places,
                                         official_names_tribes,
                                         official_names_housing_authorities,
                                         official_names_regional_transit_authorities,
                                         official_names_special_purpose_districts,
                                         official_names_school_districts,
                                         official_names_higher_ed_providers,
                                         official_names_hospitals)
  )

  return(official_names_plan)

}

get_owner_plan <- function(){

  options(drake_make_menu = FALSE)

  owner_plan <- drake::drake_plan(
    owner_name_full = make_owner_name_full(suitability, name_recode_key, owner_antijoin_names),
    owner_category = make_owner_category(owner_name_full, public_owner_name_category_key, other_exempt_owner_name_category_key)
  )

  return(owner_plan)
}

get_filter_plan <- function(parcel_sf_ready, census_tracts){

  options(drake_make_menu = FALSE)

  filter_plan <- drake::drake_plan(
    filters_census_tract = make_filters_census_tract(parcel_sf_ready, census_tracts),
    filters_zcta = make_filters_zcta(parcel_sf_ready, zcta),
    filters_place = make_filters_place(parcel_sf_ready, census_place),
    filters_place_name = make_filters_place_name(parcel_df_ready, filters_place),
    filters_owner_category = make_filters_owner_category(owner_category),
    filters_public_owner = make_filters_public_owner(owner_category),
    filters_zoning_category = make_filters_zoning_category(suitability_developable_zoning),
    filters_proximity_transit = make_filters_proximity_transit(parcel_sf_ready, transit_stops_osm),
    filters_proximity_play_space = make_filters_proximity_play_space(parcel_sf_ready, play_spaces_osm),
    filters_proximity_marijuana = make_filters_proximity_marijuana(parcel_sf_ready, mj_businesses),
    filters_proximity_el_facilities = make_filters_proximity_el_facilities(parcel_sf_ready, el_facilities),
    filters_proximity_affordable_housing = make_filters_proximity_affordable_housing(parcel_sf_ready, affordable_housing_properties),
    filters_leg_district = make_filters_leg_district(parcel_sf_ready, leg_districts),
    filters_kc_council_district = make_filters_kc_council_district(parcel_sf_ready, kc_council_districts),
    filters_seattle_council_district = make_filters_seattle_council_district(parcel_sf_ready, seattle_council_districts),
    filters_school_district = make_filters_school_district(parcel_sf_ready, school_districts),
    filters_historic = make_filters_historic(parcel_ready),
    filters_afford_expir_date = make_filters_afford_expir_date(parcel_sf_ready, affordable_housing_subsidies),
    filters_eligibility_nmtc = make_filters_eligibility_nmtc(filters_census_tract, nmtc),
    filters_eligibility_dda = make_filters_eligibility_dda(filters_zcta, dda),
    filters_eligibility_qct = make_filters_eligibility_qct(filters_census_tract, qct),
    filters_eligibility_oz = make_filters_eligibility_oz(filters_census_tract, oz),
    filters_parking = make_filters_parking(parcel_df_ready),
    filters_proximity_lightrail = make_filters_proximity_lightrail(parcel_sf_ready, future_lightrail),
    filters_brownfield = make_filters_brownfield(parcel_sf_ready, brownfield_sites),
    filters_contaminated = make_filters_contaminated(parcel_sf_ready, contaminated_sites),
    filters = make_filters(parcel_ready,
                           filter_list = list(filters_census_tract,
                                              filters_zcta,
                                              filters_place,
                                              filters_place_name,
                                              filters_owner_category,
                                              filters_public_owner,
                                              filters_zoning_category,
                                              filters_proximity_transit,
                                              filters_proximity_play_space,
                                              filters_proximity_marijuana,
                                              filters_proximity_el_facilities,
                                              filters_proximity_affordable_housing,
                                              filters_leg_district,
                                              filters_kc_council_district,
                                              filters_seattle_council_district,
                                              filters_school_district,
                                              filters_historic,
                                              filters_afford_expir_date,
                                              filters_eligibility_nmtc,
                                              filters_eligibility_dda,
                                              filters_eligibility_qct,
                                              filters_eligibility_oz,
                                              filters_parking,
                                              filters_proximity_lightrail,
                                              filters_brownfield,
                                              filters_contaminated))
  )

  return(filter_plan)
}

# INVENTORY PLAN ----------------------------------------------------------




# DOCUMENTATION PLAN ------------------------------------------------------



# EXPORT PLAN -------------------------------------------------------------


