# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------
#  -- Multimorbidity in ERGO: define incident and prevalent cases at baseline --
# .-----------------------------------------------------------------------------
# .-----------------------------------------------------------------------------

# For all questions contact Premysl Velek at p.velek@erasmusmc.nl


#' get_cohort
#'
#' Takes the ERGO multimorbidity data and re-defines prevalence of diseases (so far)
#' included in the multimorbidity data. The prevalence is defined with respect to user defined
#' baseline date.
#'
#' The output is cleaned data with ergoids, and prevalence indicator for each specified disease
#' (1 = prevalent case, 0 = disease free). As a bonus, the number of prevalent diseases at baseline
#' for each participant is also returned.
#'
#' So far, the data include participants from  RS-I, RS-II and RS-III cohorts and the following
#' diseases:
#'
#' 1. cancer (all types expect non-melanoma skin cancer)
#' 2. COPD
#' 3. asthma
#' 4. stroke
#' 5. diabetes
#' 6. coronary heart disease
#' 7. heart failure
#' 8. TIA
#' 9. dementia
#' 10. parkinsonism
#' 11. depression

#'
#' To access the ERGO multimorbidity data, contact Frank van Rooij (f.vanrooij at erasmusmc.nl) and
#' Silvan Licher (s.licher at erasmusmc.nl)


#' @importFrom magrittr "%>%"
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @inmport rlang

#' @param fu_startd data frame with exactly two columns: 'ergoid' and 'fu_startd'. ergoid  contains
#' the ids of participants for which the incidence/prevalence should be re-defined. It can also be a
#' subset of participants from RS-I through RS-III. fu_startd contains the baseline dates
#' corresponding to each participant. Participant with baseline dates set to NA will be automatically
#' assigned NA to all diseases.
#'
#' ergoid should be formatted as factor, fu_startd needs to be a date in the yyyy-mm-dd format
#'
#'
#' @param data ERGO multimorbidity data. The default is shift_data.
#'
#' @param diseases a character vector with diseases for which the baseline prevalence should be re-defined. By
#' default all diseases included in the multimorbidity data will be considered. A subset of the
#' diseases must be defined using a the following abbreviations:
#'
#' dia: diabetes
#' stroke: stroke
#' can1: cancer
#' dem: dementia
#' hf: heart failure
#' chd: coronary heart disease
#' park: parkinsonism
#' dep1: depression
#' COPD: chronic obstructive pulmonary disease
#' asthma: asthma
#' tia: transient ischemic attack
#'
#' @param removeNAs whether participants with incomplete follow-up should be removed. The default is
#' set to FALSE, meaning data for all participants specified in fu_start_df will be returned.
#'
#' @export
get_prev <- function(fu_startd,
                             data = shift_data,
                             diseases = NULL,
                             removeNA = FALSE){

  # check if the shift data is loaded into R
  temp <- rlang::as_name(rlang::enquo(data))
  if(exists(temp, where = .GlobalEnv)){
      data <- data
  } else {
      stop("The data frame ", "`", temp, "`", " does not exist", sep = "")
  }

  # get list of diseases
  if (is.null(diseases)) diseases <- c(
        "dia",
        "stroke",
        "can1",
        "dem",
        "hf",
        "chd",
        "park",
        "dep1",
        "COPD",
        "asthma",
        "tia"
  )

  fu_startd <- tidyr::drop_na(fu_startd)

  # get the ergo id, age, fu start and outcome from the index data
  res <- shift_data %>%
      dplyr::left_join(fu_startd, ., by = "ergoid") %>%
      mutate(startd_COPD = startd_lung,
             startd_asthma = startd_lung)


  for (disease in diseases){
      res <- define_prevalence(res, disease)
  }

  res <- res %>%
    dplyr::select(dplyr::all_of(diseases), sex, ergoid, fu_startd) %>%

    # calculate the disease count and set the value to NA if there are NAs
    rowwise() %>%
    mutate(count = sum(c_across(diseases[1]:diseases[length(diseases)])),
           count = if_else(count > 100, NA, count)) %>%
    ungroup()

  message("Missing values in the resulting datasets are coded as follow:\n
          101: missing follow up start date \n
          102: missing follow up end date \n
          103: missing prevalence indicator \n
          104: missing incidence indicator \n
          105: your baseline date is earlier than ERGO follow up start \n
          106: your baseline date is later than ERGO follow up end (for disease free individuals) \n
          107: any other reason not covered above (should not happen)
          ")

  if (removeNA) tidyr::drop_na(res) else res
}



# HELPERS -----------------------------------------------------------------


#
# helper function to re-define prevalence of an individual disease
# with respect to the new follow-up start
#
# the return_vector argument specifies whether the output should be a vector or whether the
# column should be updated dynamically in an existing data frame.

define_prevalence <- function(dat, disease, return_vector = FALSE){

    startd <- paste0("startd_", disease)
    endd <- paste0("endd_", disease)
    prev <- paste0("prev_", disease)
    inc <- paste0("inc_", disease)

    # we need to make sure the ergoids are always in the same order, hence arranging by ergoid
    res <- arrange(dat, ergoid) %>%
        mutate(dat,
               !!disease := case_when(

                   # catch all possibilities of missing values
                   is.na(!!sym(startd)) ~ 101,
                   is.na(!!sym(endd)) ~ 102,
                   is.na(!!sym(prev)) ~ 103,
                   is.na(!!sym(inc)) ~ 104,

                   # if disease baseline date is earlier than the date of enrolling to ERGO
                   # This should not happen (or at least not very often)
                   !!sym(startd) > fu_startd ~ 105,

                   # if prevalent
                   !!sym(prev) == "yes" ~ 1,


                   # if incident
                   (!!sym(inc) == "yes" & !!sym(endd) <= fu_startd) ~ 1,
                   (!!sym(inc) == "yes" & !!sym(endd) > fu_startd) ~ 0,

                   # if disease free
                   (!!sym(inc) == "no" & !!sym(endd) >= fu_startd) ~ 0,
                   (!!sym(inc) == "no" & !!sym(endd) < fu_startd) ~ 106,

                   # any other case not covered above
                   .default = 107
               )
        )

    if (return_vector) res <- res %>% dplyr::pull(!!disease)
    res
}


