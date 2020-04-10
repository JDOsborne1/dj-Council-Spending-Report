
# Looking up creditor names -----------------------------------------------



#' Generate Creditor Lookup
#'
#' @description While the algorithm can point out likely matches, it's not
#'   currently viable to use it to generate a lookup automatically
#'
#' @return
#' @export
#'
#' @examples
csr_ImpGenerateCreditorLookup <- function(){
        tribble(~standard.name, ~secondary.name
                , "Aspects &  Milestones Trust", "Milestones Trust"
                , "Aspects &  Milestones Trust", "Milestones - Unit 10"
                , "Aspects &  Milestones Trust", "Milestones Outreach Dementia Services"
                # Worth checking if there is a difference in the below.
                , "Bath & North East Somerset Council", "Bath & North East Somerset Dist Council"
                , "Brandon Trust", "The Brandon Trust"
                , "Brandon Trust", "The Brandon Trust  Olympus House"
                , "Eurotaxis Limited", "Eurotaxis Ltd" 
                , "Freeways Trust Limited", "Freeways Trust Ltd"
                , "Tarmac Trading Limited", "Lafarge Tarmac Trading Limited"
                # The two below arent identified as the same company on companies house, but they have identical directors so lumping together
                , "Tarmac Trading Limited", "Tarmac Aggregates Limited"
                , "Tarmac Trading Limited", "Lafarge Aggregates Limited"
                , "Medequip Assistive Technology Limited", "Medequip Assistive Technology Ltd"
                # The below are actually different legal entities, but almost certainly fill the same role
                , "Network Rail", "Network Rail Infrastructure Limited"
        )
}



# Looking up department names ---------------------------------------------

#' Generate the Departmental Lookup
#'
#' @description The spending is attributed to departments in the spending
#'   reports through use of Department codes. These codes are not made available
#'   in one place, however most of them can be indferred through the use of a
#'   variety of public documents.
#'
#' @source This data comes from multiple sources. They are listed below: 
#'   - https://www.appliedcare.co.uk/education.aspx 
#'   - https://www.southglos.gov.uk/council-and-democracy/organisational-structure/
#'   - https://www.southglos.gov.uk/documents/Audited-Annual-Financial-Report-201819.pdf
#'   - https://www.bsgconservatives.com/people/cllr-brian-allinson
#'
#' @return
#' @export
#'
#' @examples
csr_ImpGenerateDepartmentLookup <- function(){
        tribble(
                ~Dept, ~Department.Desc
                , "ACAD", "Applied Care and Development"
                , "CAHD", "Children Adults and Health Department"
                , "CCHD", "Community Care and Housing Development"
                , "CECR", "Chief Executive and Corporate Resources"
                , "COMM", NA_character_
                , "CORP", "Corporate and Central Services"
                , "CYPN", NA_character_
                , "DECS", "Department of Environment and Community Services"
                , "ECS" , "Department of Environment and Community Services"
                , "PTSE", "Planning, Transportation and Strategic Environment Committee"
                )
}
