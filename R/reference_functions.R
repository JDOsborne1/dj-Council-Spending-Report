
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