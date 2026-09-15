best <- function(state, outcome) {
     # nolint: trailing_whitespace_linter, indentation_linter.
    # Read outcome data # nolint: indentation_linter, indentation_linter.
    outcome_data <- read.csv(
        "outcome-of-care-measures.csv", # nolint: indentation_linter.
        colClasses = "character"
    ) # nolint
    
    # Check that state is valid # nolint: indentation_linter.
    if (!state %in% outcome_data$State) {
        stop("invalid state")
    }
    
    # Check that outcome is valid
    valid_outcomes <- c(
        "heart attack",
        "heart failure",
        "pneumonia"
    )
    
    if (!outcome %in% valid_outcomes) {
        stop("invalid outcome")
    }
    
    # Identify the correct outcome column
    outcome_column <- c(
        "heart attack" =
            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "heart failure" =
            "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "pneumonia" =
            "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    )
    
    # Keep only hospitals in the requested state
    state_data <- outcome_data[outcome_data$State == state, ]
    
    # Get mortality rates and convert them to numeric
    rates <- as.numeric(
        state_data[[outcome_column[outcome]]]
    )
    
    # Remove hospitals with missing rates
    valid <- !is.na(rates)
    state_data <- state_data[valid, ]
    rates <- rates[valid]
    
    # Find the lowest mortality rate
    lowest <- min(rates)
    
    # Get hospitals with the lowest rate
    best_hospitals <- state_data$Hospital.Name[
        rates == lowest
    ]
    
    # Break ties alphabetically
    best_hospitals <- sort(best_hospitals)
    
    # Return the first hospital
    best_hospitals[1]
}