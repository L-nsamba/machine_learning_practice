rankall <- function(outcome, num = "best") {
    
    # Read outcome data
    outcome_data <- read.csv(
        "outcome-of-care-measures.csv",
        colClasses = "character"
    )
    length(unique(outcome_data$State))

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
    
    # Get all states
	states <- sort(unique(outcome_data$State))    
	
    # Create empty result data frame
    result <- data.frame(
        hospital = character(length(states)),
        state = states,
        stringsAsFactors = FALSE
    )
    
    # Go through each state
    for (i in seq_along(states)) {
        
        # Keep only hospitals in this state
        state_data <- outcome_data[
            outcome_data$State == states[i],
        ]
        
        # Get mortality rates
		rates <- as.numeric(
			state_data[[outcome_column[outcome]]]
		)
        
        # Remove hospitals with missing data
        valid <- !is.na(rates)
        state_data <- state_data[valid, ]
        rates <- rates[valid]
        
        # Sort by mortality rate,
        # then hospital name for ties
        order_index <- order(
            rates,
            state_data$Hospital.Name
        )
        
        state_data <- state_data[order_index, ]
        
        # Determine the requested rank
        if (num == "best") {
            rank <- 1
        } else if (num == "worst") {
            rank <- nrow(state_data)
        } else {
            rank <- num
        }
        
        # If rank is too large, return NA
        if (rank > nrow(state_data)) {
            result$hospital[i] <- NA
        } else {
            result$hospital[i] <-
                state_data$Hospital.Name[rank]
        }
    }
    
    # Return the final data frame
    result
}