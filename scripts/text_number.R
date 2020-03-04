# this script converts a number to text, having converted it to an approximate fraction if the number is less than 1

small_as_text <- function (x, initial = FALSE, text_below = 10, accuracy = NULL) {
  
  if (x < text_below) {
    
    if (initial == TRUE) {
      
      english::Words(x)
      
    } else {
      
      english::words(x)
      
    }
    
  } else {
    
    scales::comma(x, accuracy = accuracy)
    
  }
  
}

text_number <- Vectorize(function (
  x,                   # the number
  initial = FALSE,     # will the text appear at the start of the sentence
  accuracy = NULL,     # the accuracy with which the number will be reported
  as_freq = FALSE,     # if x < 1, should it be reported as a frequency
  text_below = 10,     # number below which numbers are spelt out
  text_above = 750000, # number above which numbers are 
  smallest_num = 0.001 # the smallest number that will be displayed
) {
  
  # if accuracy is not specified and the number is <1, set accuracy to the 
  # larger of the number or 0.001
  if (is.null(accuracy)) {
    
    if (x < 1) {
      
      accuracy <- smallest_num
      
    }

  # if accuracy is set but is larger than the number, set accuracy to the number
  # to ensure a meaningful value is reported
  } else {
    
    if (x < accuracy) {
      
      accuracy <- max(
        smallest_num, 
        as.numeric(stringr::str_sub(
          x, 
          0, 
          stringr::str_length(as.character(smallest_num))
        ))
      )
      
    }

  }
  
  # NUMBERS <1 PRESENTED AS FRACTIONS OR FREQUENCIES

  if (x < 1) {
    
    fraction <- fractional::fractional(x, eps = accuracy)
    numerator <- fractional::numerators(fraction)
    denominator <- fractional::denominators(fraction)

    # if x < smallest_num, report 'less than smallest_num'
    if (x < smallest_num) {
      
      if (initial == TRUE) {
        
        result <- paste("Less than", smallest_num)
        
      } else {
        
        result <- paste("less than", smallest_num)
        
      }
      
    # if the numerator is not 1 and the denominator > 20, report number as a 
    # number
    } else if (
      (numerator > 1 & denominator > 20) | 
      (denominator > 20 & denominator %% 10 != 0)
    ) {
    
      result <- scales::number(x, accuracy = accuracy)
    
    # if numerator == denominator, report whole number
    } else if (numerator == denominator) {
      
      result <- small_as_text(denominator, initial, text_below, accuracy)
      
    # report number as a frequency
    } else if (as_freq == TRUE) {
      
      result <- paste(
        ifelse(
          initial == TRUE, 
          # if number will appears at the start of the sentence, spell it out
          english::Words(numerator),
          # otherwise, spell it out if x < text_below
          small_as_text(numerator, initial, text_below)
        ), 
        "in", 
        # spell out number if x < text_below
        small_as_text(denominator, text_below = text_below)
      )
      
    # report number as a fraction
    } else {
      
      result <- paste(
        ifelse(
          initial == TRUE, 
          # if number will appears at the start of the sentence, spell it out
          english::Words(numerator),
          # otherwise, spell it out if x < text_below
          small_as_text(numerator, initial, text_below)
        ),
        case_when(
          # if number is tiny, remove denominator
          denominator == 1 ~ "",
          # if fraction is 1/2, change denominator to "half" from "second"
          denominator == 2 ~ "half",
          # if numerator != 1, make denominator plural in text
          numerator != 1 ~ paste0(english::ordinal(denominator), "s"),
          # otherwise, denominator is an ordinal number
          TRUE ~ as.character(english::ordinal(denominator))
        )
      )
      
    }

  # NUMBERS >=1 PRESENTED AS NUMBERS, POSSIBLY SPELT OUT
  } else {
    
    if (x > text_above) {
      
      number <- scales::label_number_si()(x)
      scale <- as.numeric(dplyr::recode(
        stringr::str_sub(number, -1), 
        "K" = 10^3, 
        "M" = 10^6, 
        "B" = 10^9, 
        "T" = 10^12,
        .default = 1
      ))
      suffix <- dplyr::recode(
        stringr::str_sub(number, -1), 
        "K" = " thousand", 
        "M" = " million", 
        "B" = " billion", 
        "T" = " trillion",
        .default = ""
      )
      result <- scales::number(x, accuracy = accuracy, scale = 1 / scale, 
                               suffix = suffix)
      
    } else {
      
      result <- small_as_text(x, initial, text_below, accuracy)
    
    }
    
  }
  
  stringr::str_squish(result)
  
})

Text_number <- function (x, ...) {
  
  text_number(x, initial = TRUE, ...)
  
}

tibble(number = c(1/1234, 1/21, 0.001, 0.01, 0.1, 0.5, 0.51, 0.9, 0.999, 1, 15, 105, 10000, 10^6, 0.1 * 10^9, 1.23 * 10^20)) %>% 
  mutate(
    character = as.character(number),
    default = text_number(number),
    as_freq = text_number(number, as_freq = TRUE),
    initial = Text_number(number),
    dp_3 = text_number(number, accuracy = 0.001),
    dp_1 = text_number(number, accuracy = 0.1),
    integer = text_number(number, accuracy = 1)
  ) %>% 
  select(-number) %>% 
  View()

