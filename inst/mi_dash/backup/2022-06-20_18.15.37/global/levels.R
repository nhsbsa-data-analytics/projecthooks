nps_groups <- c(
  "Detractor" = "Detractors",
  "Passive" = "Passives",
  "Promoter" = "Promoters"
)

nes_responses <- c(
  "Extremely difficult"        = "Extremely difficult",
  "Very difficult"             = "Very difficult",
  "Fairly difficult"           = "Fairly difficult",
  "Neither easy nor difficult" = "Neither easy nor difficult",
  "Fairly easy"                = "Fairly easy",
  "Very easy"                  = "Very easy",
  "Extremely easy"             = "Extremely easy"
)

involved_onboarding_responses <- c(
  "Yes" = "Yes",
  "No" = "No"
)

answered_qs_onboarding_responses <- c(
  "Yes" = "Yes",
  "Undisclosed" = "Undisclosed",
  "No" = "No"
)

transition_reqs_met_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Yes" = "Yes",
  "No" = "No"
)

contact_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Yes" = "Yes",
  "No" = "No"
)

contact_instances_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Once" = "Once",
  "Five times or more" = "Five times or more",
  "Twice" = "Twice",
  "Three times" = "Three times",
  "Four times" = "Four times"
)

contact_resolved_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Yes, fully" = "Yes, fully",
  "No" = "No",
  "Yes, partially" = "Yes, partially"
)

contact_available_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Yes" = "Yes",
  "No" = "No"
)

contact_how_easy_rating_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Fairly easy" = "Fairly easy",
  "Extremely difficult" = "Extremely difficult",
  "Very easy" = "Very easy",
  "Neither easy nor difficult" = "Neither easy nor difficult",
  "Extremely easy" = "Extremely easy",
  "Very difficult" = "Very difficult",
  "Fairly difficult" = "Fairly difficult"
)

comms_right_amount_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Yes" = "Yes",
  "No, there were too many communications" = "No, there were too many communications",
  "No, there were not enough communications" = "No, there were not enough communications"
)

comms_reqs_met_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Yes" = "Yes",
  "No, I felt some information was missing" = "No, I felt some information was missing",
  "No, I had to contact the support team for further information/advice" = "No, I had to contact the support team for further information/advice"
)

training_ease_rating_responses <- c(
  "Useful" = "Useful",
  "Undisclosed" = "Undisclosed",
  "Not very useful" = "Not very useful",
  "Not at all useful" = "Not at all useful",
  "Very useful" = "Very useful",
  "Extremely useful" = "Extremely useful"
)

organisation_responses <- c(
  "Social Enterprise" = "Social Enterprise",
  "Other organisation type" = "Other organisation type",
  "GP practice" = "GP practice",
  "NHS Trust/Health Board" = "NHS Trust/Health Board",
  "Undisclosed" = "Undisclosed",
  "Local Authority" = "Local Authority",
  "Hospice" = "Hospice",
  "Other NHS organisation" = "Other NHS organisation",
  "Clinical Commissioning Group (CCG)" = "Clinical Commissioning Group (CCG)",
  "Commissioning Support Unit (CSU)" = "Commissioning Support Unit (CSU)",
  "Dental practice" = "Dental practice",
  "Pharmacy" = "Pharmacy"
)

region_responses <- c(
  "East of England" = "East of England",
  "National" = "National",
  "Midlands" = "Midlands",
  "London" = "London",
  "South West" = "South West",
  "South East" = "South East",
  "North East and Yorkshire" = "North East and Yorkshire",
  "Undisclosed" = "Undisclosed",
  "North West" = "North West",
  "Other" = "Other",
  "Wales" = "Wales"
)

role_responses <- c(
  "NHS Jobs Super User" = "NHS Jobs Super User",
  "Recruitment Officer" = "Recruitment Officer",
  "Hiring/Recruiting Manager" = "Hiring/Recruiting Manager",
  "HR Manager or equivalent" = "HR Manager or equivalent",
  "Other job role" = "Other job role",
  "Recruitment Team Manager" = "Recruitment Team Manager",
  "Reviewer/Shortlister" = "Reviewer/Shortlister",
  "Approver" = "Approver",
  "Undisclosed" = "Undisclosed"
)

frequency_used_responses <- c(
  "At least once per day" = "At least once per day",
  "Fortnightly" = "Fortnightly",
  "Quarterly" = "Quarterly",
  "Less frequently than quarterly" = "Less frequently than quarterly",
  "Weekly" = "Weekly",
  "Monthly" = "Monthly",
  "Undisclosed" = "Undisclosed"
)

how_long_used_responses <- c(
  "12 months or more" = "12 months or more",
  "4 to 6 months" = "4 to 6 months",
  "1 month" = "1 month",
  "2 to 3 months" = "2 to 3 months",
  "7 to 11 months" = "7 to 11 months",
  "Undisclosed" = "Undisclosed"
)

system_used_responses <- c(
  "NHS Jobs solely" = "NHS Jobs solely",
  "A combination of NHS Jobs and another system" = "A combination of NHS Jobs and another system",
  "Not sure" = "Not sure"
)

onboarding_aspects_rating_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Good" = "Good",
  "Poor" = "Poor",
  "Fair" = "Fair",
  "Very poor" = "Very poor",
  "Excellent" = "Excellent"
)

challenge_option_responses <- c(
  "challenge_option_completion" = "Completion of the on-boarding form",
  "challenge_option_communication" = "Communications from NHS Jobs",
  "challenge_option_technical" = "Technical issues",
  "challenge_option_other" = "Other",
  "challenge_option_none" = "None"
)

contact_option_responses <- c(
  "contact_option_email" = "Email",
  "contact_option_phone" = "Telephone",
  "contact_option_other" = "Other"
)

contact_not_available_option_responses <- c(
  "contact_not_available_option_hours" = "Hours",
  "contact_not_available_option_on_hold" = "it took too long to get through on the phone?",
  "contact_not_available_option_email" = "you didn't receive a reply to your email?",
  "contact_not_available_option_other" = "Other"
)

contact_aspects_rating_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Completely satisfied 10" = "Completely satisfied 10",
  "8" = "8",
  "5" = "5",
  "7" = "7",
  "9" = "9",
  "6" = "6",
  "Completely dissatisfied 1" = "Completely dissatisfied 1",
  "2" = "2",
  "4" = "4",
  "3" = "3"
)

comms_aspects_rating_responses <- c(
  "Undisclosed" = "Undisclosed",
  "Good" = "Good",
  "Fair" = "Fair",
  "Very poor" = "Very poor",
  "Excellent" = "Excellent",
  "Poor" = "Poor"
)

training_option_responses <- c(
  "training_option_faqs" = "Frequently asked questions (FAQs)",
  "training_option_guides" = "User guides",
  "training_option_videos" = "Videos",
  "training_option_website" = "NHS Jobs training website",
  "training_option_none" = "None of the above"
)

system_used_option_responses <- c(
  "system_used_option_hirelab" = "Hirelab",
  "system_used_option_trac" = "Trac",
  "system_used_option_kalidus" = "Kalidus",
  "system_used_option_broadbean" = "Broadbean",
  "system_used_option_taleo" = "Taleo",
  "system_used_option_oleeo" = "Oleeo",
  "system_used_option_jobtrain" = "Jobtrain",
  "system_used_option_other" = "Other"
)

regions <- names(region_responses)
