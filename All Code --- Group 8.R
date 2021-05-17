#load libraries
library(tidyverse)
library(readxl)
library(RTextTools)
library(arules)
library(arulesViz)
library(caret)
library(tree)
library(class)
library(glmnet)
library(ROCR) 
library(randomForest)
library(e1071)
library(gbm)
library(adabag)
library(ISLR)


#load data files
train_x <- read_csv("airbnb_train_x_2021.csv")
train_y <- read_csv("airbnb_train_y_2021.csv")
test_x <- read_csv("airbnb_test_x_2021.csv")

#external dataset
crime_data<- read_csv("crime_data_w_population_and_crime_rate.csv")

#form the whole conversation for cleaning NAs.
whole <- rbind(train_x,test_x)

#data cleaning
df <- whole %>%
  separate(first_review, c("first_review_Year", "first_review_Month", "first_review_Day"), sep = "-", extra = "drop", remove = FALSE)%>%
  separate(host_since, c("host_since_Year", "host_since_Month", "host_since_Day"), sep = "-", extra = "drop", remove = FALSE)%>%
  mutate(amenities = as.character(gsub("{", "", amenities, fixed = TRUE)),
         amenities = as.character(gsub("}", "", amenities, fixed = TRUE)),
         amenities = as.character(gsub(",", "|", amenities, fixed = TRUE)),
         amenities = as.character(gsub('"', "", amenities, fixed = TRUE)),
         host_verifications = as.character(gsub('[', "", host_verifications, fixed = TRUE)),
         host_verifications = as.character(gsub(']', "", host_verifications, fixed = TRUE)),
         host_verifications = as.character(gsub(",", "|", host_verifications, fixed = TRUE)),
         host_verifications = as.character(gsub("'", "", host_verifications, fixed = TRUE)),
         jurisdiction_names = as.character(gsub('{', "", jurisdiction_names, fixed = TRUE)),
         jurisdiction_names = as.character(gsub('}', "", jurisdiction_names, fixed = TRUE)),
         jurisdiction_names = as.character(gsub('"', "", jurisdiction_names, fixed = TRUE)),
         first_review_Year = as.numeric(first_review_Year),
         first_review_Month = as.numeric(first_review_Month),
         first_review_Day = as.numeric(first_review_Day),
         host_since_Year = as.numeric(host_since_Year),
         host_since_Month = as.numeric(host_since_Month),
         host_since_Day = as.numeric(host_since_Day),
         cleaning_fee = as.numeric(gsub("$", "", cleaning_fee, fixed = TRUE)),
         extra_people = as.numeric(gsub("$", "", extra_people, fixed = TRUE)),
         monthly_price = gsub("$", "", monthly_price, fixed = TRUE),
         monthly_price = as.numeric(gsub(",", "", monthly_price, fixed = TRUE)),
         price = as.numeric(gsub("$", "", price, fixed = TRUE)),
         security_deposit = as.numeric(gsub("$", "", security_deposit, fixed = TRUE)),
         weekly_price = as.numeric(gsub("$", "", weekly_price, fixed = TRUE)),
         host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate, fixed = TRUE)),
         host_response_rate = as.numeric(gsub("%", "", host_response_rate, fixed = TRUE)))
summary(df)

clean<- df%>%
  mutate(
    #Haoyang (access~experiences_offered)
    access = ifelse(is.na(access), "No Information", access),
    accommodates = ifelse(is.na(accommodates), mean(accommodates, na.rm=TRUE), accommodates),
    amenities = ifelse(amenities=="","No Information", amenities),
    availability_30 = ifelse(is.na(availability_30), mean(availability_30, na.rm=TRUE), availability_30),
    availability_365 = ifelse(is.na(availability_365), mean(availability_365, na.rm=TRUE), availability_365),
    availability_60 = ifelse(is.na(availability_60), mean(availability_60 , na.rm=TRUE), availability_60),
    availability_90 = ifelse(is.na(availability_90), mean(availability_90 , na.rm=TRUE), availability_90),
    bathrooms = ifelse(is.na(bathrooms), median(bathrooms, na.rm=TRUE), bathrooms),
    bed_type = ifelse(is.na(bed_type), "No Information", bed_type),
    bedrooms = ifelse(is.na(bedrooms), median(bedrooms, na.rm=TRUE), bedrooms),
    beds = ifelse(is.na(beds), median(beds, na.rm=TRUE), beds),
    cancellation_policy = ifelse(is.na(cancellation_policy), "No Information", cancellation_policy),
    city = ifelse(is.na(city), "No Information", city),
    city_name = ifelse(is.na(city_name), "No Information", city_name),
    cleaning_fee = ifelse(is.na(cleaning_fee), mean(cleaning_fee , na.rm=TRUE), cleaning_fee),
    country = ifelse(is.na(country), "No Information", country),
    country_code = ifelse(is.na(country_code), "No Information", country_code),
    description = ifelse(is.na(description), "No Information", description),
    experiences_offered = ifelse(is.na(experiences_offered), "No Information", experiences_offered),
    experiences_offered = as.factor(experiences_offered),
    
    #Pengju (extra_people~host_since_Month)
    first_review=ifelse(is.na(first_review),"No Information",first_review),
    first_review_Year=ifelse(is.na(first_review_Year),"No Information",first_review_Year),
    first_review_Month=ifelse(is.na(first_review_Month),"No Information",first_review_Month),
    first_review_Day=ifelse(is.na(first_review_Day),"No Information",first_review_Day),
    host_about=ifelse(is.na(host_about),"No Information",host_about),
    host_listings_count=ifelse(is.na(host_listings_count),mean(host_listings_count,na.rm=TRUE),host_listings_count),
    host_location=ifelse(is.na(host_location),"No Information",host_location),
    host_name=ifelse(is.na(host_name),"No Information",host_name),
    host_neighbourhood=ifelse(is.na(host_neighbourhood),"No Information",host_neighbourhood),
    host_response_rate=ifelse(is.na(host_response_rate),mean(host_response_rate,na.rm=TRUE),host_response_rate),
    host_response_time=ifelse(is.na(host_response_time),"No Information",host_response_time),
    host_since=ifelse(is.na(host_since),"No Information",host_since),
    #host_since_Year=ifelse(is.na(host_since_Year),"No Information",host_since_Year),
    host_since_Month=ifelse(is.na(host_since_Month),"No Information",host_since_Month),
    guests_included = ifelse(guests_included < 0, 0, guests_included),
    
    #Junrong's part (price~perfect_score)
    price = ifelse(is.na(price), mean(price, na.rm = TRUE), price),
    property_type = ifelse(is.na(property_type), 'No Information', property_type),
    room_type = ifelse(is.na(room_type), 'No Information', room_type),
    smart_location = ifelse(is.na(smart_location), 'No Information', smart_location),
    space = ifelse(is.na(space), 'No Information', space),
    state = ifelse(is.na(state), 'No Information', state),
    street = ifelse(is.na(street), 'No Information', street),
    summary = ifelse(is.na(summary), 'No Information', summary),
    transit = ifelse(is.na(transit), 'No Information', transit),
    zipcode = ifelse(is.na(zipcode), 'No Information', zipcode),
    security_deposit = ifelse(is.na(security_deposit), mean(security_deposit, na.rm = TRUE), security_deposit),
    host_has_profile_pic = ifelse(is.na(host_has_profile_pic), TRUE, host_has_profile_pic),
    host_identity_verified = ifelse(is.na(host_identity_verified), TRUE, host_identity_verified),
    host_is_superhost = ifelse(is.na(host_is_superhost), FALSE, host_is_superhost),
    instant_bookable = ifelse(is.na(instant_bookable), FALSE, instant_bookable),
    is_location_exact = ifelse(is.na(is_location_exact), TRUE, is_location_exact),
    require_guest_phone_verification = ifelse(is.na(require_guest_phone_verification), FALSE, require_guest_phone_verification),
    require_guest_profile_picture = ifelse(is.na(require_guest_profile_picture), FALSE, require_guest_profile_picture),
    requires_license = ifelse(is.na(requires_license), FALSE, requires_license),
    is_business_travel_ready = ifelse(is.na(is_business_travel_ready), FALSE, is_business_travel_ready),
    host_experience = 2021 - host_since_Year,
    host_experience = ifelse(is.na(host_experience), mean(host_experience, na.rm = TRUE), host_experience),
    market = case_when(market %in% c('$1,100.00', '$2,999.00', '$750.00', 'Adirondacks', 'Agra', 'Atlanta', 'Bristol', 'Carlsbad', 'Catskills and Hudson Valley',
                                     'Chico', 'Coastal Orange County', 'College Station','Cuba', 'Dallas', 'Flims',
                                     'Fontana', 'Fresno','Houston', 'Indianapolis', 'Jamaica South Coast','Lagos, NG', 'Las Vegas','London',
                                     'Malibu', 'Miami','Monterey Region', 'Nice', 'No Information', 'North Carolina Mountains',
                                     'Oregon Coast', 'Other (Domestic)', 'Other (International)', 'Palm Springs Desert', 'Paris', 'Philadelphia',
                                     'Pittsburg', 'Portland, Maine', 'Providence', 'San Antonio, US', 'South Bay, CA', 'South Florida Gulf Coast',
                                     'Temecula Valley', 'Toronto', 'Tuscany Countryside', 'Umbria Countryside', 'Venice') ~ 'Others',
                       TRUE ~ market),
    
    
    #Jinru (host_since_Day ~notes)
    
    host_since_Day = ifelse(is.na(host_since_Day ),mean(host_since_Day ,na.rm= TRUE),  host_since_Day),
    host_total_listings_count =  ifelse(is.na( host_total_listings_count),mean( host_total_listings_count,na.rm= TRUE),host_total_listings_count),
    host_verifications=  ifelse(is.na(host_verifications), 'No Information', host_verifications),
    house_rules =  ifelse(is.na(house_rules), 'No Information', house_rules),
    instant_bookable =   ifelse(is.na(instant_bookable), 'No Information', instant_bookable),
    is_location_exact = ifelse(is.na(is_location_exact), 'No Information',is_location_exact ),
    latitude = ifelse(is.na( latitude ),mean(latitude  ,na.rm= TRUE),latitude ),
    longitude = ifelse(is.na(longitude ),mean(longitude ,na.rm= TRUE),longitude),
    market = ifelse(is.na(market), 'No Information', market),
    maximum_nights =  ifelse(is.na(maximum_nights ),mean(maximum_nights ,na.rm= TRUE),maximum_nights),
    minimum_nights =  ifelse(is.na(minimum_nights ),mean( minimum_nights,na.rm= TRUE),minimum_nights),
    name = ifelse(is.na(name), 'No Information',name ),
    neighborhood_overview = ifelse(is.na(neighborhood_overview), 'No Information', neighborhood_overview),
    neighbourhood = ifelse(is.na(neighbourhood), 'No Information', neighbourhood),
    interaction= ifelse(is.na(interaction), 'No Information',interaction),
    
    #property_type modification
    
    property_type = ifelse(property_type == "Aparthotel", "Other", property_type),
    property_type = ifelse(property_type == "Barn", "Other", property_type),
    property_type = ifelse(property_type == "Bed & Breakfast", "Bed and breakfast", property_type),
    property_type = ifelse(property_type == "Boat", "Other", property_type),
    property_type = ifelse(property_type == "Boutique hotel", "Other", property_type),
    property_type = ifelse(property_type == "Camper/RV", "Other", property_type),
    property_type = ifelse(property_type == "Casa particular (Cuba)", "Other", property_type),
    property_type = ifelse(property_type == "Castle", "Other", property_type),
    property_type = ifelse(property_type == "Cave", "Other", property_type),
    property_type = ifelse(property_type == "Chalet", "Other", property_type),
    property_type = ifelse(property_type == "Cottage", "Other", property_type),
    property_type = ifelse(property_type == "Earth house", "Other", property_type),
    property_type = ifelse(property_type == "Earth House", "Other", property_type),
    property_type = ifelse(property_type == "Farm stay", "Other", property_type),
    property_type = ifelse(property_type == "Hostel", "Other", property_type),
    property_type = ifelse(property_type == "Hotel", "Other", property_type),
    property_type = ifelse(property_type == "Hut", "Other", property_type),
    property_type = ifelse(property_type == "In-law", "Other", property_type),
    property_type = ifelse(property_type == "Island", "Other", property_type),
    property_type = ifelse(property_type == "Lighthouse", "Other", property_type),
    property_type = ifelse(property_type == "Nature lodge", "Other", property_type),
    property_type = ifelse(property_type == "Plane", "Other", property_type),
    property_type = ifelse(property_type == "Resort", "Other", property_type),
    property_type = ifelse(property_type == "Serviced apartment", "Other", property_type),
    property_type = ifelse(property_type == "Tent", "Other", property_type),
    property_type = ifelse(property_type == "Timeshare", "Other", property_type),
    property_type = ifelse(property_type == "Tiny house", "Other", property_type),
    property_type = ifelse(property_type == "Tipi", "Other", property_type),
    property_type = ifelse(property_type == "Train", "Other", property_type),
    property_type = ifelse(property_type == "Treehouse", "Other", property_type),
    property_type = ifelse(property_type == "Vacation home", "Other", property_type),
    property_type = ifelse(property_type == "Yurt", "Other", property_type),
    state = ifelse(state == "ca", "CA", state),
    state = ifelse(state == "Ca", "CA", state),
    state = ifelse(state == "il", "IL", state),
    state = ifelse(state == "New York", "NY", state),
    state = ifelse(state == "ny", "NY", state),
    state = ifelse(state == "Ny", "NY", state),
    country_code = case_when(country_code %in% c('f', 'MX', 'No Information', 'UY') ~ 'Others',
                             TRUE ~ country_code),
    bed_type = case_when(bed_type %in% c('100%', '81%', 'Couch', 'No Information') ~ 'Coach and Others',
                         TRUE ~ bed_type),
    cancellation_policy = case_when(
      cancellation_policy %in% c('strict', 'super_strict_30', 'super_strict_60', 'no_refunds', '1.0', '2.0', '5.0') ~ 'Strict',
      TRUE ~ cancellation_policy),
    host_response_time = case_when(
      host_response_time %in% c('f', 'No Information') ~ 'No Information',
      TRUE ~ host_response_time
    )
  ) %>%
  mutate(bed_type = as.factor(bed_type),
         cancellation_policy = as.factor(cancellation_policy),
         city = as.factor(city),
         city_name = as.factor(city_name),
         country = as.factor(country),
         country_code = as.factor(country_code),
         experiences_offered = as.factor(experiences_offered),
         first_review_Year = as.factor(first_review_Year),
         first_review_Day = as.factor(first_review_Day),
         host_response_rate = host_response_rate/100,
         property_type = as.factor(property_type),
         room_type = as.factor(room_type),
         state = as.factor(state),
         zipcode = as.factor(zipcode),
         first_review_Month = as.factor(first_review_Month),
         neighbourhood = as.factor(neighbourhood),
         host_location = as.factor(host_location),
         market = as.factor(market),
         host_response_time = as.factor(host_response_time)
  )


#character NA detection
table(clean$access=="No Information")
table(clean$amenities=="No Information")
table(clean$bed_type=="No Information")
table(clean$cancellation_policy=="No Information")
table(clean$city=="No Information")
table(clean$city_name=="No Information")
table(clean$country=="No Information")
table(clean$country_code=="No Information")
table(clean$description=="No Information")
table(clean$experiences_offered=="No Information")
table(clean$first_review=="No Information")
table(clean$first_review_Year=="No Information")
table(clean$first_review_Month=="No Information")
table(clean$first_review_Day=="No Information")
table(clean$host_about=="No Information")
table(clean$host_location=="No Information")
table(clean$host_name=="No Information")
table(clean$host_neighbourhood=="No Information")
table(clean$host_response_time=="No Information")
table(clean$host_since=="No Information")
table(clean$host_since_Year=="No Information")
table(clean$host_since_Month=="No Information")
table(clean$property_type=="No Information")
table(clean$require_guest_phone_verification=="No Information")
table(clean$require_guest_profile_picture=="No Information")
table(clean$requires_license=="No Information")
table(clean$room_type=="No Information")
table(clean$smart_location=="No Information")
table(clean$space=="No Information")
table(clean$state=="No Information")
table(clean$street=="No Information")
table(clean$summary=="No Information")
table(clean$transit=="No Information")
table(clean$zipcode=="No Information")
table(clean$host_verifications=="No Information")
table(clean$house_rules=="No Information")
table(clean$instant_bookable=="No Information")
table(clean$interaction=="No Information")
table(clean$is_location_exact=="No Information")
table(clean$jurisdiction_names=="No Information")
table(clean$license=="No Information")
table(clean$market=="No Information")
table(clean$name=="No Information")
table(clean$neighborhood_overview=="No Information")
table(clean$neighborhood=="No Information")
table(clean$notes=="No Information")

######################################################################################################
#external data processed

#Crime Cleaning and prepared for merging
use<-dplyr::select (crime_data,county_name,crime_rate_per_100000,MURDER,RAPE,ROBBERY,BURGLRY,LARCENY)

use_new<-use%>%
  separate(county_name, c("county", "state"), sep = ",", extra = "drop", remove = FALSE)

use_new<-dplyr::select(use_new,state,crime_rate_per_100000,MURDER,RAPE,ROBBERY,BURGLRY,LARCENY)
use_new<-use_new%>%
  group_by(state)%>%
  summarise(crime_rate_per_100000_avg=mean(crime_rate_per_100000),
            MURDER_avg=mean(MURDER),
            RAPE_avg=mean(RAPE),
            ROBBERY_avg=mean(ROBBERY),
            BURGLRY_avg=mean(BURGLRY),
            LARCENY_avg=mean(LARCENY))

added_crime <- clean%>%
  mutate(crime= round(case_when(state == 'AK'~ 289.32096,
                                state == 'AL'~ 385.64569,
                                state == 'AR'~ 334.75073,
                                state == 'AZ'~ 352.62944,
                                state == 'CA'~ 416.07795,
                                state == 'CO'~ 181.82463,
                                state == 'CT'~ 180.52095,
                                state == 'DC'~ 1216.80132,
                                state == 'DE'~ 524.42467,
                                state == 'FL'~ 412.93870,
                                state == 'GA'~ 295.55260,
                                state == 'HI'~ 203.36381,
                                state == 'IA'~ 168.58334,
                                state == 'ID'~ 156.56106,
                                state == 'IL'~ 242.78788,
                                state == 'IN'~ 139.11895,
                                state == 'KS'~ 237.48037,
                                state == 'KY'~ 98.71790,
                                state == 'LA'~ 397.67840,
                                state == 'MA'~ 392.38127,
                                state == 'MD'~ 378.32896,
                                state == 'ME'~ 115.95718,
                                state == 'MI'~ 244.08254,
                                state == 'MN'~ 134.41578,
                                state == 'MO'~ 270.20933,
                                state == 'MS'~ 146.21103,
                                state == 'MT'~ 182.74077,
                                state == 'NC'~ 269.39015,
                                state == 'ND'~ 94.47706,
                                state == 'NE'~ 61.62639,
                                state == 'NH'~ 160.15512,
                                state == 'NJ'~ 270.73772,
                                state == 'NM'~ 427.22243,
                                state == 'NV'~ 434.95635,
                                state == 'NY'~ 212.16516,
                                state == 'OH'~ 138.13699,
                                state == 'OK'~ 253.93899,
                                state == 'OR'~ 166.66517,
                                state == 'PA'~ 222.14179,
                                state == 'RI'~ 157.57772,
                                state == 'SC'~ 581.13738,
                                state == 'SD'~ 99.38780,
                                state == 'TN'~ 401.28835,
                                state == 'TX'~ 255.00282,
                                state == 'UT'~ 145.26061,
                                state == 'VA'~ 173.34045,
                                state == 'VT'~ 173.34045,
                                state == 'WA'~ 205.44983,
                                state == 'WI'~ 142.59952,
                                state == 'WV'~ 252.77419,
                                state == 'WY'~ 155.00118,
                                TRUE ~ as.numeric(state))),
         
         
         murder= round(case_when(state == 'AK'~ 0.9130435,
                                 state == 'AL'~ 5.0895522,
                                 state == 'AR'~ 2.3066667,
                                 state == 'AZ'~ 21.6666667,
                                 state == 'CA'~ 32.3965517,
                                 state == 'CO'~ 2.3750000,
                                 state == 'CT'~ 13.6250000,
                                 state == 'DC'~ 88.0000000,
                                 state == 'DE'~ 18.6666667,
                                 state == 'FL'~ 15.0597015,
                                 state == 'GA'~ 3.6666667,
                                 state == 'HI'~ 4.2000000,
                                 state == 'IA'~ 0.4949495,
                                 state == 'ID'~ 0.5681818,
                                 state == 'IL'~ 7.5294118,
                                 state == 'IN'~ 3.3913043,
                                 state == 'KS'~ 0.8095238,
                                 state == 'KY'~ 1.5083333,
                                 state == 'LA'~ 7.6093750,
                                 state == 'MA'~ 8.7142857,
                                 state == 'MD'~ 15.5416667,
                                 state == 'ME'~ 1.6250000,
                                 state == 'MI'~ 8.4337349,
                                 state == 'MN'~ 1.1264368,
                                 state == 'MO'~ 3.3913043,
                                 state == 'MS'~ 2.2926829,
                                 state == 'MT'~ 0.3928571,
                                 state == 'NC'~ 4.7200000,
                                 state == 'ND'~ 0.2641509,
                                 state == 'NE'~ 0.5591398,
                                 state == 'NH'~ 1.5000000,
                                 state == 'NJ'~ 18.4761905,
                                 state == 'NM'~ 3.3437500,
                                 state == 'NV'~ 6.8823529,
                                 state == 'NY'~ 11.0000000,
                                 state == 'OH'~ 5.4318182,
                                 state == 'OK'~ 2.8051948,
                                 state == 'OR'~ 2.4444444,
                                 state == 'PA'~ 10.5522388,
                                 state == 'RI'~ 7.0000000,
                                 state == 'SC'~ 7.2391304,
                                 state == 'SD'~ 0.2272727,
                                 state == 'TN'~ 4.1789474,
                                 state == 'TX'~ 4.5196850,
                                 state == 'UT'~ 1.7586207,
                                 state == 'VA'~ 2.4029851,
                                 state == 'VT'~ 0.5714286,
                                 state == 'WA'~ 5.4102564,
                                 state == 'WI'~ 2.3333333,
                                 state == 'WV'~ 1.2727273,
                                 state == 'WY'~ 0.6086957,
                                 TRUE ~ as.numeric(state))),
         
         
         rape= round(case_when(state == 'AK'~ 19.086957,
                               state == 'AL'~ 19.149254,
                               state == 'AR'~ 16.253333,
                               state == 'AZ'~ 115.000000,
                               state == 'CA'~ 134.931034,
                               state == 'CO'~ 32.906250,
                               state == 'CT'~ 109.500000,
                               state == 'DC'~ 236.000000,
                               state == 'DE'~ 83.000000,
                               state == 'FL'~ 78.373134,
                               state == 'GA'~ 13.320755,
                               state == 'HI'~ 55.800000,
                               state == 'IA'~ 8.878788,
                               state == 'ID'~ 10.772727,
                               state == 'IL'~ 27.196078,
                               state == 'IN'~ 16.902174,
                               state == 'KS'~ 10.552381,
                               state == 'KY'~ 10.741667,
                               state == 'LA'~ 17.296875,
                               state == 'MA'~ 115.928571,
                               state == 'MD'~ 51.541667,
                               state == 'ME'~ 23.250000,
                               state == 'MI'~ 54.867470,
                               state == 'MN'~ 23.701149,
                               state == 'MO'~ 13.278261,
                               state == 'MS'~ 8.109756,
                               state == 'MT'~ 6.017857,
                               state == 'NC'~ 19.370000,
                               state == 'ND'~ 4.716981,
                               state == 'NE'~ 7.150538,
                               state == 'NH'~ 42.100000,
                               state == 'NJ'~ 49.238095,
                               state == 'NM'~ 25.156250,
                               state == 'NV'~ 54.352941,
                               state == 'NY'~ 45.596774,
                               state == 'OH'~ 41.795455,
                               state == 'OK'~ 20.948052,
                               state == 'OR'~ 31.638889,
                               state == 'PA'~ 50.283582,
                               state == 'RI'~ 58.400000,
                               state == 'SC'~ 37.152174,
                               state == 'SD'~ 7.181818,
                               state == 'TN'~ 21.494737,
                               state == 'TX'~ 30.248031,
                               state == 'UT'~ 33.137931,
                               state == 'VA'~ 11.223881,
                               state == 'VT'~ 9.142857,
                               state == 'WA'~ 55.897436,
                               state == 'WI'~ 16.736111,
                               state == 'WV'~ 7.236364,
                               state == 'WY'~ 6.391304,
                               TRUE ~ as.numeric(state))),
         
         
         robbery= round(case_when(state == 'AK'~ 25.695652,
                                  state == 'AL'~ 74.447761,
                                  state == 'AR'~ 30.693333,
                                  state == 'AZ'~ 484.200000,
                                  state == 'CA'~ 973.862069,
                                  state == 'CO'~ 52.687500,
                                  state == 'CT'~ 454.500000,
                                  state == 'DC'~ 4037.000000,
                                  state == 'DE'~ 499.333333,
                                  state == 'FL'~ 355.761194,
                                  state == 'GA'~ 77.886792,
                                  state == 'HI'~ 225.000000,
                                  state == 'IA'~ 9.646465,
                                  state == 'ID'~ 5.386364,
                                  state == 'IL'~ 189.303922,
                                  state == 'IN'~ 70.423913,
                                  state == 'KS'~ 14.209524,
                                  state == 'KY'~ 28.850000,
                                  state == 'LA'~ 83.203125,
                                  state == 'MA'~ 462.214286,
                                  state == 'MD'~ 423.875000,
                                  state == 'ME'~ 26.250000,
                                  state == 'MI'~ 125.072289,
                                  state == 'MN'~ 39.655172,
                                  state == 'MO'~ 50.269565,
                                  state == 'MS'~ 24.426829,
                                  state == 'MT'~ 3.553571,
                                  state == 'NC'~ 92.450000,
                                  state == 'ND'~ 2.207547,
                                  state == 'NE'~ 12.064516,
                                  state == 'NH'~ 45.800000,
                                  state == 'NJ'~ 541.333333,
                                  state == 'NM'~ 54.093750,
                                  state == 'NV'~ 289.000000,
                                  state == 'NY'~ 461.048387,
                                  state == 'OH'~ 170.931818,
                                  state == 'OK'~ 41.662338,
                                  state == 'OR'~ 67.277778,
                                  state == 'PA'~ 234.820896,
                                  state == 'RI'~ 143.000000,
                                  state == 'SC'~ 97.586957,
                                  state == 'SD'~ 2.136364,
                                  state == 'TN'~ 85.757895,
                                  state == 'TX'~ 119.562992,
                                  state == 'UT'~ 36.862069,
                                  state == 'VA'~ 35.201493,
                                  state == 'VT'~ 8.000000,
                                  state == 'WA'~ 147.564103,
                                  state == 'WI'~ 64.875000,
                                  state == 'WV'~ 14.436364,
                                  state == 'WY'~ 2.608696,
                                  TRUE ~ as.numeric(state))),
         
         
         burglry= round(case_when(state == 'AK'~ 77.43478,
                                  state == 'AL'~ 700.41791,
                                  state == 'AR'~ 430.66667,
                                  state == 'AZ'~ 3365.73333,
                                  state == 'CA'~ 4233.75862,
                                  state == 'CO'~ 403.06250,
                                  state == 'CT'~ 1680.87500,
                                  state == 'DC'~ 3519.00000,
                                  state == 'DE'~ 2463.33333,
                                  state == 'FL'~ 2287.49254,
                                  state == 'GA'~ 540.17610,
                                  state == 'HI'~ 1530.60000,
                                  state == 'IA'~ 171.19192,
                                  state == 'ID'~ 163.36364,
                                  state == 'IL'~ 680.11765,
                                  state == 'IN'~ 486.30435,
                                  state == 'KS'~ 179.26667,
                                  state == 'KY'~ 244.02500,
                                  state == 'LA'~ 634.92188,
                                  state == 'MA'~ 2426.28571,
                                  state == 'MD'~ 1394.58333,
                                  state == 'ME'~ 465.68750,
                                  state == 'MI'~ 781.02410,
                                  state == 'MN'~ 285.49425,
                                  state == 'MO'~ 369.78261,
                                  state == 'MS'~ 270.75610,
                                  state == 'MT'~ 65.42857,
                                  state == 'NC'~ 972.56000,
                                  state == 'ND'~ 42.39623,
                                  state == 'NE'~ 89.62366,
                                  state == 'NH'~ 492.60000,
                                  state == 'NJ'~ 2015.76190,
                                  state == 'NM'~ 596.50000,
                                  state == 'NV'~ 1295.41176,
                                  state == 'NY'~ 1033.32258,
                                  state == 'OH'~ 1145.94318,
                                  state == 'OK'~ 464.48052,
                                  state == 'OR'~ 601.47222,
                                  state == 'PA'~ 851.00000,
                                  state == 'RI'~ 1185.80000,
                                  state == 'SC'~ 979.21739,
                                  state == 'SD'~ 43.12121,
                                  state == 'TN'~ 591.06316,
                                  state == 'TX'~ 806.44488,
                                  state == 'UT'~ 441.13793,
                                  state == 'VA'~ 221.14179,
                                  state == 'VT'~ 296.14286,
                                  state == 'WA'~ 1549.15385,
                                  state == 'WI'~ 380.06944,
                                  state == 'WV'~ 201.98182,
                                  state == 'WY'~ 89.26087,
                                  TRUE ~ as.numeric(state))),
         
         
         larceny= round(case_when(state == 'AK'~ 580.2609,
                                  state == 'AL'~ 1641.0299,
                                  state == 'AR'~ 937.6933,
                                  state == 'AZ'~ 8699.1333,
                                  state == 'CA'~ 10940.5172,
                                  state == 'CO'~ 1551.4844,
                                  state == 'CT'~ 6599.2500,
                                  state == 'DC'~ 23575.0000,
                                  state == 'DE'~ 7294.0000,
                                  state == 'FL'~ 6573.6866,
                                  state == 'GA'~ 1387.0755,
                                  state == 'HI'~ 6380.2000,
                                  state == 'IA'~ 484.3535,
                                  state == 'ID'~ 524.0227,
                                  state == 'IL'~ 2245.0588,
                                  state == 'IN'~ 1388.0326,
                                  state == 'KS'~ 618.2190,
                                  state == 'KY'~ 624.9333,
                                  state == 'LA'~ 1690.2969,
                                  state == 'MA'~ 6969.2857,
                                  state == 'MD'~ 4739.1667,
                                  state == 'ME'~ 1552.9375,
                                  state == 'MI'~ 1875.1928,
                                  state == 'MN'~ 1175.0575,
                                  state == 'MO'~ 1225.4870,
                                  state == 'MS'~ 519.5854,
                                  state == 'MT'~ 358.3036,
                                  state == 'NC'~ 2079.7000,
                                  state == 'ND'~ 194.1698,
                                  state == 'NE'~ 395.3763,
                                  state == 'NH'~ 2237.9000,
                                  state == 'NJ'~ 5819.2381,
                                  state == 'NM'~ 1343.9375,
                                  state == 'NV'~ 2659.7059,
                                  state == 'NY'~ 4707.2742,
                                  state == 'OH'~ 2536.9318,
                                  state == 'OK'~ 1061.9740,
                                  state == 'OR'~ 2575.0000,
                                  state == 'PA'~ 3048.9851,
                                  state == 'RI'~ 3695.0000,
                                  state == 'SC'~ 2648.6739,
                                  state == 'SD'~ 182.1515,
                                  state == 'TN'~ 1562.9895,
                                  state == 'TX'~ 2385.1535,
                                  state == 'UT'~ 2258.3793,
                                  state == 'VA'~ 1041.1194,
                                  state == 'VT'~ 781.0714,
                                  state == 'WA'~ 4209.0513,
                                  state == 'WI'~ 1419.1944,
                                  state == 'WV'~ 517.4727,
                                  state == 'WY'~ 443.3913,
                                  TRUE ~ as.numeric(state)))
  )





######################################################################################################
#############################################################
#interactive term
# add_column : 1.price*minimum nights, 2. guest response time*price , 3. cleaning fee/guest_included
# only can add in logistic :1:  market*price  2: room type*price 3: property type*price 4: state*price , 5: cleaning fee*state 6: cleaning fee*market, 7: cleaning fee*room type
add_column_old<-dplyr::select(clean,price, minimum_nights,cleaning_fee,guests_included,security_deposit,bedrooms,beds,host_response_time,host_response_rate,bed_type)
add_column_old$price_with_minimum_nights<-add_column_old$price*add_column_old$minimum_nights
add_column_old$cleaning_fee_with_guest_included<-add_column_old$cleaning_fee*add_column_old$guests_included
add_column_old$deposit_cost_per_night<-add_column_old$security_deposit/add_column_old$minimum_nights
add_column_old$deposit_cost_per_night[is.infinite(add_column_old$deposit_cost_per_night)]<-500
add_column_old$money_of_return<-add_column_old$security_deposit-add_column_old$cleaning_fee
add_column_old$potential_cost<-add_column_old$price+add_column_old$security_deposit
add_column_old$price_per_bedrooms<-add_column_old$price/add_column_old$bedrooms
add_column_old$price_per_bedrooms[is.infinite(add_column_old$price_per_bedrooms)]<-91
add_column_old$price_per_bedrooms[is.na(add_column_old$price_per_bedrooms)]<-91
add_column_old$price_per_beds<-add_column_old$price/add_column_old$beds
add_column_old$price_per_beds[is.infinite(add_column_old$price_per_beds)]<-75

############## know the price interactives##############
need_group_by_time<-dplyr::select(add_column_old,host_response_time,price)
need_group_by_rate<-dplyr::select(add_column_old,host_response_rate,price)
need_group_by_rate$host_response_rate<-ifelse(need_group_by_rate$host_response_rate>=0.95,"care",need_group_by_rate$host_response_rate)
need_group_by_rate$host_response_rate<-ifelse(need_group_by_rate$host_response_rate<0.95,"uncare",need_group_by_rate$host_response_rate)
need_group_by_bed<-dplyr::select(add_column_old,bed_type,price)
need_group_by_time<-need_group_by_time%>%group_by(host_response_time)%>%summarise(host_response_time_Avgprice=mean(price))
need_group_by_rate<-need_group_by_rate%>%group_by(host_response_rate)%>%summarise(host_response_rate_Avgprice=mean(price))
need_group_by_bed<-need_group_by_bed%>%group_by(bed_type)%>%summarise(host_response_bed_Avgprice=mean(price))

add_column_old<-add_column_old%>%
  mutate(need_group_by_time_with_price= round(case_when(host_response_time=="a few days or more"~141.4122,
                                                        host_response_time=="No Information"~139.2426,
                                                        host_response_time=="within a day"~154.3844,
                                                        host_response_time=="within a few hours"~154.3375,
                                                        host_response_time=="within an hour"~142.2460,
                                                        TRUE ~ as.numeric(host_response_time))),
         need_group_by_rate_with_price=case_when( host_response_rate>=0.95~1,
                                                  host_response_rate<0.95~0,
                                                  TRUE ~ as.numeric(host_response_rate)),
         need_group_by_bed_with_price=case_when(bed_type=="Real Bed"~146.67674,
                                                bed_type=="Not a Real Bed"~86.12718,
                                                TRUE ~ as.numeric(bed_type)))

##new column
add_column_new<-dplyr::select(add_column_old,cleaning_fee_with_guest_included,
                              price_with_minimum_nights,
                              deposit_cost_per_night,
                              money_of_return,
                              potential_cost,
                              price_per_bedrooms,
                              price_per_beds,
                              need_group_by_time_with_price,
                              need_group_by_rate_with_price,
                              need_group_by_bed_with_price )

###################################################################################################################################################
#Text mining
###################################################################################################################################################
#amenities
amenities<-df$amenities
amenities<-data.frame(amenities)
amenities<-amenities%>%
  mutate(amenities = as.character(gsub("{", "", amenities, fixed = TRUE)),
         amenities = as.character(gsub("}", "", amenities, fixed = TRUE)),
         amenities = as.character(gsub(",", "|", amenities, fixed = TRUE)),
         amenities = as.character(gsub('"', " ", amenities, fixed = TRUE)),
         amenities = as.character(gsub("|", " ", amenities, fixed = TRUE)))

amenities$amenities<-ifelse(amenities$amenities=="","No Information",amenities$amenities)
old_dtm_amenities<-create_matrix(amenities$amenities,
                                 language="english", 
                                 removeSparseTerms = 0.95,  ##only keep terms that are less than 95% sparse
                                 stripWhitespace=TRUE, 
                                 toLower=TRUE)
add_new_amenities<- data.frame(as.matrix(old_dtm_amenities), stringsAsFactors = FALSE)
#table(add_new_amenities)
summary(add_new_amenities$c)

amenities_text<-dplyr::select(add_new_amenities, conditioning, X24hour,hair,parking,internet)

table(amenities_text$conditioning)
table(amenities_text$ X24hour)
table(amenities_text$ hair)
table(amenities_text$ parking)
table(amenities_text$ internet)

amenities_text<-amenities_text%>%
  mutate(conditioning = as.numeric(gsub("2", "1", conditioning, fixed = TRUE)),
         parking = as.numeric(gsub("2", "1", parking, fixed = TRUE)),
         parking = as.numeric(gsub("3", "1", parking, fixed = TRUE)),
         parking = as.numeric(gsub("4", "1", parking, fixed = TRUE)),
         parking = as.numeric(gsub("5", "1", parking, fixed = TRUE)),
         internet = as.numeric(gsub("2", "1", internet, fixed = TRUE))
  )

write.csv(x = amenities_text,file = "amenities_text.csv")
###########################################################################################################
# house rules

house_rules<-df$house_rules
house_rules<-data.frame(house_rules)
house_rules$house_rules[is.na(house_rules$house_rules)]<-"No Information"
old_dtm_house_rules<-create_matrix(house_rules$house_rules,
                                   language="english", 
                                   removeSparseTerms = 0.96,  ##only keep terms that are less than 96% sparse
                                   stripWhitespace=TRUE, 
                                   toLower=TRUE)
add_new_house_rules<- data.frame(as.matrix(old_dtm_house_rules), stringsAsFactors = FALSE)  
#table(add_new_house_rules)
summary(add_new_house_rules)

# house rules

house_rules_text_old<-dplyr::select(add_new_house_rules,quiet, respectful,respect,smoking,smoke)
house_rules_text_old<-house_rules_text_old %>% mutate(manner=quiet+respectful+respect,
                                                      non_smoke=smoking+smoke)

house_rules_text<-dplyr::select(house_rules_text_old,manner,non_smoke)

table(house_rules_text$ manner)
table(house_rules_text$ non_smoke)

house_rules_text<-house_rules_text%>%mutate(manner= as.numeric(gsub("2", "1", manner, fixed = TRUE)),
                                            manner= as.numeric(gsub("3", "1", manner, fixed = TRUE)),
                                            manner= as.numeric(gsub("4", "1", manner, fixed = TRUE)),
                                            manner= as.numeric(gsub("5", "1", manner, fixed = TRUE)),
                                            manner= as.numeric(gsub("6", "1", manner, fixed = TRUE)),
                                            manner= as.numeric(gsub("7", "1", manner, fixed = TRUE)),
                                            manner= as.numeric(gsub("8", "1", manner, fixed = TRUE)),
                                            manner= as.numeric(gsub("9", "1", manner, fixed = TRUE)),
                                            non_smoke=as.numeric(gsub("2","1",non_smoke,fixed = TRUE)),
                                            non_smoke=as.numeric(gsub("3","1",non_smoke,fixed = TRUE)),
                                            non_smoke=as.numeric(gsub("4","1",non_smoke,fixed = TRUE)),
                                            non_smoke=as.numeric(gsub("5","1",non_smoke,fixed = TRUE)),
                                            non_smoke=as.numeric(gsub("6","1",non_smoke,fixed = TRUE)),
                                            non_smoke=as.numeric(gsub("7","1",non_smoke,fixed = TRUE)),
                                            non_smoke=as.numeric(gsub("8","1",non_smoke,fixed = TRUE)),
)

write.csv(x = house_rules_text,file = "house_rules_text.csv")

##################################################################################################################
#  interaction
interaction<-df$interaction
interaction<-data.frame(interaction)
interaction$interaction[is.na(interaction$interaction)]<-"No Information"
old_dtm_interaction<-create_matrix(interaction$interaction,
                                   language="english", 
                                   removeSparseTerms = 0.96,  ##only keep terms that are less than 96% sparse
                                   stripWhitespace=TRUE, 
                                   toLower=TRUE)
add_new_interaction<- data.frame(as.matrix(old_dtm_interaction), stringsAsFactors = FALSE) 
#table(add_new_interaction)
summary(add_new_interaction)


interaction_text_old<-dplyr::select(add_new_interaction,text,call,email,available,feel,need,needed,guest,guests)
interaction_text_old<-interaction_text_old%>% mutate(friendly=text+call+email+available+feel+need+needed+guest+guests)
interaction_text<-dplyr::select(interaction_text_old,friendly)
table(interaction_text$friendly)

interaction_text<-interaction_text%>% mutate( friendly= as.numeric(gsub("2","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("3","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("4","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("5","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("6","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("7","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("8","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("9","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("10","1",friendly,fixed=TRUE)),
                                              friendly= as.numeric(gsub("11","1",friendly,fixed=TRUE)),
)
write.csv(x = interaction_text,file = "interaction_text.csv")

################################################################################################
# neighborhood_overview

neighborhood_overview<-df$neighborhood_overview
neighborhood_overview<-data.frame(neighborhood_overview)
neighborhood_overview$neighborhood_overview[is.na(neighborhood_overview$neighborhood_overview)]<-"No Information"
old_dtm_neighborhood_overview<-create_matrix(neighborhood_overview$neighborhood_overview,
                                             language="english", 
                                             removeSparseTerms = 0.97,  ##only keep terms that are less than 97% sparse
                                             stripWhitespace=TRUE, 
                                             toLower=TRUE)
add_new_neighborhood_overview<- data.frame(as.matrix(old_dtm_neighborhood_overview), stringsAsFactors = FALSE) 

summary(add_new_neighborhood_overview)

neighborhood_overview_old<-dplyr::select(add_new_neighborhood_overview,bar,bars,food,restaurant,restaurants,cafes,coffee, bus,subway,train,walk,walking,short,park,parks)
neighborhood_overview_old<-neighborhood_overview_old%>%mutate( easy_food=bar+bars+food+restaurant+restaurants+cafes+coffee,
                                                               easy_traffic=bus+subway+train+walk+walking+short+park+parks)
neighborhood_overview_text<-dplyr::select(neighborhood_overview_old,easy_food,easy_traffic)
table(neighborhood_overview_text$easy_food)
table(neighborhood_overview_text$easy_traffic)

neighborhood_overview_text<-neighborhood_overview_text%>% mutate( 
  easy_food= as.numeric(gsub("2","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("3","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("4","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("5","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("6","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("7","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("8","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("9","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("10","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("11","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("12","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("13","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("14","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("15","1",easy_food,fixed=TRUE)),
  easy_food= as.numeric(gsub("17","1",easy_food,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("2","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("3","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("4","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("5","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("6","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("7","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("8","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("9","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("10","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("11","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("12","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("13","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("14","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("15","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("16","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("17","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("18","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("19","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("20","1",easy_traffic,fixed=TRUE)),
  easy_traffic=as.numeric(gsub("22","1",easy_traffic,fixed=TRUE)),
)


write.csv(x = neighborhood_overview_text,file = "neighborhood_overview_text.csv")
############################################################################################################
#add all texts selected
amenities_text<-read_csv("amenities_text.csv")
house_rules_text<-read_csv("house_rules_text.csv")
interaction_text<-read_csv("interaction_text.csv")
neighborhood_overview_text<-read_csv("neighborhood_overview_text.csv")

all<-cbind(amenities_text,house_rules_text,interaction_text,neighborhood_overview_text)
all<-dplyr::select(all,-X1)

write.csv(x = all,file = "all_text.csv")


#####################################################################################

#combine all dataframe (text+ clean+crime+interactive)
text_data<- read.csv("all_text.csv")
final_clean<-cbind(added_crime,text_data)
final_clean$certain_crime_percent_effect <- (final_clean$murder +final_clean$rape+final_clean$robbery)/(final_clean$murder+final_clean$rape+final_clean$robbery+final_clean$burglry+final_clean$larceny)/final_clean$host_listings_count*100
summary(final_clean$certain_crime_percent_effect)

final_clean$certain_crime_percent_effect[is.infinite(final_clean$certain_crime_percent_effect)]<-4.309500
final_clean<-cbind(final_clean,add_column_new)


final_clean<-cbind(final_clean,crime_all)

######################################################################################################


#choose variables 
selected_clean<-final_clean%>%
  select(accommodates,availability_30,availability_365,availability_60,availability_90,bathrooms,bed_type,bedrooms,beds,cancellation_policy,
         cleaning_fee,extra_people,first_review_Month,guests_included,host_identity_verified,host_is_superhost,
         host_listings_count,host_response_rate,host_response_time,host_experience, instant_bookable,
         is_business_travel_ready,is_location_exact,latitude,longitude,market,maximum_nights,minimum_nights,price,property_type,
         require_guest_phone_verification,require_guest_profile_picture,requires_license,room_type,security_deposit,state, certain_crime_percent_effect,
         conditioning,X24hour,hair,parking,internet,manner,non_smoke,friendly,easy_food,easy_traffic,
         cleaning_fee_with_guest_included,price_with_minimum_nights,deposit_cost_per_night,money_of_return,potential_cost,price_per_bedrooms,
         price_per_beds, need_group_by_time_with_price,need_group_by_bed_with_price)

summary(selected_clean)

       

#################################################################################
#sample 28500 instances from the original train dataset
train_y_all <- train_y %>% mutate(perfect_score = as.factor(perfect_score))
selected_clean$host_identity_verified <- as.factor( selected_clean$host_identity_verified )
#selected_clean$host_has_profile_pic <- as.factor( selected_clean$host_has_profile_pic )
selected_clean$host_is_superhost <- as.factor( selected_clean$host_is_superhost )
selected_clean$instant_bookable <- as.factor( selected_clean$instant_bookable )
selected_clean$is_business_travel_ready <- as.factor( selected_clean$is_business_travel_ready )
selected_clean$is_location_exact <- as.factor( selected_clean$is_location_exact )
selected_clean$require_guest_phone_verification <- as.factor( selected_clean$require_guest_phone_verification )
selected_clean$requires_license <- as.factor( selected_clean$requires_license )
selected_clean$require_guest_profile_picture <- as.factor( selected_clean$require_guest_profile_picture )


######################################################################################################
#model #1
#SVM Model

#choose variables
svm_train_y <- train_y %>% mutate(perfect_score = as.factor(perfect_score))

#Split into the original train and test
svm_train_x <- selected_clean[1:100000, ]
svm_test_x <- selected_clean[100001:112199,]
svm_train_set<-cbind(svm_train_x,svm_train_y)

#Sample some instances(3000) for training
set.seed(1)
svm_index <- sample(nrow(svm_train_set), 5000)
svm_small_data <- svm_train_set[svm_index, ]

#split small data into train and valid
svm_train <- sample(nrow(svm_small_data),.7*nrow(svm_small_data))
svm_x_train <- svm_small_data[svm_train, ]
svm_x_valid <- svm_small_data[-svm_train, ]

##########find best cost with cross=5
svm_acc_mean<- NULL
costs<-c(0.1,0.5,1,2,3,5,7,10)
for (cost in costs){
  svm_model<- svm(perfect_score~ . ,
                  data=svm_x_train, 
                  kernel='linear', cost=costs,cross=5,probability=TRUE)
  svm_acc_mean=c(svm_acc_mean,mean(svm_model$accuracies))
}
best_cost<-costs[which.max(svm_acc_mean)]

result_data_svm <- data.frame(best = costs, valid = svm_acc_mean)
result_data_svm %>%
  ggplot(aes(x = best, y = valid, color = 'SVM')) +
  geom_line(size = 1.5)+
  labs(title = 'SVM Accuracy in Different costs', x = 'costs', y = 'Accuracy', color = 'Models')+
  theme_classic()


#### svm prediction
set.seed(1)
svm.mod <- svm(perfect_score~ . ,
               data=svm_x_train, 
               kernel='linear', cost=best_cost,cross=5,probability=TRUE)
###make prediction
svm.preds<- predict(svm.mod,svm_x_valid,probability=TRUE)

###make classification
set.seed(1)
svm.probs<-attr(svm.preds, "probabilities")[,2]

svm_class <- ifelse(svm.probs>0.26378,1,0)
pred_best_svm <- prediction(svm_class, svm_x_valid$perfect_score)
roc_best_svm <- performance(pred_best_svm, 'tpr', 'fpr')
plot(roc_best_svm, col = 'green', lwd = 2, main = 'SVM ROC curve')

###TPR & FPR
svm_confusematrixs=table(svm_x_valid$perfect_score,svm_class)
svm_TP = svm_confusematrixs[2,2]
svm_TN = svm_confusematrixs[1,1]
svm_FP = svm_confusematrixs[1,2]
svm_FN = svm_confusematrixs[2,1]
svm_accuracy = (svm_TP+svm_TN)/(svm_TP+svm_TN+svm_FP+svm_FN)
svm_TPR = svm_TP/(svm_TP+svm_FN)
svm_FPR = svm_FP/(svm_TN+svm_FP)
#Return svm TPR,FPR
cat('svm_Accuracy:',svm_accuracy,'\n','svm_TPR:', svm_TPR,'\n', 'svm_FPR:', svm_FPR)



#########################################################################################################################################################
#Model #2
#logistic regression_lasso

accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}
set.seed(1)
grid_lasso <- 10^seq(10,-4,length=100)
k_lasso <- 5

#small dataset sampling

train_x_all_lasso <- selected_clean[1:100000, ]
test_x_lasso <- selected_clean[100001:112199,]

train_small_lasso <- sample(nrow(train_x_all_lasso), 28500)

train_x_lasso <- train_x_all_lasso[train_small_lasso, ]
train_y_lasso <- train_y[train_small_lasso, ]

va_inst_lasso <- sample(nrow(train_x_lasso), .3*nrow(train_x_lasso))



cleaned_x_small <- model.matrix(~.,train_x_lasso)
cleaned_y_small <- train_y_lasso$perfect_score

x_train_lasso_small <- cleaned_x_small[va_inst_lasso,]
x_valid_lasso_small <- cleaned_x_small[-va_inst_lasso,]

y_train_lasso_small <- cleaned_y_small[va_inst_lasso]
y_valid_lasso_small <- cleaned_y_small[-va_inst_lasso]





cv.out_lasso <- cv.glmnet(x_train_lasso_small, y_train_lasso_small, family="binomial", alpha=1, lambda=grid_lasso, nfolds=k_lasso)

plot(cv.out_lasso)

bestlam <- cv.out_lasso$lambda.min
bestlam

coeffs <- coef(cv.out_lasso, s = "lambda.min")

best.lasso.preds <- predict(cv.out_lasso, s=bestlam, newx = x_valid_lasso_small,type="response")
best.lasso.accuracy <- accuracy(best.lasso.preds, y_valid_lasso_small)
best.lasso.accuracy


classifications_lasso_small <- ifelse(best.lasso.preds > .45, 1, 0)
classifications_lasso_small <- ifelse(is.na(classifications_lasso_small), 0, classifications_lasso_small)
table(classifications_lasso_small)

gen_CM_lasso <- table(y_valid_lasso_small, classifications_lasso_small)
TP_lasso <- gen_CM_lasso[2,2]
FP_lasso <- gen_CM_lasso[1,2]
TN_lasso <- gen_CM_lasso[1,1]
FN_lasso <- gen_CM_lasso[2,1]
TPR_lasso = TP_lasso/(TP_lasso+FN_lasso)
FPR_lasso = FP_lasso/(FP_lasso+TN_lasso)
accuracy_lasso = (TP_lasso+TN_lasso)/(TP_lasso+TN_lasso+FP_lasso+FN_lasso)
table(y_valid_lasso_small)
summary(cv.out_lasso)
print(paste("True Positive Rate = ",TPR_lasso,", False Positive Rate = ",FPR_lasso, 
            ", Accuracy = ", accuracy_lasso))

pred_lasso <- prediction(best.lasso.preds, y_valid_lasso_small)
roc_lasso <- performance(pred_lasso, "tpr", "fpr")
plot(roc_lasso,  col = "green", ylab = '', lwd = 2)


#full dataset sampling
#Sys.setenv(LANG = "en")


cleaned_x_full <- model.matrix(~.,selected_clean)

mode(cleaned_x_full)
any(is.na(cleaned_x_full))
typeof(cleaned_x_full)

x_train_lasso_full <- cleaned_x_full[1:100000, ]
x_valid_lasso_full <- cleaned_x_full[100001:112199,]
y_train_lasso_full <- train_y$perfect_score



cv.out.full <- cv.glmnet(x_train_lasso_full, y_train_lasso_full, family="binomial", alpha=1, lambda=grid_lasso, nfolds=k_lasso)

bestlam_full <- cv.out.full$lambda.min
bestlam_full

coeffs.full <- coef(cv.out.full, s = "lambda.min")

best.lasso.full.preds <- predict(cv.out.full, s=bestlam_full, newx = x_valid_lasso_full,type="response")


classifications_lasso <- ifelse(best.lasso.full.preds > .45, 1, 0)
classifications_lasso <- ifelse(is.na(classifications_lasso), 0, classifications_lasso)
summary(classifications_lasso)
table(classifications_lasso)



######################################################################################################
#Model #3
#logistic regression_ridge
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}
set.seed(1)
grid_ridge <- 10^seq(10,-4,length=100)
k_ridge <- 5
#small dataset sampling
train_x_all_ridge <- selected_clean[1:100000, ]
test_x_ridge <- selected_clean[100001:112199,]

train_small_ridge <- sample(nrow(train_x_all_ridge), 28500)

train_x_ridge <- train_x_all_ridge[train_small_ridge, ]
train_y_ridge <- train_y[train_small_ridge, ]

va_inst_ridge <- sample(nrow(train_x_ridge), .3*nrow(train_x_ridge))



cleaned_x_small <- model.matrix(~.,train_x_ridge)
cleaned_y_small <- train_y_ridge$perfect_score

x_train_ridge_small <- cleaned_x_small[va_inst_ridge,]
x_valid_ridge_small <- cleaned_x_small[-va_inst_ridge,]

y_train_ridge_small <- cleaned_y_small[va_inst_ridge]
y_valid_ridge_small <- cleaned_y_small[-va_inst_ridge]





cv.out_ridge <- cv.glmnet(x_train_ridge_small, y_train_ridge_small, family="binomial", alpha=0, lambda=grid_ridge, nfolds=k_ridge)

plot(cv.out_ridge)

bestlam_ridge <- cv.out_ridge$lambda.min
bestlam_ridge

coeffs_ridge <- coef(cv.out_ridge, s = "lambda.min")

best.ridge.preds <- predict(cv.out_ridge, s=bestlam_ridge, newx = x_valid_ridge_small,type="response")
best.ridge.accuracy <- accuracy(best.ridge.preds, y_valid_ridge_small)
best.ridge.accuracy


classifications_ridge_small <- ifelse(best.ridge.preds > .445, 1, 0)
classifications_ridge_small <- ifelse(is.na(classifications_ridge_small), 0, classifications_ridge_small)
table(classifications_ridge_small)

gen_CM_ridge <- table(y_valid_ridge_small, classifications_ridge_small)
TP_ridge <- gen_CM_ridge[2,2]
FP_ridge <- gen_CM_ridge[1,2]
TN_ridge <- gen_CM_ridge[1,1]
FN_ridge <- gen_CM_ridge[2,1]
TPR_ridge = TP_ridge/(TP_ridge+FN_ridge)
FPR_ridge = FP_ridge/(FP_ridge+TN_ridge)
accuracy_ridge = (TP_ridge+TN_ridge)/(TP_ridge+TN_ridge+FP_ridge+FN_ridge)
table(y_valid_ridge_small)
summary(cv.out_ridge)
print(paste("True Positive Rate = ",TPR_ridge,", False Positive Rate = ",FPR_ridge, 
            ", Accuracy = ", accuracy_ridge))

pred_ridge <- prediction(best.ridge.preds, y_valid_ridge_small)
roc_ridge <- performance(pred_ridge, "tpr", "fpr")
plot(roc_ridge,  col = "green", ylab = '', lwd = 2)

#full dataset sampling
Sys.setenv(LANG = "en")


cleaned_x_full <- model.matrix(~.,selected_clean)

mode(cleaned_x_full)
any(is.na(cleaned_x_full))
typeof(cleaned_x_full)

x_train_ridge_full <- cleaned_x_full[1:100000, ]
x_valid_ridge_full <- cleaned_x_full[100001:112199,]
y_train_ridge_full <- train_y$perfect_score



cv.out.full_ridge <- cv.glmnet(x_train_ridge_full, y_train_ridge_full, family="binomial", alpha=0, lambda=grid_ridge, nfolds=k_ridge)

bestlam_full_ridge <- cv.out.full_ridge$lambda.min
bestlam_full_ridge

coeffs.full_ridge <- coef(coeffs.full_ridge, s = "lambda.min")

best.ridge.full.preds <- predict(cv.out.full_ridge, s=bestlam_full_ridge, newx = x_valid_ridge_full,type="response")


classifications_ridge <- ifelse(best.ridge.full.preds > .441, 1, 0)
classifications_ridge <- ifelse(is.na(classifications_ridge), 0, classifications_ridge)
summary(classifications_ridge)
table(classifications_ridge)
######################################################################################################################################################################
#Model #4
# For gbm boosting


train_y_all <- train_y %>% mutate(perfect_score = as.integer(perfect_score))
selected_clean$host_identity_verified <- as.factor( selected_clean$host_identity_verified )
selected_clean$host_is_superhost <- as.factor( selected_clean$host_is_superhost )
selected_clean$instant_bookable <- as.factor( selected_clean$instant_bookable )
selected_clean$is_business_travel_ready <- as.factor( selected_clean$is_business_travel_ready )
selected_clean$is_location_exact <- as.factor( selected_clean$is_location_exact )
selected_clean$require_guest_phone_verification <- as.factor( selected_clean$require_guest_phone_verification )
selected_clean$requires_license <- as.factor( selected_clean$requires_license )
selected_clean$require_guest_profile_picture <- as.factor( selected_clean$require_guest_profile_picture )


train_x_all <- selected_clean[1:100000, ]
test_x <- selected_clean[100001:112199,]

#Sample some instances for training
set.seed(1)
train_small_boosting <- sample(nrow(train_x_all), 100000)
train_x_boosting<- train_x_all[train_small_boosting, ]
train_y_boosting<- train_y_all[train_small_boosting, ]

#Split into the original train and test
boosting_all_train<-cbind(train_x_boosting,train_y_boosting)

#Split train into train and val
train_ind_small <- sample(nrow(boosting_all_train), 0.7*nrow(boosting_all_train))
boosting_x_train_use<-boosting_all_train[train_ind_small,]
boosting_x_vaild_use<-boosting_all_train[-train_ind_small,]



###########################################################################################################################################################################################################################################################################################
#boosting GBM random search loop
gridsize_gbm = 15
gbm_grid_random_tree<- sample(1000:3000, gridsize_gbm , replace=T)
gbm_grid_random_deep<- sample(5:15, gridsize_gbm , replace=T)


for (i in c(1:gridsize_gbm)){
  n <- gbm_grid_random_tree[i]
  m <- gbm_grid_random_deep[i]
  boost.mod_gbm <- gbm(perfect_score~.,
                       data=boosting_x_train_use,
                       distribution="bernoulli",
                       n.trees=n,
                       interaction.depth=m)
  boost_preds_gbm <- predict(boost.mod_gbm,newdata=boosting_x_vaild_use,type='response',n.trees=n)
  boost_class_gbm <- ifelse(boost_preds_gbm >.5,1,0)
  boost_confusematrixs <- table(boosting_x_vaild_use$perfect_score, boost_class_gbm)
  TP_gbm <- boost_confusematrixs[2,2]
  FP_gbm <- boost_confusematrixs[1,2]
  TN_gbm <- boost_confusematrixs[1,1]
  FN_gbm <- boost_confusematrixs[2,1]
  TPR_gbm = TP_gbm/(TP_gbm+FN_gbm)
  FPR_gbm = FP_gbm/(FP_gbm+TN_gbm)
  accuracy_gbm = (TP_gbm+TN_gbm)/(TP_gbm+TN_gbm+FP_gbm+FN_gbm)
  print(paste("Boosting depth = ",m,", ntree = ",n,", True Positive Rate =",TPR_gbm,", False Positive Rate = ",FPR_gbm))
  accuracy
}



##############################################################################################################################################
###final GBM

boost.mod <- gbm(perfect_score~.,
                 data=boosting_x_train_use,
                 distribution="bernoulli",
                 n.trees=900,
                 interaction.depth=6)
summary(boost.mod)

boost_preds <- predict(boost.mod,newdata=boosting_x_vaild_use,type='response',n.trees=900,interaction.depth=6)
summary(boost_preds)
boost_class <- ifelse(boost_preds>.5,1,0)
boost_confusematrixs <- table(boosting_x_vaild_use$perfect_score, boost_class)
TP_gbm<- boost_confusematrixs[2,2]
FP_gbm <- boost_confusematrixs[1,2]
TN_gbm<- boost_confusematrixs[1,1]
FN_gbm<- boost_confusematrixs[2,1]
TPR_gbm = TP_gbm/(TP_gbm+FN_gbm)
FPR_gbm = FP_gbm/(FP_gbm+TN_gbm)
accuracy_gbm = (TP_gbm+TN_gbm)/(TP_gbm+TN_gbm+FP_gbm+FN_gbm)
print(paste("Boosting depth = ",6,", mtry = ",900,", True Positive Rate =",TPR_gbm,", False Positive Rate = ",FPR_gbm))
accuracy_gbm



################################################################################################################
#test prediction  GBM

boost_preds_test<-predict(boost.mod,newdata=test_x,type='response',n.trees=900,interaction.depth=6)
boost_class_test <- ifelse(boost_preds_test>.5,1,0)
write.table(boost_class_test, "predictions_group_8_boosting_gbm.csv", row.names = FALSE, col.names = FALSE)


#################################################################################################################################

#Model #5
# For AdaBoost

train_y_all <- train_y %>% mutate(perfect_score = as.factor(perfect_score))

train_x_all <- selected_clean[1:100000, ]
test_x <- selected_clean[100001:112199,]

#Sample some instances for training
set.seed(1)
train_small_boosting <- sample(nrow(train_x_all), 100000)
train_x_boosting<- train_x_all[train_small_boosting, ]
train_y_boosting<- train_y_all[train_small_boosting, ]

#Split into the original train and test
boosting_all_train<-cbind(train_x_boosting,train_y_boosting)

#Split train into train and val
train_ind_small <- sample(nrow(boosting_all_train), 0.7*nrow(boosting_all_train))
boosting_x_train_use<-boosting_all_train[train_ind_small,]
boosting_x_vaild_use<-boosting_all_train[-train_ind_small,]
#create a simple model and generate predictions in the test data
gridsize_ada = 15
mfinal_grid_random<- sample(100:2000, gridsize_ada , replace=T)
maxdepth_grid_random <- sample(5:15, gridsize_ada , replace=T)

#Adaboost random search loop
for (i in c(1:gridsize_ada)){
  n <- mfinal_grid_random[i]
  m <- maxdepth_grid_random[i]
  data.adaboost <- boosting(perfect_score~.,
                            data=boosting_x_train_use, 
                            mfinal=n,
                            boos = TRUE,
                            control = rpart.control(maxdepth = m))
  boost_preds_ada <- predict(data.adaboost,newdata=boosting_x_vaild_use)
  # boost_class_ada<-table(boosting_x_vaild_use$perfect_score,boost_preds_ada)
  TP_ada <- boost_preds_ada$confusion[2,2]
  FP_ada <- boost_preds_ada$confusion[1,2]
  TN_ada<- boost_preds_ada$confusion[1,1]
  FN_ada <- boost_preds_ada$confusion[2,1]
  TPR_ada = TP_ada/(TP_ada+FN_ada)
  FPR_ada = FP_ada/(FP_ada+TN_ada)
  accuracy_ada = (TP_ada+TN_ada)/(TP_ada+TN_ada+FP_ada+FN_ada)
  print(paste("Num trees = ",n,", mtry = ",m,", True Positive Rate =
",TPR_ada,", False Positive Rate = ",FPR_ada, ", Accuracy = ", accuracy_ada))
}


#############################################################################################################################################
#final Adaboost
data.adaboost <- boosting(perfect_score~.,
                          data=boosting_x_train_use, 
                          mfinal=850,
                          boos = TRUE,
                          control = rpart.control(maxdepth = 13))
summary(data.adaboost)
boost_preds_ada <- predict(data.adaboost,newdata=boosting_x_vaild_use)
classifications_ada <- ifelse(boost_preds_ada$prob > .62, 1, 0)
classifications_ada <- as.data.frame(classifications_ada)
boosting_y <- boosting_x_vaild_use$perfect_score
boosting_cla <- classifications_ada$V2
boosting_ada <- table(boosting_cla, boosting_y)

TP_ada <-boosting_ada[2,2]
FP_ada <- boosting_ada[1,2]
TN_ada<- boosting_ada[1,1]
FN_ada <- boosting_ada[2,1]
TPR_ada = TP_ada/(TP_ada+FN_ada)
FPR_ada = FP_ada/(FP_ada+TN_ada)
accuracy_ada = (TP_ada+TN_ada)/(TP_ada+TN_ada+FP_ada+FN_ada)
print(paste("Boosting_ada depth = ",13,", mtry = ",850,", True Positive Rate_ada =",TPR,", False Positive Rate_ada = ",FPR))


################################################################################################################################################
#Model #6
#Random Forest
clean_x_all_rf <- model.matrix(~.+ price:state + price:room_type + host_response_rate:price + host_experience:host_is_superhost
                               + is_business_travel_ready:instant_bookable + cancellation_policy:price
                               + conditioning:price + security_deposit:host_response_rate + X24hour:host_response_rate + availability_30:bed_type 
                               + availability_30:cancellation_policy + beds:bed_type + bedrooms:beds + cancellation_policy:cleaning_fee 
                               + cancellation_policy:extra_people + bathrooms:cleaning_fee + host_is_superhost:availability_30, selected_clean)
one_hot_x_all_rf <- data.frame(clean_x_all_rf)
one_hot_x_all_rf <- one_hot_x_all_rf %>% select(-X.Intercept.)

#################################################################################

#Split into the original train and test
train_x_all_rf <- one_hot_x_all_rf[1:100000, ]
test_x_rf <- one_hot_x_all_rf[100001:112199,]

#Trun perfect_score into factor
train_y_all_rf <- train_y %>% mutate(perfect_score = as.factor(perfect_score))

#sample 28500 instances from the original train dataset
set.seed(1)
train_small_rf <- sample(nrow(train_x_all_rf), 30000)
train_x_rf <- train_x_all_rf[train_small_rf, ]
train_y_rf <- train_y_all_rf[train_small_rf, ]

small_train_set_rf <- cbind(train_x_rf, train_y_rf)

#Split train into train and val
val_ind_rf <- sample(nrow(train_x_rf), 0.3*nrow(train_x_rf))

x_train_rf <- train_x_rf[-val_ind_rf, ]
y_train_rf <- train_y_rf[-val_ind_rf, ]
x_val_rf <- train_x_rf[val_ind_rf, ]
y_val_rf <- train_y_rf[val_ind_rf, ]

#create a simple model and generate predictions in the test data
gridsize_rf = 30

ntree_grid_random_rf <- sample(100:2000, gridsize_rf, replace=T)
mtry_grid_random_rf <- sample(5:15, gridsize_rf, replace=T)

memory.limit(size=56000)

for (i in c(1:gridsize_rf)){
  n <- ntree_grid_random_rf[i]
  m <- mtry_grid_random_rf[i]
  rf.mod <- randomForest(perfect_score~.,
                         data=small_train_set_rf,
                         subset=-val_ind_rf,
                         mtry=m, ntree=n)
  rf_preds <- predict(rf.mod, newdata=x_val_rf, type = "response")
  gen_CM <- table(y_val_rf$perfect_score, rf_preds)
  TP <- gen_CM[2,2]
  FP <- gen_CM[1,2]
  TN <- gen_CM[1,1]
  FN <- gen_CM[2,1]
  TPR = TP/(TP+FN)
  FPR = FP/(FP+TN)
  Accuracy = (TP+TN)/(TP+FP+FN+TN)
  print(paste("Num trees = ",n,", mtry = ",m, 
              ", True Positive Rate = ",TPR,", False Positive Rate = ",FPR, 
              ", Accuracy = ", Accuracy))
}

#Best result:
#"Num trees =  1257 , mtry =  12 , True Positive Rate =  0.195985832349469 , False Positive Rate =  0.056045827527481 , Accuracy =  0.732777777777778"

original_train_rf <- cbind(train_x_all_rf, train_y_all_rf)

small_train_set_2_rf <- original_train_rf[-train_small_rf, ]
y_val_2_rf <- small_train_set_2_rf$perfect_score
x_val_2_rf <- small_train_set_2_rf %>% select(-perfect_score)

rf.mod.2 <- randomForest(perfect_score~.,
                         data=original_train_rf,
                         subset=train_small_rf,
                         mtry=12, ntree=1257, cutoff=c(0.5487, 1-0.5487))
rf_preds_2 <- predict(rf.mod.2, newdata=x_val_2_rf, type = "response")
gen_CM_2_rf <- table(y_val_2_rf, rf_preds_2)
TP2_rf <- gen_CM_2_rf[2,2]
FP2_rf <- gen_CM_2_rf[1,2]
TN2_rf <- gen_CM_2_rf[1,1]
FN2_rf <- gen_CM_2_rf[2,1]
TPR2_rf = TP2_rf/(TP2_rf+FN2_rf)
#rf_TPR2 <- append(rf_TPR2, TPR2_rf)
FPR2_rf = FP2_rf/(FP2_rf+TN2_rf)
#rf_FPR2 <- append(rf_FPR2, FPR2_rf) 
Accuracy2_rf = (TP2_rf+TN2_rf)/(TP2_rf+FP2_rf+FN2_rf+TN2_rf) 

print(TPR2_rf)
print(FPR2_rf)
print(Accuracy2_rf)


#Train the model again with the hyperparameters above and with all original train data:
rf.mod <- randomForest(perfect_score~.,
                       data=original_train_rf,
                       mtry=12, ntree=1257, cutoff=c(0.5487, 1-0.5487))

rf_preds <- predict(rf.mod, newdata=test_x, type = "response")


