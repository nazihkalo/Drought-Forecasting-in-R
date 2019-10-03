# Drought-Forecasting-in-R
Drought Forecasting using Time Series models in R.

# Seasonal Drought Forecasting:

## Relevance:

Drought is among the most disastrous natural hazards and occurs in virtually all geographical areas. Severe drought events in recent decades, including 2010–2011 East Africa drought, 2011 Texas drought, 2012 U.S. Central Great Plains drought, and 2012–2015 California drought, have caused huge losses to agriculture, society, and ecosystems with profound impacts on crop production and water supply. Therefore, drought forecasting is of critical importance to early warning for drought managements.

## Variable:

Geophysicists differentiate between four sorts of droughts: Meteorological, agricultural, hydrological and socioeconomic. Causes are cascading, meaning meteorological droughts cause agricultural droughts and hydrological droughts, which in turn cause socioeconomic droughts. As Meteorological droughts are the root cause, our effort concentrates on predicting these. A main predictor for meteorological droughts is the Standardized Precipitation Evapotranspiration Index (SPEI). The SPEI is designed to take into account both precipitation and potential evaporation in determining drought. Thus, the SPEI captures the main impact of increased temperatures on water demand. The lower the index, the more severe the drought (usual values range between -2 and 2).

## Dataset:

Our dataset consists of monthly SPEI data between January 1950 and November 2018 (1,380 observations)1. Although data are available almost on a global scale, we would like to focus on regions especially affected by droughts (e.g. African countries in the Saharan region). Also, regions near the equator are easier to interpret data-wise, as seasons are less prevalent. See below for exemplary SPEI time series data (in this case for western Eritrea):

