# 311 Requests and Demographics in Chicago
This is a Data Science for Social Good data science project that examines 311 (non-emergency) service requests in the city of Chicago, trying to understand the underlying patterns and the correlations with demographic characteristics through visualization, machine learning, and statistical modeling.

## The Problem:
Through the 311 system, every Chicago citizen can request city services such as graffiti removal and pothole filling, among many other types. The records of 311 service requests reflect the needs of the city and its inhabitants.

The aim of this project is to investigate how the service request patterns are related to the characteristics of different Chicago neighborhoods, in order to answer two related questions: what do service requests tell us about the different neighborhoods in Chicago, and how can we use neighborhood characteristics to predict the request volume for each service type?

## The Solution: <desc of the problem, i.e. "time-series regression">
In order to answer those big questions, we performed a three-step analysis, briefly described below:

1. Exploratory analysis through visualization of time series and scatterplots,
2. Application of clustering methods to identify areas with the same request patterns, and
3. Construction of statistical models for the prediction of 311 requests. In particular we trained a Generalized Poisson Linear Model in order to mine the relevant predictors.

## The Project
### <Project piece 1, i.e. "scraper scripts">
### <Project piece 2, i.e. "models">
### <Project piece 3, i.e. "webapp">
### <etc>

## The Data
Short paragraph or two that describes the data and links to the in-depth wiki section about the data.

## Contributing to the Project
-- Issue Tracker
-- Email Addresses (good to use google group)
 
## License

Copyright (C) 2013 [Data Science for Social Good Fellowship at the University of Chicago](http://dssg.io)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

This project examines 3-1-1 (non-emergency) service requests with the ultimate aim of constructing a statistical model of the occurrence of service requests around the city.  Some exploratory work is in directories "acs-census" and "misc", with some preliminary models for potholes and graffiti in the "analysis" directory.
