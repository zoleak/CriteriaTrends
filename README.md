![Capture](https://github.com/zoleak/CriteriaTrends/assets/36116239/df255436-7799-44eb-a23a-26038c760d77)


## Overview

Air quality is a critical aspect of environmental health, and understanding the levels of pollutants in the atmosphere is crucial for safeguarding public health. The Clean Air Act mandates the Environmental Protection Agency (EPA) to establish National Ambient Air Quality Standards (NAAQS) for six pollutants, known as [criteria air pollutants](https://www.epa.gov/criteria-air-pollutants). These standards aim to protect both human health and the environment.

**CriteriaTrends** is an R Shiny application designed to facilitate the exploration and visualization of air quality data in New Jersey. By leveraging data from the New Jersey Department of Environmental Protection's (NJDEP) database spanning the years 1990-2022, this application allows users to interactively analyze and plot pollutant concentrations recorded across various air monitoring sites throughout the state.

The goal of this project is to provide a user-friendly interface for individuals, researchers, and policymakers to gain insights into trends related to criteria air pollutants. Whether it's tracking the annual average concentrations of PM₂.₅ or exploring the 1-hour NAAQS for SO₂, **CriteriaTrends** empowers users to make informed assessments of air quality data in New Jersey.

### Key Features
- Filter and visualize pollutant data from multiple air monitoring sites.
- Explore trends and concentrations of criteria air pollutants over the years.
- User-friendly interface for interactive data analysis.
## Data

The dataset used in this project originates from the New Jersey Department of Environmental Protection's (NJDEP) database. The dataset spans the years 1990 to 2022 and includes information on various criteria air pollutants monitored at different sites across New Jersey.

### Data Structure

The primary dataset, after processing, is combined from multiple Excel files corresponding to different pollutants. Each file includes the following key variables:

- **Year:** The year of the recorded air quality data.
- **County:** The county in New Jersey where the monitoring site is located.
- **Station_Name:** The name or identifier of the monitoring station.
- **Value:** The concentration of the specific pollutant recorded at the monitoring station.

Additionally, pollutant-specific datasets may have pollutant-specific variables. For example, the PM10 dataset might include specific information about particulate matter with a diameter of 10 micrometers.

### Data Cleaning and Preprocessing

Before analysis, the datasets underwent cleaning and preprocessing steps to ensure data integrity. Common preprocessing steps included:

- Handling missing values: Any missing or anomalous values were addressed to maintain the integrity of the analyses.
- Unifying formats: Ensuring consistency in variable names and formats across different pollutant datasets for seamless integration.

The tidy datasets were then combined into a single dataframe, facilitating streamlined analysis and visualization within the Shiny application.
## Application Features

**CriteriaTrends** offers a range of features to enhance the user experience and provide valuable insights into air quality trends in New Jersey. Key features include:

- **Interactive Filtering:** Users can dynamically filter pollutant data based on specific criteria such as pollutant type, county, and monitoring station.
  
- **Customizable Plots:** The application generates dynamic and customizable plots, allowing users to visualize pollutant concentrations over different time periods.

- **NAAQS Guidelines:** Integrated annotations highlight National Ambient Air Quality Standards (NAAQS) thresholds for each pollutant, providing context for concentration levels.

- **Downloadable Plots and Data:** Users can download generated plots or the entire dataset for further analysis.

- **User-Friendly Interface:** The Shiny app offers an intuitive and user-friendly interface, making it accessible to a wide range of users, from researchers to policymakers.

For a hands-on experience, access the live deployment [here](https://kzolea695.shinyapps.io/criteriatrends/) and explore the full range of features.



