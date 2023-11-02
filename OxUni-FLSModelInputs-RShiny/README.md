# OxUni-FLSModelInputs-RShiny

This is an Rshiny app that offers an interactive interface to view and compare FLS model inputs between all the countries and within ALL the possible scenarios.

# How to update and republish the app

NOTE: Data used in this app comes from running the FLS calculator/model.

## Step 1: Update the datasets

Update the contents of the folder `inputs_databases`. Go to the [FLS model](https://github.com/ndorms-botnar/OxfordUni.HEOR-FLSModel) project, navigate to the following folder `inputs_databases`


Copy the contents of the this folder and paste them on THIS project's `inputs_databases` folder. Once this is done, the data for the app will be the most recent data for inputs.

## Step2: Run and re-publish the app

The app is published publicly in Oxford university's `https://trainingidn.shinyapps.io` server.

- To launch the app, open either of the following `ui.R`, `server.R` or `global.R`.
- A green `Run App`will appear at the top right corner, click on it to launch the app.
- Once the app launches, click on the blue button `Republish` located at the top-right corner of launched app window.
- Clikc `publish` to send the app to the Shiny server [wait, the publishing takes a few minutes to complete]. DO NOT close the RStudio project.

