# [Covidgraphics](https://github.com/rnnh/covidgraphics)
by [Ronan Harrington](https://github.com/rnnh/)

![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/rnnh/covidgraphics)
![GitHub repo size](https://img.shields.io/github/repo-size/rnnh/covidgraphics)
![GitHub](https://img.shields.io/github/license/rnnh/covidgraphics)

This is a [Shiny](https://shiny.rstudio.com/) app written in `R` that creates graphs using international COVID-19 data, which can be used online or run locally.
The data used is automatically fetched from the [COVID-19 Data Hub](https://covid19datahub.io/) whenever the app is run, using the [COVID-19 Data Hub `R` package](https://cran.r-project.org/web/packages/COVID19/).

It can be used online at: <https://rnnh.shinyapps.io/covidgraphics/>

## Example output

![An animated GIF generated using covidgraphics](assets/covidgraphics_example.gif)

## Running the app locally

- Clone this repo:
```bash
$ git clone https://github.com/rnnh/covidgraphics.git
```
- Install [`R` version 4.0.2](https://www.r-project.org/) or later
- Install [RStudio version 1.3.959](https://rstudio.com/) or later
- Open [app.R](app.R) in RStudio
- Install the required packages as prompted
- Select "Run App" or use the keyboard shortcut (`Command`/`Ctrl` + `Shift` + `Enter`) to run the app

## References

- Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi: [10.21105/joss.02376](https://doi.org/10.21105/joss.02376).
