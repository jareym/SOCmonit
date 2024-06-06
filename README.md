# SOCmonit R Package

The SOCmonit package was developed as part of the [SOCmonit project](https://service.ble.de/ptdb/index2.php?detail_id=484352&site_key=141). It provides a set of tools for soil organic carbon (SOC) prediction. The package is designed to support the analysis of point and on-the-go (OTG) hyperspectral data and UAV (Unmanned Aerial Vehicle) multispectral data for SOC monitoring. It comprises tools for data processing and modelling.

## Authors

- Javier Reyes (UFZ - Helmholtz Centre for Environmental Research, Halle, Germany)
- Anna Brand (RSS - Remote Sensing Solutions GmbH, Munich, Germany)
- Werner Wiedemann (RSS - Remote Sensing Solutions GmbH, Munich, Germany)
- Jonas Franke (RSS - Remote Sensing Solutions GmbH, Munich, Germany)
- Mareike Ließ (University of Applied Sciences Weihenstephan - Triesdorf, Weidenbach, Germany | UFZ - Helmholtz Centre for Environmental Research, Halle, Germany)

## Key Features

- Preprocessing of proximal sensing spectral data and UAV spectral data
- Generating predictor-response datasets
- Building predictive models for SOC monitoring
- Visualization and analysis of spectral data

## Installation
You can install the SOCmonit package from R using the following command:

```R
library (remotes)

install_github('jareym/SOCmonit')

#include vignettes

install_github('jareym/SOCmonit', build_vignettes = TRUE)
```

## Documentation
The package documentation, including function descriptions and usage examples, can be accessed using the 'help' function in R. For example, to get help for the 'rd.spec.otg' function, you can run:

```R
help('rd.spec.otg', package = 'SOCmonit')
```

## Vignette
The package includes the following vignettes that provide detailed guides on different aspects of using the SOCmonit package:

1. **Preprocessing and Modeling on-the-go Spectral Data**: This vignette covers the preprocessing and modeling of on-the-go spectral data for SOC prediction. It includes steps for reading, testing, and preprocessing spectral data, as well as building predictive models using techniques such as partial least squares regression (PLSR) and spatial interpolation through ordinary kriging.

2. **Preprocessing and Modeling point Spectral Data**: This vignette focuses on the analysis of proximal spectral data for SOC prediction. It includes steps for reading and preprocessing proximal spectral data, building PLSR models, evaluating model performance, and generating predictions.

3. **Comparison of in situ Spectrometer and UAV Data**: This vignette demonstrates how to compare in situ spectrometer data with UAV data for SOC estimation. It covers the steps for reading and preprocessing UAV images, loading and correcting ASD (Analytical Spectral Device) data, visualizing the spectral response functions (SRFs), conducting image correction, and generating predictor-response datasets.

## Example Data
Example data files are included in the package's extdata directory. These files contain spectral data and related information obtained from the ['Static Fertilization Experiment V120'](https://www.ufz.de/index.php?en=39220) (Bad Lauchstädt) long-term experiment. These data files can be used to explore the functionalities of the SOCmonit package and try out the code examples provided in the vignettes.

## References
- Reyes, J., Ließ, M. (2024). Spectral Data Processing for Field-Scale Soil Organic Carbon Monitoring. Sensors 24(3), 849. https://doi.org/10.3390/s24030849
- Reyes, J., Ließ, M. (2023). On-the-Go Vis-NIR Spectroscopy for Field-Scale Spatial-Temporal Monitoring of Soil Organic Carbon. Agriculture 2023, 13, 1611. https://doi.org/10.3390/agriculture13081611
- Reyes, J., Wiedemann, W., Brand, A., Franke, J., Ließ, M. (2024). Predictive monitoring of soil organic carbon using multispectral UAV imagery: a case study on a long-term experimental field. Spatial Information Research [accepted for publication 24 May 2024]
 

## Issues and Contributions
If you encounter any issues with the SOCmonit package or have suggestions for improvements, please open an issue on the GitHub repository. Contributions, such as bug fixes or new features, are also welcome. You can submit a pull request with your changes.

## License
This package is released under the [GPL (>= 2)](https://www.gnu.org/licenses/gpl-2.0.html) license.Spectral Data Processing for Field-Scale Soil Organic Carbon Monitoring 

## Funding
The R package was developed on behalf of the SOCmonit project, supported by funds of the Federal Ministry of Food and Agriculture (BMEL) based on a decision of the Parliament of the Federal Republic of Germany via the Federal Office for Agriculture and Food (BLE) under the innovation support programme.
