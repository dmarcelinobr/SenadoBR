
<!-- README.md is generated from README.Rmd. Please edit that file -->

SenadoBR <img src="inst/figures/SenadoBR-logo.png" width="240px" align="right" />
=================================================================================

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build
Status](https://travis-ci.org/danielmarcelino/SeanadoBR.svg?branch=master)](https://travis-ci.org/danielmarcelino/SeanadoBR)
![CRAN Version](https://www.r-pkg.org/badges/version/SeanadoBR)
![License](https://img.shields.io/badge/license-MIT-blueviolet.svg?style=flat)

R package for accessing the Brazilian Senate RESTful API
--------------------------------------------------------

Installation
------------

To get the current development version from Github:

    ## install devtools package if it's not already
    if (!requireNamespace("devtools", quietly = TRUE)) {
      install.packages("devtools")
    }
    ## install dev version of SenadoBR from github
    devtools::install_github("danielmarcelino/SenadoBR")

Usage
-----

### Get the actual list of senators


    ## load SenadoBR package
    library(SenadoBR)
    #> Warning: replacing previous import 'stats::filter' by 'dplyr::filter' when
    #> loading 'SenadoBR'

    sents = fetchSenators()

    print(sents)
    #> # A tibble: 81 x 7
    #>    legislator_id legislator_cham… legislator_name legislator_sex
    #>    <chr>         <chr>            <chr>           <chr>         
    #>  1 4981          SF               Acir Gurgacz    Masculino     
    #>  2 5982          SF               Alessandro Vie… Masculino     
    #>  3 945           SF               Alvaro Dias     Masculino     
    #>  4 5967          SF               Angelo Coronel  Masculino     
    #>  5 5529          SF               Antonio Anasta… Masculino     
    #>  6 751           SF               Arolde de Oliv… Masculino     
    #>  7 6295          SF               Carlos Fávaro   Masculino     
    #>  8 5990          SF               Carlos Viana    Masculino     
    #>  9 470           SF               Chico Rodrigues Masculino     
    #> 10 5973          SF               Cid Gomes       Masculino     
    #> # … with 71 more rows, and 3 more variables: legislator_state <chr>,
    #> #   legislator_party <chr>, legislator_img <chr>

### Fetch bloc leaders



    fetchGovBlocLeader(bloc="Governo")
    #> # A tibble: 1 x 3
    #>   SiglaUnidLideranca CodigoParlamentar NomeParlamentar        
    #>   <chr>              <chr>             <chr>                  
    #> 1 Governo            5540              Fernando Bezerra Coelho
     

    fetchGovBlocLeader(bloc="Maioria")
    #> # A tibble: 1 x 3
    #>   SiglaUnidLideranca CodigoParlamentar NomeParlamentar
    #>   <chr>              <chr>             <chr>          
    #> 1 Maioria            4994              Eduardo Braga

     
    fetchGovBlocLeader(bloc="Minoria")
    #> # A tibble: 1 x 3
    #>   SiglaUnidLideranca CodigoParlamentar NomeParlamentar   
    #>   <chr>              <chr>             <chr>             
    #> 1 Minoria            5012              Randolfe Rodrigues
     

    fetchGovBlocLeader(bloc="Bloco")
    #> # A tibble: 5 x 3
    #>   SiglaUnidLideranca CodigoParlamentar NomeParlamentar    
    #>   <chr>              <chr>             <chr>              
    #> 1 Bloco              22                Esperidião Amin    
    #> 2 Bloco              5905              Rodrigo Cunha      
    #> 3 Bloco              5979              Leila Barros       
    #> 4 Bloco              1173              Wellington Fagundes
    #> 5 Bloco              374               Paulo Rocha


    fetchGovBlocLeader(bloc="PODE")
    #> # A tibble: 1 x 3
    #>   SiglaUnidLideranca CodigoParlamentar NomeParlamentar
    #>   <chr>              <chr>             <chr>          
    #> 1 PODEMOS            945               Alvaro Dias
