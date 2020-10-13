
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

### Get bloc leaders



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

### Get the list of rollcall votes


    fetchRollcallVotesByInterval(initial_date = "01/02/2019")
    #> # A tibble: 274 x 6
    #>    bill_id rollcall_id rollcall_info     datetime   votacao_secreta link_pdf    
    #>    <chr>   <chr>       <chr>             <date>               <dbl> <chr>       
    #>  1 135251  5942        "Submete à aprec… 2019-02-26               1 https://rl.…
    #>  2 135087  5943        "Submete à aprec… 2019-02-26               1 https://rl.…
    #>  3 135250  5944        "Submete à consi… 2019-02-26               1 https://rl.…
    #>  4 135252  5945        "Submete à consi… 2019-02-26               1 https://rl.…
    #>  5 129807  5946        "Altera os arts.… 2019-03-12               0 https://rl.…
    #>  6 135603  5947        "Altera a Lei Co… 2019-03-13               0 https://rl.…
    #>  7 129807  5948        "Proposta de Eme… 2019-03-19               0 https://rl.…
    #>  8 134910  5949        "PLC nº 135, de … 2019-03-19               0 https://rl.…
    #>  9 135088  5950        "Submete à aprec… 2019-03-26               1 https://rl.…
    #> 10 123903  5951        "PEC nº 141, de … 2019-04-02               0 https://rl.…
    #> # … with 264 more rows

### Retrieve detailed information of a bill rollcall session


    fetchRollcallVotes(bill_id = 135251)
    #> # A tibble: 1 x 4
    #>   bill_id rollcall_info                           datetime            session_id
    #>     <dbl> <chr>                                   <dttm>              <chr>     
    #> 1  135251 "Submete à apreciação do Senado Federa… 2019-02-26 14:00:00 86315

### Retrieve rollcall votes hisotry


    infos <- fetchRollcallVotesByInterval(initial_date = "01/09/2020")


    rollcalls = extractRollcallVotes(infos)
    #> [1] "Extracting detailed information from rollcall id 6173"
    #> [1] "Extracting detailed information from rollcall id 6174"
    #> [1] "Extracting detailed information from rollcall id 6175"
    #> [1] "Extracting detailed information from rollcall id 6176"
    #> [1] "Extracting detailed information from rollcall id 6177"
    #> [1] "Extracting detailed information from rollcall id 6178"
    #> [1] "Extracting detailed information from rollcall id 6179"
    #> [1] "Extracting detailed information from rollcall id 6180"
    #> [1] "Extracting detailed information from rollcall id 6181"
    #> [1] "Extracting detailed information from rollcall id 6220"
    #> [1] "Extracting detailed information from rollcall id 6221"
    #> [1] "Extracting detailed information from rollcall id 6222"
    #> [1] "Extracting detailed information from rollcall id 6223"


    print(rollcalls)
    #> # A tibble: 1,053 x 9
    #>    bill_id rollcall_id rollcall_info datetime    year legislator_name
    #>    <chr>   <chr>       <chr>         <date>     <dbl> <chr>          
    #>  1 143597  6173        Votação nomi… 2020-09-01  2020 " Alessandro V…
    #>  2 143597  6173        Votação nomi… 2020-09-01  2020 " Eliziane Gam…
    #>  3 143597  6173        Votação nomi… 2020-09-01  2020 " Jorge Kajuru"
    #>  4 143597  6173        Votação nomi… 2020-09-01  2020 " Chico Rodrig…
    #>  5 143597  6173        Votação nomi… 2020-09-01  2020 " Davi Alcolum…
    #>  6 143597  6173        Votação nomi… 2020-09-01  2020 " Jayme Campos"
    #>  7 143597  6173        Votação nomi… 2020-09-01  2020 " Marcos Rogér…
    #>  8 143597  6173        Votação nomi… 2020-09-01  2020 " Maria do Car…
    #>  9 143597  6173        Votação nomi… 2020-09-01  2020 " Rodrigo Pach…
    #> 10 143597  6173        Votação nomi… 2020-09-01  2020 " Confúcio Mou…
    #> # … with 1,043 more rows, and 3 more variables: legislator_state <chr>,
    #> #   legislator_party <chr>, legislator_vote <chr>
