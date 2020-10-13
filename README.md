
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
    #> # A tibble: 274 x 7
    #>    bill_id rollcall_id rollcall_info datetime   votacao_secreta url_votacao
    #>    <chr>   <chr>       <chr>         <date>               <dbl> <chr>      
    #>  1 135251  5942        "Submete à a… 2019-02-26               1 https://ww…
    #>  2 135087  5943        "Submete à a… 2019-02-26               1 https://ww…
    #>  3 135250  5944        "Submete à c… 2019-02-26               1 https://ww…
    #>  4 135252  5945        "Submete à c… 2019-02-26               1 https://ww…
    #>  5 129807  5946        "Altera os a… 2019-03-12               0 https://ww…
    #>  6 135603  5947        "Altera a Le… 2019-03-13               0 https://ww…
    #>  7 129807  5948        "Proposta de… 2019-03-19               0 https://ww…
    #>  8 134910  5949        "PLC nº 135,… 2019-03-19               0 https://ww…
    #>  9 135088  5950        "Submete à a… 2019-03-26               1 https://ww…
    #> 10 123903  5951        "PEC nº 141,… 2019-04-02               0 https://ww…
    #> # … with 264 more rows, and 1 more variable: link_pdf <chr>

### Retrieve detailed information of a bill rollcall session


    fetchRollcallVotes(bill_id = 135251)
    #> # A tibble: 1 x 4
    #>   bill_id rollcall_info                           datetime            session_id
    #>     <dbl> <chr>                                   <dttm>              <chr>     
    #> 1  135251 "Submete à apreciação do Senado Federa… 2019-02-26 14:00:00 86315

### Retrieve rollcall votes hisotry


    infos <- fetchRollcallVotesByInterval(initial_date = "01/10/2020")


    print(infos)
    #> # A tibble: 3 x 7
    #>   bill_id rollcall_id rollcall_info datetime   votacao_secreta url_votacao
    #>   <chr>   <chr>       <chr>         <date>               <dbl> <chr>      
    #> 1 142499  6221        Votação do P… 2020-10-01               0 https://ww…
    #> 2 143156  6222        Votação do P… 2020-10-06               0 https://ww…
    #> 3 144655  6223        Votação do P… 2020-10-07               0 https://ww…
    #> # … with 1 more variable: link_pdf <chr>

    rollcalls = extractRollcallVotes(infos)
    #> [1] "Extracting details from rollcall id 6221"
    #> [1] "Extracting details from rollcall id 6222"
    #> [1] "Extracting details from rollcall id 6223"


    rollcalls %>% 
      select(rollcall_id, datetime, legislator_name, legislator_party, legislator_vote)
    #> # A tibble: 243 x 5
    #>    rollcall_id datetime   legislator_name   legislator_party legislator_vote    
    #>    <chr>       <date>     <chr>             <chr>            <chr>              
    #>  1 6221        2020-10-01 Alessandro Vieira CIDADANIA        Sim                
    #>  2 6221        2020-10-01 Eliziane Gama     CIDADANIA        AP                 
    #>  3 6221        2020-10-01 Jorge Kajuru      CIDADANIA        Sim                
    #>  4 6221        2020-10-01 Chico Rodrigues   DEM              AP                 
    #>  5 6221        2020-10-01 Davi Alcolumbre   DEM              AP                 
    #>  6 6221        2020-10-01 Jayme Campos      DEM              Sim                
    #>  7 6221        2020-10-01 Marcos Rogério    DEM              AP                 
    #>  8 6221        2020-10-01 Maria do Carmo A… DEM              AP                 
    #>  9 6221        2020-10-01 Rodrigo Pacheco   DEM              Presidente (art. 5…
    #> 10 6221        2020-10-01 Confúcio Moura    MDB              Sim                
    #> # … with 233 more rows
