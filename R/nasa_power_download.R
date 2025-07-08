df <- read.csv('data-raw/coord.csv',sep=';')


df |>
  ggplot2::ggplot(ggplot2::aes(x=longitude,y=latitude,col=Biome))+
  ggplot2::geom_point()

power <- function(lon,lat,bh){
  df <- nasapower::get_power(
  community = 'ag',
  lonlat = c(lon,lat),
  pars = c('ALLSKY_SFC_SW_DWN','RH2M','T2M','PRECTOTCORR','WS2M','WD2M'),
  dates = c('1981-01-01','2021-12-31'),
  temporal_api = 'daily'
)
  write.csv(df,paste0('data-raw/',bh,'/',lon,'_',lat,'.csv'))
}

### Caatinga data download

tab <- df |> dplyr::filter(Biome=="CAA") |>
  dplyr::mutate(lon=longitude,lat=latitude) |>
  dplyr::select(lon,lat) |>
  as.data.frame()


for (i in 1:nrow(tab)){
  repeat{
    dw <- try(
      power(tab[i,1],tab[i,2],'bh_caat')
              )
    if (!(inherits(dw,"try-error")))
      break
  }
}


### Cerrado data download

tab <- df |> dplyr::filter(Biome=="CER") |>
  dplyr::mutate(lon=longitude,lat=latitude) |>
  dplyr::select(lon,lat) |>
  as.data.frame()


for (i in 1:nrow(tab)){
  repeat{
    dw <- try(
      power(tab[i,1],tab[i,2],'bh_cerr')
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}


### Amazon data download

tab <- df |> dplyr::filter(Biome=="AMZ") |>
  dplyr::mutate(lon=longitude,lat=latitude) |>
  dplyr::select(lon,lat) |>
  as.data.frame()


for (i in 1:nrow(tab)){
  repeat{
    dw <- try(
      power(tab[i,1],tab[i,2],'bh_amz')
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}



### Atlantic Forest data download

tab <- df |> dplyr::filter(Biome=="MAT") |>
  dplyr::mutate(lon=longitude,lat=latitude) |>
  dplyr::select(lon,lat) |>
  as.data.frame()


for (i in 1:nrow(tab)){
  repeat{
    dw <- try(
      power(tab[i,1],tab[i,2],'bh_af')
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}
