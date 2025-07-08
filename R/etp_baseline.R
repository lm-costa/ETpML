### fun degree to rad
deg2rad <- function(x){
  return((x*pi)/180)
}

##Atantic Forest

files_names <- list.files('data-raw/bh_af/',full.names = T)
## NASA POWER

for (i in 1:length(files_names)){
  if(i ==1){
    df <- read.csv(files_names[i])
  }else{
    df_a <- read.csv(files_names[i])
    df <- rbind(df,df_a)
  }
}


tab_training <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year<2015) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )


tab_training <- data.frame(
  lon = tab_training$longitude,
  lat=tab_training$latitude,
  year=tab_training$year,
  month=tab_training$month,
  prec=tab_training$prec,
  temp=tab_training$temp,
  etp= tab_training$ETP)

write.csv(tab_training,'data/bh_af/tab_training.csv')

tab_validation <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year>2014) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )

tab_validation <- data.frame(
  lon = tab_validation$longitude,
  lat=tab_validation$latitude,
  year=tab_validation$year,
  month=tab_validation$month,
  prec=tab_validation$prec,
  temp=tab_validation$temp,
  etp= tab_validation$ETP)

write.csv(tab_validation,'data/bh_af/tab_validation.csv')


## Cerrado Biome
### CFSR

files_names <- list.files('data-raw/bh_cerr/',full.names = T)

## NASA POWER

for (i in 1:length(files_names)){
  if(i ==1){
    df <- read.csv(files_names[i])
  }else{
    df_a <- read.csv(files_names[i])
    df <- rbind(df,df_a)
  }
}


tab_training <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year<2015) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )


tab_training <- data.frame(
  lon = tab_training$longitude,
  lat=tab_training$latitude,
  year=tab_training$year,
  month=tab_training$month,
  prec=tab_training$prec,
  temp=tab_training$temp,
  etp= tab_training$ETP)

write.csv(tab_training,'data/bh_cerr/tab_training.csv')

tab_validation <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year>2014) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )

tab_validation <- data.frame(
  lon = tab_validation$longitude,
  lat=tab_validation$latitude,
  year=tab_validation$year,
  month=tab_validation$month,
  prec=tab_validation$prec,
  temp=tab_validation$temp,
  etp= tab_validation$ETP)

write.csv(tab_validation,'data/bh_cerr/tab_validation.csv')


## Caatinga Biome

files_names <- list.files('data-raw/bh_caat/',full.names = T)

## NASA POWER

for (i in 1:length(files_names)){
  if(i ==1){
    df <- read.csv(files_names[i])
  }else{
    df_a <- read.csv(files_names[i])
    df <- rbind(df,df_a)
  }
}


tab_training <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year<2015) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )


tab_training <- data.frame(
  lon = tab_training$longitude,
  lat=tab_training$latitude,
  year=tab_training$year,
  month=tab_training$month,
  prec=tab_training$prec,
  temp=tab_training$temp,
  etp= tab_training$ETP)

write.csv(tab_training,'data/bh_caat/tab_training.csv')

tab_validation <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year>2014) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )

tab_validation <- data.frame(
  lon = tab_validation$longitude,
  lat=tab_validation$latitude,
  year=tab_validation$year,
  month=tab_validation$month,
  prec=tab_validation$prec,
  temp=tab_validation$temp,
  etp= tab_validation$ETP)

write.csv(tab_validation,'data/bh_caat/tab_validation.csv')


## Amazon Biome


files_names <- list.files('data-raw/bh_amz/',full.names = T)

## NASA POWER

for (i in 1:length(files_names)){
  if(i ==1){
    df <- read.csv(files_names[i])
  }else{
    df_a <- read.csv(files_names[i])
    df <- rbind(df,df_a)
  }
}


tab_training <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year<2015) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )


tab_training <- data.frame(
  lon = tab_training$longitude,
  lat=tab_training$latitude,
  year=tab_training$year,
  month=tab_training$month,
  prec=tab_training$prec,
  temp=tab_training$temp,
  etp= tab_training$ETP)

write.csv(tab_training,'data/bh_amz/tab_training.csv')

tab_validation <- df |>
  dplyr::mutate(year= lubridate::year(YYYYMMDD),
                month = lubridate::month(YYYYMMDD),
                longitude=LON,
                latitude=LAT) |>
  dplyr::group_by(longitude, latitude, year, month) |>
  dplyr::summarise(
    prec = sum(PRECTOTCORR),
    temp=mean(T2M)
  ) |>
  dplyr::filter(year>2014) |>
  dplyr::mutate(
    nda= dplyr::case_when(
      month==1~1,
      month==2~31,
      month==3~59,
      month==4~90,
      month==5~120,
      month==6~151,
      month==7~181,
      month==8~212,
      month==9~243,
      month==10~273,
      month==11~304,
      month==12~334
    ),
    nd=dplyr::case_when(
      month==1~30,
      month==2~28,
      month==3~31,
      month==4~30,
      month==5~31,
      month==6~30,
      month==7~31,
      month==8~31,
      month==9~30,
      month==10~31,
      month==11~30,
      month==12~31
    ),
    lamb=23.45*sin(
      deg2rad((360/365)*(nda-81))
    ),
    x1=-tan(deg2rad(latitude)),
    x2= tan(deg2rad(lamb)),
    hn=acos((x1*x2))*180/pi,
    N = 2*hn/15,
    i = (0.2*temp)^1.514,
    I=sum(i),
    a=0.49+0.018*I-7.7*
      (10^-5)*(I^2)+6.75*(10^-7)*(I^3),
    ETP=16*((10*temp/I)^a*N/12*nd/30)
  )

tab_validation <- data.frame(
  lon = tab_validation$longitude,
  lat=tab_validation$latitude,
  year=tab_validation$year,
  month=tab_validation$month,
  prec=tab_validation$prec,
  temp=tab_validation$temp,
  etp= tab_validation$ETP)

write.csv(tab_validation,'data/bh_amz/tab_validation.csv')
