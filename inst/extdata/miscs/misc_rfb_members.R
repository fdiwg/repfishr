require(readr)
require(jsonlite)

cnt = as.data.frame( readr::read_csv("https://raw.githubusercontent.com/fdiwg/fdi-codelists/refs/heads/main/global/cwp/cl_country_and_territory_iso3.csv") )

rfbs = jsonlite::read_json("https://www.fao.org/figis/monikerj/figismapdata?format=json")

country_rfbs = do.call(rbind, lapply(rfbs$rfbs$rfb, function(rfb){
  data.frame(
    sender_type = "country",
    sender_id = unlist(rfb$members$country),
    sender_name = sapply(unlist(rfb$members$country), function(x){ cnt[cnt$code == x & !is.na(cnt$code),]$label }),
    receiver_type = "organization",
    receiver_id = rfb$name,
    receiver_name = rfb$descriptor$title,
    dcf = NA,
    dcf_ref_url = NA
  )
}))

country_rfbs = rbind(
  country_rfbs,
  data.frame(
    sender_type = "country",
    sender_id = unique(country_rfbs$sender_id),
    sender_name = sapply(unique(country_rfbs$sender_id), function(x){ cnt[cnt$code == x & !is.na(cnt$code),]$label }),
    receiver_type = "organization",
    receiver_id = "UN-FAO",
    receiver_name = "Food and Agriculture Organization of the United Nations",
    dcf = NA,
    dcf_ref_url = NA
  )
)

country_rfbs[country_rfbs$receiver_id == "GFCM",]$dcf = "GFCM DCRF"
country_rfbs[country_rfbs$receiver_id == "GFCM",]$dcf_ref_url = "https://www.fao.org/gfcm/data/dcrf"
country_rfbs[country_rfbs$receiver_id == "WECAFC",]$dcf = "WECAFC DCRF"
country_rfbs[country_rfbs$receiver_id == "WECAFC",]$dcf_ref_url = "https://www.fao.org/wecafc/data/dcrf"
country_rfbs[country_rfbs$receiver_id == "RECOFI",]$dcf = "RECOFI MDR PILOT"
country_rfbs[country_rfbs$receiver_id == "ICCAT",]$dcf = "ICCAT Reporting Requirements"
country_rfbs[country_rfbs$receiver_id == "ICCAT",]$dcf_ref_url = "https://www.iccat.int/en/submitSTAT.html"


readr::write_csv(country_rfbs, "inst/extdata/reporting_flow_actors.csv")
