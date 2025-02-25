require(jsonlite)
rfbs = jsonlite::read_json("https://www.fao.org/figis/monikerj/figismapdata?format=json")
country_rfbs = do.call(rbind, lapply(rfbs$rfbs$rfb, function(rfb){
  data.frame(
    sender_type = "country",
    sender = unlist(rfb$members$country),
    receiver_type = "organization",
    receiver = rfb$name,
    dcf = NA,
    dcf_ref_url = NA
  )
}))

country_rfbs = rbind(
  country_rfbs,
  data.frame(
    sender_type = "country",
    sender = unique(country_rfbs$sender),
    receiver_type = "organization",
    receiver = "UN-FAO",
    dcf = NA,
    dcf_ref_url = NA
  )
)

country_rfbs[country_rfbs$receiver == "GFCM",]$dcf = "GFCM DCRF"
country_rfbs[country_rfbs$receiver == "GFCM",]$dcf_ref_url = "https://www.fao.org/gfcm/data/dcrf"
country_rfbs[country_rfbs$receiver == "WECAFC",]$dcf = "WECAFC DCRF"
country_rfbs[country_rfbs$receiver == "WECAFC",]$dcf_ref_url = "https://www.fao.org/wecafc/data/dcrf"
country_rfbs[country_rfbs$receiver == "RECOFI",]$dcf = "RECOFI MDR PILOT"
country_rfbs[country_rfbs$receiver == "ICCAT",]$dcf = "ICCAT Reporting Requirements"
country_rfbs[country_rfbs$receiver == "ICCAT",]$dcf_ref_url = "https://www.iccat.int/en/submitSTAT.html"


readr::write_csv(country_rfbs, "inst/extdata/reporting_flow_actors.csv")
