
get_final_source <- function(data) {


  data$fuente <- NA


  for (i in seq_along(data$source))  {

      organic <- grepl("start.iminent.com|websearch.com|
                       crawler.com|allmyweb.com",data$source[i], ignore.case = T)
      adwords <- grepl("cpc|ccp",data$source[i], ignore.case = T)
      referral <- grepl(".*google\\.com\\.pe.*|.*google\\.co\\.ve\\.*|
                        .*google\\.com\\.br.*|.*google\\.com\\.bo\\.*|
                        .*google\\.com\\.ar.*|.*google\\.com.*", data$source[i],
                        ignore.case = T)
      spam <- grepl("site.*|.*event.*|.*free.*|.*theguardlan.*|
                    ^guardlink.*|.*torture.*|.*forum.*|
                    .*buy.*|.*share.*|.*buttons.*|
                    .*pyme\\.lavoztx\\.com\\.*|.*amezon.*|
                    computrabajo.com.pe|.*porn.*|quality",
                    data$source[i],
                    ignore.case = T)

      adsense <- grepl("tpc.googlesyndication.com|
                       googleads[.]g[.]doubleclick[.]net",data$source[i], ignore.case = T)


      redes.sociales <- grepl("facebook.com|
                              twitter.com",data$source[i], ignore.case = T)



      if (data$source[i] == "(direct)") {
        data$fuente[i] <- "directo"
      }

      else if (data$Medium[i] == "organic" |
               organic) {
        data$fuente[i] <- "organico"
      }

      else if (data$source[i] == "google"
               & adwords) {

        data$fuente[i] <- "adwords"
      }

      else if (referral) {

        data$fuente[i] <- "referencias"
      }

      else if (adsense) {

        data$fuente[i] <- "adsense"
      }

      else if (redes.sociales) {

        data$fuente[i] <- "redes sociales"
      }


      else if (spam) {

        data$fuente[i] <- "spam"
      }



      else {
        data$fuente[i] <- data$fuente[i]
      }
  }

  data

}
