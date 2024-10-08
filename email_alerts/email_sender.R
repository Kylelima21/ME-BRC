## Script to produce and send the email alerts to those signed up
## Schoodic Institute at Acadia National Park


#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
library(blastula)



#------------------------------------------------#
####         Create and send email            ####
#------------------------------------------------#

## Can run to see example of what the email will look like
# my_email_object <- render_email('outputs/emailtest.Rmd')
# print(my_email_object)


## Create the credentials key needed to send from my email
# create_smtp_creds_key(
#   id = "kmail",
#   user = "klima@schoodicinstitute.org",
#   provider = "gmail",
#   #overwrite = TRUE
# )

# create_smtp_creds_file(
#   file = "kmail",
#   user = "klima@schoodicinstitute.org",
#   provider = "gmail"
# )


## Produce and send the email
smtp_send(render_email('email_alerts/email_material.Rmd'),
          from = "klima@schoodicinstitute.org",
          to = c("klima@schoodicinstitute.org", "bmarvil1@gmail.com", "dhitchcox@mac.com", "lrbevier@colby.edu"),
          subject = "ME-BRC iNaturalist Report",
          credentials = creds_file("email_alerts/kmail")
)




