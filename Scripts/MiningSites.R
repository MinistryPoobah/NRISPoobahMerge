# Develop map of mining sites

#import most recent AMS auths.
# filter by the unique auth numbers on the LAN

mining_dir <- list.files("//Sfp.idir.bcgov/s140/s40086/WANSHARE/ROB/Mining/Authorizations 10400-60")

mining_auths <- unique(na.omit(str_extract(mining_dir, "[[:digit:]]+")))

mining_auths <- mining_auths[mining_auths != "2" & mining_auths != "2020"]