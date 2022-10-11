public_lands_colours <- setNames(
  
  # these manually transcribed from the H-1553-Publications Standards Manual
  # Handbook - hopefully no errors.
  # [H-1553](https://www.ntc.blm.gov/krc/uploads/223/Ownership_Map_Color_Reference_Sheet.pdf)
  
  c( # colours
    rgb(254, 230, 121, max = 255), # BLM
    rgb(204, 235, 197, max = 255), # USFS
    rgb(202, 189, 220, max = 255), # NPS
    rgb(127, 204, 167, max = 255), # FWS
    rgb(255, 255, 179, max = 255), # USBR
    rgb(253, 180, 108, max = 255), # TRIB
    rgb(251, 180, 206, max = 255), # DOD
    rgb(228, 196, 159, max = 255), # OTHF
    rgb(179, 227, 238, max = 255), # SLB
    rgb(255, 255, 255, max = 255), # PVT
    rgb(143, 181, 190, max = 255) # CITY CNTY
  ), 
  
  c( # names
    'BLM', 'USFS', 'NPS', 'FWS', 'USBR', 'TRIB', 'DOD', 'OTHF', 'SLB', 'PVT', 'CITY_CNTY_SDC_SDNR_SPR'
  ))

strata_colours <- setNames(
  
  #' these colours were selected via extraction of colour hex's from images using
  #' the RImagePalette package. Thanks to Megan Bashfor taking multiple of the 
  #' images which were used in the process.

  c('#4A5A28', '#ADB1B9', '#CEB88E', '#574039', '#B64841'),
  c('PJ', 'SS', 'SD', 'MMS', 'AS')
  
)

# scales::show_col(strata_colours) 
# scales::show_col(public_lands_colours) 
