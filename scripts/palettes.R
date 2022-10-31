public_lands_pal <- setNames(
  
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
    'BLM', 'USFS', 'NPS', 'FWS', 'USBR', 'TRIB', 'DOD', 'OTHF', 'SLB', 'PVT', 'CITY_CNTY_SDC_SDNR_SPR')
)

# scales::show_col(public_lands_pal) 

strata_pal <- setNames(
  
  #' these colours were selected via extraction of colour hex's from images using
  #' the RImagePalette package. Thanks to Megan Bach for taking multiple of the 
  #' images which were used in the process.

  c('#4A5A28', '#ADB1B9', '#CEB88E', '#574039', '#B64841',
    '#1357a6', '#1B1212', '#F9E076', '#39993A', '#00688B'),
  c('PJ', 'SS', 'SD', 'MMS', 'AS', 'RI', 'OT', 'GR', 'PP', 'MC')
  
)

# scales::show_col(strata_pal) 


lifeform_pal <- setNames(
  
  #' these colours were largely influenced by extraction of colour from images and
  #' then refined using canva. similar strata are closer in color space
  #' than less similar strata. 
  
  c('#37C864', '#30A934', '#AE0F86', '#860FAE','#DD4513', '#D4D7D4', '#e7b013'),
  c('AG', 'PG', 'AF', 'PF', 'T', 'SH', 'SU')
  
)


# scales::show_col(lifeform_pal) 
