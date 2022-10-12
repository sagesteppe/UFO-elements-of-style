library(jpeg)
library(RImagePalette)
library(scales)
library(here)
# set_here('/media/sagesteppe/ExternalHD/UFO_elements_of_style')
list.files()



path <- file.path(here(), '/data/photos/')


# here we run this for STRATA

juniper <- readJPEG(file.path(path, "juniper.jpg"))
juni_pal <- image_palette(juniper, n = 5)
display_image(juniper)
show_col(juni_pal)  #4A5A28 !!or #444B3E

sage <- readJPEG(file.path(path, "sage.jpg"))
sage_pal <- image_palette(sage, n = 5)
display_image(sage)
show_col(sage_pal) #C9CACF - #ACB0AA  !! or #D6DAE5 !!  #ADB1B9

aspen <- readJPEG(file.path(path, "aspen.jpg"))
aspen_pal <- image_palette(aspen, n = 5)
display_image(aspen)
show_col(aspen_pal) #B64841 !! or 

mancos <- readJPEG(file.path(path, "mancos.jpg"))
mancos_pal <- image_palette(mancos, n = 5)
display_image(mancos)
show_col(mancos_pal)  #E5CFA4 - 

mixedmtn <- readJPEG(file.path(path, "mixedmtn.jpg"))
mixedmtn_pal <- image_palette(mixedmtn, n = 5)
display_image(mixedmtn)
show_col(mixedmtn_pal)  #30201D

myPal <- c('#4A5A28', '#ADB1B9', '#CEB88E', '#574039', '#B64841')
show_col(myPal)

rm(aspen, aspen_pal, juni_pal, juniper, mancos, mancos_pal, mixedmtn, 
   mixedmtn_pal, sage, sage_pal)

# here we repeat the process for LIFEFORMS

# perennial grass ( green!), # annual grass (beige)
# perennial forb (purpleish), annual forb (pinkish), 
# tree (dark green), shrub brownish... 

annualgrass <- readJPEG(file.path(path, "annualgrass.jpg"))
annualgrass_pal <- image_palette(annualgrass, n = 5)
display_image(annualgrass)
show_col(annualgrass_pal)  #E4D7B1

pergrass <- readJPEG(file.path(path, "perennialgrass.jpg"))
pergrass_pal <- image_palette(pergrass, n = 5)
display_image(pergrass)
show_col(pergrass_pal)  #80A57B 


perforb <- readJPEG(file.path(path, "perforb.jpg"))
perforb_pal <- image_palette(perforb, n = 5)
display_image(perforb)
show_col(perforb_pal)  #994DC8

#annforb <- readJPEG(file.path(path, "annforb.jpg"))
#annforb_pal <- image_palette(annforb, n = 5)
#display_image(annforb) # use complementary color instead. #C84DB9 #C84DB9
#show_col(annforb_pal)  #7F9BE5 !!! #CA4AA8

succulent <- readJPEG(file.path(path, "succulent.jpg"))
succulent_pal <- image_palette(succulent, n = 5)
display_image(succulent)
show_col(succulent_pal)  #F4D11F 

shrub <- readJPEG(file.path(path, "shrub.jpg"))
shrub_pal <- image_palette(shrub, n = 5)
display_image(shrub)
show_col(shrub_pal)  #D4D7D4

lifeform_pal <- c('#E4D7B1', '#37AE0F', '#994DC8',  '#C84DB9', '#F4D11F', '#D4D7D4')
show_col(lifeform_pal)

# green - > #37AE0F
#860FAE perennial #AE0F86 annual

pal <- c('#37C864', '#30A934', '#AE0F86', '#860FAE', #
         '#DD4513', '#D4D7D4', '#e7b013')
show_col(pal)

rm(annforb, annforb_pal, annualgrass, annualgrass_pal, juni_pal, juniper,
   mixedmtn ,mixedmtn_pal, perforb, perforb_pal, pergrass, 
   pergrass_pal, shrub, shrub_pal, succulent, succulent_pal)


