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

pal <- c('#37C864', '#30A934', '#AE0F86', '#860FAE',
         '#DD4513', '#D4D7D4', '#e7b013')

show_col(pal)


