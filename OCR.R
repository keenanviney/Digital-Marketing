library(tesseract)
library(imagerExtra)
library(magick)


#loads the image
papers=image_read("/Users/joeyl/Desktop/creative images/SPL137.png")

#this runs the OCR
eng <- tesseract("eng")
text <- tesseract::ocr(p, engine = eng)
cat(text)

#this had the best result
p=image_negate(papers)
p3=image_modulate(p, brightness = 120, saturation = 50, hue = 30)

#other cleaning attempts

papers= image_reducenoise(papers)
papers2=image_convert(papers, type = 'Greyscale')
p3=image_modulate(p, brightness = 120, saturation = 50, hue = 30)



image_convert()$Type

p2=image_fill(p, "orange", point = "+200+300", fuzz = 30)

image_convert(papers, colorspace = black)

hello <- DenoiseDCT(papers, 0.01) %>% ThresholdAdaptive(., 0.1, range = c(0,1))




#part 2 color extract
library(RImagePalette)
library(paletter)
papers=image_read("/Users/joeyl/Desktop/creative images/SPL137.png")

convert.g(path = NULL, fileroot= "*", from = "pdf",
          to = "png", create.path = TRUE, options = NULL)

create_palette(papers, number_of_colors =20, type_of_variable = "categorical")

display_image(papers)
image_palette(papers)




