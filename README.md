# standard-R-output
An R library to output ggplot graphs and huxtable tables in a standard publication friendly format


# setup instructions for Arial on ubuntu
sudo apt install ttf-mscorefonts-installer
sudo apt-get install ttf2ufm
cd /usr/share/fonts/truetype/msttcorefonts
sudo ttf2ufm Arial*.ttf
sudo cp Arial.* ../../type1/
sudo fc-cache -f -v
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts()