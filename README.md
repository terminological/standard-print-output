# standard-print-output
An R library to output ggplot graphs and huxtable tables in a standard publication friendly format that will play nicely with latex or journal submission systems.

## Installation

installation from github can be achieved with the usual:

```R
install.packages("devtools")
library(devtools)
install_github("terminological/standard-print-output")
library(standardPrintOutput)
```

Dependencies:

Needs a working ImageMagick:
https://imagemagick.org/script/download.php

Cairo installation is required:
https://www.cairographics.org/download/

PDF rotation is provided by pdftk
https://www.pdflabs.com/tools/pdftk-server/
http://manpages.ubuntu.com/manpages/xenial/man1/pdftk.1.html


### setup instructions for Arial on ubuntu

Arial fonts must be available on the system. This is less than simple on Linux.

```bash
sudo apt install ttf-mscorefonts-installer
sudo apt-get install ttf2ufm
cd /usr/share/fonts/truetype/msttcorefonts
sudo ttf2ufm Arial*.ttf
sudo cp Arial.* ../../type1/
sudo fc-cache -f -v
```

Followed by

```R
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts()
```

### installing webshot

webshot is a dependency and should be installed automatically but phantomjs needs to be installed also
(see http://wch.github.io/webshot/)

```R
install.packages("webshot")
webshot::install_phantomjs()
```

N.b. although webshot has a newer version which uses headless chrome this has less satisfactory pdf output than the phantomjs version

## Usage

The examples in the help pages are the easiest to get started.
In general the library assumes that the current working directory is pointing at where the output is required:

```R
# utilities for creating and publishing ggplot figures
?standardPrintOutput::defaultFigureLayout
?standardPrintOutput::narrowAndTall
?standardPrintOutput::saveFigure
?standardPrintOutput::saveFullPageFigure
?standardPrintOutput::saveHalfPageFigure
?standardPrintOutput::saveThirdPageFigure
?standardPrintOutput::saveSixthPageFigure
?standardPrintOutput::watermark

# utilities for creating and publishing huxtable tables
?standardPrintOutput::defaultTableLayout
?standardPrintOutput::mergeCells
?standardPrintOutput::saveTable
?standardPrintOutput::saveTableLandscape
?standardPrintOutput::saveMultiPage
?standardPrintOutput::saveMultiPageLandscape

# day of year and month of year scales for time series data
?standardPrintOutput::scale_day_of_year_discrete
?standardPrintOutput::scale_hours_continuous
?standardPrintOutput::scale_month_discrete
?standardPrintOutput::scale_week_continuous

```
