# %%
# install.packages("webshot")

# %%
# install.packages("webshot2")

# %%
# webshot::install_phantomjs()

# %%
html_text <- "<div style=\"border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:1000px; overflow-x: scroll; width:100%; \"><table class=\"table table-striped table-hover\" style=\"font-size: 16px; width: auto !important; margin-left: auto; margin-right: auto;\">
 <thead>
  <tr>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\"> Variable </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\">  </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\"> TOTAL Semaglutide vs Insulins Population </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\">  </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\"> Semaglutide Population </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\">  </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\"> Insulins Population </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\">  </th>
   <th style=\"text-align:right;position: sticky; top:0; background-color: #FFFFFF;\"> p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style=\"text-align:right;\"> n </td>
   <td style=\"text-align:right;\">  </td>
   <td style=\"text-align:right;\"> 8,872 </td>
   <td style=\"text-align:right;\">  </td>
   <td style=\"text-align:right;\"> 1,009 </td>
   <td style=\"text-align:right;\">  </td>
   <td style=\"text-align:right;\"> 7,863 </td>
   <td style=\"text-align:right;\">  </td>
   <td style=\"text-align:right;\">  </td>
  </tr>
  <tr>
   <td style=\"text-align:right;\"> Demographics </td>
   <td style=\"text-align:right;\"> Age (mean (SD)) </td>
   <td style=\"text-align:right;\"> 58 </td>
   <td style=\"text-align:right;\"> <span style=\"     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 42, 122, 255) !important;\">13.20%</span> </td>
   <td style=\"text-align:right;\"> 55 </td>
   <td style=\"text-align:right;\"> <span style=\"     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(72, 41, 121, 255) !important;\">12.93%</span> </td>
   <td style=\"text-align:right;\"> 58 </td>
   <td style=\"text-align:right;\"> <span style=\"     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 42, 122, 255) !important;\">13.18%</span> </td>
   <td style=\"text-align:right;\"> <span style=\" font-weight: bold;    color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(254, 206, 145, 255) !important;\">&lt;0.001</span> </td>
  </tr>
  <tr>
   <td style=\"text-align:right;\"> Sex </td>
   <td style=\"text-align:right;\"> Male </td>
   <td style=\"text-align:right;\"> 2,936 </td>
   <td style=\"text-align:right;\"> <span style=\"     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(54, 93, 141, 255) !important;\">33.1%</span> </td>
   <td style=\"text-align:right;\"> 222 </td>
   <td style=\"text-align:right;\"> <span style=\"     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(65, 66, 135, 255) !important;\">22.0%</span> </td>
   <td style=\"text-align:right;\"> 2,714 </td>
   <td style=\"text-align:right;\"> <span style=\"     color: white !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(52, 97, 141, 255) !important;\">34.5%</span> </td>
   <td style=\"text-align:right;\"> <span style=\" font-weight: bold;    color: black !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(254, 206, 145, 255) !important;\">&lt;0.001</span> </td>
  </tr>
</tbody>
</table></div>"

html_text <- as.character(html_text)
    IRdisplay::display_html(
    html_text
        )

# %%
# dir.create("html_tables")

# %%
html_filename <- "html_tables/Semaglutide_vs_Insulins-Basic_Demographics.html"
writeLines(html_text, html_filename)

# %%
png_filename <- "Semaglutide_vs_Insulins-Basic_Demographics.png"


# %%
webshot::webshot(html_filename, png_filename, debug = TRUE)

# %%
# Locate the webshot.js script
script <- system.file("bin/webshot.js", package = "webshot")

# Run PhantomJS directly, capturing all output
res <- system2("phantomjs",
               args = c(shQuote(script),
                        shQuote(html_filename),
                        shQuote(png_filename)),
               stdout = TRUE,
               stderr = TRUE)

# Inspect what went wrong
cat(res, sep = "\n")

# %%
f <- normalizePath(html_filename)
url <- paste0("file:///", f)
webshot::webshot(url, png_filename)

# %%
file.exists(html_filename)
normalizePath(html_filename)

# %%
webshot2::webshot(html_filename, png_filename)

# %%
chromote::find_chrome()

# %%

# %%
