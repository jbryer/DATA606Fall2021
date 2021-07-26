DATA 606 Spring 2021
================

<a href='https://spring2021.data606.net'><img src='website/static/images/course_logo.png' align="right" height="200" /></a>

## Course Website

The course website is built created using the
[Blogdown](https://bookdown.org/yihui/blogdown) which extends the
[Hugo](https://gohugo.io) framework for generating static websites with
the use of RMarkdown files. Specifically, Blogdown will first convert
`Rmd` files to `md` files before being converted to HTML by Hugo. By
default, this repository will use the
[Techdoc](https://github.com/thingsym/hugo-theme-techdoc) theme.
Documentation on configuring the website is available here:
<https://themes.gohugo.io/theme/hugo-theme-techdoc/>

``` r
install.packages(c('blogdown', 'tidyverse', 'devtools', 'ggweekly', 'rlang', 
                   'stringr', 'configr', 'readxl', 'lubridate', 'XML')


blogdown::hugo_version()
blogdown::install_hugo()
blogdown::update_hugo()
```

``` r
blogdown::new_site(dir = 'website',
                   theme = "thingsym/hugo-theme-techdoc",
                   format = 'toml', to_yaml = FALSE)
```

``` r
wd <- setwd('website'); blogdown::serve_site(); setwd(wd)
```

``` r
blogdown::stop_server()
```

#### Steps to modify the template for your course:

-   `Schedule.xml` - This Excel file has two tabs that need be modified.
    The `Schedule` tab lists the date ranges for each unit/module in the
    course. The `Meetups` tab lists the dates and times for each meetup.
    There are `Topic` and `Resources` columns to provide additional
    information for students. Note that any Markdown used in the columns
    will be converted. This is particularly useful for including links
    to slides or recordings.
-   `website/config.toml` - Modify this file with values corresponding
    to your course. This includes the following variables:
    -   `title` - The course title.
    -   `github_repository` - The Github URL for where the site is
        hosted.
    -   `github_doc_repository` - This is the same as
        `github_repository` with the addition of `/website`
        subdirectory.
    -   Under params, `title` - The course title.
    -   Under the menu section, the `url` for the Slack (or whichever
        communication system you use) should be set.
    -   Other menu items can be added or removed in this section (these
        appear accross the top of each page).
-   `website/content/course-overview/_index.md` - Edit course syllabus.
-   `website/content/course-overview/instructor.md`
-   `website/content/course-overview/schedule.Rmd`
-   `website/content/course-overview/meetups.Rmd`
-   `website/content/course-overview/textbooks.md`
-   `website/content/course-overview/software.md`
-   `website/content/course-overview/materials.Rmd`
-   Edit files in the `website/content/assignments/` folder. Each
    markdown file should correspond to a particular assignment in the
    course. The exact structure is entirely up to you, but by way of
    example a page for each common type of assignment is provided,
    namely homework (i.e. textbook assignments), labs, project, and
    exam(s). Pages can be deleted, renamed, or newly created depending
    on your structure.
-   Edit files in the `website/content/chapters/` folder. The purpose of
    these files is to provide information (e.g. course notes, links,
    videos, etc.) for each unit of study. If your units are organized
    around chapters of a textbook, this structure may work for you. If
    you design your own learning modules, the folder can be renamed
    (e.g. to `modules`) and files as appropriate to your course
    organization.
-   The markdown files in the `website/content/blog/` folder correspond
    to announcements, or news items. These will be listed on the course
    homepage or from the “Announcements” link on the top navigation bar.

#### Navigation Bar

There are two ways to include links on the top navigation bar. 1.
Specify links in the `config.toml`. Look for the `[menu]` section of the
configuration file for details. 2. Include `menu: "main"` in the YAML of
any markdown file you wish to have linked in the navigation bar. The
Meetup page is included in the navigation bar by default.

#### Announcements / Posts

You can create announcements (blog posts) in the `website/content/blog/`
folder. There is an exmaple Rmarkdown file there that you can use as a
template. By default the site is configured to show the full post on the
homepage. If you wish to show only the summary (e.g. if your posts are
long) you can change that option in the
`website/themes/hugo-theme-techdoc/layouts/blog/summary.html` file. Note
the comment in that file indicating where `.Content` option can be
changed to `.Summary`.

#### Building and Deploying the Site

The `blogdown::build_site('webiste')` function will build the website
into the `docs` subdirectory. This is the default location for Github
pages. Be sure to verify this on the settings page for the repository
under the “GitHub Pages” heading.

``` r
blogdown::build_site()
```

Additionally, you can configure a custom URL if you wish on the Github
settings page. Once the website is built, push the `docs` folder to your
Github repository.

## Learning Management System (LMS)

Coming soon…
