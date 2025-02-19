library(downlit)
library(xml2)
library(httr)
library(jsonlite)

create_pub_listing <- function(bib_file, author = "Hongxiang", author_zh = '范宏翔') {
  bib <- strsplit(paste(readLines(bib_file), collapse = "\n"), "\n@")[[1]]
  articles <- lapply(
    X = paste0("@", bib[bib != ""]),
    FUN = function(ibib) {
      f <- tempfile()
      on.exit(unlink(f))
      writeLines(ibib, f)
      article <- tail(
        head(
          system(
            command = paste("pandoc", f, "--standalone", "--from=bibtex", "--to=markdown"),
            intern = TRUE
          ),
          -2
        ),
        -3
      )
      # authors <- sub(".*- family: ", "", grep("- family:", article, value = TRUE))
      
      if (grepl("chinese", grep("keyword:", article, value = TRUE))) {
        authors <- sub(".*- family: ", "", grep("- family:", article, value = TRUE))
        if (grepl("firstauthor", grep("keyword:", article, value = TRUE))) {
          first <- "  first: '*As first or corresponding*'"
        } else {
          first <- sprintf("  first: '%s'", paste(rep("&emsp;", 3), collapse = ""))
        }
        position <- sprintf("  position: '%s/%s'", grep(author_zh, authors), length(authors))
      } else {
        authors <- sub(".*given: ", "", grep("given:", article, value = TRUE))
        if (grepl("firstauthor", grep("keyword:", article, value = TRUE))) {
          first <- "  first: '*As first or corresponding*'"
        } else {
          first <- sprintf("  first: '%s'", paste(rep("&emsp;", 3), collapse = ""))
        }
        position <- sprintf("  position: '%s/%s'", grep(author, authors), length(authors))
      }
      
      article <- c(
        article,
        sub("  container-title: (.*)", "  journal-title: '*\\1*'", grep("  container-title:", article, value = TRUE)),
        sub("  issued: ", "  date: ", grep("  issued:", article, value = TRUE)),
        sub("  doi: ", "  path: https://doi.org/", grep("doi:", article, value = TRUE)),
        position,
        first
      )
      article
    }
  )
  writeLines(text = unlist(articles), con = sub("\\.bib$", ".yml", bib_file))
  
  write.csv(data.frame(
    table(
      sapply(articles, 
             function(x) sub("-.*", "", sub("  issued: ", "", grep("  issued:", x, value = TRUE)))))
  ),
  sub("\\.bib$", ".csv", bib_file), quote = F, row.names = F
  )
  
  yaml_text <- c(
    "---",
    "title: 'Publications (%s)'",
    "page-layout: full",
    "title-block-banner: true",
    "image: /assets/images/social-profile.png",
    "date-format: 'MMMM,<br>YYYY'",
    "listing:",
    "  contents:",
    "    - publications.yml",
    "  page-size: 10",
    "  sort: 'issued desc'",
    "  type: table",
    "  categories: false",
    "  sort-ui: [date, title, journal-title, position, first]",
    "  filter-ui: [date, title, journal-title]",
    "  fields: [date, title, journal-title, first, position]",
    "  field-display-names:",
    "    date: Issued",
    "    journal-title: Journal",
    "    position: Rank",
    "    first: 'First'",
    "---"
  )
  
  writeLines(
    text = sprintf(
      yaml_text,
      paste(
        table(
          factor(
            x = sapply(articles, function(x) any(grepl("As first or corresponding", x))),
            levels = c("TRUE", "FALSE")
          )
        )[c("TRUE", "FALSE")],
        collapse = " + "
      )
    ),
    con = sub("\\.bib$", ".qmd", bib_file)
  )
}

# 加载必要的包


# 定义 GitHub 仓库和文件路径
owner <- "hxfan1227"  # 替换为 GitHub 用户名
repo <- "curriculum-vitae"   # 替换为仓库名称
path <- "data/cv.bib" # 替换为文件路径，例如 "README.md"

# 构建 API URL
url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path)

response <- GET(url)

# 检查请求是否成功
if (status_code(response) == 200) {
  # 解析 JSON 响应
  file_info <- fromJSON(content(response, "text"))
  
  # 获取文件内容（Base64 编码）
  file_content <- file_info$content
  
  # 解码 Base64 内容
  decoded_content <- rawToChar(base64enc::base64decode(file_content))
  
  # 打印文件内容
  writeLines(decoded_content, 'publications.bib')
} else {
  # 如果请求失败，打印错误信息
  cat("Failed to fetch file. Status code:", status_code(response), "\n")
}

create_pub_listing("publications.bib")

unlink('publications.bib')
