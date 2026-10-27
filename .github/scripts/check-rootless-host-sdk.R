# Exercise the native header/bootstrap path before the complete R dependency build.
source("install-pixi.R")
stopifnot(Sys.info()[["effective_user"]] != "root")
stopifnot(!file.exists("/usr/include/png.h"), !file.exists("/usr/include/zlib.h"))
before <- tools::md5sum("/var/lib/dpkg/status")
host_home <- R.home()
cpp <- "/usr/lib/x86_64-linux-gnu/libstdc++.so.6"
if (file.exists(cpp)) dyn.load(cpp, local = FALSE)
prefix <- file.path(Sys.getenv("RUNNER_TEMP"), "sdk-tools", ".pixi", "envs", "default")
library <- .drugsignet_default_host_library()
dir.create(library, recursive = TRUE, showWarnings = FALSE)
settings <- .drugsignet_prepare_host_sdk(prefix, library)
root <- Sys.getenv("PKG_CONFIG_SYSROOT_DIR")
stopifnot(nzchar(root), file.exists(file.path(root, ".ready")))
work <- tempfile("native-sdk-probe-")
dir.create(work)
setwd(work)
flags <- system2(Sys.which("pkg-config"), c("--cflags", "fontconfig", "freetype2", "libpng", "libxml-2.0", "zlib"), stdout = TRUE)
libs <- system2(Sys.which("pkg-config"), c("--libs", "fontconfig", "freetype2", "libpng", "libxml-2.0", "zlib"), stdout = TRUE)
writeLines(c(settings, paste0("PKG_CPPFLAGS = ", paste(flags, collapse = " ")),
             paste0("PKG_LIBS = ", paste(libs, collapse = " "))), "Makevars")
Sys.setenv(R_MAKEVARS_USER = file.path(work, "Makevars"))
writeLines(c('#include <R.h>', '#include <Rinternals.h>', '#include <zlib.h>',
 '#include <png.h>', '#include <libxml/parser.h>', '#include <fontconfig/fontconfig.h>',
 '#include <ft2build.h>', '#include FT_FREETYPE_H',
 'SEXP drugsignet_sdk_probe(void) {',
 '  FT_Library ft; int ok = FT_Init_FreeType(&ft) == 0;',
 '  if (ok) FT_Done_FreeType(ft);',
 '  xmlInitParser();',
 '  return ScalarLogical(ok && zlibVersion() && png_access_version_number() > 0 && FcGetVersion() > 0);',
 '}'), "probe.c")
stopifnot(system2(file.path(R.home(), "bin", "R"), c("CMD", "SHLIB", "probe.c")) == 0L)
dll <- dyn.load(paste0("probe", .Platform$dynlib.ext))
stopifnot(isTRUE(.Call("drugsignet_sdk_probe")))
dyn.unload(dll[["path"]])
stopifnot(identical(R.home(), host_home), identical(tools::md5sum("/var/lib/dpkg/status"), before))
stopifnot(!file.exists("/usr/include/png.h"), !file.exists("/usr/include/zlib.h"))
files <- basename(list.files(root, recursive = TRUE))
stopifnot(!any(grepl("^(libstdc\\+\\+|libgcc_s|libc\\.so|ld-linux)", files)))
message("User-local native SDK compiled and loaded successfully in the original unprivileged R process.")
