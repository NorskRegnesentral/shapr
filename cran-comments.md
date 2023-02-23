
# Patch release, shapr 0.2.1

* fix warning from development version of data.table due to the use of nomatch argument in merge()


## Test environments

* GitHub Actions (windows-latest): R 4.0
* GitHub Actions (ubuntu-16.04): R 4.0, 3.6, 3.5
* GitHub Actions (macOS-latest): R-release, 4.0
* win-builder (x86_64-w64-mingw32): R 4.0, 3.6, R-devel
* local Ubuntu 18.04: R 3.6
* local Windows 10: R 4.0
* R-hub (windows-x86_64-devel): R-devel
* R-hub (macos-highsierra-release-cran): R-release

* local Ubuntu 18.04: R 3.6 (without packages in Suggests): 
```devtools::check(vignettes = FALSE, env_vars=c(`_R_CHECK_DEPENDS_ONLY_` = "true"))```

## R CMD check results

There were no ERRORs or WARNINGs.

There was 2 NOTES 

*NOTE 1 (on local Windows 10: R 4.0):

  Note: information on .o files for i386 is not available
  Note: information on .o files for x64 is not available
  File 'C:/Users/jullum/Dropbox/Local_work/Git/shapr.Rcheck/shapr/libs/i386/shapr.dll':
    Found '_exit', possibly from '_exit' (C)
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
  File 'C:/Users/jullum/Dropbox/Local_work/Git/shapr.Rcheck/shapr/libs/x64/shapr.dll':
    Found '_exit', possibly from '_exit' (C)
    Found 'abort', possibly from 'abort' (C), 'runtime' (Fortran)
    Found 'exit', possibly from 'exit' (C), 'stop' (Fortran)
    Found 'printf', possibly from 'printf' (C)
  
  Compiled code should not call entry points which might terminate R nor
  write to stdout/stderr instead of to the console, nor use Fortran I/O
  nor system RNGs. The detected symbols are linked into the code but
  might come from libraries and not actually be called.
  
  See 'Writing portable packages' in the 'Writing R Extensions' manual.

> I believe this is a false-positive ref https://stackoverflow.com/questions/64402688/information-on-o-files-for-x64-is-not-available-note-on-r-package-checks-using

*NOTE 2 (on all winbuilder + R-hub servers)

Days since last update: 6

> The previous release was a basic patch after the package was taken off CRAN. This is a proper release with new features.

## Downstream dependencies
There are currently no downstream dependencies for this package.
