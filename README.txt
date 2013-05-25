This is an experiment in splitting the Racket repository into
packages.

The conversion at this point is partial: some collections need to be
organized better into different or new packages, and some packages
don't specify their dependencies (either correctly or at all).

The "pkgs" directory contains the draft packages. Each directory is
intended as a postential separate git repository, but a single
repository can have multiple packages. So, some directories in "pkgs"
represent immediate packages, and some represent sets of packages to
be kept together in the same repository. Furthermore, some package
directories are single-colleciton packages, while others are
multi-collection packages. The scripts that link or set up catalogs
for the packages look for an "info.rkt" file to indicated a package
(i.e., even though a package is not constrained in general to have a
package-level "info.rkt" file, the packages in "pkgs" must each have
an "info.rkt" file).

See "INSTALL.txt" for information on building and creating
platform-specific installers from this repository.

About the Experimental Packages
-------------------------------

Some packages that install with not too many dependencies:

 - r5rs-lib
 - r6rs-lib
 - draw-lib
 - snip-lib
 - pict-lib
 - scribble-lib
 - gui-lib
 - web-server-lib
 - typed-racket-lib
 - slideshow-lib
 - readline-lib
 - profile-lib
 - xrepl-lib
 - macro-stepper-text-lib

Some other fun packages to try:

 - drracket (sans teaching languages)
 - htdp (depends on drracket)
 - picturing-programs (depends on htdp)

(The "distro-build" package is somewhat special. When building
platforms-specific installers, some of the libraries in "distro-build"
are accessed directly to bootstrap package setup, instead of through a
package installation.)


Part of this experiment is seeing what it looks like to separate
implementation from documentation, on the grounds that dependencies
grow much faster for documentation than for implementation. 

 - You can install any of the "-lib" packages without getting
   documentation, while the documentation is in a corersponding
   "-doc" package, and the package without "-lib" or "-doc" includes
   both. (This naming convention is subject to change, of course.)

 - The "-doc" packages depends on "racket-doc". 

   Note that "racket-doc" depends on many more things than just
   "racket", which illustrates how documentation dependencies grow
   faster. (The current dependencies are not quite right; possibly,
   most of the missing links should be supplied as indirect.)

   At the same time, the dependencies are build-time dependencies,
   which means they could be ignored for binary-package installation.

   The main hard dependency is "racket-index", which adds
   documention-building and -indexing support to `raco pkg' and `raco
   setup'.

 - There are a few "-lib"s without "-doc"s, because the documentation
   for the library is in some larger package. The "at-exp-lib" package
   is an example. Maybe there should be "at-exp-doc" and "at-exp"
   packages, anyway.

 - There are packages without "-lib" and "-doc" variants, such as
   "drracket". In that case, you can only get the variant with
   documentation, because it does not seem worthwhile to split
   them.

It's not clear that the "-lib" versus "-doc" split is wortwhile,
particularly given the possibility of binary packages to limit
dependencies, but the split is just part of the experiment.


License
-------

Racket
Copyright (c) 2010-2013 PLT Design Inc.

Racket is distributed under the GNU Lesser General Public License
(LGPL).  This means that you can link Racket into proprietary
applications, provided you follow the rules stated in the LGPL.  You can
also modify Racket; if you distribute a modified version, you must
distribute it under the terms of the LGPL, which in particular means
that you must release the source code for the modified software.  See
doc/release-notes/COPYING_LESSER.txt for more information.
