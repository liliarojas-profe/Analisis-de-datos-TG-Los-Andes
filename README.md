# Analisis-de-datos-TG-Los-Andes
Documentos relativos a la etapa de análisis de datos del artículo de investigación educativa titulado "Factores asociados a los resultados de la evaluación de habilidades en Pensamiento Computacional para estudiantes de grado undécimo - Colegio El Libertador", presentado como trabajo de grado para optar al título de "Maestría en Educación" de la Universidad de Los Andes en el año 2020.

Este repositorio contiene dos carpetas:

 - Datos: En esta carpeta se encuentran las bases de datos utilizadas durante el proceso, en formatos .cvs, .xslx y .RData, además del archivo original del análisis, con comentarios, en código R.
 - Informe: En esta carpeta se encuentran: El archivo del informe de análisis en formato .pdf, el archivo del informe de análsis en formato .Rmd, la base de datos de referencia, el archivo .ris de bibliografía y el archivo .csl de formato de documento.

NOTA: Todos los documentos de este repositorio hacen parte de los anexos del artículo "Factores asociados a los resultados de la evaluación de habilidades en Pensamiento Computacional para estudiantes de grado undécimo - Colegio El Libertador", autora: Rojas F., Lilia C.,  por lo cual deben ser citados como tales.

NOTA 2: El informe se elaboró utilizando R-Markdown para facilitar la presentación del análisis y su actualización automática, en caso de correcciones o adiciones de nuevos datos. RMarkdown es un tipo de documento de RStudio que permite integrar texto con código de R.


### Licencia de uso R
R version 4.0.3 (2020-10-10) -- "Bunny-Wunnies Freak Out"
Copyright (C) 2020 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is a project which is attempting to provide a modern piece of
statistical software for the GNU suite of software.

The current R is the result of a collaborative effort with
contributions from all over the world.


Authors of R.

R was initially written by Robert Gentleman and Ross Ihaka—also known as "R & R"
of the Statistics Department of the University of Auckland.

Since mid-1997 there has been a core group with write access to the R
source, currently consisting of

Douglas Bates
John Chambers
Peter Dalgaard
Robert Gentleman
Kurt Hornik
Ross Ihaka
Tomas Kalibera
Michael Lawrence
Friedrich Leisch
Uwe Ligges
Thomas Lumley
Martin Maechler
Martin Morgan
Paul Murrell
Martyn Plummer
Brian Ripley
Deepayan Sarkar
Duncan Temple Lang
Luke Tierney
Simon Urbanek

plus Heiner Schwarte up to October 1999, Guido Masarotto up to June 2003,
Stefano Iacus up to July 2014, Seth Falcon up to August 2015 and Duncan Murdoch
up to September 2017.


Current R-core members can be contacted via email to R-project.org
with name made up by replacing spaces by dots in the name listed above.

(The authors of code from other projects included in the R distribution
are listed in the COPYRIGHTS file.)

R would not be what it is today without the invaluable help of these
people outside of the (former and current) R Core team, who
contributed by donating code, bug fixes and documentation:

Valerio Aimale, Suharto Anggono, Thomas Baier, Gabe Becker, Henrik
Bengtsson, Roger Bivand, Ben Bolker, David Brahm, G"oran Brostr"om,
Patrick Burns, Vince Carey, Saikat DebRoy, Matt Dowle, Brian D'Urso,
Lyndon Drake, Dirk Eddelbuettel, Claus Ekstrom, Sebastian
Fischmeister, John Fox, Paul Gilbert, Yu Gong, Gabor Grothendieck,
Frank E Harrell Jr, Peter M. Haverty, Torsten Hothorn, Robert King,
Kjetil Kjernsmo, Roger Koenker, Philippe Lambert, Jan de Leeuw, Jim
Lindsey, Patrick Lindsey, Catherine Loader, Gordon Maclean, Arni
Magnusson, John Maindonald, David Meyer, Ei-ji Nakama, Jens
Oehlschl"agel, Steve Oncley, Richard O'Keefe, Hubert Palme, Roger
D. Peng, Jose' C. Pinheiro, Tony Plate, Anthony Rossini, Jonathan
Rougier, Petr Savicky, Guenther Sawitzki, Marc Schwartz, Arun
Srinivasan, Detlef Steuer, Bill Simpson, Gordon Smyth, Adrian
Trapletti, Terry Therneau, Rolf Turner, Bill Venables, Gregory
R. Warnes, Andreas Weingessel, Morten Welinder, James Wettenhall,
Simon Wood, and Achim Zeileis.

Others have written code that has been adopted by R and is
acknowledged in the code files, including

J. D. Beasley, David J. Best, Richard Brent, Kevin Buhr, Michael
A. Covington, Bill Cleveland, Robert Cleveland,, G. W. Cran,
C. G. Ding, Ulrich Drepper, Paul Eggert, J. O. Evans, David M. Gay,
H. Frick, G. W. Hill, Richard H. Jones, Eric Grosse, Shelby Haberman,
Bruno Haible, John Hartigan, Andrew Harvey, Trevor Hastie, Min Long
Lam, George Marsaglia, K. J. Martin, Gordon Matzigkeit,
C. R. Mckenzie, Jean McRae, Cyrus Mehta, Fionn Murtagh, John C. Nash,
Finbarr O'Sullivan, R. E. Odeh, William Patefield, Nitin Patel, Alan
Richardson, D. E. Roberts, Patrick Royston, Russell Lenth, Ming-Jen
Shyu, Richard C. Singleton, S. G. Springer, Supoj Sutanthavibul, Irma
Terpenning, G. E. Thomas, Rob Tibshirani, Wai Wan Tsang, Berwin
Turlach, Gary V. Vaughan, Michael Wichura, Jingbo Wang, M. A. Wong,
and the Free Software Foundation (for autoconf code and utilities).
See also files under src/extras.

Many more, too numerous to mention here, have contributed by sending bug
reports and suggesting various improvements.

Simon Davies whilst at the University of Auckland wrote the original
version of glm().

Julian Harris and Wing Kwong (Tiki) Wan whilst at the University of
Auckland assisted Ross Ihaka with the original Macintosh port.

R was inspired by the S environment which has been principally
developed by John Chambers, with substantial input from Douglas Bates,
Rick Becker, Bill Cleveland, Trevor Hastie, Daryl Pregibon and
Allan Wilks.

A special debt is owed to John Chambers who has graciously contributed
advice and encouragement in the early days of R and later became a
member of the core team.

Stefano Iacus (a former member of R Core) and Simon Urbanek developed
the macOS port, including the R.app GUI, toolchains and packaging.

The Windows port was developed by Guido Masarotto (for a while a
member of R Core) and Brian Ripley, then Duncan Murdoch (a former
member of R Core) and currently by Jeroen Ooms (base) and Uwe Ligges
(packages).

Tomas Kalibera's work has been sponsored by Jan Vitek and funded by
his European Research Council grant "Evolving Language Ecosystems
(ELE)".

Computing support (including hardware, hosting and infrastructure) has
been provided/funded by the R Foundation, employers of R-Core members
(notably WU Wien, ETH Zurich, U Oxford and U Iowa) and by Northeastern
University and the University of Kent.

Distributions of R contain the recommended packages, whose
authors/contributors are listed in their DESCRIPTION files.
