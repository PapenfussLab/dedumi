{
  stdenvNoCC,
  inara,
  pandoc,
  texlive,
  hack-font,
}:
stdenvNoCC.mkDerivation {
  name = "paper";
  src = inara;
  buildInputs = [
    pandoc
    (texlive.combine {
      inherit
        (texlive)
        scheme-basic
        latexmk
        marginnote
        xcolor
        preprint
        etoolbox
        titlesec
        pgf
        hyperxmp
        ifmtarg
        luacode
        luatexbase
        caption
        orcidlink
        tcolorbox
        environ
        seqsplit
        xstring
        float
        fontspec
        fontsetup
        unicode-math
        lualatex-math
        newcomputermodern
        selnolig
        ;
    })
  ];
  buildPhase = ''
    export HOME=$(mktemp -d)
    make ARTICLE=${./.}/paper.md
  '';
  installPhase = "cp -r publishing-artifacts $out";
  OSFONTDIR = hack-font;
}
