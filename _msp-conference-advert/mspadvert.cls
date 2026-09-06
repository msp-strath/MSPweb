\ProvidesClass{mspadvert}

\PassOptionsToClass{pdftex,british,final,aspectratio=169}{beamer}
\LoadClass{beamer}

\RequirePackage{relsize}
\RequirePackage{etoolbox}
\RequirePackage{adjustbox}
\RequirePackage{strathclyde-colours}
\RequirePackage{strathclyde-tikz}

\RequirePackage{xkeyval}
\RequirePackage{ifthen}

\RequirePackage[skins]{tcolorbox}

\colorlet{bgcolour}{strath-green} %default
\newcommand{\drawPic}{} %default
\DeclareOptionX{colour}[strath-green]{%
  \ifthenelse{\equal{#1}{random}}%
  {\pgfmathsetseed{\number\pdfrandomseed}%
    \pgfmathrandominteger{\rr}{1}{255}%
    \pgfmathrandominteger{\gg}{1}{255}%
    \pgfmathrandominteger{\bb}{1}{255}%
    \definecolor{random}{RGB}{\rr,\gg,\bb}%
  }{}%
  \colorlet{bgcolour}{#1}}
\DeclareOptionX{picture}{%
  \renewcommand{\drawPic}{%
    \begin{scope}
    \def\ig{\includegraphics[height=3cm]{#1}}%
    \node [inner sep=0pt](mypicture) at ($ (opac.south east) + (-2cm, 0cm)$) {\phantom{\ig}};%
    \clip[rounded corners=5mm] ($(mypicture.south west)+(1mm,1mm)$) rectangle ($(mypicture.north east)-(1mm,1mm)$);%
    \node[inner sep=0pt](mypicture) at ($ (opac.south east) + (-2cm, 0cm)$) {\ig};%
    \draw[black, rounded corners=5mm, line width=3pt] ($(mypicture.south west)+(1mm,1mm)$) rectangle ($(mypicture.north east)-(1mm,1mm)$);%
  \end{scope}}%
}
\ProcessOptionsX

\DeclareGraphicsExtensions{.pdf,.png,.eps,.gif,.jpg}

\setbeamertemplate{navigation symbols}{}

\setbeamertemplate{background}
{
  \includegraphics[keepaspectratio, width=\paperwidth]{msp-background}
}
\setbeamertemplate{headline}{}
\setbeamertemplate{footline}{}

\usetikzlibrary{calc}

\newcommand{\conferenceHook}{}

\setbeamertemplate{title page}
{%
  \begin{tikzpicture}[remember picture, overlay]
    \begin{pgfonlayer}{background layer}
      \fill[bgcolour, nearly transparent]
           (current page.north west) rectangle (current page.south east);

       \end{pgfonlayer}
       \node (uni)
             [anchor = south west]
             at (current page.south west)
             {\includegraphics[keepaspectratio, width=0.3\paperwidth]{strath_main}};

       \node (msp)
             [anchor = south east]
             at (current page.south east)
             {\includegraphics[keepaspectratio, height=0.15\paperheight]{msp}};

       \node (mspname)
             [ left
             , align=right
             , text width = 0.25\paperwidth
             , font=\bfseries
             ]
             at (msp.west) {};

        \node (conf)
              [font=\bfseries\huge]
              at (mspname.west)
            {
              \conferenceHook{}
            }
            ;

    \begin{pgfonlayer}{background layer}
      \node (badges) [fit = (current page.south west) (msp.north east), fill=white] {};
    \end{pgfonlayer}
    \node (title)
          [ font=\bfseries\Large
          , text width = 0.8\paperwidth
          , align = left
          , below = 0.1\paperheight of current page.north
          ]
          {
            \inserttitle
          };

    \node (author)
            [ font=\bfseries\large
            , text width=0.8\paperwidth,
            , align=left
            , below = 0.03\paperheight of title
            ]
            {\insertauthor}
            ;

    \node (institute)
            [ font=\bfseries\large
            , text width=0.8\paperwidth,
            , align=left
            , below = 0.03\paperheight of author
            ]
            {\insertinstitute}
            ;

     \node (date)
           [ font=\bfseries
           , text width=0.8\paperwidth
           , align=left
           , below = 0.03\paperheight of institute.south
           ]
           {\insertdate}
           ;
     \begin{pgfonlayer}{background layer}
        \node (opac) [fit= (title) (date), fill=white, draw=none, semitransparent]{};
     \end{pgfonlayer}
     \drawPic
    \end{tikzpicture}
}

\endinput
