beginfig(1);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
 % label.ulft(btex $\Theta$ etex, point 1.2 of kl );
  label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ$ etex, z00-(24,0));

  % Tf
  dotlabel.urt(btex $ $ etex, z10);
  label.top(btex $f(\theta^\circ)$ etex, z10+(24,0));

  % Operatorpfeil mit "T"
  drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  label.top(btex $f$ etex, (0,.25u));

endfig;



beginfig(2);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
%draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
 % label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ$ etex, z00-(5,0));
 % dotlabel.top(btex $\theta^\circ$ etex, z00);

  % Tf
  dotlabel.urt(btex $ $ etex, z10);
  label.top(btex $f(\theta^\circ)$ etex, z10+(10,0));
 % dotlabel.urt(btex $\lambda\theta^\circ$ etex, z10);

  % Operatorpfeil mit "T"
  drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  label.top(btex $f$ etex, (0,.25u));


  % Tf + epsilon
  path d;
  d = z10--z10-(0,1u);

  path e;
  e = z00--z00-(0,1.85u);

  z7 = point .5 of d;
  z8 = point .8 of e;

  draw z10--z7 dashed withdots scaled .2;
  dotlabel.rt(btex $ $ etex, z7);
  label.bot(btex $Y$ etex, z7+(24,0));

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $ $ etex, z8);
  label.ulft(btex $\widehat{\theta}=f^{-1}Y$ etex, z8+(2,0));

  path f;
  f = z7-(.05u,.05u){dir 200}..{dir 150}z8+(.05u,-.05u);
  drawarrow f dashed evenly;
  
  label.ulft(btex $f^{-1}$ etex, point .5 of f  +(20,4));

  

endfig;

beginfig(3);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
%draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
 % label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ$ etex, z00-(5,0));
 % dotlabel.top(btex $\theta^\circ$ etex, z00);

  % Tf
  dotlabel.urt(btex $ $ etex, z10);
  label.top(btex $\lambda\theta^\circ$ etex, z10+(10,0));
 % dotlabel.urt(btex $\lambda\theta^\circ$ etex, z10);

  % Operatorpfeil mit "T"
  drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  label.top(btex $\lambda$ etex, (0,.25u));


  % Tf + epsilon
  path d;
  d = z10--z10-(0,1u);

  path e;
  e = z00--z00-(0,1.85u);

  z7 = point .5 of d;
  z8 = point .6 of e;

  draw z10--z7 dashed withdots scaled .2;
  dotlabel.rt(btex $ $ etex, z7);
  label.bot(btex $Y$ etex, z7+(5,0));

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $ $ etex, z8);
  label.lft(btex $\widehat{\theta}^m=\lambda^{+}_m Y$ etex, z8 +(2,0));
  %label.top(btex $\widehat{\theta}^m=(Y_j/\lambda_j1_{\{j\leq m\}})_j$ etex, z8+(5,4));

  path f;
  f = z7-(.05u,.05u){dir 200}..{dir 150}z8+(.05u,-.05u);
  drawarrow f dashed evenly;
  
  %label.lrt(btex $\lambda_m^+=(\lambda_j^{-1}1_{\{j\leq m\}})$ etex, point .55 of f);
  label.ulft(btex $\lambda_m^+=(\lambda_j^{-1}1_{\{j\leq m\}})_j$ etex, point .5 of f +(35,4));

  

endfig;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% fester blauer kringel 4

beginfig(4);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  %draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
  %label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  % Tf
  %dotlabel.urt(btex $\lambda\theta^\circ$ etex, z10);

  % Operatorpfeil mit "T"
  %drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  %label.top(btex $\lambda$ etex, (0,.25u));


  % Tf + epsilon
  path d;
  d = z10--z10-(0,1u);

  path e;
  e = z00--z00-(0,1.85u);

  z7 = point .5 of d;
  z8 = point .52 of e;

  %draw z10--z7 dashed withdots scaled .2;
  %dotlabel.rt(btex $Y$ etex, z7);

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}^m$ etex, z8);

  %path f;
  %f = z7-(.05u,.05u){dir 200}..{dir 150}z8+(.05u,-.05u);
  %drawarrow f dashed evenly;
  
  %label.top(btex ${\lambda}_m^{+}$ etex, point .5 of f);



  % blauer kringel
  pickup pencircle scaled .8pt;
  draw o scaled 9.5 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;

  

endfig;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oberere und untere  Schranke 5-7

beginfig(5);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);

  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  z8 = point .4 of e;
  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}$ etex, z8);

  % blau kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled 8 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;
  
  % blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled 6.8 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;

endfig;

beginfig(6);

  u := 2cm;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);
  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;


  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );

  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  z8 = point .25 of e;

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}$ etex, z8);

  % blue kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled 5.5 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;

  % blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled 4.3 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;

endfig;

beginfig(7);

  u := 2cm;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );

  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  z8 = point .1 of e;

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}$ etex, z8);


  % blauer kringel upper 
  pickup pencircle scaled .8pt;
  draw o scaled 3 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;

  % blue kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled 1.5 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;


endfig;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Untere schranke 8-10

beginfig(8);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );

  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  z8 = point .4 of e;

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}^{m}$ etex, z8);

  % blauer kringel
  pickup pencircle scaled .8pt;
  draw o scaled 6.8 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;
  

endfig;

beginfig(9);

  u := 2cm;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);
  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;


  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;
  draw kl;

  label.ulft(btex $\Theta$ etex, point 1.2 of kl );

  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  z8 = point .25 of e;

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}^m$ etex, z8);


  % blauer kringel
  pickup pencircle scaled .8pt;
  draw o scaled 4.3 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;


endfig;

beginfig(10);

  u := 2cm;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;
  draw kl;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );

  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  z8 = point .1 of e;

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}^m$ etex, z8);

  % blue kringel
  pickup pencircle scaled .8pt;
  draw o scaled 1.5 shifted z00 withcolor blue dashed withdots;
  pickup pencircle scaled .5pt;

endfig;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ohne kringel 11

beginfig(11);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );

  % f zeichnen
  dotlabel.top(btex $\theta^\circ$ etex, z00);

  z8 = point .52 of e;

  draw z00--z8 dashed withdots scaled .2;
  dotlabel.lft(btex $\widehat{\theta}^m$ etex, z8);
  

endfig;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% intro 12-16


beginfig(12);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
%draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
 % label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ=(\theta^\circ_j)_j$ etex, z00-(24,0));

  % % Tf
  % dotlabel.urt(btex $ $ etex, z10);
  % label.top(btex $\lambda\theta^\circ=(\lambda_j\theta^\circ_j)_j$ etex, z10+(24,0));

  % % Operatorpfeil mit "T"
  % drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  % label.top(btex $\lambda=(\lambda_j)_j$ etex, (0,.25u));

endfig;

beginfig(13);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
%draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
 % label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ=(\theta^\circ_j)_j$ etex, z00-(24,0));

  % Tf
  dotlabel.urt(btex $ $ etex, z10);
  label.top(btex $\lambda\theta^\circ=(\lambda_j\theta^\circ_j)_j$ etex, z10+(24,0));

  % Operatorpfeil mit "T"
  drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  label.top(btex $\lambda=(\lambda_j)_j$ etex, (0,.25u));

endfig;

beginfig(14);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
%draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
 % label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ=(\theta^\circ_j)_j$ etex, z00-(24,0));

  % Tf
  dotlabel.urt(btex $ $ etex, z10);
  label.top(btex $\lambda\theta^\circ=(\lambda_j\theta^\circ_j)_j$ etex, z10+(24,0));

  % Operatorpfeil mit "T"
  drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  label.top(btex $\lambda=(\lambda_j)_j$ etex, (0,.25u));

  %inverser operator mit "T^-1"
  drawarrow z10+(-.05u,-.05u)..(0,-.25u)..z00+(.05u,-.05u);
  label.top(btex $\lambda^{-1}=(\lambda^{-1}_j)_j$ etex, (0,-.25u));

endfig;

beginfig(15);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
%draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
 % label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ=(\theta^\circ_j)_j$ etex, z00-(24,0));

  % Tf
  dotlabel.urt(btex $ $ etex, z10);
  label.top(btex $\lambda\theta^\circ=(\lambda_j\theta^\circ_j)_j$ etex, z10+(24,0));

  % Operatorpfeil mit "T"
  drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  label.top(btex $\lambda=(\lambda_j)_j$ etex, (0,.25u));

  % Tf + epsilon
  path d;
  d = z10--z10-(0,1u);

  path e;
  e = z00--z00-(0,1.85u);

  z7 = point .5 of d;
  z8 = point .8 of e;

  draw z10--z7 dashed withdots scaled .2;
  dotlabel.rt(btex $ $ etex, z7);
  label.bot(btex $Y=(Y_j)_j$ etex, z7+(24,0));

  % draw z00--z8 dashed withdots scaled .2;
  % dotlabel.lft(btex $ $ etex, z8);
  % label.ulft(btex $\widehat{\theta}=\lambda^{-1}Y$ etex, z8+(2,0));

  % path f;
  % f = z7-(.05u,.05u){dir 200}..{dir 150}z8+(.05u,-.05u);
  % drawarrow f dashed evenly;
  
  % label.ulft(btex $\lambda^{-1}=(\lambda_j^{-1})_j$ etex, point .5 of f  +(20,4));

endfig;

beginfig(16);

  u := 2cm;


  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);


  
  z10 = (2u,0u);
  z11 = (1u,0u);
  z12 = (2u,2u);
  z13 = (3u,0u);
  z14 = (2u,-2u);


  draw (0,-2.50u)--(0,2.50u) withcolor white;
    % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  
%draw (-4*u, -4*u)--(4*u,4*u) withcolor white;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  path kr;
  kr = z11{up}..z12..z13..z14..{up}z11;

  draw kl;
  draw kr;
  label.ulft(btex $\Theta$ etex, point 1.2 of kl );
 % label.urt(btex $ $ etex, point 1.2 of kr );

  z20 = (-4,0);

  % f zeichnen
  dotlabel.lft(btex $ $ etex, z00);
  label.top(btex $\theta^\circ=(\theta^\circ_j)_j$ etex, z00-(24,0));

  % Tf
  dotlabel.urt(btex $ $ etex, z10);
  label.top(btex $\lambda\theta^\circ=(\lambda_j\theta^\circ_j)_j$ etex, z10+(24,0));

  % Operatorpfeil mit "T"
  drawarrow z00+(.05u,.05u)..(0,.25u)..z10+(-.05u,.05u);
  label.top(btex $\lambda=(\lambda_j)_j$ etex, (0,.25u));

  % Tf + epsilon
  path d;
  d = z10--z10-(0,1u);

  path e;
  e = z00--z00-(0,1.85u);

  z7 = point .5 of d;
  z8 = point .8 of e;

  draw z10--z7 dashed withdots scaled .2;
  dotlabel.rt(btex $ $ etex, z7);
  label.bot(btex $Y=(Y_j)_j$ etex, z7+(24,0));

  % draw z00--z8 dashed withdots scaled .2;
  % dotlabel.lft(btex $ $ etex, z8);
  % label.ulft(btex $\widehat{\theta}=\lambda^{-1}Y$ etex, z8+(2,0));

  path f;
  f = z7-(.05u,.05u){dir 200}..{dir 150}z8+(.05u,-.05u);
  drawarrow f dashed evenly;
  
  label.ulft(btex $\lambda^{-1}=(\lambda_j^{-1})_j$ etex, point .5 of f  +(20,4));


endfig;


end;
