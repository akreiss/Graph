%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Oberere und untere  Schranke 5-7

beginfig(1);

  u := 2cm;
  b := -.3u;
  d :=3.5;
  e :=2.5;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);

  
  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta^{\circ}$ etex, point 1.2 of kl );

  z10 = z00+(.4u,1.2u);
  z11 = z00+(.5u,.4u);
  z12 = z00+(-.4u,1.2u);
  z13 = z00+(-.5u,.4u);
  z14 = z00+(.4u,-1.2u);
  z15 = z00+(.5u,-.4u);
  z16 = z00+(-.4u,-1.2u);
  z17 = z00+(-.5u,-.4u);


  z20 = z10+(0u,b-.05u);
  z21 = z11+(0u,b+.1u);
  z22 = z12+(0u,b+.15u);
  z23 = z13+(0u,b+.05u);
  z24 = z14+(0u,b+.4u);
  z25 = z15+(0u,b+.35u);
  z26 = z16+(0u,b+.45u);
  z27 = z17+(0u,b+.5u);

  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z10);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z11);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z12);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z13);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z14);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z15);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z16);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z17);

%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z20);
%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z21);
%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z22);
%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z23);
%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z24);
%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z25);
%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z26);
%  % f zeichnen
%  dotlabel.lft(btex $\widetilde{\theta}$ etex, z27);

  % black contraction casewise
  pickup pencircle scaled .8pt;
  draw o scaled 3.0 shifted z10;
  pickup pencircle scaled .8pt;
  draw o scaled 2.0 shifted z11;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z13;
  pickup pencircle scaled .8pt;
  draw o scaled 1.0 shifted z14;
  pickup pencircle scaled .8pt;
  draw o scaled 1.5 shifted z15;
  pickup pencircle scaled .8pt;
  draw o scaled 2.2 shifted z16;
  pickup pencircle scaled .8pt;
  draw o scaled 2.3 shifted z17;
  
  % red kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z17 withcolor red dashed withdots;


% blauer kringel lower
  % pickup pencircle scaled .8pt;
  % draw o scaled 4.3 shifted z00 withcolor red dashed withdots;


endfig;


beginfig(2);

  u := 2cm;
  b := -.3u;
  d :=3.5;
  e :=2.5;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);

  


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta^{\circ}$ etex, point 1.2 of kl );

  z10 = z00+(.4u,1.2u);
  z11 = z00+(.5u,.4u);
  z12 = z00+(-.4u,1.2u);
  z13 = z00+(-.5u,.4u);
  z14 = z00+(.4u,-1.2u);
  z15 = z00+(.5u,-.4u);
  z16 = z00+(-.4u,-1.2u);
  z17 = z00+(-.5u,-.4u);


  z20 = z10+(0u,b-.05u);
  z21 = z11+(0u,b+.1u);
  z22 = z12+(0u,b+.15u);
  z23 = z13+(0u,b+.05u);
  z24 = z14+(0u,b+.4u);
  z25 = z15+(0u,b+.35u);
  z26 = z16+(0u,b+.45u);
  z27 = z17+(0u,b+.5u);

  % z20 = z10+(0u,b);
  % z21 = z11+(0u,b);
  % z22 = z12+(0u,b);
  % z23 = z13+(0u,b);
  % z24 = z14+(0u,b);
  % z25 = z15+(0u,b);
  % z26 = z16+(0u,b);
  % z27 = z17+(0u,b);

  % f zeichnen
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z10);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z11);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z12);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z13);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z14);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z15);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z16);
  % f zeichnen
  dotlabel.lft(btex $\theta^{\circ}$ etex, z17);

  % black contraction casewise
  pickup pencircle scaled .8pt;
  draw o scaled 3.0 shifted z10;
  pickup pencircle scaled .8pt;
  draw o scaled 2.0 shifted z11;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z13;
  pickup pencircle scaled .8pt;
  draw o scaled 1.0 shifted z14;
  pickup pencircle scaled .8pt;
  draw o scaled 1.5 shifted z15;
  pickup pencircle scaled .8pt;
  draw o scaled 2.2 shifted z16;
  pickup pencircle scaled .8pt;
  draw o scaled 2.3 shifted z17;

  
  % red kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 3.5 shifted z17 withcolor red dashed withdots;


% blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled 2.5 shifted z17 withcolor red dashed withdots;


endfig;




beginfig(3);

  u := 2cm;
  b := -.25u;
  d :=3;
  e :=2;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);

  


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta^{\circ}$ etex, point 1.2 of kl );

  z10 = z00+(.4u,1.2u);
  z11 = z00+(.5u,.4u);
  z12 = z00+(-.4u,1.2u);
  z13 = z00+(-.5u,.4u);
  z14 = z00+(.4u,-1.2u);
  z15 = z00+(.5u,-.4u);
  z16 = z00+(-.4u,-1.2u);
  z17 = z00+(-.5u,-.4u);

  z20 = z10+(0u,b);
  z21 = z11+(0u,b);
  z22 = z12+(0u,b);
  z23 = z13+(0u,b);
  z24 = z14+(0u,b);
  z25 = z15+(0u,b);
  z26 = z16+(0u,b);
  z27 = z17+(0u,b);


  % f zeichnen
  dotlabel.lft(btex  etex, z10);
  % f zeichnen
  dotlabel.lft(btex  etex, z11);
  % f zeichnen
  dotlabel.lft(btex  etex, z12);
  % f zeichnen
  dotlabel.lft(btex  etex, z13);
  % f zeichnen
  dotlabel.lft(btex  etex, z14);
  % f zeichnen
  dotlabel.lft(btex  etex, z15);
  % f zeichnen
  dotlabel.lft(btex  etex, z16);
  % f zeichnen
  dotlabel.lft(btex  etex, z17);

    % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z20);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z21);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z22);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z23);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z24);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z25);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z26);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z27);


  %   % f zeichnen
  % dotlabel.lft(btex  etex, z20);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z21);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z22);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z23);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z24);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z25);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z26);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z27);

  
  % red kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z17 withcolor red dashed withdots;


% blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z17 withcolor red dashed withdots;


endfig;

beginfig(4);

  u := 2cm;
  b := -.2u;
  d :=2.5;
  e :=1.5;
  
  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);

  


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta^{\circ}$ etex, point 1.2 of kl );

  z10 = z00+(.4u,1.2u);
  z11 = z00+(.5u,.4u);
  z12 = z00+(-.4u,1.2u);
  z13 = z00+(-.5u,.4u);
  z14 = z00+(.4u,-1.2u);
  z15 = z00+(.5u,-.4u);
  z16 = z00+(-.4u,-1.2u);
  z17 = z00+(-.5u,-.4u);

  z20 = z10+(0u,b);
  z21 = z11+(0u,b);
  z22 = z12+(0u,b);
  z23 = z13+(0u,b);
  z24 = z14+(0u,b);
  z25 = z15+(0u,b);
  z26 = z16+(0u,b);
  z27 = z17+(0u,b);


  % f zeichnen
  dotlabel.lft(btex  etex, z10);
  % f zeichnen
  dotlabel.lft(btex  etex, z11);
  % f zeichnen
  dotlabel.lft(btex  etex, z12);
  % f zeichnen
  dotlabel.lft(btex  etex, z13);
  % f zeichnen
  dotlabel.lft(btex  etex, z14);
  % f zeichnen
  dotlabel.lft(btex  etex, z15);
  % f zeichnen
  dotlabel.lft(btex  etex, z16);
  % f zeichnen
  dotlabel.lft(btex  etex, z17);

    % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z20);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z21);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z22);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z23);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z24);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z25);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z26);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z27);


  %   % f zeichnen
  % dotlabel.lft(btex  etex, z20);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z21);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z22);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z23);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z24);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z25);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z26);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z27);

  
  % red kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z17 withcolor red dashed withdots;


% blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z17 withcolor red dashed withdots;


endfig;

beginfig(5);

  u := 2cm;
  b := -.15u;
  d :=2;
  e :=1.5;
  
  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);

  


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta^{\circ}$ etex, point 1.2 of kl );

  z10 = z00+(.4u,1.2u);
  z11 = z00+(.5u,.4u);
  z12 = z00+(-.4u,1.2u);
  z13 = z00+(-.5u,.4u);
  z14 = z00+(.4u,-1.2u);
  z15 = z00+(.5u,-.4u);
  z16 = z00+(-.4u,-1.2u);
  z17 = z00+(-.5u,-.4u);

  z20 = z10+(0u,b);
  z21 = z11+(0u,b);
  z22 = z12+(0u,b);
  z23 = z13+(0u,b);
  z24 = z14+(0u,b);
  z25 = z15+(0u,b);
  z26 = z16+(0u,b);
  z27 = z17+(0u,b);


  % f zeichnen
  dotlabel.lft(btex  etex, z10);
  % f zeichnen
  dotlabel.lft(btex  etex, z11);
  % f zeichnen
  dotlabel.lft(btex  etex, z12);
  % f zeichnen
  dotlabel.lft(btex  etex, z13);
  % f zeichnen
  dotlabel.lft(btex  etex, z14);
  % f zeichnen
  dotlabel.lft(btex  etex, z15);
  % f zeichnen
  dotlabel.lft(btex  etex, z16);
  % f zeichnen
  dotlabel.lft(btex  etex, z17);

    % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z20);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z21);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z22);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z23);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z24);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z25);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z26);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z27);


  %   % f zeichnen
  % dotlabel.lft(btex  etex, z20);
  % % f zeichnen
  % dotlabel.lft(btex  etex, z21);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z22);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z23);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z24);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z25);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z26);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z27);

  
  % red kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z17 withcolor red dashed withdots;


% blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z17 withcolor red dashed withdots;


endfig;

beginfig(6);

  u := 2cm;
  b := -.1u;
  d :=1.5;
  e :=0.75;
  
  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);

  


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta^{\circ}$ etex, point 1.2 of kl );

  z10 = z00+(.4u,1.2u);
  z11 = z00+(.5u,.4u);
  z12 = z00+(-.4u,1.2u);
  z13 = z00+(-.5u,.4u);
  z14 = z00+(.4u,-1.2u);
  z15 = z00+(.5u,-.4u);
  z16 = z00+(-.4u,-1.2u);
  z17 = z00+(-.5u,-.4u);

  z20 = z10+(0u,b);
  z21 = z11+(0u,b);
  z22 = z12+(0u,b);
  z23 = z13+(0u,b);
  z24 = z14+(0u,b);
  z25 = z15+(0u,b);
  z26 = z16+(0u,b);
  z27 = z17+(0u,b);


  % f zeichnen
  dotlabel.lft(btex  etex, z10);
  % f zeichnen
  dotlabel.lft(btex  etex, z11);
  % f zeichnen
  dotlabel.lft(btex  etex, z12);
  % f zeichnen
  dotlabel.lft(btex  etex, z13);
  % f zeichnen
  dotlabel.lft(btex  etex, z14);
  % f zeichnen
  dotlabel.lft(btex  etex, z15);
  % f zeichnen
  dotlabel.lft(btex  etex, z16);
  % f zeichnen
  dotlabel.lft(btex  etex, z17);

  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z20);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z21);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z22);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z23);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z24);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z25);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z26);
  % % f zeichnen
  % dotlabel.lft(btex $\widehat{\theta}$ etex, z27);

  
  % red kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z17 withcolor red dashed withdots;


% blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z17 withcolor red dashed withdots;


endfig;



beginfig(7);

  u := 2cm;
  b := -.3u;
  d :=3.5;
  e :=2.5;

  z00 = (-2u,0u);
  z01 = (-1u,0u);
  z02 = (-2u,2u);
  z03 = (-3u,0u);
  z04 = (-2u,-2u);

  


  draw (0,-2.50u)--(0,2.50u) withcolor white;
  % kringel
  path o;
  o = (-.1u,0)..(0,-.1u)..(.1u,0)..(0,.1u)..cycle;

  path kl;
  kl = z01{up}..z02..z03..z04..{up}z01;

  draw kl;
  label.ulft(btex $\Theta^{\circ}$ etex, point 1.2 of kl );

  z10 = z00+(.4u,1.2u);
  z11 = z00+(.5u,.4u);
  z12 = z00+(-.4u,1.2u);
  z13 = z00+(-.5u,.4u);
  z14 = z00+(.4u,-1.2u);
  z15 = z00+(.5u,-.4u);
  z16 = z00+(-.4u,-1.2u);
  z17 = z00+(-.5u,-.4u);

  
  % z20 = z10+(0u,b);
  % z21 = z11+(0u,b);
  % z22 = z12+(0u,b);
  % z23 = z13+(0u,b);
  % z24 = z14+(0u,b);
  % z25 = z15+(0u,b);
  % z26 = z16+(0u,b);
  % z27 = z17+(0u,b);

  z20 = z10+(0u,b-.05u);
  z21 = z11+(0u,b+.1u);
  z22 = z12+(0u,b+.15u);
  z23 = z13+(0u,b+.05u);
  z24 = z14+(0u,b+.4u);
  z25 = z15+(0u,b+.35u);
  z26 = z16+(0u,b+.45u);
  z27 = z17+(0u,b+.5u);


  % f zeichnen
  dotlabel.lft(btex  etex, z10);
  % f zeichnen
  dotlabel.lft(btex  etex, z11);
  % f zeichnen
  dotlabel.lft(btex  etex, z12);
  % f zeichnen
  dotlabel.lft(btex  etex, z13);
  % f zeichnen
  dotlabel.lft(btex  etex, z14);
  % f zeichnen
  dotlabel.lft(btex  etex, z15);
  % f zeichnen
  dotlabel.lft(btex  etex, z16);
  % f zeichnen
  dotlabel.lft(btex  etex, z17);


  
    % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z20);
  % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z21);
  % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z22);
  % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z23);
  % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z24);
  % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z25);
  % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z26);
  % f zeichnen
  dotlabel.lft(btex $\widehat{\theta}$ etex, z27);

  
  % red kringel upper
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled d shifted z17 withcolor red dashed withdots;


% blauer kringel lower
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z10 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z11 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z12 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z13 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z14 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z15 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z16 withcolor red dashed withdots;
  pickup pencircle scaled .8pt;
  draw o scaled e shifted z17 withcolor red dashed withdots;


endfig;


end;
