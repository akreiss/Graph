

beginfig(1);

  u := 1cm;

z0 = (0,0);
z1 = (0,u);
z2 = (u,0);
z3 = (0,-u);
z4 = (-u,0);

draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;

path p;
p = z1..z2..z3..z4..z1;

draw p;

z5 = point 1.4 of p;
z6 = point 3.6 of p;
z7 = point 1 of p;



% dotlabel.top("0",z1);


% dotlabel.lrt(btex $X$ etex, z5); 

% dotlabel.ulft(btex $\varepsilon$ etex ,z6);

% dotlabel.rt(btex $Y$ etex ,z7);



endfig;


beginfig(2);

  u := 1cm;

z0 = (0,0);
z1 = (0,u);
z2 = (u,0);
z3 = (0,-u);
z4 = (-u,0);

draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;

path p;
p = z1..z2..z3..z4..z1;

draw p;

z5 = point 1.4 of p;
z6 = point 3.6 of p;
z7 = point 1 of p;



dotlabel.top("0",z1);


% dotlabel.lrt(btex $X$ etex, z5); 

% dotlabel.ulft(btex $\varepsilon$ etex ,z6);

% dotlabel.rt(btex $Y$ etex ,z7);



endfig;



beginfig(3);

  u := 1cm;

z0 = (0,0);
z1 = (0,u);
z2 = (u,0);
z3 = (0,-u);
z4 = (-u,0);

draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;

path p;
p = z1..z2..z3..z4..z1;

draw p;

z5 = point 1.4 of p;
z6 = point 3.6 of p;
z7 = point 1.0 of p;



dotlabel.top("0",z1);


dotlabel.lrt(btex $X$ etex, z5); 

z15 = (2.25)[z0,z5];
drawarrow z0--z15 dashed withdots scaled .25;





% dotlabel.ulft(btex $\varepsilon$ etex ,z6);

% dotlabel.rt(btex $Y$ etex ,z7);



endfig;

beginfig(4);

  u := 1cm;

z0 = (0,0);
z1 = (0,u);
z2 = (u,0);
z3 = (0,-u);
z4 = (-u,0);

draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;
path p;
p = z1..z2..z3..z4..z1;

draw p;

z5 = point 1.4 of p;
z6 = point 3.6 of p;
z7 = point 1.0 of p;



dotlabel.top("0",z1);


dotlabel.lrt(btex $X$ etex, z5); 
z15 = (2.25)[z0,z5];
drawarrow z0--z15 dashed withdots scaled .25;



dotlabel.ulft(btex $U$ etex ,z6);



% dotlabel.rt(btex $Y$ etex ,z7);



endfig;
beginfig(5);

  u := 1cm;

z0 = (0,0);
z1 = (0,u);
z2 = (u,0);
z3 = (0,-u);
z4 = (-u,0);

draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;
path p;
p = z1..z2..z3..z4..z1;

draw p;

dotlabel.top("0",z1);

z5 = point 1.4 of p;
z6 = point 3.6 of p;
z7 = point 1.0 of p;






dotlabel.lrt(btex $X$ etex, z5); 
z15 = (2.25)[z0,z5];
drawarrow z0--z15 dashed withdots scaled .25;


dotlabel.ulft(btex $U$ etex ,z6);

dotlabel.rt(btex $Y$ etex ,z7);



endfig;





% beginfig(5);

% u:=1cm;

% z0 = (0,0);
% z1 = (0,u);
% z2 = (u,0);
% z3 = (0,-u);
% z4 = (-u,0);


% path p;
% p = z1..z2..z3..z4..z1;


% z9 = point 1.4 of p;
% z10 = point 3.6 of p;
% z11 = point 1.0 of p;

% dotlabel.lrt(btex $X$ etex, z9); 

% dotlabel.ulft(btex $\varepsilon$ etex ,z10);

% dotlabel.rt(btex $Y$ etex ,z11);





% z5 = (-u,-1.5*u);
% z6 = (u,-1.5*u);

% draw z1..z2..z3..z4..cycle;

% draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;

% dotlabel.top(btex $0 \equiv 1$ etex,z1);


% endfig;


beginfig(6);

u:=1cm;

z0 = (0,0);
z1 = (0,u);
z2 = (u,0);
z3 = (0,-u);
z4 = (-u,0);

z5 = (-1.5*u,-2*u);
z6 = (1.5*u,-2*u);

draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;

path p;
p = z1..z2..z3..z4..z1;


z9 = point 1.4 of p;
z10 = point 3.6 of p;
z11 = point 1.0 of p;

dotlabel.lrt(btex $X$ etex, z9); 

dotlabel.ulft(btex $U$ etex ,z10);

dotlabel.rt(btex $Y$ etex ,z11);





draw z1..z2..z3..z4..cycle;

dotlabel.top(btex $0 \equiv 1$ etex,z1);


draw z5--z6;
dotlabel.bot("0",z5);
label.bot("1",z6);


z12 = (0.65)[z5,z6];
dotlabel.bot(btex $X$ etex, z12);

z13 = (0.1)[z5,z6];
dotlabel.bot(btex $U$ etex, z13);

z14 = (0.75)[z5,z6];
dotlabel.bot(btex $Y$ etex, z14);

endfig;




beginfig(7);

u:=1cm;

z0 = (0,0);
z1 = (0,u);
z2 = (u,0);
z3 = (0,-u);
z4 = (-u,0);

z5 = (-1.5*u,-2*u);
z6 = (1.5*u,-2*u);

draw (-3*u, -3*u)--(3*u,1.4*u) withcolor white;

path p;
p = z1..z2..z3..z4..z1;


z9 = point 1.4 of p;
z10 = point 3.6 of p;
z11 = point 1.0 of p;

dotlabel.lrt(btex $X$ etex, z9); 

dotlabel.ulft(btex $U$ etex ,z10);

dotlabel.rt(btex $Y$ etex ,z11);



draw p;
dotlabel.top(btex $0 \equiv 1$ etex,z1);

draw z5--z6;
dotlabel.bot("0",z5);

label.bot("1",z6);


z7 = (1/4)[z5,z6];
%z8 = (-u,-u);

% label.llft(btex $s$ etex, z8);
drawarrow z7{dir 105}..{dir 0}z4 dashed evenly;



z12 = (0.65)[z5,z6];
dotlabel.bot(btex $X$ etex, z12);

z13 = (0.1)[z5,z6];
dotlabel.bot(btex $U$ etex, z13);

z14 = (0.75)[z5,z6];
dotlabel.bot(btex $Y$ etex, z14);


endfig;











end;


