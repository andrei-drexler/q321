#pragma once
// auto-generated, do not modify (see cook_shader.js)

// trim_newlines       : true
// consistent_floats   : false
// rename_ids          : true
// rename_vec_fields   : true
// rename_globals      : true
// min_macro_savings   : 8

// src/demo/data/shaders/fragment_shaders.glsl: 62477 => 26578 (42.5%)
const char g_fragment_shaders[] =
"#define B vec2\n"
"#define C float\n"
"#define F vec3\n"
"#define I return\n"
"#define J mix\n"
"#define L abs\n"
"#define P fract\n"
"#define Q vec4\n"
"#define T max\n"
"#define U length\n"
"#define V min\n"
"#define W rgb\n"
"#define Y int\n"
"#define aa UV\n"
"#define ad if\n"
"#define ae FCol\n"
"#define ag Time\n"
"#define ah void\n"
"#define am for\n"
"#define ao Texture0\n"
"#define aq floor\n"
"#define ar texture\n"
"#define ay dot\n"
"#define a2 sqrt\n"
"#define aQ normalize\n"
"uniform Q ag,Cam;uniform sampler2D ao,Texture1;in F Pos,Nor,Ref;in B aa,LUV;in Q Clr;out Q ae;\n"
"#define bh(x)((x)*(x)*(3.-2.*(x)))\n"
"#define O(x)(x)*(x)\n"
"#define bi(x)ay(x,x)\n"
"#define ai(x)clamp(x,0.,1.)\n"
"#define K(r,g,b)(F(r,g,b)/255.)\n"
"C a6=3.1415927,bj=2.*a6,aS=1.618034;B bJ(C i){C G=1.324718;I P(.5+i/B(G,G*G));}C a7(C i){I P(.5+i*aS);}C bK(C f){C i=aq(f);I J(a7(i),a7(i+1.),bh(f-i));}C aT(B v){I V(v.r,v.g);}C aT(F v){I V(v.r,V(v.g,v.b));}C aT(Q v){I V(V(v.r,v.g),V(v.b,v.a));}C aC(B v){I T(v.r,v.g);}C aC(F v){I T(v.r,T(v.g,v.b));}C aC(Q v){I T(T(v.r,v.g),T(v.b,v.a));}C aU(B v){I v.r+v.g;}C bk(C a,C b){I L(a)<L(b)?a:b;}C bl(C x,C s){I L(x)-s;}C aZ(C x,C s){I sign(x)*T(0.,L(x)-s);}B bL(B v){C l=ay(v,v);I l>0.?v/a2(l):v;}C D(C bM,C bN,C x){I 1.-ai(L(x-bM)/bN);}C E(C bm,C bO,C x){I ai((x-bm)/(bO-bm));}C D(C a,C b,C c,C x){I V(E(a,b,x),E(c,b,x));}B aj(C x){I B(sin(x),cos(x));}mat2 a3(C x){B v=aj(radians(x));I mat2(v.g,v.r,-v.r,v.g);}C bn(B p){I P(atan(p.g,p.r)/bj);}B af(B A,C p,C s){I A+sin(A.gr*a6*p)*s;}B af(B A,C t,C p,C s){I A+sin(A.gr*a6*p+t)*s;}C ba(C v,C m){I m-L(v-m);}B ba(B v,C m){v.r=ba(v.r,m);I v;}B bo(B A,B s){A.r+=aq(A.g*s.g)*(.5/s.r);I P(A)*s;}F aI(B A,C r){I F(A-=clamp(A,r,1.-r),U(A)/r);}F aI(B A,B s,C r){s=s.gr/aT(s);A*=s;I F(A-=clamp(A,B(r),s-r),U(A)/r);}C H(B p){F q=P(p.rgr*.09153);q+=ay(q,q.gbr+19.19);I P((q.r+q.g)*q.b);}C H(C p){p=P(p*.1031);p*=p+33.33;p*=p+p;I P(p);}F cm(C p){F as=P(F(p)*F(.1031,.1030,.0973));as+=ay(as,as.gbr+33.33);I P((as.rrg+as.gbb)*as.bgr);}B bP(B p){F as=P(F(p.rgr)*F(.1031,.1030,.0973));as+=ay(as,as.gbr+33.33);I P((as.rr+as.gb)*as.bg);}Q bQ(C p){Q at=P(Q(p)*Q(.1031,.1030,.0973,.1099));at+=ay(at,at.abrg+33.33);I P((at.rrgb+at.gbba)*at.bgar);}Q bQ(B p){Q at=P(Q(p.rgrg)*Q(.1031,.1030,.0973,.1099));at+=ay(at,at.abrg+33.33);I P((at.rrgb+at.gbba)*at.bgar);}C ak(C x,C p){I H(mod(x,p));}C N(C x){C i;I J(H(i=aq(x)),H(i+1.),bh(x-i));}C X(C x,C p){C i;I J(ak(i=aq(x),p),ak(i+1.,p),x-i);}C ak(B p,B s){I H(mod(p,s));}C X(B p,B s){p*=s;B i=aq(p);p-=i;p*=p*(3.-2.*p);C bR=ak(i+B(0,0),s);C bS=ak(i+B(0,1),s);C bT=ak(i+B(1,1),s);C bU=ak(i+B(1,0),s);I J(J(bR,bU,p.r),J(bS,bT,p.r),p.g);}C M(B p,B a4,C aJ,C aK,Y bc){C aL=X(p,a4),a8=1.,tw=1.;am(Y i=0;i<bc;++i){p=P(p+aS);a4*=aK;a8*=aJ;aL+=X(p,a4)*a8;tw+=a8;}I aL/tw;}C M(B p,B a4,C aJ,C aK){I M(p,a4,aJ,aK,4);}\n"
"#define a9(v)D(.5,.5,v)\n"
"C bp(B p,B a4,C aJ,C aK,Y bc){C aL=a9(X(p,a4)),a8=1.,tw=1.;am(Y i=0;i<bc;++i){p=P(p+aS);a4*=aK;a8*=aJ;aL+=a9(X(p,a4))*a8;tw+=a8;}I aL/tw;}B aV(B p,B a,B b){B ab=b-a,ap=p-a;C t=ai(ay(ap,ab)/ay(ab,ab));I ab*t+a;}C Z(B p,B b){B d=L(p)-b;I V(T(d.r,d.g),0.)+U(T(d,0.));}C aM(B p,B b){I aC(L(p)-b);}C ac(B p,C r){I U(p)-r;}C aN(B p,B r){I ac(p/r,1.)/V(r.r,r.g);}C aA(C a,C b){I T(a,-b);}C bq(C a,C b,C k){C h=ai(.5+0.5*(b-a)/k);I J(b,a,h)-k*h*(1.-h);}B bs(C x){B d=B(dFdx(x),dFdy(x));I d/T(U(d),1e-8);}C S(C s,C d){I ai(1.-s/d);}C S(C s){I ai(1.-s/fwidth(s));}\n"
"#define aW(bV,A,bW){B p[3];C r[3];p[0]=A;p[1]=A+dFdx(A);p[2]=A+dFdy(A);am(Y i=0;i<3;++i)r[i]=bW;bV=F(bL(B(r[1],r[2])-r[0]),r[0]);}\n"
"Q bt(B A,C s){I Q(A/=s,a2(ai(1.-bi(A))),U(A)-1.);}C bX(F n){C l=aU(n.gb)*.7;I pow(ai(l),4.)+l;}C bu(B A,C s){A/=s;A.g+=.06;A.r*=2.;I E(.3,.0,U(A));}F aO(F c,B A,C s){Q b=bt(A,s);c*=1.+bX(b.W)*S(b.a)*.5;c*=1.-O(bu(A,20.*s))*(1.-S(b.a))*.3;I c;}F bw(B p,C aj,C bY){F e=F(-1,0,1),r=F(1e5);B au=aq(p*aj),cn=e.gg;p-=(au+.5)/aj;C al=.5*ak(au+e.gg,B(aj)),aD=.5*ak(au+e.rg,B(aj)),aE=.5*ak(au+e.gb,B(aj)),aF=.5*ak(au+e.bg,B(aj)),aG=.5*ak(au+e.gr,B(aj)),by=.5*ak(au+e.rb,B(aj)),bz=.5*ak(au+e.bb,B(aj)),b0=.5*ak(au+e.br,B(aj)),b1=.5*ak(au+e.rr,B(aj));B[4]az,l;ad(mod(au.r+au.g,2.)<.5){l[0]=1.+B(aG-aD,al-b1);l[1]=1.+B(aF-aG,al-b0);l[2]=1.+B(aE-aD,by-al);l[3]=1.+B(aF-aE,bz-al);az[0]=B(aG,al);az[1]=B(aG,al);az[2]=B(aE,al);az[3]=B(aE,al);}else{l[0]=1.+B(al-b1,aD-aG);l[1]=1.+B(b0-al,aF-aG);l[2]=1.+B(al-by,aE-aD);l[3]=1.+B(bz-al,aE-aF);az[0]=B(al,aD);az[1]=B(al,aF);az[2]=B(al,aD);az[3]=B(al,aF);}am(Y i=0;i<4;i++){az[i]+=l[i]*(B(i&1,i/2)-.5);l[i]/=aj;C bx=aM(p-az[i]/aj,l[i]/2.-bY/aj);ad(bx<r.r)r=F(bx,au+az[i]);}I r;}\n"
"#define b2(a0,bZ)F a0(B p,B b3){p*=b3;B n=aq(p),f=p-n,b4,g,o,r;C aP=8.,aX=aP,d;am(Y i=0;i<9;++i){g=B(i%3-1,i/3-1);o=bP(mod(n+g,b3));r=g+o-f;d=bZ;ad(d<aP){aX=aP;aP=d;b4=r;}else ad(d<aX){aX=d;}}I F(b4,aX-aP);}\n"
"b2(bd,aU(L(r)))b2(ca,U(r))C b5(F p){p=aQ(p);F a=mod(degrees(atan(p,p.gbr)),360.);I X(a.r/8.,45.)*E(.9,.0,L(p.b))+X(a.g/8.,45.)*E(.7,.0,L(p.r));}B b6(F p,Y ax){I(ax==0)?p.gb:(ax==1)?p.rb:p.rg;}Y b7(F n){n=L(n)+F(.01,.02,.03);C m=aC(n);I(m==n.r)?0:(m==n.g)?1:2;}F cb(F c){F W=ai(L(mod(c.r*6.+F(0,4,2),6.)-3.)-1.);W*=W*(3.-2.*W);I c.b*J(F(1.),W,c.g);}F a1(){F d=Cam.W-Pos;C b=M(d.rg/256.*a3(Cam.a),B(3),.7,3.,4),l=1.-E(14.,-6.,U(d.rg)-b*8.)*E(128.,48.,d.b)*step(.1,Nor.b);I ar(Texture1,LUV).W*2.*l;}\n"
"#define R(a0)F a0(B);ah a0(){ae=Q(a0(aa),1);}F a0(B A)\n"
"#define av(a0)Q a0(B);ah a0(){ae=a0(aa);}Q a0(B A)\n"
"R(cmet52){C b=M(A,B(5),.9,3.,4);F c=J(K(48,41,33),K(103,101,104),b);I c;}R(ptrshn){C b=M(A,B(3),.9,3.,4);F c=J(K(49,45,43),K(81,75,78),b*b);I c;}R(dmnd2c){C b=M(A,B(7),.9,3.);A.r*=-1.5;A.g+=A.r*.5;A.r=1.-A.r+A.g;A=P(A*28.);C f=ai(1.-U(.1-A));f*=E(.6,.2,U(.6-A));f*=E(.6,.8,U(.1-A));f*=E(.2,.6,b)*2.+1.;C l=1.-E(.2,b+2.,aC(L(A-.5)));I F((f+1.)*J(.21,.29,b*b)*l);}R(dmnd2cow){C b=M(A,B(7),.9,3.);F c=dmnd2c(A);C r=U(A-.5);c=J(c,c*K(70,61,53),E(.5,.2,r+b*b*b));I c;}C b8(B A,C s){C d=1e6,i=0.;am(;i<5.;++i){B p=B(0,-s)*a3(i*72.);d=V(d,U(A-aV(A,p,p*a3(144.))));}I d;}av(dmnd2pnt){F c=dmnd2cow(A);A=P(A)-.5;C b=M(A,B(3),.9,3.),d=V(L(U(A)-.4),b8(A,.35));I Q(c,S(d-.02+b*.02,.01));}ah dmnd2pnt_m(){Q c=ar(ao,aa);ae=Q(c.W*a1()+K(111,55,0)*c.a*(sin(ag.r*a6)*.5+.5),1);}R(dmnd2cjp){C b=M(A,B(7),.9,3.,4);F c=dmnd2c(A);C r=U(A-.5);C m=E(.46,.45,r);C l=1.5-1.5*E(.0,.3,r*r);l=J(l,2.5,D(.42,.07,r));l=J(l,3.5,D(.44,.05,r));l=J(l,2.6,D(.36,.03,r));C n=.3+.2*E(.35,.30,r);l*=1.-n*E(.3,.7,b);l*=1.-.3*O(E(.13,.05,r));l=J(l,2.5,E(.04,.01,r));l-=l*D(.03,.01,r)*.7;c=J(c,K(68,66,54)*l,m);c*=1.-O(D(.34,.02,r));c*=1.-O(D(.46,.03,r));c*=1.-D(.41,.03,r)*.7;I c;}B cc(B A,C s){I B(1.-U(A)/s,S(U(A)-s));}av(lpdmnd){C b=M(A,B(5),.9,3.),t,o,k,r;F c=dmnd2c(A);B u,v;u.r=L(A.r-.5);u.g=V(A.g,.4);r=U(u-B(0,.4))-(.18-.06*E(.4,1.,A.g));k=.25-.15*E(.9,.96,A.g)+.03*O(E(.82,.86,A.g))+.07*E(.8,.2,A.g)+.07*O(E(.35,.22,A.g))-.07*E(.22,.0,A.g);o=Z(A-B(.5,.5),B(k,.46));o=T(o,-Z(u,B(.15,.03))+.06);c=J(c,F(.6,.55,.55)-A.g*.3+b*.2,S(o));c*=1.-.7*D(.0,.013,o);c*=1.-(r/.5-.1)*S(o);t=T(r,A.g-.96);o=L(t-.02)-.03;o=T(o,A.g-1.+u.r*.5);o=T(o,A.g-.96);c=J(c,F(1,1,.9)-A.g*.55,D(-.01,.01,o));c=J(c,F(.2*b+.1),S(t,.01));c*=1.-.2*D(.0,.05,t)*S(o);v=cc(u=A-B(.5,.4),.02);c*=1.+K(111,80,70)*D(.03,.01,U(u));c*=1.-.5*D(.02,.01,U(u));c=J(c,K(111,66,44)*(v.r*1.5+.2),v.g);I Q(c,S(t-.03,.02));}ah lpdmnd_m(){Q c=ar(ao,aa);B A=P(aa);A.r=L(.5-A.r);C t=P(-ag.r),r=U(A-B(0,.4)),l=t*pow(T(0.,1.-r),4.)*c.a;ad(t>.75)l+=E(.03,.01,L(P(A.g+A.r*.5+t*2.)-.45))*E(.1,.08,A.r);ae=Q(c.W*a1()+K(180,150,5)*l,1);}R(mtlfw10){C b=M(A,B(5),.9,3.,4);F c=J(K(44,14,16),K(93,63,63),b*b);I c;}F b9(B A){C e=3e-3,a=0.;B g=B(6),r=bd(A,g).rg;am(Y i=0;i<9;++i)a+=aU(L(bd(B(i%3-1,i/3-1)*e+A,g).rg-r));I F(A+r.rg/g,a);}R(mtlfw15){C b=M(A,B(3),.9,3.,4);F c=J(K(80,70,72),K(128,120,120),b*b);F v=b9(A);c*=J(.95,1.1,X(v.rg,B(6)));c=J(c,K(168,128,120),E(.5,1.,v.b)*b*.7);I c;}av(mtlfw15ow){C b=M(A,B(3),.9,3.,4);F c=J(K(80,70,72),K(128,120,120),b*b);F v=b9(A);C m=E(.5,1.,v.b);C r=E(.4,.2,U(.5-P(v.rg)));c*=J(.95,1.1,X(v.rg,B(6)))-2.*r*b*b;c=J(c,K(168,128,120),m*b*.7);I Q(c,m*r);}ah mtlfw15ow_m(){Q c=ar(ao,aa);ae=Q(c.W*a1()+D(.5,.125,P(aa.g*.5+ag.r*.5))*c.a*.3,1);}R(mtlfb3){C b=M(A,B(5),.9,3.,4);F pt=bw(A,8.,.31);F c=J(K(66,58,55),K(118,107,105),b);C l=1.-.5*E(.034,.036,pt.r);l=J(l,1.4,D(.033,.004,pt.r));I c*l;}C cd(B A){C b=X(A,B(64)),f=0.,d=1e6;am(;f<11.;++f)d=bq(d,L(U(.5-L(A-bJ(f)))-J(.36,.29,a7(f+.7)))-J(.015,.03,b),.01);I d*1e2;}F bA(B A){F s,p;am(Y i=0;i<3;++i){p=F(A,0);p[i]+=1e-4;s[i]=cd(p.rg);}I F(aQ(s.rg-s.b),s.b);}R(mtlt12f){C b=M(A,B(5),.9,3.,4),l;F c=J(K(51,46,43),K(165,147,143),b*b),d=bA(A);l=1.-.5*(d.g-d.r)*D(.5,3.,d.b)*E(1.,.0,d.b);I c*l*.8;}R(mtlt6f){C b=M(A,B(3),1.1,3.,4),l;F c=J(K(51,46,43),K(165,147,143),b*b),d=bA(A);l=1.-.5*(d.g-d.r)*D(.5,3.,d.b)*E(1.,.0,d.b);I c*l;}R(mtlbk03){C b=M(A,B(5),.9,3.,4),l=.18*(.7+b*b);F g;A=af(A,13.,.007);aW(g,A,O(E(.3+b*.2,.9,M(p[i],B(23),.5,2.,4))));I F(l*(1.-g.g*g.b));}R(giron01e){C b=M(A,B(5),.9,3.,4);F c=J(K(77,55,53),K(62,48,48),X(A,B(128,13)))*(.7+b*b),g;A=af(A,13.,.007);aW(g,A,O(E(.4+b*.4,.95,X(p[i],B(63,43)))));c*=E(1.3,.9,g.b);I F(c*(1.+g.g*g.b));}C bB(B A,B s){A.g=T(A.g,0.);I aN(A,s);}F aY(F c,F b,B A,C h,C s){C y=(A.g-h)/s,p=1.-y*y;c*=1.-D(-1.,1.,y);ad(p>0.)c=b*(p*(.8+.2*D(.5,.25,P(A.r/s))))*(.7+O(D(.2,.7,y)));I c;}F ce(B A,C n){I F(n*n*.4);}R(giron01nt3){A.r*=.5;C b=M(A*B(2,1),B(3,5),.9,3.,4),n=.75+b*b,t=A.g+.2*V(.4,D(.5,.33,P(A.r*4.))),cf=bB(A-B(.25,.62),B(3,2)/32.),aR=bB(A-B(.25,.55),B(3,2)/48.),r;F c=J(K(66,50,51),K(111,66,44),a2(D(.31,.01,A.g))),an;B p=A,q;p.r=P(p.r*4.);ad(A.g>.3)c=aO(c,B(4.*L(p.r-.5)-1.6,P(A.g*16.)-.5),.07);r=L(p.r-.5);c*=1.-.3*E(.31,.32,A.g)*E(.87,.86,A.g)*(E(.035,.03,.5-r)+D(.48,.01,r)-D(.46,.02,r));c=J(c*n,ce(A,b),T(E(.31,.3,A.g),S(aR)));c*=E(1.5,.7,A.g);ad(A.g<.306)c*=1.-D(.3,.05,A.g)*S(-aR+10.,20.);c*=1.-D(.316,.004,A.g)*S(-aR);ad(A.g<.1)c*=.0;q=A;q.g+=D(.1,.01,mod(q.r,.33))/2e2;c=aY(c,2.*b*K(93,84,79),A,.185,.015);c=aY(c,2.*b*K(138,77,48),A,.13,.025);c=aY(c,2.*b*K(112,71,51),A,.09,.015);c=aY(c,2.*b*K(138,77,48),q,.05,.015);p.r=L(P(A.r*6.-.5)-.5)/6.;c*=1.+.5*E(.04,.03,p.r)*D(.18,.03,p.g);r=aM(p-B(0,.12),B(.03,.01));r=aA(r,aM(p-B(0,.11),B(.01)));c*=1.-O(D(.0,.04,r));c=J(c,K(166,99,77)*2.*b*(.75+.5*O(D(.125,.01,A.g))),S(r));q=p;q.g-=.07;r=ac(q,.03);c*=1.-O(D(.0,.07,r));c=J(c,K(127,83,72)*b*2.*E(.01,-.005,r),E(.005,.0,r));q.g-=.004;r=ac(q,.015);c*=O(E(-.01,.01,r));q.g+=.013;r=ac(q,.05);c+=K(67,38,30)*4.*a2(b)*O(D(-.02,.015,r)*D(.023,.02,A.g));r=aA(cf,aR);r=aA(r,(A.g-.3)*3e2);c*=1.-.5*D(-2.,17.,aR)*E(.26,.3,A.g);an=K(67,39,17)*n;an=J(an,F(n*.2),D(0.,4.,r)*b);an*=1.-.4*pow(D(.0,3.,r),4.);an+=(an+.6)*a2(b)*O(D(-6.,8.,r)*D(.66,.04,A.g))*S(r);ad(A.g<.56)an=aO(an,B(24.*L(A.r-.25)-1.85,P(A.g*24.+.5)-.5),.15);c=J(c,giron01e(A),E(.85,.9,t)+step(A.g,1./256.));c*=1.+D(.88,.015,t)-O(D(.87,.03,t));I J(c,an,E(1.,.1,r));}F cg(F c,F k,B A,Y w,Y h){C b=M(A,B(w,h),.5,2.,2);c*=.9-.3*E(.15,.1,L(b-.5));I J(c,k,D(.5,.1,b));}R(gmtlbg6){A=af(A,9.,.005);Y i=0,l[]=Y[](13,43,17,47,23,59,27,63);C b=M(A,B(19),.7,2.,4);F c=J(K(40,50,60),K(46,33,27),b)*(.5+b);am(;i<8;i+=2)c=cg(c,J(K(145,140,137),K(132,123,116),b),A,l[i],l[i+1]);I c;}F ch(F c,F k,B A,Y w,Y h){C b=M(A,B(w,h),.5,2.,2);c*=1.-.15*O(E(.15,.1,L(b-.5)));I J(c,k,D(.5,.1,b));}R(gblks17f2){C b=M(A,B(13),.9,3.,4),n=M(A,B(7),.9,3.,4);F c=J(K(111,66,55),K(80,55,52),O(E(.8,.2,n)))*(.8+.8*b*b),k=c;A=af(A,13.,.01);Y i=0,l[]=Y[](13,43,17,47,23,59,27,63);b=M(A,B(19),.7,2.,4);am(;i<6;i+=2)c=ch(c,k,A,l[i],l[i+1]);I c;}F ci(F c,F k,B A,Y w,Y h){C b=M(A,B(w,h),.5,2.,1);c*=.9-.3*O(E(.15,.1,L(b-.5)));I J(c,k,D(.5,.1,b));}R(gkarnclma2r){C b=M(A,B(3,29),.9,2.,4),t=.8+.8*b*b,d=L(A.g-.61),o=E(.25,.24,d),m;F c=K(140,127,127),k=c;B p=A;c*=1.-.1*E(.85,.86,A.g);c=t*J(c,K(110,55,50),E(.33,.32,A.g));p.g+=p.r*.11+b*.007;p.g=P(p.g*9.)-.5;m=E(.0,.1,L(p.g)-.2);Y i=0,l[]=Y[](3,29,5,37,9,63,27,63);am(;i<6;i+=2)c=J(c,ci(c,k,A,l[i],l[i+1]),m*o);c*=1.+t*o*(+.6*D(.1,.1,p.g)-.7*D(-.25,.3,p.g)-.5*D(.2,.1,p.g));c=J(c,K(99,66,51)*t,D(-.15,.1,p.g)*o);c*=1.+D(.36,.005,A.g)+D(.34,.005,A.g)+D(.865,.005,A.g)+D(.89,.01,A.g)-.5*O(D(.245,.01,d))-.7*O(D(.35,.01,A.g))-.5*O(D(.325,.02,A.g))-.8*O(D(.875,.02,A.g))-.3*O(D(.9,.02,A.g));c*=.3+a2(a9(A.r));I c;}F bC(F c,B p,C s,C m){s=bd(p,B(s)).b/s*1e2;c*=1.+.5*m*E(.9,.2,s)-.5*m*D(2.5,.5,.3,s);I c;}F bD(F c,B A,C cj){C b=M(A,B(4,9),.9,3.,4),t=.8+.8*b*b,a,d,m,s,k,i,v,r,z;F aB=J(K(133,100,88),K(133,100,100),b)*t;B p,q;p=q=A;q.r=L(q.r);d=ac(p,.31);v=bn(q);m=E(.01,.0,d);c=J(c*E(.0,.05,d+cj),F(.13*t),m);c=bC(c,p,37.,E(.04,.02,L(d+.07)));a=v*22.;i=aq(a);s=a-i;k=E(.23,.22,L(v-.25))+a7(i)*E(.0,.1,q.g);d-=r=(d*.3+.005)*k;m=E(.0,.1,q.g)*S(L(d+.015)-.015);c=J(c,aB,D(-.005,.01,d));c=J(c,K(130,75,44)*t,D(-.02,.005,d)*E(.0,.1,q.g));c*=1.-.3*E(.025,.03,-d)-.5*E(.4,.5,L(s-.5))*m+.2*D(.5,.3,L(s-.5))*m-.5*D(-.015,.007,d)-.5*D(-.03,.007,d)-.5*D(-.1,.005,d+r)-.5*D(-.115,.005,d+r)-.5*D(-.125,.015,d+r)-.5*D(-.145,.005,d+r)+.9*D(-.11,.007,d+r)+.5*D(-.14,.005,d+r)-b*D(.225,.005,L(v-.25))*S(L(d+.015)-.015);a=v*72.;i=aq(a);s=a-i;k=step(.7,H(i))*step(q.g,.0)*E(.02,.0,L(d+.02));c=J(c,F(aB*.6),k*E(.4,.3,L(s-.5)));c*=1.-.7*k*D(.4,.1,L(s-.5));I c;}F be(B A){C b=M(A,B(4,9),.9,3.,4),t=.8+.8*b*b,a,d,m,s,k,i,v,r,z;F aB=J(K(133,100,88),K(133,100,100),b)*t,c=F(.1*t);B p,q;p.r=A.r-.5;p.g=T(A.g-.2,0.)*1.89;v=atan(p.g,L(p.r))/a6;d=ac(p,.48);k=E(.3,.31,v);d*=1.-.2*E(.3,.31,v)-.1*E(.43,.44,v);a=v*(v>.44?2.:v>.3?63.:31.);c=J(c,aB,E(.03,.01,L(d)));m=S(L(d-.01)-.02);i=aq(a);s=a-i;ad(v>.33&&v<.44)s=P(s+H(i)*.6-.3);c*=1.-.5*m*D(.307,.01,v)-t*m*D(.5,.1+k*.2,s)+b*m*D(.52,.2+k*.2,s);c*=1.-.9*D(-.015,.015,d)-.5*D(.0,.01,d)-.7*D(.03,.02,d)+D(.01,.015,d);q=p;q.g-=.5;q.r=L(q.r)+.6;d=ac(q,1.13);m=E(.03,.02,L(d))*E(."/*continued on next line*/
/*cont.*/"5,.6,q.g);c=J(c,aB*ai(1.-L(d-.015)/.03),m);c*=1.-.5*m*D(.005,.01,d)+.5*m*D(.017,.005,d);q.r=L(A.r-.5)-.35;q.g=A.g*9./4.-2.1;d=ac(q,.13)*10.;a=bn(q)*49.;i=aq(a);s=a-i;v=E(.85,.9,H(i));am(Y j=0;j<2;++j,d+=.3){c=J(c,aB*(b*.5+.2),E(.09,.03,L(d)));c*=1.+.7*O(D(.01,.05,d));}p.g=(A.g-.7)*9./4.;d=ac(p,.43);a=atan(p.g,L(p.r))/a6;a=T(a,-.48);r=a;i=aq(a*=23.);s=a-i;k=a7(i)*.2-.1*E(.0,.1,-d);v=E(.1,.2,L(s-.5)-k);d+=v*.007;r=(1.-v)*O(D(.5,.3,r))*E(.25,.05,L(s-.5));d-=.17*r;m=D(.04,.0,-.4,d);c=J(c,aB*(b*.4+.4),m);c=bC(c,p,31.,E(.1,.05,L(d+.15))*v);c*=J(1.,1.-D(.1,.2,.4,L(s-.5)-k),m*b);c*=1.-.7*O(D(.03,.03,d))-.7*O(D(.03,.03,d+.05))*v-.7*O(D(.0,.02,d+.05))*v-.3*E(.04,.06,-d)*v+.5*D(.02,.0,-.1,d)+D(.0,.01+.07*r,d)+D(.0,.01,d+.03)*v;p.g-=.05;c=bD(c,p,0.);I c;}R(gkarnarcfnltp){A.g=(A.g+8.)/9.;I be(A);}R(gkarnarcfnlmd){A.g=(A.g*4.+4.)/9.;I be(A);}R(gkarnarcfnlbt){ad(A.g<.01)++A.g;A.g=A.g*4./9.;I be(A);}R(gblks15){C b=M(A,B(5),.9,3.,4),t=bp(af(A,4.,.01),B(7),.5,3.,5),n=X(af(A,4.,.05),B(9)),a5,e;F pt=bw(A,4.,.1+n*t*.05),c;B d=bs(pt.r);a5=H(P(pt.gb));c=K(74,65,62)*(.8+.8*b*b);c+=D(.6,.3,n)*E(.3,.9,b*t)*.2;c*=1.-D(.5,.4,n)*E(.5,.7,t)*.1;c=J(c,K(86,74,78),D(.5,.1,b)*D(.7,.3,a5)*.7);c=J(c,K(105,90,70),D(.3,.1,t)*D(.3,.3,a5)*.3);e=D(.015,.005+.015*n,pt.r)+D(.4,.1,n*t)*.4;c*=1.-b*E(.015,.05,pt.r)*.7;c*=1.+e*b*(d.g-.5)*.7;c*=.9+.2*a5;c*=.9+.2*a9(X(A-pt.gr,B(5)));I c;}R(gkblkgmtrn){F c=gblks15(A);A-=.5;c=bD(c,A*.9,.02);I c;}F aH(B A){A*=1.5;C b=M(af(A,7.,.02),B(9),.7,3.,2),t=b,n=X(A,B(13)),a,s,d;F c=gblks15(A);B p;p.r=L(A.r-.75);p.g=T(A.g-.58,0.)*1.15;a=atan(p.r,p.g)/a6;s=P(a*7.+.5);d=ac(p,.45);d-=.06*E(.4,.33,A.g);d-=.05*E(.15,.07,L(s-.5))*step(.63,A.g);d=aA(d,A.g-.107);ad(A.g<.6)d=aA(d,L(p.r-.493)-.113);d=aA(d,ac(p,.6)+.044*E(.48,.43,A.g));c=J(c,K(144,125,115)*t,S(d-.1,.005));c*=1.-.3*D(.12,.11,.1,d)+.5*D(.1,.005+.015*n*n,d);I c;}R(gskdr_a){I aH(B(5,0)/6.+A*B(1,4)/6.);}R(gskdr_b){I aH(B(1,0)/6.+A*B(4)/6.);}R(gskdr_c){I aH(A*B(1,4)/6.);}R(gskdr_d){I aH(B(5,4)/6.+A*B(1,2)/6.);}R(gskdr_e){I aH(B(1,4)/6.+A*B(4,2)/6.);}R(gskdr_f){I aH(B(0,4)/6.+A*B(1,2)/6.);}R(gblks18c){C b=M(A,B(13,1),.7,2.,3);F c=gblks15(A)*.7;c*=1.-O(E(.4,1.,b));I c;}F bE(B A,B s){C b=M(A,B(5),.9,3.,4),n=M(A,B(31,3),.5,3.,3),t=.75+b*b;B p=A;F c=gblks15(A);ad(A.g<.38)c=J(K(92,43,15),K(66,44,33),E(.1,.05,A.g))*t*(.5+.5*E(.0,.35,A.g));c+=b*s.g*O(D(.32,s.r*.015,A.g))+.3*b*D(.34,.05,A.g);c*=1.-D(.38,.005+b*b*.03,A.g)+3.*D(.15,.2,A.g)*(n-.5);I c;}R(gklblki){C b=M(A,B(5),.9,3.,4),t=.75+b*b,d,o,i;B p=A;F c=bE(A,B(1));p.r=mod(p.r,1./7.)-.07;p.g-=.21;d=ac(B(.75*p.r,aZ(p.g,.1)),.033);o=S(d,.005);d=ac(B(.75*p.r,aZ(p.g+.005,.09)),.033);i=S(d+.015);c=J(c,K(83,81,66)*t,(o-i)*E(.1,.3,A.g));c*=1.-E(.17,.25,A.g)*i;c+=O(D(.0,.015,d))*D(.32,.03,A.g);c*=1.+3.*pow(D(-.01,.03,d),4.)*D(.09,.03,A.g);d=ac(B(.75*p.r,aZ(p.g+.03,.1)),.033);c*=1.-S(d+.01,.02)*(1.-o);ad(A.g>.09&&A.g<.3)c=aO(c,B((L(p.r)-.035)*36.,P(A.g*36.)-.5),.1);I c;}B ck(B p){p.r=L(p.r);B q=p,v;q.g-=.5;C d=ac(q,.35),e,c;v=q/.35;q.g+=.25;q.r-=.15;d=V(d,Z(q,B(.09,.05))-.1);e=aN(q,B(.15,.1))/5e1;c=.1+ay(B(v.g,a2(ai(1.-bi(v)))),B(.3,.3));q.g+=.2;q.r=p.r;c=T(c,ai(.4-U(q)));c+=.15*D(.0,.1,e)-.1*S(e+.12,.15);d=V(d,Z(q,B(.15-E(-.15,.15,q.g)*.07,.03))-.09);c*=1.-E(.05,.25,q.r)*E(.2,.1,L(q.g+.12));q.g-=.06;c-=.5*S(aN(q,B(.05-E(-.1,.1,q.g)*.03,.06))/1e3+.03,.05);I B(ai(c),S(d,.02));}R(gklblki4){C b=M(A,B(9),.7,2.,4),t=.75+b*b;F c=bE(A,B(4,.3));B p=A,s;p.r=mod(p.r,.2);p-=.1;s=ck(p*5.);I J(c,J(F(.5,.4,.3),F(.95,.8,.55),t)*t*s.r,s.g);}R(glrgbk3b){C b=M(af(A,5.,.02),B(5),1.,3.,5),n=X(A,B(13));B p=bo(A,B(8)),q=P(p),a5=p-q;F c=K(91,61,42)*(.8+.8*b*b);c=J(c,K(70,30,15)*(.8+.8*b*b),aI(q,.1).b*.3);c*=1.-O(aI(q,.01+b*.05).b)*n*b*b;c*=1.+D(.4,.3,aI(q,.01+b*.07).b)*a2(b)*.3;c*=.9+.2*H(a5)*(1.-aI(q,.1).b);c*=.9+.4*pow(bp(A-H(a5/8.),B(5),.6,2.,4),4.);I c;}av(gcntr2trn){C b=M(A-=.5,B(5),.9,3.,4),t=.75+b*b,n=X(af(A,7.,.02),B(17)),r=U(A),k=r>.4?38.:r>.32?28.:16.,a=P(atan(A.g,A.r)/bj),i=aq(a*k),aw=L(L(r-.41-n*.002)*1e2-6.),m=E(1.5,1.4,aw),bf[]=C[](1.,3.,-.145,-1.,2.,.166),d,bF,s;B p=A;F c=K(78,68,63);c*=1.+.5*O(D(.49,.005+.015*n*n+.015*b,r));c=J(c,K(83,52,47)*(.6+.4*n*n),m)*t;c*=1.-.5*D(1.5,.5,aw)+b*D(1.,.5+.5*n,L(r-.418)*1e2-5.)-b*D(.5,.08,P(a*k+.5))*m+b*D(.5,.1,P(a*k+.55))*m;m=E(.34,.33,r);c=J(c*(1.-.5*m),K(83,52,47)*t,n*b*m);c=J(c,K(112,86,31)*t,m*O(D(.1,.15,.45,b)));c=J(c,K(77,66,77)*t,m*E(.5,.8,b)*.5);c*=1.-.7*D(.27,.34,.35,r);aw=r+n*.004;m=r>.21&&r<.31?1.:0.;c*=1.-D(.325,.005,aw)-D(.31,.005,aw)-b*O(D(.29,.005,aw))-b*O(D(.23,.01,aw))-.5*O(D(.21,.02,aw))+O(D(.3,.01,aw))*b+O(D(.22,.01,aw))*b-b*D(.5,.07,P(a*k+.5))*m;ad(r<.23)i+=37.;ad(r<.31)i+=73.;ad(r<.31)i+=91.;c*=J(1.,.9+.2*a7(i),m);m=E(.01,.0,L(r-.411)-.039);i=aq(a*72.);p*=a3(i*5.);s=0.;d=1e6;Y j=0;am(;j<6;j+=3){d=bk(d,bF=ay(p,aQ(B(bf[j],bf[j+1])))+bf[j+2]);s+=s+C(bF>0.);}ad(s==3.)++i;else i+=66.*s;i=a7(i);c=J(c,t*K(90,80,75),m);c=J(c,t*K(127,111,88),i*b*m);c*=J(1.,.7+.6*H(i),m);c*=1.-m*O(D(.0,.006,d))*b+m*O(D(.006,.006,L(d)))*b*.5;i=aq(a*4.);p=L(A*a3(i*90.+45.));d=1e6;am(j=0;j<2;++j,p=L(p*a3(45.)))d=bk(d,L(U(p-B(0,.12))-.16));m=E(.21,.2,r);aw=bl(bl(d,.012),.001);c*=1.-E(.21,.2,r)*S(.012-d)+b*m*O(D(.005,.005,d))-.5*m*O(S(aw-.001,.001));I Q(c,(1.-E(.21,.15,r)*S(.028-d,.02))*E(.07,.087,r));}av(gcntr2trn_m){B p=P(aa)-.5;C b=M(a3(ag.r*333.)*p/(.8+.2*sin(ag.r*61.)),B(53),.7,2.,4);Q c=Q(1.-b*F(0,.3,1),1),an=ar(ao,(a3(ag.r*30.)*p/(.8+.2*sin(ag.r*1.26)))+.5);c.W=J(c.W,an.W,an.a);an=ar(ao,aa);c.W=J(c.W,an.W,an.a)*a1();I c;}R(gtprst3){C b=M(A,B(13),.9,3.,4),n=M(A,B(7),.9,3.,4);F c=J(K(60,50,50),K(87,47,37),O(E(.7,.25,n)))*(.7+.8*b*b),g;A=af(A,31.,.003);aW(g,A,a2(E(.0,.9,X(p[i],B(93)))));c*=1.-(g.g+.4)*O(b*g.b)*g.b;I c;}R(gsklvtg02b){C b=M(A,B(5),.9,3.,4);F c=K(67,64,63)*(.6+.5*b),g;A=af(A,31.,.003);aW(g,A,a2(X(p[i],B(53,93))));c*=1.-.3*g.g*g.b*g.b;I c;}R(skcpthrt){A=af(A,7.,.01);C b=M(A,B(9),.7,2.,4),n=M(A,B(13),.5,2.,4);F c=K(127,70,55)*(.85+.3*b);c*=1.-.2*O(E(.3,.0,b*b))-.2*O(D(.6,.3,M(af(A,5.,.03),B(6),.6,2.,4)))-.3*E(.6,.77,n)+.3*E(.5,.9,b);I c;}R(skcpthrt2){B p=A-.5;p=af(p,17.,.007);p.r*=2.-A.g*1.5;C b=M(A,B(9),.7,2.,4),n=X(A,B(7)),d=U(p),s;F c=skcpthrt(A);s=P(d*=13.);ad(d<=6.){c*=1.-pow(E(6.,.5,d+b*b),4.);n=E(.3,.8,n);c*=1.-n*b*O(D(.4,.3,s))+n*b*D(.6,.4,s);}I c;}R(sktongue){A=af(A,13.,.003);C b=M(A,B(7),.9,3.,4),n=M(A,B(5),.5,2.,4),t=.5+b;F c=K(80,38,34),v=ca(A,B(23));c=J(c,J(K(180,125,118),K(165,78,51),n),b*D(.0,.4+n*.4,v.b))*t;I c;}R(gskull4){C b=M(A,B(13),.9,3.,4);F c=K(60,50,46)*(.875+b*b);I c;}R(gmtlspsld){C b=M(A,B(7),.9,3.,4),n=M(A,B(3),.5,3.,4);F c=J(K(103,56,53),K(73,58,71),E(.1,.7,n))*(.75+b*b);I c;}R(gmtlsp4b){C b=M(A,B(13),.9,3.,4),n=X(af(A,5.,.05),B(9)),d=a9(P(A.r*4.)),m=E(.1,.15,d)*E(1.,.99,A.g);F c=K(51,44,44);c=J(c,K(73,55,52),E(.2,.2,b)*n*m);c=J(c,K(69,60,66),E(.7,.1,b)*b*m);c=J(c,K(99,77,77),E(.1,.5,n)*n*m*b*b*.3);c*=.6+.3*b+.3*b*b;c*=1.+.9*O(D(.21,.02+.1*n,d+b*.05))*m*b;c*=1.-O(E(.49,.5,L(A.g-.5)));c*=1.-E(.05,.2,d)*E(.16,.1,d);c*=1.+D(.99,.007,A.g);I aO(c,B(d-.4,P(A.g*8.)-.5),.07);}F bG(B A,C s){C b=M(A,B(3,1.+s+s),.7,2.,4),d=a9(A.r),m;A.g*=2.;F c=J(K(71,60,58),K(110,88,77),E(.1,.05,d))*(.7+.6*b);c*=1.-E(.05,.0,A.r)*(1.-b*b);c*=1.+.5*D(.05,.02,A.r);B p=B(d-.35,P(A.g*s)-.5);Q k=bt(p,.11);m=S(k.a);c*=1.-.7*bu(p,1.1)*(1.-m);c=J(c,(k.g>.0?K(128,105,88):K(200,111,66)*E(-.2,.7,k.b))*(.4+2.*b*pow(ai(aU(k.gb*.7)),4.))*(1.-.6*D(-.1,.4,k.g)),m);I c;}R(gspbdrbb){I bG(A,4.);}R(gkarntwr4a){I gspbdrbb(A.gr);}R(gkarntwrst){I bG(A,1.);}R(gxstrtop4){C b=M(A,B(40,5),.9,3.,4);F c=K(110,110,98)*(.8+.8*b*b);ad(A.g<1./4.)c*=.5;c*=1.-.4*E(.4,.0,b)+.5*E(.02,.0,A.g)+.2*D(.24,.01,A.g);I c;}R(gwdclg1a){B p=A,q;p.g*=22.;q=P(p);C b=M(A,B(3,23),1.,2.,6),n=M(A,B(3,33),.7,3.,4),a5=H(p.g-q.g);F c=K(92,67,53)*(.8+.8*b*b);c*=1.-O(E(.1,.0,V(q.g,1.-q.g)))*b;c*=1.-.2*smoothstep(.3,.7,n);c*=.8+.3*b*a5;I c;}R(gwdclg1bd){C b=M(A,B(13),.9,3.,4),x=A.r*16./3.;F c=gwdclg1a(A)*E(.15,.21,A.r);ad(x<1.)c=K(59,48,49)*(.7+.6*b);c*=1.+.5*D(.05,.05,a9(x));I aO(c,B(L(A.r-3./32.)-.07,mod(A.g,.1)-.05),.004);}R(gsltrfc){B p=bo(A,B(6,4)),q=P(p),u=q;C b=M(af(A-=.5,5.,.03),B(13),.9,2.,3),n=X(A,B(73,7)),t=(.75+b*b)*(.8+.4*bK(A.r*93.)),r;F c=F(.25*t);u.g+=u.g*2.-.01-.03*n;r=U(u-=clamp(u,B(.49,.5),B(.51,3)));c*=1.-.7*b*O(E(.07,.03,L(r-.5)))+.5*b*D(.35,.1,r)*O(E(.2,.1,q.g))-.3*O"/*continued on next line*/
/*cont.*/"(E(.8,1.,q.g))-.3*(E(.3,.1,q.g))*E(.4,.6,r)+.2*O(E(.5,.1,q.g))*E(.45,.4,r);I c;}R(cable){C b=M(A,B(5),.9,3.,4),h=P(A.g*10.);F c=J(K(53,48,42),K(38,38,36),b);c*=.6+b*.8;c*=1.-.5*O(D(.5,.5,h));c*=1.+.5*O(D(.25,.25,h));c*=1.+.5*O(D(.65,.35,h));I c;}R(bmtsprt){C b=M(A,B(7,3),.9,3.,4),h=A.g+b*.04,l=1.-.15;F c=J(K(59,48,40),K(110,108,102),b*b);l=J(l,.5,D(.34,.05,A.g));l=J(l,.5,E(.08,.05,L(A.g-.7)));l=J(l,.3,D(.7,.03,A.g));l=J(l,1.5,D(.01,.03,A.g));l=J(l,2.2,D(.89,.1,h));l=J(l,1.6,E(.07,.04,L(A.g-.44)));l=J(l,2.5,D(.5,.04,h));l=J(l,1.7,D(.18,.04,h));I c*l;}R(brdr11b){C b=M(A,B(5,3),.9,3.,4);F c=J(K(74,66,55),K(99,90,78),b*b);A.r*=2.;B p=aV(A,B(.5,.625),B(1.5,.625));C d=U(p-A),m=E(.22,.20,d),l=1.-.15*m;l=J(l,.5,E(.7,.9,A.g)*m);l=J(l,1.-bs(d).g*.5,D(.22,.04,d));l=J(l,.6,O(D(.19,.05,d)));l=J(l,.5,E(.05,0.,A.g));l=J(l,.5,D(.26,.05,A.g));l=J(l,1.7,E(.93,1.,A.g));l=J(l,1.7,D(.23,.04,A.g));I c*l;}av(blt414k){C b=M(A,B(1,5),.4,3.,4);F c=J(K(56,49,43),K(142,136,136),b);A=.5-L(A-.5);A.g*=4.;C a=D(.0,.1,U(A-aV(A,B(.41,.5),B(.42,3.5)))),d=aT(A),l=1.-.7*T(0.,1.-d/.15);l*=1.-.8*E(.24,.31,V(d,A.g-.1));c+=K(80,80,20)*a;I Q(c*J(l,2.7,a),a);}av(light5){C b=M(A,B(1,5),.4,3.,4);F c=J(K(56,49,43),K(142,136,136),b);A=.5-L(A-.5);A.g*=8.;C d=U(A-aV(A,B(.27,.3),B(.27,7.7))),a=D(.0,.17,d),l=1.-.5*D(.17,.07,d);c+=K(80,80,20)*a;I Q(c*J(l,2.7,a),a);}av(lt2){B p=L(A-.5);C b=M(A,B(1),.4,3.,4),r=U(p),a=E(.37,.33,r)*(.5+2.*b),l=1.+.0*E(.08,.03,L(r-.41));F c=J(K(56,49,43),K(142,136,136),b);l=J(l,7.,E(.44,.1*b,r));l*=1.-.5*O(D(.46,.04,r));l*=1.-.4*O(D(.36,.04,r));I Q(c*l,a);}av(gpntgmlt1k){C b=M(A,B(5),.9,3.,4),d=bq(b8(A-=.5,.35),L(ac(A,.4)),.02),a=pow(S(d-.02,.15),8.),o=V(T(Z(A,B(.46)),-ac(A,.51)),L(ac(A,.44)));F c=K(76,62,47)*(.8+.8*b*b);c*=1.+(b+.5)*S(L(o)-.01,.01);c*=1.-E(.1,.05,d)*S(ac(A,.4));I Q(c+1.*F(1,1,.3)*a,a);}C bH(B A){A.r=L(A.r);A.g-=.07;C d=aN(A,B(.31,.12))/50.;d=T(d,-aN(A-B(0,.01),B(.28,.07))/120.);d=T(d,-Z(A-B(.0,.1),B(.22,.12)));d=T(d,-Z(A-B(.0,.1),B(.09,.31)));d=V(d,aM(A-B(.0,-.09),B(D(-.09,.32,A.g)*.04,.32)));d=V(d,aM(A-B(.11,-.21),B(D(-.07,.3,A.g)*.03,.15)));I d;}av(icon){A-=B(.48,.5);C d=bH(A),b=U(A)-.47;F c=1.-F(.5,1,1)*S(T(.007-d,b+.04));I Q(c,1)*S(b);}C bI(B A,C s,C i,C cl){i=(A.g-O(L(A.r-.5))*cl)*s-i;I 2.*D(.5,.4,i)*(P(i)-.5);}av(bwprtbnr){C b=M(A,B(5,9),.9,3.,4),t=.8+.8*b*b,n=M(A,B(5,9),.9,3.,2),x=L(A.r-.5),d;F c=K(77,60,44)*t;B p=A*B(1,2)-B(.5,.7);c*=1.-.55*S(aA(ac(p,.3),bH(a3(45.)*p*.8)-.01));c*=1.+O(E(.6,.9,b))+D(.2,.5,A.g)*D(.2,.3,x)*bI(A,4.,.2,4.);am(C f=6.;f<9.;++f)c*=1.+D(.8,.5,A.g)*D(.2,.3,x)*bI(A,12.,f,1.);d=A.g-.81-O(ai(x*4.))*.09;c=J(c,K(82,66,60)*t,E(.0,.01,d));c*=1.-.5*(D(.01,.02,d))+.5*(D(.02,.01,d));d=.15*(1.-A.g);d=Z(A-.5,B(.49)-d)+n*.1*a2(1.-A.g)-d;I Q(c*O(S(d+.01,.05)),S(d));}av(bwprtbnr_m){Q c=ar(ao,A);ad(c.a<.5)discard;c.W*=O(a1()*.5);I c;}av(q3bnr){A*=B(256,64);A.g+=2.;C d=ac(A-B(81,30),11.);d=T(d,A.r-80.);d=T(d,-ac(A-B(84,26),9.));d=V(d,Z(A-B(73,37),B(4,9))-4.);d=T(d,-Z(A-B(73,37),B(0,7))+1.);d=V(d,Z(A-B(91.5,47),B(4,19))-4.);d=T(d,-Z(A-B(91.5,47),B(0,17.5))+1.);d=V(d,Z(ba(A,111.)-B(105.+E(23.,50.,A.g)*3.,43),B(3.5,19)));d=V(d,Z(A-B(111,32),B(4,3)));d=V(d,Z(A-B(126,37),B(3,13)));d=V(d,Z(A-B(125.5+E(23.,50.,A.g)*10.,44),B(3.5,6)));d=V(d,Z(A-B(136.5-E(23.,50.,A.g)*9.,32),B(3.5,8)));d=V(d,Z(A-B(148.5,37),B(7,13)));d=T(d,-Z(A-B(155,33),B(6,3)));d=T(d,-Z(A-B(155,43),B(6,2)));d=V(d,Z(A-B(168,37),B(3.5,13)));d=V(d,Z(A-B(178.,37),B(3.5,13)));d=V(d,Z(A-B(188,37),B(3.5,13)));d=T(d,A.g-50.);I Q(S(d,.8),0,0,H(A*511.));}ah q3bnr_m(){F c=ar(ao,aa*2.).W*step(.5,P(ag.r*.5));c=J(c*a1(),F(.5,0,0),D(P(ag.r*2.),1./64.,P(aa.g)));ae=Q(c+b5(Ref)*.25+ar(ao,aa+H(ag.rr)).a*.1,1);}ah beam(){B A=P(b6(Pos.W,b7(Nor))/128.);A.r+=ag.r/33.;C b=M(A,B(7),.9,2.),f=P(Pos.b/128.-.375);ae=Q(2.*K(95,85,80)*f*f*f*f*J(1.,b,.5),0.);}ah flame(){B A=P(aa),p=A;p.g+=p.g-ag.r;A.r+=sin(p.g*7.)*.2*A.g;C n=M(p+sin(p.gr*a6*9.+B(0,ag.r*9.))*.015+X(p,B(5))*.1,B(13),.4,3.,4),b=Z(A-B(.5,.25),B(.05*O(E(.4,.2,A.g)),.1)),m=O(S(b+n*.25,.35));ae=E(.0,.4,m)*Q(5,2,.7,0);}ah Generic(){C l=ay(Nor,aQ(F(2,0,8)));l=l*.4+.7;B A=b6(Pos,b7(Nor));F c=F(.5);c*=cb(F(P(aS*ag.a+.25),1.,1.));ae=Q(c*l,1);}ah fixture(){Q c=ar(ao,aa);ae=Q(c.W*J(a1(),F(1),c.a),1);}ah dmnd2cjp_m(){Q c=ar(ao,aa);C r=U(P(aa)-.5);C s=J(.4,8.,P(ag.r*1.5));ae=Q(c.W*a1()+K(240,130,5)*D(.1,.05,r/s)*E(.37,.32,r),1);}ah Lmapped(){F c=ar(ao,aa).W;ae=Q(c*a1(),1);}ah shiny(){Q c=ar(ao,aa);c.W*=1.+c.a*b5(Ref);ae=Q(c.W*a1(),1);}ah blacksky(){ae=Q(0);}ah timhel(){F d=aQ(Pos-Cam.W);d.b=d.b*4.+2.;B A=aQ(d).rg*2.;C b=E(.2,1.,M(A-ag.r*B(.1,.2),B(5),.5,2.,6));A.g*=1.5;C s=E(.3,1.,M(A-ag.r*B(.1,.18),B(5),.6,2.,6));ae=Q(F(b,0,0)+K(80,30,8)*s*s*2.,1);}ah lava(){B A=af(aa/8.,ag.r*.5,2.,.05);C b=M(A,B(7),.9,2.,4);F c=K(91,22,14)*(.2+1.6*b);c=J(c,K(144,44,0),D(.6,.2,M(A,B(3),.7,3.,4)));c=J(c,K(244,144,66)*b*2.,O(D(.55,.25,M(A,B(11),.5,2.,4))));ae=Q(c*ai(aC(a1())),1);}ah lavaf(){lava();}ah Loading(){ae=ar(ao,(.5+aa*127.)/128.,2.5);ae.W*=.7+.3*X(aa,.5/fwidth(aa));}ah UI(){ae=ar(ao,aa)*Clr;}";

// src/demo/data/shaders/vertex_shaders.glsl: 3094 => 1986 (64.2%)
const char g_vertex_shaders[] =
"#define c void\n"
"#define e vec4\n"
"#define f location\n"
"#define h gl_Position\n"
"uniform mat4 MVP;uniform e Time,Cam;layout(f=0)in e P;layout(f=1)in e T;layout(f=2)in vec3 N;layout(f=3)in e C;out vec3 Pos,Nor,Ref;out vec2 UV,LUV;out e Clr;c Generic(){h=MVP*P;Pos=P.rgb;Nor=N;UV=T.rg;LUV=T.ba;Ref=normalize(reflect((P-Cam).rgb,N));}c d(){h=P;UV=P.rg*.5+.5;}c UI(){h=e(2.*P.r-1.,1.-2.*P.g,1,1);UV=T.rg;Clr=C;}c cmet52(){d();}c dmnd2c(){d();}c dmnd2cow(){d();}c dmnd2cjp(){d();}c dmnd2pnt(){d();}c lpdmnd(){d();}c ptrshn(){d();}c mtlfw10(){d();}c mtlfw15(){d();}c mtlfw15ow(){d();}c mtlfb3(){d();}c mtlt12f(){d();}c mtlt6f(){d();}c mtlbk03(){d();}c gmtlbg6(){d();}c glrgbk3b(){d();}c gblks15(){d();}c gblks18c(){d();}c gklblki(){d();}c gklblki4(){d();}c gtprst3(){d();}c gsklvtg02b(){d();}c gblks17f2(){d();}c gkarnclma2r(){d();}c gkarnarcfnltp(){d();}c gkarnarcfnlmd(){d();}c gkarnarcfnlbt(){d();}c gkblkgmtrn(){d();}c gskdr_a(){d();}c gskdr_b(){d();}c gskdr_c(){d();}c gskdr_d(){d();}c gskdr_e(){d();}c gskdr_f(){d();}c skcpthrt(){d();}c skcpthrt2(){d();}c sktongue(){d();}c gskull4(){d();}c gcntr2trn(){d();}c gmtlspsld(){d();}c gmtlsp4b(){d();}c gspbdrbb(){d();}c gkarntwr4a(){d();}c gkarntwrst(){d();}c giron01e(){d();}c giron01nt3(){d();}c gxstrtop4(){d();}c gwdclg1a(){d();}c gwdclg1bd(){d();}c gsltrfc(){d();}c bmtsprt(){d();}c cable(){d();}c brdr11b(){d();}c blt414k(){d();}c light5(){d();}c lt2(){d();}c gpntgmlt1k(){d();}c icon(){d();}c bwprtbnr(){d();}c q3bnr(){d();}c Loading(){d();}c Lmapped(){Generic();}c mtlfw15ow_m(){Generic();}c dmnd2cjp_m(){Generic();}c dmnd2pnt_m(){Generic();}c lpdmnd_m(){Generic();}c fixture(){Generic();}c beam(){Generic();}c flame(){Generic();}c q3bnr_m(){Generic();}c shiny(){Generic();}c gcntr2trn_m(){Generic();}c blacksky(){Generic();}c timhel(){Generic();}c lavaf(){Generic();}c i(float j,float k,float l){h+=k*MVP*e(N,0)*sin(6.28*(Time.r*l+dot(P.rgb/j,vec3(1))));}c bwprtbnr_m(){Generic();i(30.,3.,.2);i(100.,3.,.7);}c lava(){Generic();i(100.,3.,.1);}";
