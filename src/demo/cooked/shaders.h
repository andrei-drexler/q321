#pragma once
// auto-generated, do not modify (see cook_shader.js)

// trim_newlines       : true
// consistent_floats   : false
// rename_ids          : true
// rename_vec_fields   : true
// rename_globals      : true
// min_macro_savings   : 8

// src/demo/data/shaders/fragment_shaders.glsl: 23166 => 9480 (40.9%)
const char g_fragment_shaders[] =
"#define j vec2\n"
"#define u float\n"
"#define y vec3\n"
"#define z return\n"
"#define A mix\n"
"#define E vec4\n"
"#define F fract\n"
"#define J min\n"
"#define K max\n"
"#define L abs\n"
"#define O length\n"
"#define S rgb\n"
"#define U FCol\n"
"#define X void\n"
"#define Z Texture0\n"
"#define ac texture\n"
"#define ae Time\n"
"#define af clamp\n"
"#define aE normalize\n"
"uniform E ae,Cam;uniform sampler2D Z,Texture1;in y Pos,Nor,Ref;in j UV,LUV;in E Clr;out E U;\n"
"#define aJ(x)((x)*(x)*(3.-2.*(x)))\n"
"#define sqr(x)(x)*(x)\n"
"#define sat(x)af(x,0.,1.)\n"
"#define B(r,g,b)(y(r,g,b)/255.)\n"
"u aK=3.1415927,bf=2.*aK,ar=1.618034;j aL(u i){u G=1.324718;z F(.5+i/j(G,G*G));}u aM(u i){z F(.5+i*ar);}u as(j v){z J(v.r,v.g);}u as(y v){z J(v.r,J(v.g,v.b));}u as(E v){z J(J(v.r,v.g),J(v.b,v.a));}u an(j v){z K(v.r,v.g);}u an(y v){z K(v.r,K(v.g,v.b));}u an(E v){z K(K(v.r,v.g),K(v.b,v.a));}u az(j v){z v.r+v.g;}u D(u aN,u aO,u x){z 1.-af(L(x-aN)/aO,0.,1.);}u C(u a0,u aP,u x){z af((x-a0)/(aP-a0),0.,1.);}j P(u x){z j(sin(x),cos(x));}mat2 aQ(u x){j v=P(x);z mat2(v.g,v.r,-v.r,v.g);}u H(j p){y q=F(p.rgr*.09153);q+=dot(q,q.gbr+19.19);z F((q.r+q.g)*q.b);}u H(u p){p=F(p*.1031);p*=p+33.33;p*=p+p;z F(p);}y bh(u p){y V=F(y(p)*y(.1031,.1030,.0973));V+=dot(V,V.gbr+33.33);z F((V.rrg+V.gbb)*V.bgr);}j aR(j p){y V=F(y(p.rgr)*y(.1031,.1030,.0973));V+=dot(V,V.gbr+33.33);z F((V.rr+V.gb)*V.bg);}E bi(u p){E ag=F(E(p)*E(.1031,.1030,.0973,.1099));ag+=dot(ag,ag.abrg+33.33);z F((ag.rrgb+ag.gbba)*ag.bgar);}u Q(u x,u p){z H(mod(x,p));}u N(u x){u i;z A(H(i=floor(x)),H(i+1.),aJ(x-i));}u aa(u x,u p){u i;z A(Q(i=floor(x),p),Q(i+1.,p),x-i);}u Q(j p,j s){z H(mod(p,s));}u aa(j p,j s){p*=s;j i=floor(p);p-=i;p*=p*(3.-2.*p);u aS=Q(i+j(0,0),s);u aT=Q(i+j(0,1),s);u aU=Q(i+j(1,1),s);u aV=Q(i+j(1,0),s);z A(A(aS,aV,p.r),A(aT,aU,p.r),p.g);}u I(j p,j ao,u at,u au,int aW){u a1=aa(p,ao),av=1.,tw=1.;for(int i=0;i<aW;++i){p=F(p+ar);ao*=au;av*=at;a1+=aa(p,ao)*av;tw+=av;}z a1/tw;}u I(j p,j ao,u at,u au){z I(p,ao,at,au,4);}j aw(j p,j a,j b){j ab=b-a,ap=p-a;u t=af(dot(ap,ab)/dot(ab,ab),0.,1.);z ab*t+a;}u M(j p,j b){j d=L(p)-b;z J(K(d.r,d.g),0.)+O(K(d,0.));}u ay(j p,j b){z an(L(p)-b);}u a2(j p,u r){z O(p)-r;}u a3(j p,j r){z(O(p/r)-1.)/J(r.r,r.g);}u aX(u a,u b,u k){u h=af(.5+0.5*(b-a)/k,.0,1.);z A(b,a,h)-k*h*(1.-h);}j aY(u x){j d=j(dFdx(x),dFdy(x));z d/K(O(d),1e-8);}u aq(u s,u d){z af(1.-s/d,0.,1.);}u aq(u s){z af(1.-s/fwidth(s),0.,1.);}j aZ(j v,u m){z j(m-L(v.r-m),v.g);}y ba(j p,u P,u bc){j W=floor(p*P);p-=(W+.5)/P;y e=y(-1,0,1);u R=Q(W,j(P))*.5;u ah=Q(W+e.rg,j(P))*.5;u ai=Q(W+e.gb,j(P))*.5;u aj=Q(W+e.bg,j(P))*.5;u ak=Q(W+e.gr,j(P))*.5;u a4=Q(W+e.rb,j(P))*.5;u a5=Q(W+e.bb,j(P))*.5;u a6=Q(W+e.br,j(P))*.5;u a7=Q(W+e.rr,j(P))*.5;j[4]Y,l;if(mod((W.r+W.g),2.)<.5){l[0]=j(ai-ah,a4-R)+1.;l[1]=j(-ai+aj,a5-R)+1.;l[2]=j(-ak+aj,-a6+R)+1.;l[3]=j(ak-ah,-a7+R)+1.;Y[0]=j(ai,R)+l[0]*j(-.5,.5);Y[1]=j(ai,R)+l[1]*j(.5,.5);Y[2]=j(ak,R)+l[2]*j(.5,-.5);Y[3]=j(ak,R)+l[3]*j(-.5,-.5);}else{l[0]=j(-a4+R,ai-ah)+1.;l[1]=j(a5-R,ai-aj)+1.;l[2]=j(a6-R,-ak+aj)+1.;l[3]=j(-a7+R,-ak+ah)+1.;Y[0]=j(R,ah)+l[0]*j(-.5,.5);Y[1]=j(R,aj)+l[1]*j(.5,.5);Y[2]=j(R,aj)+l[2]*j(.5,-.5);Y[3]=j(R,ah)+l[3]*j(-.5,-.5);}u d=1e5;j a8=j(0);for(int i=0;i<4;i++){l[i]/=P;u bx=ay(p-Y[i]/P,l[i]/2.-bc/P);if(bx<d){d=bx;a8=W+Y[i];}}z y(d,a8);}j a9(j p,j aA){p*=aA;j n=floor(p),f=p-n,aB,g,o,r;u aC=8.0,d;for(int i=0;i<9;++i){g=j(i%3-1,i/3-1);o=aR(mod(n+g,aA));r=g+o-f;d=az(L(r));if(d<aC){aC=d;aB=r;}}z aB;}u aD(y p){p=aE(p);y a=mod(degrees(atan(p,p.gbr)),360.);z aa(a.r/8.,45.)*C(.9,.0,L(p.b))+aa(a.g/8.,45.)*C(.7,.0,L(p.r));}j aF(y p,int ax){z(ax==0)?p.gb:(ax==1)?p.rb:p.rg;}int aG(y n){n=L(n)+y(.01,.02,.03);u m=an(n);z(m==n.r)?0:(m==n.g)?1:2;}y bd(y c){y S=af(L(mod(c.r*6.+y(0,4,2),6.)-3.)-1.,0.,1.);S*=S*(3.-2.*S);z c.b*A(y(1.),S,c.g);}y al(){y d=Cam.S-Pos;u b=I(d.rg/256.*aQ(Cam.a),j(3),.7,3.,4),l=1.-C(14.,-6.,O(d.rg)-b*8.)*C(128.,48.,d.b)*step(.1,Nor.b);z ac(Texture1,LUV).S*2.*l;}\n"
"#define T(ad)y ad(j);X ad(){U=E(ad(UV),1);}y ad(j w)\n"
"#define am(ad)E ad(j);X ad(){U=ad(UV);}E ad(j w)\n"
"T(cmet52){u b=I(w,j(5),.9,3.,4);y c=A(B(48,41,33),B(103,101,104),b);z c;}T(ptrshn){u b=I(w,j(3),.9,3.,4);y c=A(B(49,45,43),B(81,75,78),b*b);z c;}T(dmnd2c){u b=I(w,j(7),.9,3.);w.r*=-1.5;w.g+=w.r*.5;w.r=1.-w.r+w.g;w=F(w*28.);u f=sat(1.-O(.1-w));f*=C(.6,.2,O(.6-w));f*=C(.6,.8,O(.1-w));f*=C(.2,.6,b)*2.+1.;u l=1.-C(.2,b+2.,an(L(w-.5)));z y((f+1.)*A(.21,.29,b*b)*l);}T(dmnd2cow){u b=I(w,j(7),.9,3.);y c=dmnd2c(w);u r=O(w-.5);c=A(c,c*B(70,61,53),C(.5,.2,r+b*b*b));z c;}T(dmnd2cjp){u b=I(w,j(7),.9,3.,4);y c=dmnd2c(w);u r=O(w-.5);u m=C(.46,.45,r);u l=1.5-1.5*C(.0,.3,r*r);l=A(l,2.5,D(.42,.07,r));l=A(l,3.5,D(.44,.05,r));l=A(l,2.6,D(.36,.03,r));u n=.3+.2*C(.35,.30,r);l*=1.-n*C(.3,.7,b);l*=1.-.3*sqr(C(.13,.05,r));l=A(l,2.5,C(.04,.01,r));l-=l*D(.03,.01,r)*.7;c=A(c,B(68,66,54)*l,m);c*=1.-sqr(D(.34,.02,r));c*=1.-sqr(D(.46,.03,r));c*=1.-D(.41,.03,r)*.7;z c;}T(lpdmnd){y c=dmnd2c(w);z c;}T(mtlfw10){u b=I(w,j(5),.9,3.,4);y c=A(B(44,14,16),B(93,63,63),b*b);z c;}y aH(j w){u e=3e-3,a=0.;j g=j(6),r=a9(w,g);for(int i=0;i<9;++i)a+=az(L(a9(j(i%3-1,i/3-1)*e+w,g)-r));z y(w+r.rg/g,a);}T(mtlfw15){u b=I(w,j(3),.9,3.,4);y c=A(B(80,70,72),B(128,120,120),b*b);y v=aH(w);c*=A(.95,1.1,aa(v.rg,j(6)));c=A(c,B(168,128,120),C(.5,1.,v.b)*b*.7);z c;}am(mtlfw15ow){u b=I(w,j(3),.9,3.,4);y c=A(B(80,70,72),B(128,120,120),b*b);y v=aH(w);u m=C(.5,1.,v.b);u r=C(.4,.2,O(.5-F(v.rg)));c*=A(.95,1.1,aa(v.rg,j(6)))-2.*r*b*b;c=A(c,B(168,128,120),m*b*.7);z E(c,m*r);}X mtlfw15ow_m(){E c=ac(Z,UV,-.5);U=E(c.S*al()+D(.5,.125,F(UV.g*.5+ae.r*.5))*c.a*.3,1);}T(mtlfb3){u b=I(w,j(5),.9,3.,4);y pt=ba(w,8.,.31);y c=A(B(66,58,55),B(118,107,105),b);u l=1.-.5*C(.034,.036,pt.r);l=A(l,1.4,D(.033,.004,pt.r));z c*l;}u be(j w){u b=aa(w,j(64)),f=0.,d=1e6;for(;f<11.;++f)d=aX(d,L(O(.5-L(w-aL(f)))-A(.36,.29,aM(f+.7)))-A(.015,.03,b),.01);z d*1e2;}y aI(j w){y s,p;for(int i=0;i<3;++i){p=y(w,0);p[i]+=1e-4;s[i]=be(p.rg);}z y(aE(s.rg-s.b),s.b);}T(mtlt12f){u b=I(w,j(5),.9,3.,4),l;y c=A(B(51,46,43),B(165,147,143),b*b),d=aI(w);l=1.-.5*(d.g-d.r)*D(.5,3.,d.b)*C(1.,.0,d.b);z c*l*.8;}T(mtlt6f){u b=I(w,j(3),1.1,3.,4),l;y c=A(B(51,46,43),B(165,147,143),b*b),d=aI(w);l=1.-.5*(d.g-d.r)*D(.5,3.,d.b)*C(1.,.0,d.b);z c*l;}T(mtlbk03){u b=I(w,j(5),.9,3.,4);y c=A(B(36,35,33),B(56,54,52),b*b);z c;}T(cable){u b=I(w,j(5),.9,3.,4),h=F(w.g*10.);y c=A(B(53,48,42),B(38,38,36),b);c*=.6+b*.8;c*=1.-.5*sqr(D(.5,.5,h));c*=1.+.5*sqr(D(.25,.25,h));c*=1.+.5*sqr(D(.65,.35,h));z c;}T(bmtsprt){u b=I(w,j(7,3),.9,3.,4),h=w.g+b*.04,l=1.-.15;y c=A(B(59,48,40),B(110,108,102),b*b);l=A(l,.5,D(.34,.05,w.g));l=A(l,.5,C(.08,.05,L(w.g-.7)));l=A(l,.3,D(.7,.03,w.g));l=A(l,1.5,D(.01,.03,w.g));l=A(l,2.2,D(.89,.1,h));l=A(l,1.6,C(.07,.04,L(w.g-.44)));l=A(l,2.5,D(.5,.04,h));l=A(l,1.7,D(.18,.04,h));z c*l;}T(brdr11b){u b=I(w,j(5,3),.9,3.,4);y c=A(B(74,66,55),B(99,90,78),b*b);w.r*=2.;j p=aw(w,j(.5,.625),j(1.5,.625));u d=O(p-w),m=C(.22,.20,d),l=1.-.15*m;l=A(l,.5,C(.7,.9,w.g)*m);l=A(l,1.-aY(d).g*.5,D(.22,.04,d));l=A(l,.6,sqr(D(.19,.05,d)));l=A(l,.5,C(.05,0.,w.g));l=A(l,.5,D(.26,.05,w.g));l=A(l,1.7,C(.93,1.,w.g));l=A(l,1.7,D(.23,.04,w.g));z c*l;}am(blt414k){u b=I(w,j(1,5),.4,3.,4);y c=A(B(56,49,43),B(142,136,136),b);w=.5-L(w-.5);w.g*=4.;u a=D(.0,.1,O(w-aw(w,j(.41,.5),j(.42,3.5)))),d=as(w),l=1.-.7*K(0.,1.-d/.15);l*=1.-.8*C(.24,.31,J(d,w.g-.1));c+=B(80,80,20)*a;z E(c*A(l,2.7,a),a);}am(light5){u b=I(w,j(1,5),.4,3.,4);y c=A(B(56,49,43),B(142,136,136),b);w=.5-L(w-.5);w.g*=8.;u d=O(w-aw(w,j(.27,.3),j(.27,7.7))),a=D(.0,.17,d),l=1.-.5*D(.17,.07,d);c+=B(80,80,20)*a;z E(c*A(l,2.7,a),a);}am(lt2){j p=L(w-.5);u b=I(w,j(1),.4,3.,4),r=O(p),a=C(.37,.33,r)*(.5+2.*b),l=1.+.0*C(.08,.03,L(r-.41));y c=A(B(56,49,43),B(142,136,136),b);l=A(l,7.,C(.44,.1*b,r));l*=1.-.5*sqr(D(.46,.04,r));l*=1.-.4*sqr(D(.36,.04,r));z E(c*l,a);}am(icon){w.g-=.57;w.r=L(w.r-.48);u d=a3(w,j(.31,.12))/50.;d=K(d,-a3(w-j(0,.01),j(.28,.07))/120.);d=K(d,-M(w-j(.0,.1),j(.22,.12)));d=K(d,-M(w-j(.0,.1),j(.09,.31)));d=J(d,ay(w-j(.0,-.09),j(D(-.09,.32,w.g)*.04,.32)));d=J(d,ay(w-j(.11,-.21),j(D(-.07,.3,w.g)*.03,.15)));w.g+=.07;u b=O(w)-.47,m=aq(b);y c=1.-y(.7,1,1)*aq(K(.007-d,b+.04));z E(c*m,m);}am(q3bnr){w*=j(256,64);w.g+=2.;u d=a2(w-j(81,30),11.);d=K(d,w.r-80.);d=K(d,-a2(w-j(84,26),9.));d=J(d,M(w-j(73,37),j(4,9))-4.);d=K(d,-M(w-j(73,37),j(0,7))+1.);d=J(d,M(w-j(91.5,47),j(4,19))-4.);d=K(d,-M(w-j(91.5,47),j(0,17.5))+1.);d=J(d,M(aZ(w,111.)-j(105.+C(23.,50.,w.g)*3.,43),j(3.5,19)));d=J(d,M(w-j(111,32),j(4,3)));d=J(d,M(w-j(126,37),j(3,13)));d=J(d,M(w-j(125.5+C(23.,50.,w.g)*10.,44),j(3.5,6)));d=J(d,M(w-j(136.5-C(23.,50.,w.g)*9.,32),j(3.5,8)));d=J(d,M(w-j(148.5,37),j(7,13)));d=K(d,-M(w-j(155,33),j(6,3)));d=K(d,-M(w-j(155,43),j(6,2)));d=J(d,M(w-j(168,37),j(3.5,13)));d=J(d,M(w-j(178.,37),j(3.5,13)));d=J(d,M(w-j(188,37),j(3.5,13)));d=K(d,w.g-50.);z E(aq(d,.8),0,0,H(w*511.));}X q3bnr_m(){y c=ac(Z,UV*2.,-.5).S*step(.5,F(ae.r*.5));c=A(c*al(),y(.5,0,0),D(F(ae.r*2.),1./64.,F(UV.g)));U=E(c+aD(Ref)*.25+ac(Z,UV+H(ae.rr)).a*.1,1);}X beam(){j w=F(aF(Pos.S,aG(Nor))/128.);w.r+=ae.r/33.;u b=I(w,j(7),.9,2.),f=F(Pos.b/128.-.375);U=E(2.*B(95,85,80)*f*f*f*f*A(1.,b,.5),0.);}X Generic(){u l=dot(Nor,aE(y(2,0,8)));l=l*.4+.7;j w=aF(Pos,aG(Nor));y c=y(.5);c*=bd(y(F(ar*ae.a+.25),1.,1.));U=E(c*l,1);}X fixture(){E c=ac(Z,UV,-.5);U=E(c.S*A(al(),y(1),c.a),1);}X dmnd2cjp_m(){E c=ac(Z,UV,-.5);u r=O(F(UV)-.5);u s=A(.4,8.,F(ae.r*1.5));U=E(c.S*al()+B(240,130,5)*D(.1,.05,r/s)*C(.37,.32,r),1);}X Lmapped(){y c=ac(Z,UV,-.5).S;U=E(c*al(),1);}X shiny(){E c=ac(Z,UV,-.5);c.S*=1.+c.a*aD(Ref);U=E(c.S*al(),1);}X Loading(){U=ac(Z,(.5+UV*127.)/128.,2.5);U.S*=.7+.3*aa(UV,.5/fwidth(UV));}X UI(){U=ac(Z,UV)*Clr;}";

// src/demo/data/shaders/vertex_shaders.glsl: 1504 => 918 (61.0%)
const char g_vertex_shaders[] =
"#define c void\n"
"#define e location\n"
"#define f gl_Position\n"
"uniform mat4 MVP;uniform vec4 Cam;layout(e=0)in vec4 P;layout(e=1)in vec4 T;layout(e=2)in vec3 N;layout(e=3)in vec4 C;out vec3 Pos,Nor,Ref;out vec2 UV,LUV;out vec4 Clr;c Generic(){f=MVP*P;Pos=P.rgb;Nor=N;UV=T.rg;LUV=T.ba;Ref=normalize(reflect((P-Cam).rgb,N));}c d(){f=P;UV=P.rg*.5+.5;}c UI(){f=vec4(2.*P.r-1.,1.-2.*P.g,1,1);UV=T.rg;Clr=C;}c cmet52(){d();}c dmnd2c(){d();}c dmnd2cow(){d();}c dmnd2cjp(){d();}c lpdmnd(){d();}c ptrshn(){d();}c mtlfw10(){d();}c mtlfw15(){d();}c mtlfw15ow(){d();}c mtlfb3(){d();}c mtlt12f(){d();}c mtlt6f(){d();}c mtlbk03(){d();}c bmtsprt(){d();}c cable(){d();}c brdr11b(){d();}c blt414k(){d();}c light5(){d();}c lt2(){d();}c icon(){d();}c q3bnr(){d();}c Loading(){d();}c Lmapped(){Generic();}c mtlfw15ow_m(){Generic();}c dmnd2cjp_m(){Generic();}c fixture(){Generic();}c beam(){Generic();}c q3bnr_m(){Generic();}c shiny(){Generic();}";
