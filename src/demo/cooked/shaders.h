#pragma once
// auto-generated, do not modify (see cook_shader.js)

// trim_newlines       : true
// consistent_floats   : false
// rename_ids          : true
// rename_vec_fields   : true
// rename_globals      : true
// min_macro_savings   : 8

// src/demo/data/shaders/fragment_shaders.glsl: 25411 => 10636 (41.9%)
const char g_fragment_shaders[] =
"#define j vec2\n"
"#define y float\n"
"#define z vec3\n"
"#define A return\n"
"#define B mix\n"
"#define F vec4\n"
"#define I fract\n"
"#define J max\n"
"#define K length\n"
"#define L abs\n"
"#define O min\n"
"#define Q rgb\n"
"#define U FCol\n"
"#define Y void\n"
"#define Z Texture0\n"
"#define ad texture\n"
"#define af Time\n"
"#define ak clamp\n"
"#define aI normalize\n"
"uniform F af,Cam;uniform sampler2D Z,Texture1;in z Pos,Nor,Ref;in j UV,LUV;in F Clr;out F U;\n"
"#define aN(x)((x)*(x)*(3.-2.*(x)))\n"
"#define aa(x)(x)*(x)\n"
"#define sat(x)ak(x,0.,1.)\n"
"#define D(r,g,b)(z(r,g,b)/255.)\n"
"y a2=3.1415927,bj=2.*a2,au=1.618034;j aO(y i){y G=1.324718;A I(.5+i/j(G,G*G));}y aP(y i){A I(.5+i*au);}y av(j v){A O(v.r,v.g);}y av(z v){A O(v.r,O(v.g,v.b));}y av(F v){A O(O(v.r,v.g),O(v.b,v.a));}y ar(j v){A J(v.r,v.g);}y ar(z v){A J(v.r,J(v.g,v.b));}y ar(F v){A J(J(v.r,v.g),J(v.b,v.a));}y a3(j v){A v.r+v.g;}y E(y aQ,y aR,y x){A 1.-ak(L(x-aQ)/aR,0.,1.);}y C(y a4,y aS,y x){A ak((x-a4)/(aS-a4),0.,1.);}j R(y x){A j(sin(x),cos(x));}mat2 aw(y x){j v=R(radians(x));A mat2(v.g,v.r,-v.r,v.g);}y H(j p){z q=I(p.rgr*.09153);q+=dot(q,q.gbr+19.19);A I((q.r+q.g)*q.b);}y H(y p){p=I(p*.1031);p*=p+33.33;p*=p+p;A I(p);}z bk(y p){z W=I(z(p)*z(.1031,.1030,.0973));W+=dot(W,W.gbr+33.33);A I((W.rrg+W.gbb)*W.bgr);}j aT(j p){z W=I(z(p.rgr)*z(.1031,.1030,.0973));W+=dot(W,W.gbr+33.33);A I((W.rr+W.gb)*W.bg);}F bl(y p){F al=I(F(p)*F(.1031,.1030,.0973,.1099));al+=dot(al,al.abrg+33.33);A I((al.rrgb+al.gbba)*al.bgar);}y S(y x,y p){A H(mod(x,p));}y N(y x){y i;A B(H(i=floor(x)),H(i+1.),aN(x-i));}y ag(y x,y p){y i;A B(S(i=floor(x),p),S(i+1.,p),x-i);}y S(j p,j s){A H(mod(p,s));}y ag(j p,j s){p*=s;j i=floor(p);p-=i;p*=p*(3.-2.*p);y aU=S(i+j(0,0),s);y aV=S(i+j(0,1),s);y aW=S(i+j(1,1),s);y aX=S(i+j(1,0),s);A B(B(aU,aX,p.r),B(aV,aW,p.r),p.g);}y M(j p,j as,y ay,y az,int aY){y a5=ag(p,as),a0=1.,tw=1.;for(int i=0;i<aY;++i){p=I(p+au);as*=az;a0*=ay;a5+=ag(p,as)*a0;tw+=a0;}A a5/tw;}y M(j p,j as,y ay,y az){A M(p,as,ay,az,4);}j at(j p,j a,j b){j ab=b-a,ap=p-a;y t=ak(dot(ap,ab)/dot(ab,ab),0.,1.);A ab*t+a;}y P(j p,j b){j d=L(p)-b;A O(J(d.r,d.g),0.)+K(J(d,0.));}y a1(j p,j b){A ar(L(p)-b);}y a6(j p,y r){A K(p)-r;}y a7(j p,j r){A(K(p/r)-1.)/O(r.r,r.g);}y aZ(y a,y b,y k){y h=ak(.5+0.5*(b-a)/k,.0,1.);A B(b,a,h)-k*h*(1.-h);}j ba(y x){j d=j(dFdx(x),dFdy(x));A d/J(K(d),1e-8);}y ac(y s,y d){A ak(1.-s/d,0.,1.);}y ac(y s){A ak(1.-s/fwidth(s),0.,1.);}j bc(j v,y m){A j(m-L(v.r-m),v.g);}z bd(j p,y R,y be){j X=floor(p*R);p-=(X+.5)/R;z e=z(-1,0,1);y T=S(X,j(R))*.5;y am=S(X+e.rg,j(R))*.5;y an=S(X+e.gb,j(R))*.5;y ao=S(X+e.bg,j(R))*.5;y aq=S(X+e.gr,j(R))*.5;y a8=S(X+e.rb,j(R))*.5;y a9=S(X+e.bb,j(R))*.5;y aA=S(X+e.br,j(R))*.5;y aB=S(X+e.rr,j(R))*.5;j[4]ae,l;if(mod((X.r+X.g),2.)<.5){l[0]=j(an-am,a8-T)+1.;l[1]=j(-an+ao,a9-T)+1.;l[2]=j(-aq+ao,-aA+T)+1.;l[3]=j(aq-am,-aB+T)+1.;ae[0]=j(an,T)+l[0]*j(-.5,.5);ae[1]=j(an,T)+l[1]*j(.5,.5);ae[2]=j(aq,T)+l[2]*j(.5,-.5);ae[3]=j(aq,T)+l[3]*j(-.5,-.5);}else{l[0]=j(-a8+T,an-am)+1.;l[1]=j(a9-T,an-ao)+1.;l[2]=j(aA-T,-aq+ao)+1.;l[3]=j(-aB+T,-aq+am)+1.;ae[0]=j(T,am)+l[0]*j(-.5,.5);ae[1]=j(T,ao)+l[1]*j(.5,.5);ae[2]=j(T,ao)+l[2]*j(.5,-.5);ae[3]=j(T,am)+l[3]*j(-.5,-.5);}y d=1e5;j aC=j(0);for(int i=0;i<4;i++){l[i]/=R;y bx=a1(p-ae[i]/R,l[i]/2.-be/R);if(bx<d){d=bx;aC=X+ae[i];}}A z(d,aC);}j aD(j p,j aE){p*=aE;j n=floor(p),f=p-n,aF,g,o,r;y aG=8.0,d;for(int i=0;i<9;++i){g=j(i%3-1,i/3-1);o=aT(mod(n+g,aE));r=g+o-f;d=a3(L(r));if(d<aG){aG=d;aF=r;}}A aF;}y aH(z p){p=aI(p);z a=mod(degrees(atan(p,p.gbr)),360.);A ag(a.r/8.,45.)*C(.9,.0,L(p.b))+ag(a.g/8.,45.)*C(.7,.0,L(p.r));}j aJ(z p,int ax){A(ax==0)?p.gb:(ax==1)?p.rb:p.rg;}int aK(z n){n=L(n)+z(.01,.02,.03);y m=ar(n);A(m==n.r)?0:(m==n.g)?1:2;}z bf(z c){z Q=ak(L(mod(c.r*6.+z(0,4,2),6.)-3.)-1.,0.,1.);Q*=Q*(3.-2.*Q);A c.b*B(z(1.),Q,c.g);}z ai(){z d=Cam.Q-Pos;y b=M(d.rg/256.*aw(Cam.a),j(3),.7,3.,4),l=1.-C(14.,-6.,K(d.rg)-b*8.)*C(128.,48.,d.b)*step(.1,Nor.b);A ad(Texture1,LUV).Q*2.*l;}\n"
"#define V(ah)z ah(j);Y ah(){U=F(ah(UV),1);}z ah(j w)\n"
"#define aj(ah)F ah(j);Y ah(){U=ah(UV);}F ah(j w)\n"
"V(cmet52){y b=M(w,j(5),.9,3.,4);z c=B(D(48,41,33),D(103,101,104),b);A c;}V(ptrshn){y b=M(w,j(3),.9,3.,4);z c=B(D(49,45,43),D(81,75,78),b*b);A c;}V(dmnd2c){y b=M(w,j(7),.9,3.);w.r*=-1.5;w.g+=w.r*.5;w.r=1.-w.r+w.g;w=I(w*28.);y f=sat(1.-K(.1-w));f*=C(.6,.2,K(.6-w));f*=C(.6,.8,K(.1-w));f*=C(.2,.6,b)*2.+1.;y l=1.-C(.2,b+2.,ar(L(w-.5)));A z((f+1.)*B(.21,.29,b*b)*l);}V(dmnd2cow){y b=M(w,j(7),.9,3.);z c=dmnd2c(w);y r=K(w-.5);c=B(c,c*D(70,61,53),C(.5,.2,r+b*b*b));A c;}aj(dmnd2pnt){z c=dmnd2cow(w);w=I(w)-.5;y b=M(w,j(3),.9,3.),d=L(K(w)-.4),i=0.;for(;i<360.;i+=72.){j p=j(0,.35)*aw(i);d=O(d,K(w-at(w,p,p*aw(144.))));}A F(c,ac(d-.02+b*.02,.01));}Y dmnd2pnt_m(){F c=ad(Z,UV,-.5);U=F(c.Q*ai()+D(111,55,0)*c.a*(sin(af.r*a2)*.5+.5),1);}V(dmnd2cjp){y b=M(w,j(7),.9,3.,4);z c=dmnd2c(w);y r=K(w-.5);y m=C(.46,.45,r);y l=1.5-1.5*C(.0,.3,r*r);l=B(l,2.5,E(.42,.07,r));l=B(l,3.5,E(.44,.05,r));l=B(l,2.6,E(.36,.03,r));y n=.3+.2*C(.35,.30,r);l*=1.-n*C(.3,.7,b);l*=1.-.3*aa(C(.13,.05,r));l=B(l,2.5,C(.04,.01,r));l-=l*E(.03,.01,r)*.7;c=B(c,D(68,66,54)*l,m);c*=1.-aa(E(.34,.02,r));c*=1.-aa(E(.46,.03,r));c*=1.-E(.41,.03,r)*.7;A c;}j bh(j w,y s){A j(1.-K(w)/s,ac(K(w)-s));}aj(lpdmnd){y b=M(w,j(5),.9,3.),t,o,k,r;z c=dmnd2c(w);j u,v;u.r=L(w.r-.5);u.g=O(w.g,.4);r=K(u-j(0,.4))-(.18-.06*C(.4,1.,w.g));k=.25-.15*C(.9,.96,w.g)+.03*aa(C(.82,.86,w.g))+.07*C(.8,.2,w.g)+.07*aa(C(.35,.22,w.g))-.07*C(.22,.0,w.g);o=P(w-j(.5,.5),j(k,.46));o=J(o,-P(u,j(.15,.03))+.06);c=B(c,z(.6,.55,.55)-w.g*.3+b*.2,ac(o));c*=1.-.7*E(.0,.013,o);c*=1.-(r/.5-.1)*ac(o);t=J(r,w.g-.96);o=L(t-.02)-.03;o=J(o,w.g-1.+u.r*.5);o=J(o,w.g-.96);c=B(c,z(1,1,.9)-w.g*.55,E(-.01,.01,o));c=B(c,z(.2*b+.1),ac(t,.01));c*=1.-.2*E(.0,.05,t)*ac(o);v=bh(u=w-j(.5,.4),.02);c*=1.+D(111,80,70)*E(.03,.01,K(u));c*=1.-.5*E(.02,.01,K(u));c=B(c,D(111,66,44)*(v.r*1.5+.2),v.g);A F(c,ac(t-.03,.02));}Y lpdmnd_m(){F c=ad(Z,UV,-.5);j w=I(UV);w.r=L(.5-w.r);y t=I(-af.r),r=K(w-j(0,.4)),l=t*pow(J(0.,1.-r),4.)*c.a;if(t>.75)l+=C(.03,.01,L(I(w.g+w.r*.5+t*2.)-.45))*C(.1,.08,w.r);U=F(c.Q*ai()+D(180,150,5)*l,1);}V(mtlfw10){y b=M(w,j(5),.9,3.,4);z c=B(D(44,14,16),D(93,63,63),b*b);A c;}z aL(j w){y e=3e-3,a=0.;j g=j(6),r=aD(w,g);for(int i=0;i<9;++i)a+=a3(L(aD(j(i%3-1,i/3-1)*e+w,g)-r));A z(w+r.rg/g,a);}V(mtlfw15){y b=M(w,j(3),.9,3.,4);z c=B(D(80,70,72),D(128,120,120),b*b);z v=aL(w);c*=B(.95,1.1,ag(v.rg,j(6)));c=B(c,D(168,128,120),C(.5,1.,v.b)*b*.7);A c;}aj(mtlfw15ow){y b=M(w,j(3),.9,3.,4);z c=B(D(80,70,72),D(128,120,120),b*b);z v=aL(w);y m=C(.5,1.,v.b);y r=C(.4,.2,K(.5-I(v.rg)));c*=B(.95,1.1,ag(v.rg,j(6)))-2.*r*b*b;c=B(c,D(168,128,120),m*b*.7);A F(c,m*r);}Y mtlfw15ow_m(){F c=ad(Z,UV,-.5);U=F(c.Q*ai()+E(.5,.125,I(UV.g*.5+af.r*.5))*c.a*.3,1);}V(mtlfb3){y b=M(w,j(5),.9,3.,4);z pt=bd(w,8.,.31);z c=B(D(66,58,55),D(118,107,105),b);y l=1.-.5*C(.034,.036,pt.r);l=B(l,1.4,E(.033,.004,pt.r));A c*l;}y bi(j w){y b=ag(w,j(64)),f=0.,d=1e6;for(;f<11.;++f)d=aZ(d,L(K(.5-L(w-aO(f)))-B(.36,.29,aP(f+.7)))-B(.015,.03,b),.01);A d*1e2;}z aM(j w){z s,p;for(int i=0;i<3;++i){p=z(w,0);p[i]+=1e-4;s[i]=bi(p.rg);}A z(aI(s.rg-s.b),s.b);}V(mtlt12f){y b=M(w,j(5),.9,3.,4),l;z c=B(D(51,46,43),D(165,147,143),b*b),d=aM(w);l=1.-.5*(d.g-d.r)*E(.5,3.,d.b)*C(1.,.0,d.b);A c*l*.8;}V(mtlt6f){y b=M(w,j(3),1.1,3.,4),l;z c=B(D(51,46,43),D(165,147,143),b*b),d=aM(w);l=1.-.5*(d.g-d.r)*E(.5,3.,d.b)*C(1.,.0,d.b);A c*l;}V(mtlbk03){y b=M(w,j(5),.9,3.,4);z c=B(D(36,35,33),D(56,54,52),b*b);A c;}V(cable){y b=M(w,j(5),.9,3.,4),h=I(w.g*10.);z c=B(D(53,48,42),D(38,38,36),b);c*=.6+b*.8;c*=1.-.5*aa(E(.5,.5,h));c*=1.+.5*aa(E(.25,.25,h));c*=1.+.5*aa(E(.65,.35,h));A c;}V(bmtsprt){y b=M(w,j(7,3),.9,3.,4),h=w.g+b*.04,l=1.-.15;z c=B(D(59,48,40),D(110,108,102),b*b);l=B(l,.5,E(.34,.05,w.g));l=B(l,.5,C(.08,.05,L(w.g-.7)));l=B(l,.3,E(.7,.03,w.g));l=B(l,1.5,E(.01,.03,w.g));l=B(l,2.2,E(.89,.1,h));l=B(l,1.6,C(.07,.04,L(w.g-.44)));l=B(l,2.5,E(.5,.04,h));l=B(l,1.7,E(.18,.04,h));A c*l;}V(brdr11b){y b=M(w,j(5,3),.9,3.,4);z c=B(D(74,66,55),D(99,90,78),b*b);w.r*=2.;j p=at(w,j(.5,.625),j(1.5,.625));y d=K(p-w),m=C(.22,.20,d),l=1.-.15*m;l=B(l,.5,C(.7,.9,w.g)*m);l=B(l,1.-ba(d).g*.5,E(.22,.04,d));l=B(l,.6,aa(E(.19,.05,d)));l=B(l,.5,C(.05,0.,w.g));l=B(l,.5,E(.26,.05,w.g));l=B(l,1.7,C(.93,1.,w.g));l=B(l,1.7,E(.23,.04,w.g));A c*l;}aj(blt414k){y b=M(w,j(1,5),.4,3.,4);z c=B(D(56,49,43),D(142,136,136),b);w=.5-L(w-.5);w.g*=4.;y a=E(.0,.1,K(w-at(w,j(.41,.5),j(.42,3.5)))),d=av(w),l=1.-.7*J(0.,1.-d/.15);l*=1.-.8*C(.24,.31,O(d,w.g-.1));c+=D(80,80,20)*a;A F(c*B(l,2.7,a),a);}aj(light5){y b=M(w,j(1,5),.4,3.,4);z c=B(D(56,49,43),D(142,136,136),b);w=.5-L(w-.5);w.g*=8.;y d=K(w-at(w,j(.27,.3),j(.27,7.7))),a=E(.0,.17,d),l=1.-.5*E(.17,.07,d);c+=D(80,80,20)*a;A F(c*B(l,2.7,a),a);}aj(lt2){j p=L(w-.5);y b=M(w,j(1),.4,3.,4),r=K(p),a=C(.37,.33,r)*(.5+2.*b),l=1.+.0*C(.08,.03,L(r-.41));z c=B(D(56,49,43),D(142,136,136),b);l=B(l,7.,C(.44,.1*b,r));l*=1.-.5*aa(E(.46,.04,r));l*=1.-.4*aa(E(.36,.04,r));A F(c*l,a);}aj(icon){w.g-=.57;w.r=L(w.r-.48);y d=a7(w,j(.31,.12))/50.;d=J(d,-a7(w-j(0,.01),j(.28,.07))/120.);d=J(d,-P(w-j(.0,.1),j(.22,.12)));d=J(d,-P(w-j(.0,.1),j(.09,.31)));d=O(d,a1(w-j(.0,-.09),j(E(-.09,.32,w.g)*.04,.32)));d=O(d,a1(w-j(.11,-.21),j(E(-.07,.3,w.g)*.03,.15)));w.g+=.07;y b=K(w)-.47,m=ac(b);z c=1.-z(.7,1,1)*ac(J(.007-d,b+.04));A F(c*m,m);}aj(q3bnr){w*=j(256,64);w.g+=2.;y d=a6(w-j(81,30),11.);d=J(d,w.r-80.);d=J(d,-a6(w-j(84,26),9.));d=O(d,P(w-j(73,37),j(4,9))-4.);d=J(d,-P(w-j(73,37),j(0,7))+1.);d=O(d,P(w-j(91.5,47),j(4,19))-4.);d=J(d,-P(w-j(91.5,47),j(0,17.5))+1.);d=O(d,P(bc(w,111.)-j(105.+C(23.,50.,w.g)*3.,43),j(3.5,19)));d=O(d,P(w-j(111,32),j(4,3)));d=O(d,P(w-j(126,37),j(3,13)));d=O(d,P(w-j(125.5+C(23.,50.,w.g)*10.,44),j(3.5,6)));d=O(d,P(w-j(136.5-C(23.,50.,w.g)*9.,32),j(3.5,8)));d=O(d,P(w-j(148.5,37),j(7,13)));d=J(d,-P(w-j(155,33),j(6,3)));d=J(d,-P(w-j(155,43),j(6,2)));d=O(d,P(w-j(168,37),j(3.5,13)));d=O(d,P(w-j(178.,37),j(3.5,13)));d=O(d,P(w-j(188,37),j(3.5,13)));d=J(d,w.g-50.);A F(ac(d,.8),0,0,H(w*511.));}Y q3bnr_m(){z c=ad(Z,UV*2.,-.5).Q*step(.5,I(af.r*.5));c=B(c*ai(),z(.5,0,0),E(I(af.r*2.),1./64.,I(UV.g)));U=F(c+aH(Ref)*.25+ad(Z,UV+H(af.rr)).a*.1,1);}Y beam(){j w=I(aJ(Pos.Q,aK(Nor))/128.);w.r+=af.r/33.;y b=M(w,j(7),.9,2.),f=I(Pos.b/128.-.375);U=F(2.*D(95,85,80)*f*f*f*f*B(1.,b,.5),0.);}Y Generic(){y l=dot(Nor,aI(z(2,0,8)));l=l*.4+.7;j w=aJ(Pos,aK(Nor));z c=z(.5);c*=bf(z(I(au*af.a+.25),1.,1.));U=F(c*l,1);}Y fixture(){F c=ad(Z,UV,-.5);U=F(c.Q*B(ai(),z(1),c.a),1);}Y dmnd2cjp_m(){F c=ad(Z,UV,-.5);y r=K(I(UV)-.5);y s=B(.4,8.,I(af.r*1.5));U=F(c.Q*ai()+D(240,130,5)*E(.1,.05,r/s)*C(.37,.32,r),1);}Y Lmapped(){z c=ad(Z,UV,-.5).Q;U=F(c*ai(),1);}Y shiny(){F c=ad(Z,UV,-.5);c.Q*=1.+c.a*aH(Ref);U=F(c.Q*ai(),1);}Y Loading(){U=ad(Z,(.5+UV*127.)/128.,2.5);U.Q*=.7+.3*ag(UV,.5/fwidth(UV));}Y UI(){U=ad(Z,UV)*Clr;}";

// src/demo/data/shaders/vertex_shaders.glsl: 1597 => 986 (61.7%)
const char g_vertex_shaders[] =
"#define c void\n"
"#define e location\n"
"#define f gl_Position\n"
"uniform mat4 MVP;uniform vec4 Cam;layout(e=0)in vec4 P;layout(e=1)in vec4 T;layout(e=2)in vec3 N;layout(e=3)in vec4 C;out vec3 Pos,Nor,Ref;out vec2 UV,LUV;out vec4 Clr;c Generic(){f=MVP*P;Pos=P.rgb;Nor=N;UV=T.rg;LUV=T.ba;Ref=normalize(reflect((P-Cam).rgb,N));}c d(){f=P;UV=P.rg*.5+.5;}c UI(){f=vec4(2.*P.r-1.,1.-2.*P.g,1,1);UV=T.rg;Clr=C;}c cmet52(){d();}c dmnd2c(){d();}c dmnd2cow(){d();}c dmnd2cjp(){d();}c dmnd2pnt(){d();}c lpdmnd(){d();}c ptrshn(){d();}c mtlfw10(){d();}c mtlfw15(){d();}c mtlfw15ow(){d();}c mtlfb3(){d();}c mtlt12f(){d();}c mtlt6f(){d();}c mtlbk03(){d();}c bmtsprt(){d();}c cable(){d();}c brdr11b(){d();}c blt414k(){d();}c light5(){d();}c lt2(){d();}c icon(){d();}c q3bnr(){d();}c Loading(){d();}c Lmapped(){Generic();}c mtlfw15ow_m(){Generic();}c dmnd2cjp_m(){Generic();}c dmnd2pnt_m(){Generic();}c lpdmnd_m(){Generic();}c fixture(){Generic();}c beam(){Generic();}c q3bnr_m(){Generic();}c shiny(){Generic();}";
