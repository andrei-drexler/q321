#pragma once
// auto-generated, do not modify (see cook_shader.js)

// trim_newlines       : true
// consistent_floats   : false
// rename_ids          : true
// rename_vec_fields   : true
// rename_globals      : true
// min_macro_savings   : 8

// src/demo/data/shaders/fragment_shaders.glsl: 26184 => 11639 (44.5%)
const char g_fragment_shaders[] =
"#define j vec2\n"
"#define y float\n"
"#define z vec3\n"
"#define A return\n"
"#define B mix\n"
"#define F vec4\n"
"#define I fract\n"
"#define K max\n"
"#define L length\n"
"#define M abs\n"
"#define O min\n"
"#define Q rgb\n"
"#define S FCol\n"
"#define W void\n"
"#define aa Time\n"
"#define ac Texture0\n"
"#define af texture\n"
"#define a0 normalize\n"
"uniform F aa,Cam;uniform sampler2D ac,Texture1;in z Pos,Nor,Ref;in j UV,LUV;in F Clr;out F S;\n"
"#define aQ(x)((x)*(x)*(3.-2.*(x)))\n"
"#define ad(x)(x)*(x)\n"
"#define sat(x)clamp(x,0.,1.)\n"
"#define C(r,g,b)(z(r,g,b)/255.)\n"
"y as=3.1415927,bk=2.*as,ay=1.618034;j aR(y i){y G=1.324718;A I(.5+i/j(G,G*G));}y aS(y i){A I(.5+i*ay);}y a2(j v){A O(v.r,v.g);}y a2(z v){A O(v.r,O(v.g,v.b));}y a2(F v){A O(O(v.r,v.g),O(v.b,v.a));}y at(j v){A K(v.r,v.g);}y at(z v){A K(v.r,K(v.g,v.b));}y at(F v){A K(K(v.r,v.g),K(v.b,v.a));}y a6(j v){A v.r+v.g;}y D(y aT,y aU,y x){A 1.-sat(M(x-aT)/aU);}y E(y a7,y aV,y x){A sat((x-a7)/(aV-a7));}j T(y x){A j(sin(x),cos(x));}mat2 a3(y x){j v=T(radians(x));A mat2(v.g,v.r,-v.r,v.g);}y H(j p){z q=I(p.rgr*.09153);q+=dot(q,q.gbr+19.19);A I((q.r+q.g)*q.b);}y H(y p){p=I(p*.1031);p*=p+33.33;p*=p+p;A I(p);}z bl(y p){z X=I(z(p)*z(.1031,.1030,.0973));X+=dot(X,X.gbr+33.33);A I((X.rrg+X.gbb)*X.bgr);}j aW(j p){z X=I(z(p.rgr)*z(.1031,.1030,.0973));X+=dot(X,X.gbr+33.33);A I((X.rr+X.gb)*X.bg);}F bm(y p){F am=I(F(p)*F(.1031,.1030,.0973,.1099));am+=dot(am,am.abrg+33.33);A I((am.rrgb+am.gbba)*am.bgar);}y U(y x,y p){A H(mod(x,p));}y N(y x){y i;A B(H(i=floor(x)),H(i+1.),aQ(x-i));}y Y(y x,y p){y i;A B(U(i=floor(x),p),U(i+1.,p),x-i);}y U(j p,j s){A H(mod(p,s));}y Y(j p,j s){p*=s;j i=floor(p);p-=i;p*=p*(3.-2.*p);y aX=U(i+j(0,0),s);y aY=U(i+j(0,1),s);y aZ=U(i+j(1,1),s);y ba=U(i+j(1,0),s);A B(B(aX,ba,p.r),B(aY,aZ,p.r),p.g);}y J(j p,j ah,y au,y av,int a4){y aw=Y(p,ah),al=1.,tw=1.;for(int i=0;i<a4;++i){p=I(p+ay);ah*=av;al*=au;aw+=Y(p,ah)*al;tw+=al;}A aw/tw;}y J(j p,j ah,y au,y av){A J(p,ah,au,av,4);}\n"
"#define a8(v)D(.5,.5,v)\n"
"y bc(j p,j ah,y au,y av,int a4){y aw=a8(Y(p,ah)),al=1.,tw=1.;for(int i=0;i<a4;++i){p=I(p+ay);ah*=av;al*=au;aw+=a8(Y(p,ah))*al;tw+=al;}A aw/tw;}j az(j p,j a,j b){j ab=b-a,ap=p-a;y t=sat(dot(ap,ab)/dot(ab,ab));A ab*t+a;}y P(j p,j b){j d=M(p)-b;A O(K(d.r,d.g),0.)+L(K(d,0.));}y a5(j p,j b){A at(M(p)-b);}y a9(j p,y r){A L(p)-r;}y aA(j p,j r){A(L(p/r)-1.)/O(r.r,r.g);}y bd(y a,y b,y k){y h=sat(.5+0.5*(b-a)/k);A B(b,a,h)-k*h*(1.-h);}j aB(y x){j d=j(dFdx(x),dFdy(x));A d/K(L(d),1e-8);}y ae(y s,y d){A sat(1.-s/d);}y ae(y s){A sat(1.-s/fwidth(s));}j be(j v,y m){A j(m-M(v.r-m),v.g);}z aC(j p,y T,y bf){z e=z(-1,0,1),r=z(1e5);j Z=floor(p*T),bn=e.gg;p-=(Z+.5)/T;y V=.5*U(Z+e.gg,j(T));y an=.5*U(Z+e.rg,j(T));y ao=.5*U(Z+e.gb,j(T));y aq=.5*U(Z+e.bg,j(T));y ar=.5*U(Z+e.gr,j(T));y aD=.5*U(Z+e.rb,j(T));y aE=.5*U(Z+e.bb,j(T));y aF=.5*U(Z+e.br,j(T));y aG=.5*U(Z+e.rr,j(T));j[4]ag,l;if(mod((Z.r+Z.g),2.)<.5){l[0]=1.+j(ao-an,aD-V);l[1]=1.+j(-ao+aq,aE-V);l[2]=1.+j(-ar+aq,-aF+V);l[3]=1.+j(ar-an,-aG+V);ag[0]=j(ao,V)+l[0]*j(-.5,.5);ag[1]=j(ao,V)+l[1]*j(.5,.5);ag[2]=j(ar,V)+l[2]*j(.5,-.5);ag[3]=j(ar,V)+l[3]*j(-.5,-.5);}else{l[0]=1.+j(-aD+V,ao-an);l[1]=1.+j(aE-V,ao-aq);l[2]=1.+j(aF-V,-ar+aq);l[3]=1.+j(-aG+V,-ar+an);ag[0]=j(V,an)+l[0]*j(-.5,.5);ag[1]=j(V,aq)+l[1]*j(.5,.5);ag[2]=j(V,aq)+l[2]*j(.5,-.5);ag[3]=j(V,an)+l[3]*j(-.5,-.5);}for(int i=0;i<4;i++){l[i]/=T;y bx=a5(p-ag[i]/T,l[i]/2.-bf/T);if(bx<r.r)r=z(bx,Z+ag[i]);}A r;}j aH(j p,j aI){p*=aI;j n=floor(p),f=p-n,aJ,g,o,r;y aK=8.0,d;for(int i=0;i<9;++i){g=j(i%3-1,i/3-1);o=aW(mod(n+g,aI));r=g+o-f;d=a6(M(r));if(d<aK){aK=d;aJ=r;}}A aJ;}y aL(z p){p=a0(p);z a=mod(degrees(atan(p,p.gbr)),360.);A Y(a.r/8.,45.)*E(.9,.0,M(p.b))+Y(a.g/8.,45.)*E(.7,.0,M(p.r));}j aM(z p,int ax){A(ax==0)?p.gb:(ax==1)?p.rb:p.rg;}int aN(z n){n=M(n)+z(.01,.02,.03);y m=at(n);A(m==n.r)?0:(m==n.g)?1:2;}z bh(z c){z Q=sat(M(mod(c.r*6.+z(0,4,2),6.)-3.)-1.);Q*=Q*(3.-2.*Q);A c.b*B(z(1.),Q,c.g);}z aj(){z d=Cam.Q-Pos;y b=J(d.rg/256.*a3(Cam.a),j(3),.7,3.,4),l=1.-E(14.,-6.,L(d.rg)-b*8.)*E(128.,48.,d.b)*step(.1,Nor.b);A af(Texture1,LUV).Q*3.*l;}\n"
"#define R(ai)z ai(j);W ai(){S=F(ai(UV),1);}z ai(j w)\n"
"#define ak(ai)F ai(j);W ai(){S=ai(UV);}F ai(j w)\n"
"R(cmet52){y b=J(w,j(5),.9,3.,4);z c=B(C(48,41,33),C(103,101,104),b);A c;}R(ptrshn){y b=J(w,j(3),.9,3.,4);z c=B(C(49,45,43),C(81,75,78),b*b);A c;}R(dmnd2c){y b=J(w,j(7),.9,3.);w.r*=-1.5;w.g+=w.r*.5;w.r=1.-w.r+w.g;w=I(w*28.);y f=sat(1.-L(.1-w));f*=E(.6,.2,L(.6-w));f*=E(.6,.8,L(.1-w));f*=E(.2,.6,b)*2.+1.;y l=1.-E(.2,b+2.,at(M(w-.5)));A z((f+1.)*B(.21,.29,b*b)*l);}R(dmnd2cow){y b=J(w,j(7),.9,3.);z c=dmnd2c(w);y r=L(w-.5);c=B(c,c*C(70,61,53),E(.5,.2,r+b*b*b));A c;}ak(dmnd2pnt){z c=dmnd2cow(w);w=I(w)-.5;y b=J(w,j(3),.9,3.),d=M(L(w)-.4),i=0.;for(;i<5.;++i){j p=j(0,-.35)*a3(i*72.);d=O(d,L(w-az(w,p,p*a3(144.))));}A F(c,ae(d-.02+b*.02,.01));}W dmnd2pnt_m(){F c=af(ac,UV);S=F(c.Q*aj()+C(111,55,0)*c.a*(sin(aa.r*as)*.5+.5),1);}R(dmnd2cjp){y b=J(w,j(7),.9,3.,4);z c=dmnd2c(w);y r=L(w-.5);y m=E(.46,.45,r);y l=1.5-1.5*E(.0,.3,r*r);l=B(l,2.5,D(.42,.07,r));l=B(l,3.5,D(.44,.05,r));l=B(l,2.6,D(.36,.03,r));y n=.3+.2*E(.35,.30,r);l*=1.-n*E(.3,.7,b);l*=1.-.3*ad(E(.13,.05,r));l=B(l,2.5,E(.04,.01,r));l-=l*D(.03,.01,r)*.7;c=B(c,C(68,66,54)*l,m);c*=1.-ad(D(.34,.02,r));c*=1.-ad(D(.46,.03,r));c*=1.-D(.41,.03,r)*.7;A c;}j bi(j w,y s){A j(1.-L(w)/s,ae(L(w)-s));}ak(lpdmnd){y b=J(w,j(5),.9,3.),t,o,k,r;z c=dmnd2c(w);j u,v;u.r=M(w.r-.5);u.g=O(w.g,.4);r=L(u-j(0,.4))-(.18-.06*E(.4,1.,w.g));k=.25-.15*E(.9,.96,w.g)+.03*ad(E(.82,.86,w.g))+.07*E(.8,.2,w.g)+.07*ad(E(.35,.22,w.g))-.07*E(.22,.0,w.g);o=P(w-j(.5,.5),j(k,.46));o=K(o,-P(u,j(.15,.03))+.06);c=B(c,z(.6,.55,.55)-w.g*.3+b*.2,ae(o));c*=1.-.7*D(.0,.013,o);c*=1.-(r/.5-.1)*ae(o);t=K(r,w.g-.96);o=M(t-.02)-.03;o=K(o,w.g-1.+u.r*.5);o=K(o,w.g-.96);c=B(c,z(1,1,.9)-w.g*.55,D(-.01,.01,o));c=B(c,z(.2*b+.1),ae(t,.01));c*=1.-.2*D(.0,.05,t)*ae(o);v=bi(u=w-j(.5,.4),.02);c*=1.+C(111,80,70)*D(.03,.01,L(u));c*=1.-.5*D(.02,.01,L(u));c=B(c,C(111,66,44)*(v.r*1.5+.2),v.g);A F(c,ae(t-.03,.02));}W lpdmnd_m(){F c=af(ac,UV);j w=I(UV);w.r=M(.5-w.r);y t=I(-aa.r),r=L(w-j(0,.4)),l=t*pow(K(0.,1.-r),4.)*c.a;if(t>.75)l+=E(.03,.01,M(I(w.g+w.r*.5+t*2.)-.45))*E(.1,.08,w.r);S=F(c.Q*aj()+C(180,150,5)*l,1);}R(mtlfw10){y b=J(w,j(5),.9,3.,4);z c=B(C(44,14,16),C(93,63,63),b*b);A c;}z aO(j w){y e=3e-3,a=0.;j g=j(6),r=aH(w,g);for(int i=0;i<9;++i)a+=a6(M(aH(j(i%3-1,i/3-1)*e+w,g)-r));A z(w+r.rg/g,a);}R(mtlfw15){y b=J(w,j(3),.9,3.,4);z c=B(C(80,70,72),C(128,120,120),b*b);z v=aO(w);c*=B(.95,1.1,Y(v.rg,j(6)));c=B(c,C(168,128,120),E(.5,1.,v.b)*b*.7);A c;}ak(mtlfw15ow){y b=J(w,j(3),.9,3.,4);z c=B(C(80,70,72),C(128,120,120),b*b);z v=aO(w);y m=E(.5,1.,v.b);y r=E(.4,.2,L(.5-I(v.rg)));c*=B(.95,1.1,Y(v.rg,j(6)))-2.*r*b*b;c=B(c,C(168,128,120),m*b*.7);A F(c,m*r);}W mtlfw15ow_m(){F c=af(ac,UV);S=F(c.Q*aj()+D(.5,.125,I(UV.g*.5+aa.r*.5))*c.a*.3,1);}R(mtlfb3){y b=J(w,j(5),.9,3.,4);z pt=aC(w,8.,.31);z c=B(C(66,58,55),C(118,107,105),b);y l=1.-.5*E(.034,.036,pt.r);l=B(l,1.4,D(.033,.004,pt.r));A c*l;}y bj(j w){y b=Y(w,j(64)),f=0.,d=1e6;for(;f<11.;++f)d=bd(d,M(L(.5-M(w-aR(f)))-B(.36,.29,aS(f+.7)))-B(.015,.03,b),.01);A d*1e2;}z aP(j w){z s,p;for(int i=0;i<3;++i){p=z(w,0);p[i]+=1e-4;s[i]=bj(p.rg);}A z(a0(s.rg-s.b),s.b);}R(mtlt12f){y b=J(w,j(5),.9,3.,4),l;z c=B(C(51,46,43),C(165,147,143),b*b),d=aP(w);l=1.-.5*(d.g-d.r)*D(.5,3.,d.b)*E(1.,.0,d.b);A c*l*.8;}R(mtlt6f){y b=J(w,j(3),1.1,3.,4),l;z c=B(C(51,46,43),C(165,147,143),b*b),d=aP(w);l=1.-.5*(d.g-d.r)*D(.5,3.,d.b)*E(1.,.0,d.b);A c*l;}R(mtlbk03){y b=J(w,j(5),.9,3.,4);z c=B(C(36,35,33),C(56,54,52),b*b);A c;}R(gmtlbg6){y b=J(w,j(13),1.,3.,4);z c=B(C(36,35,31),C(132,132,132),b);A c;}R(glrgbk3b){y b=J(w,j(13),1.,3.,4);z c=B(C(67,37,27),C(152,117,85),b);A c;}R(gblks15){y b=J(w,j(5),.9,3.,4),t=bc(w+sin(w.gr*as*4.)*.01,j(5),1.,2.,5),n=Y(w+sin(w.gr*as*4.)*.05,j(9)),a1,e;z pt=aC(w,4.,.1+n*t*.05),c;j d=aB(pt.r);a1=H(I(pt.gb));c=C(91,67,61)*(.8+b*b*.8);c+=D(.6,.3,n)*E(.3,.9,b*t)*.3;c*=1.-D(.5,.4,n)*E(.5,.7,t)*.2;c=B(c,C(86,74,78),D(.5,.1,b)*D(.7,.3,a1)*.7);c=B(c,C(105,90,70),D(.3,.1,t)*D(.3,.3,a1)*.3);e=K(D(.02,.007,pt.r),D(.4,.03,n*t));c*=1.-b*t*E(.015,.022,pt.r)+e*b*d.g*.6;c*=.9+.2*a1;A c;}R(gtprst3){y b=J(w,j(13),.9,3.,4),n=Y(w+sin(w.gr*as*4.)*.05,j(9));z c=B(C(56,48,49),C(100,50,40),.7*(n+b)*.5)*(.875+b*b);A c;}R(cable){y b=J(w,j(5),.9,3.,4),h=I(w.g*10.);z c=B(C(53,48,42),C(38,38,36),b);c*=.6+b*.8;c*=1.-.5*ad(D(.5,.5,h));c*=1.+.5*ad(D(.25,.25,h));c*=1.+.5*ad(D(.65,.35,h));A c;}R(bmtsprt){y b=J(w,j(7,3),.9,3.,4),h=w.g+b*.04,l=1.-.15;z c=B(C(59,48,40),C(110,108,102),b*b);l=B(l,.5,D(.34,.05,w.g));l=B(l,.5,E(.08,.05,M(w.g-.7)));l=B(l,.3,D(.7,.03,w.g));l=B(l,1.5,D(.01,.03,w.g));l=B(l,2.2,D(.89,.1,h));l=B(l,1.6,E(.07,.04,M(w.g-.44)));l=B(l,2.5,D(.5,.04,h));l=B(l,1.7,D(.18,.04,h));A c*l;}R(brdr11b){y b=J(w,j(5,3),.9,3.,4);z c=B(C(74,66,55),C(99,90,78),b*b);w.r*=2.;j p=az(w,j(.5,.625),j(1.5,.625));y d=L(p-w),m=E(.22,.20,d),l=1.-.15*m;l=B(l,.5,E(.7,.9,w.g)*m);l=B(l,1.-aB(d).g*.5,D(.22,.04,d));l=B(l,.6,ad(D(.19,.05,d)));l=B(l,.5,E(.05,0.,w.g));l=B(l,.5,D(.26,.05,w.g));l=B(l,1.7,E(.93,1.,w.g));l=B(l,1.7,D(.23,.04,w.g));A c*l;}ak(blt414k){y b=J(w,j(1,5),.4,3.,4);z c=B(C(56,49,43),C(142,136,136),b);w=.5-M(w-.5);w.g*=4.;y a=D(.0,.1,L(w-az(w,j(.41,.5),j(.42,3.5)))),d=a2(w),l=1.-.7*K(0.,1.-d/.15);l*=1.-.8*E(.24,.31,O(d,w.g-.1));c+=C(80,80,20)*a;A F(c*B(l,2.7,a),a);}ak(light5){y b=J(w,j(1,5),.4,3.,4);z c=B(C(56,49,43),C(142,136,136),b);w=.5-M(w-.5);w.g*=8.;y d=L(w-az(w,j(.27,.3),j(.27,7.7))),a=D(.0,.17,d),l=1.-.5*D(.17,.07,d);c+=C(80,80,20)*a;A F(c*B(l,2.7,a),a);}ak(lt2){j p=M(w-.5);y b=J(w,j(1),.4,3.,4),r=L(p),a=E(.37,.33,r)*(.5+2.*b),l=1.+.0*E(.08,.03,M(r-.41));z c=B(C(56,49,43),C(142,136,136),b);l=B(l,7.,E(.44,.1*b,r));l*=1.-.5*ad(D(.46,.04,r));l*=1.-.4*ad(D(.36,.04,r));A F(c*l,a);}ak(icon){w.g-=.57;w.r=M(w.r-.48);y d=aA(w,j(.31,.12))/50.;d=K(d,-aA(w-j(0,.01),j(.28,.07))/120.);d=K(d,-P(w-j(.0,.1),j(.22,.12)));d=K(d,-P(w-j(.0,.1),j(.09,.31)));d=O(d,a5(w-j(.0,-.09),j(D(-.09,.32,w.g)*.04,.32)));d=O(d,a5(w-j(.11,-.21),j(D(-.07,.3,w.g)*.03,.15)));w.g+=.07;y b=L(w)-.47,m=ae(b);z c=1.-z(.7,1,1)*ae(K(.007-d,b+.04));A F(c*m,m);}ak(q3bnr){w*=j(256,64);w.g+=2.;y d=a9(w-j(81,30),11.);d=K(d,w.r-80.);d=K(d,-a9(w-j(84,26),9.));d=O(d,P(w-j(73,37),j(4,9))-4.);d=K(d,-P(w-j(73,37),j(0,7))+1.);d=O(d,P(w-j(91.5,47),j(4,19))-4.);d=K(d,-P(w-j(91.5,47),j(0,17.5))+1.);d=O(d,P(be(w,111.)-j(105.+E(23.,50.,w.g)*3.,43),j(3.5,19)));d=O(d,P(w-j(111,32),j(4,3)));d=O(d,P(w-j(126,37),j(3,13)));d=O(d,P(w-j(125.5+E(23.,50.,w.g)*10.,44),j(3.5,6)));d=O(d,P(w-j(136.5-E(23.,50.,w.g)*9.,32),j(3.5,8)));d=O(d,P(w-j(148.5,37),j(7,13)));d=K(d,-P(w-j(155,33),j(6,3)));d=K(d,-P(w-j(155,43),j(6,2)));d=O(d,P(w-j(168,37),j(3.5,13)));d=O(d,P(w-j(178.,37),j(3.5,13)));d=O(d,P(w-j(188,37),j(3.5,13)));d=K(d,w.g-50.);A F(ae(d,.8),0,0,H(w*511.));}W q3bnr_m(){z c=af(ac,UV*2.).Q*step(.5,I(aa.r*.5));c=B(c*aj(),z(.5,0,0),D(I(aa.r*2.),1./64.,I(UV.g)));S=F(c+aL(Ref)*.25+af(ac,UV+H(aa.rr)).a*.1,1);}W beam(){j w=I(aM(Pos.Q,aN(Nor))/128.);w.r+=aa.r/33.;y b=J(w,j(7),.9,2.),f=I(Pos.b/128.-.375);S=F(2.*C(95,85,80)*f*f*f*f*B(1.,b,.5),0.);}W flame(){S=F(0);}W Generic(){y l=dot(Nor,a0(z(2,0,8)));l=l*.4+.7;j w=aM(Pos,aN(Nor));z c=z(.5);c*=bh(z(I(ay*aa.a+.25),1.,1.));S=F(c*l,1);}W fixture(){F c=af(ac,UV);S=F(c.Q*B(aj(),z(1),c.a),1);}W dmnd2cjp_m(){F c=af(ac,UV);y r=L(I(UV)-.5);y s=B(.4,8.,I(aa.r*1.5));S=F(c.Q*aj()+C(240,130,5)*D(.1,.05,r/s)*E(.37,.32,r),1);}W Lmapped(){z c=af(ac,UV).Q;S=F(c*aj(),1);}W shiny(){F c=af(ac,UV);c.Q*=1.+c.a*aL(Ref);S=F(c.Q*aj(),1);}W timhel(){z d=a0(Pos-Cam.Q);d.b=d.b*4.+2.;j w=a0(d).rg*2.;y b=J(w-aa.r*j(.1,.2),j(5),.5,2.,4);w.g*=1.5;y s=E(.3,1.,J(w-aa.r*j(.1,.18),j(5),.6,2.,4));S=F(z(b,0,0)+C(80,30,8)*s*s*2.,1);}W Loading(){S=af(ac,(.5+UV*127.)/128.,2.5);S.Q*=.7+.3*Y(UV,.5/fwidth(UV));}W UI(){S=af(ac,UV)*Clr;}";

// src/demo/data/shaders/vertex_shaders.glsl: 1761 => 1098 (62.4%)
const char g_vertex_shaders[] =
"#define c void\n"
"#define e location\n"
"#define f gl_Position\n"
"uniform mat4 MVP;uniform vec4 Cam;layout(e=0)in vec4 P;layout(e=1)in vec4 T;layout(e=2)in vec3 N;layout(e=3)in vec4 C;out vec3 Pos,Nor,Ref;out vec2 UV,LUV;out vec4 Clr;c Generic(){f=MVP*P;Pos=P.rgb;Nor=N;UV=T.rg;LUV=T.ba;Ref=normalize(reflect((P-Cam).rgb,N));}c d(){f=P;UV=P.rg*.5+.5;}c UI(){f=vec4(2.*P.r-1.,1.-2.*P.g,1,1);UV=T.rg;Clr=C;}c cmet52(){d();}c dmnd2c(){d();}c dmnd2cow(){d();}c dmnd2cjp(){d();}c dmnd2pnt(){d();}c lpdmnd(){d();}c ptrshn(){d();}c mtlfw10(){d();}c mtlfw15(){d();}c mtlfw15ow(){d();}c mtlfb3(){d();}c mtlt12f(){d();}c mtlt6f(){d();}c mtlbk03(){d();}c gmtlbg6(){d();}c glrgbk3b(){d();}c gblks15(){d();}c gtprst3(){d();}c bmtsprt(){d();}c cable(){d();}c brdr11b(){d();}c blt414k(){d();}c light5(){d();}c lt2(){d();}c icon(){d();}c q3bnr(){d();}c Loading(){d();}c Lmapped(){Generic();}c mtlfw15ow_m(){Generic();}c dmnd2cjp_m(){Generic();}c dmnd2pnt_m(){Generic();}c lpdmnd_m(){Generic();}c fixture(){Generic();}c beam(){Generic();}c flame(){Generic();}c q3bnr_m(){Generic();}c shiny(){Generic();}c timhel(){Generic();}";
