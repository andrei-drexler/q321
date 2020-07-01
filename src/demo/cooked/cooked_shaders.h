#pragma once

// auto-generated, do not modify
static_assert(0x88fa0672U == Demo::Shader::Version, "Shader definition mismatch, please recompile the shader compiler & shaders");

// src/demo/data/shaders/fragment_shaders.glsl: 75973 => 30880 (40.6%)
static constexpr char g_fragment_shaders[] =
"#define d vec2\n"
"#define h float\n"
"#define k vec3\n"
"#define l return\n"
"#define o mix\n"
"#define C abs\n"
"#define F vec4\n"
"#define G fract\n"
"#define K length\n"
"#define M max\n"
"#define O min\n"
"#define P void\n"
"#define S UV\n"
"#define U Time\n"
"#define W int\n"
"#define X if\n"
"#define ae xyz\n"
"#define ah floor\n"
"#define ai Texture0\n"
"#define aj texture\n"
"#define a8 sqrt\n"
"#define aE normalize\n"
"#define b3(x)((x)*(x)*(3.-2.*(x)))\n"
"#define E(x)((x)*(x))\n"
"#define aT(x)dot(x,x)\n"
"#define ac(x)clamp(x,0.,1.)\n"
"#define n(r,g,b)(k(r,g,b)/255.)\n"
"#define a6(u)i(.5,.5,u)\n"
"#define H(a7)k a7(d);P a7(){N=F(a7(S),1);}k a7(d c)\n"
"#define an(a7)F a7(d);P a7(){N=a7(S);}F a7(d c)\n"
"#define bh(ca,c,cb){d p[3];h r[3];p[0]=c;p[1]=c+dFdx(c);p[2]=c+dFdy(c);for(W m=0;m<3;++m)r[m]=cb;ca=k(cc(d(r[1],r[2])-r[0]),r[0]);}\n"
"#define b4(a7,cd)k a7(d p,d b5){p*=b5;d A=ah(p),T=p-A,b6,g,ad,r;h aU=8.,bi=aU,f;for(W m=0;m<9;++m){g=d(m%3-1,m/3-1);ad=ce(mod(A+g,b5));r=g+ad-T;f=cd;X(f<aU){bi=aU;aU=f;b6=r;}else X(f<bi){bi=f;}}l k(b6,bi-aU);}\n"
"uniform F U,Cam;uniform sampler2D ai,Texture1;in k Pos,Nor,WNor,Ref;in d S,LUV;in F Clr;out F N;h aH=3.1415927,b7=2.*aH,aV=1.618034;d b8(h m){h bo=1.324718;l G(.5+m/d(bo,bo*bo));}h aI(h m){l G(.5+m*aV);}h cf(h T){h m=ah(T);l o(aI(m),aI(m+1.),b3(T-m));}h bj(d u){l O(u.x,u.y);}h bj(k u){l O(u.x,O(u.y,u.z));}h bj(F u){l O(O(u.x,u.y),O(u.z,u.w));}h aF(d u){l M(u.x,u.y);}h aF(k u){l M(u.x,M(u.y,u.z));}h aF(F u){l M(M(u.x,u.y),M(u.z,u.w));}h aL(d u){l u.x+u.y;}h aL(k u){l u.x+u.y+u.z;}h b9(h a,h b){l C(a)<C(b)?a:b;}h bu(h x,h s){l C(x)-s;}h bv(h x,h s){l sign(x)*M(0.,C(x)-s);}d cc(d u){h v=dot(u,u);l v>0.?u/a8(v):u;}h i(h cg,h ch,h x){l 1.-ac(C(x-cg)/ch);}h j(h bA,h ci,h x){l ac((x-bA)/(ci-bA));}h i(h a,h b,h e,h x){l O(j(a,b,x),j(e,b,x));}d ak(h x){l d(sin(x),cos(x));}mat2 au(h x){d u=ak(radians(x));l mat2(u.y,u.x,-u.x,u.y);}h aM(d p){l G(atan(p.y,p.x)/b7);}d cj(d p,h A){l p*au(360./A*(ah(aM(p)*A+1.5)-1.));}d Y(d c,h p,h s){l c+sin(c.yx*aH*p)*s;}d Y(d c,h t,h p,h s){l c+sin(c.yx*aH*p+t)*s;}h bk(h u,h B){l B-C(u-B);}d bk(d u,h B){u.x=bk(u.x,B);l u;}h ck(h x,h p,h v,h R){l x-p*clamp(ah(x/p+.5),v,R);}d bB(d c,d s){c.x+=ah(c.y*s.y)*(.5/s.x);l G(c)*s;}k bC(d c,h r){l k(c-=clamp(c,r,1.-r),K(c)/r);}k bC(d c,d s,h r){s=s.yx/bj(s);c*=s;l k(c-=clamp(c,d(r),s-r),K(c)/r);}h af(d p){k q=G(p.xyx*.09153);q+=dot(q,q.yzx+19.19);l G((q.x+q.y)*q.z);}h af(h p){p=G(p*.1031);p*=p+33.33;p*=p+p;l G(p);}k c9(h p){k av=G(k(p)*k(.1031,.1030,.0973));av+=dot(av,av.yzx+33.33);l G((av.xxy+av.yzz)*av.zyx);}d ce(d p){k av=G(k(p.xyx)*k(.1031,.1030,.0973));av+=dot(av,av.yzx+33.33);l G((av.xx+av.yz)*av.zy);}F cl(h p){F a0=G(F(p)*F(.1031,.1030,.0973,.1099));a0+=dot(a0,a0.wzxy+33.33);l G((a0.xxyz+a0.yzzw)*a0.zywx);}F cl(d p){F a0=G(F(p.xyxy)*F(.1031,.1030,.0973,.1099));a0+=dot(a0,a0.wzxy+33.33);l G((a0.xxyz+a0.yzzw)*a0.zywx);}h al(h x,h p){l af(mod(x,p));}h cm(h x){h m;l o(af(m=ah(x)),af(m+1.),b3(x-m));}h V(h x,h p){h m;l o(al(m=ah(x),p),al(m+1.,p),x-m);}h al(d p,d s){l af(mod(p,s));}h V(d p,d s){p*=s;d m=ah(p);p-=m;p*=p*(3.-2.*p);h cn=al(m+d(0,0),s);h co=al(m+d(0,1),s);h cp=al(m+d(1,1),s);h cq=al(m+d(1,0),s);l o(o(cn,cq,p.x),o(co,cp,p.x),p.y);}h D(d p,d a1,h aW,h aX,W aY){h aG=V(p,a1),a9=1.,tw=1.;for(W m=0;m<aY;++m){p=G(p+aV);a1*=aX;a9*=aW;aG+=V(p,a1)*a9;tw+=a9;}l aG/tw;}h D(h p,h a1,h aW,h aX,W aY){h aG=V(p*a1,a1),a9=1.,tw=1.;for(W m=0;m<aY;++m){p=G(p+aV);a1*=aX;a9*=aW;aG+=V(p*a1,a1)*a9;tw+=a9;}l aG/tw;}h bD(d p,d a1,h aW,h aX,W aY){h aG=a6(V(p,a1)),a9=1.,tw=1.;for(W m=0;m<aY;++m){p=G(p+aV);a1*=aX;a9*=aW;aG+=a6(V(p,a1))*a9;tw+=a9;}l aG/tw;}d bl(d p,d a,d b){d ab=b-a,ap=p-a;h t=ac(dot(ap,ab)/dot(ab,ab));l ab*t+a;}h cr(d c,d f){l dot(c,au(90.)*aE(f));}h Q(d p,d b){d f=C(p)-b;l O(M(f.x,f.y),0.)+K(M(f,0.));}h aZ(d p,d b){l aF(C(p)-b);}h Z(d p,h r){l K(p)-r;}h bc(d p,d r){l Z(p/r,1.)/O(r.x,r.y);}h aC(h a,h b){l M(a,-b);}h bE(h a,h b,h L){h R=ac(.5+0.5*(b-a)/L);l o(b,a,R)-L*R*(1.-R);}d bF(h x){d f=d(dFdx(x),dFdy(x));l f/M(K(f),1e-8);}h J(h s,h f){l ac(1.-s/f);}h J(h s){l ac(1.-s/fwidth(s));}F bG(d c,h s){l F(c/=s,a8(ac(1.-aT(c))),K(c)-1.);}h cs(k A){h v=aL(A.yz)*.7;l pow(ac(v),4.)+v;}h bH(d c,h s){c/=s;c.y+=.06;c.x*=2.;l j(.3,.0,K(c));}k bd(k e,d c,h s){F b=bG(c,s);e*=1.+cs(b.ae)*J(b.w)*.5;e*=1.-E(bH(c,20.*s))*(1.-J(b.w))*.3;l e;}h be(d c,h t){c.x=C(c.x);c.y-=.07;h f=bc(c,d(.31,.12-t*.02))/50.;f=M(f,-bc(c-d(0,.01+.01*t),d(.28+t*.01,.07))/75.);f=M(f,-Q(c-d(0,.1),d(.22-.02*t,.12)));f=M(f,-Q(c-d(0,.1),d(.084-.012*t,.31)));f=O(f,aZ(c-d(0,-.09),d(i(-.09,.32,c.y)*(.04-.015*t),.32)));f=O(f,aZ(c-d(.11-.02*t,-.21+.01*t),d(i(-.07,.3,c.y)*(.03-.01*t),.15)));l f;}h ct(d c){h f=Z(c-d(0,.17),.32);f=aC(f,Z(c-d(0,.235),.27));f=aC(f,Z(c-d(0,.5),.15));c.y-=.09;d s=d(.09,.52);h R=ac(-c.y/s.y);s*=.5;s.x*=j(1.05,.6,R)+E(j(.1,.02,R));c.y+=s.y;f=O(f,Q(c,s));l f;}k bI(d p,h ak,h cu){k I=k(-1,0,1),r=k(1e5);d a2=ah(p*ak),cA=I.yy;p-=(a2+.5)/ak;h am=.5*al(a2+I.yy,d(ak)),aN=.5*al(a2+I.xy,d(ak)),aO=.5*al(a2+I.yz,d(ak)),aP=.5*al(a2+I.zy,d(ak)),aQ=.5*al(a2+I.yx,d(ak)),bJ=.5*al(a2+I.xz,d(ak)),bK=.5*al(a2+I.zz,d(ak)),bL=.5*al(a2+I.zx,d(ak)),bM=.5*al(a2+I.xx,d(ak));d[4]aA,v;X(mod(a2.x+a2.y,2.)<.5){v[0]=1.+d(aQ-aN,am-bM);v[1]=1.+d(aP-aQ,am-bL);v[2]=1.+d(aO-aN,bJ-am);v[3]=1.+d(aP-aO,bK-am);aA[0]=d(aQ,am);aA[1]=d(aQ,am);aA[2]=d(aO,am);aA[3]=d(aO,am);}else{v[0]=1.+d(am-bM,aN-aQ);v[1]=1.+d(bL-am,aP-aQ);v[2]=1.+d(am-bJ,aO-aN);v[3]=1.+d(bK-am,aO-aP);aA[0]=d(am,aN);aA[1]=d(am,aP);aA[2]=d(am,aN);aA[3]=d(am,aP);}for(W m=0;m<4;m++){aA[m]+=v[m]*(d(m&1,m/2)-.5);v[m]/=ak;h bx=aZ(p-aA[m]/ak,v[m]/2.-cu/ak);X(bx<r.x)r=k(bx,a2+aA[m]);}l r;}b4(bm,aL(C(r)))b4(b0,K(r))h aJ(k p,h s){p=aE(p);k a=G(degrees(atan(p,p.yzx))/360.);l V(a.x*s,s)*j(.9,.0,C(p.z))+V(a.y*s,s)*j(.7,.0,C(p.x));}h aJ(k p){l aJ(p,45);}d cv(k p,W ax){l(ax==0)?p.yz:(ax==1)?p.xz:p.xy;}W cw(k A){A=C(A)+k(.01,.02,.03);h B=aF(A);l(B==A.x)?0:(B==A.y)?1:2;}k cx(k e){k rgb=ac(C(mod(e.x*6.+k(0,4,2),6.)-3.)-1.);rgb*=rgb*(3.-2.*rgb);l e.z*o(k(1.),rgb,e.y);}k a3(){k f=Cam.ae-Pos;h b=D(f.xy/256.*au(Cam.w),d(3),.7,3.,4),v=1.-j(14.,-6.,K(f.xy)-b*8.)*j(128.,48.,f.z)*step(.1,Nor.z);l aj(Texture1,LUV).ae*2.*v;}H(_8){h b=D(c,d(5),.9,3.,4);k e=o(n(48,41,33),n(103,101,104),b);l e;}H(_9){h b=D(c,d(3),.9,3.,4);k e=o(n(49,45,43),n(81,75,78),b*b);l e;}H(_10){h b=D(c,d(7),.9,3.,4);c.x*=-1.5;c.y+=c.x*.5;c.x=1.-c.x+c.y;c=G(c*28.);h T=ac(1.-K(.1-c));T*=j(.6,.2,K(.6-c));T*=j(.6,.8,K(.1-c));T*=j(.2,.6,b)*2.+1.;h v=1.-j(.2,b+2.,aF(C(c-.5)));l k((T+1.)*o(.21,.29,b*b)*v);}H(_11){h b=D(c,d(7),.9,3.,4),r=K(c-.5);k e=_10(c);e=o(e,e*n(70,61,53),j(.5,.2,r+b*b*b));l e;}h bN(d c,h s){h f=1e6,m=0.;for(;m<5.;++m){d p=d(0,-s)*au(m*72.);f=O(f,K(c-bl(c,p,p*au(144.))));}l f;}an(_12){k e=_11(c);c=G(c)-.5;h b=D(c,d(3),.9,3.,4),f=O(C(K(c)-.4),bN(c,.35));l F(e,J(f-.02+b*.02,.01));}P _13(){F e=aj(ai,S);N=F(e.ae*a3()+n(111,55,0)*e.w*(sin(U.x*aH)*.5+.5),1);}H(_70){h b=D(c,d(7),.9,3.,4);k e=_10(c);h r=K(c-.5);h B=j(.46,.45,r);h v=1.5-1.5*j(.0,.3,r*r);v=o(v,2.5,i(.42,.07,r));v=o(v,3.5,i(.44,.05,r));v=o(v,2.6,i(.36,.03,r));h A=.3+.2*j(.35,.30,r);v*=1.-A*j(.3,.7,b);v*=1.-.3*E(j(.13,.05,r));v=o(v,2.5,j(.04,.01,r));v-=v*i(.03,.01,r)*.7;e=o(e,n(68,66,54)*v,B);e*=1.-E(i(.34,.02,r));e*=1.-E(i(.46,.03,r));e*=1.-i(.41,.03,r)*.7;l e;}d cy(d c,h s){l d(1.-K(c)/s,J(K(c)-s));}an(_72){h b=D(c,d(5),.9,3.,4),t,ad,L,r;k e=_10(c);d a4,u;a4.x=C(c.x-.5);a4.y=O(c.y,.4);r=K(a4-d(0,.4))-(.18-.06*j(.4,1.,c.y));L=.25-.15*j(.9,.96,c.y)+.03*E(j(.82,.86,c.y))+.07*j(.8,.2,c.y)+.07*E(j(.35,.22,c.y))-.07*j(.22,.0,c.y);ad=Q(c-d(.5,.5),d(L,.46));ad=M(ad,-Q(a4,d(.15,.03))+.06);e=o(e,k(.6,.55,.55)-c.y*.3+b*.2,J(ad));e*=1.-.7*i(.0,.013,ad);e*=1.-(r/.5-.1)*J(ad);t=M(r,c.y-.96);ad=C(t-.02)-.03;ad=M(ad,c.y-1.+a4.x*.5);ad=M(ad,c.y-.96);e=o(e,k(1,1,.9)-c.y*.55,i(-.01,.01,ad));e=o(e,k(.2*b+.1),J(t,.01));e*=1.-.2*i(.0,.05,t)*J(ad);u=cy(a4=c-d(.5,.4),.02);e*=1.+n(111,80,70)*i(.03,.01,K(a4));e*=1.-.5*i(.02,.01,K(a4));e=o(e,n(111,66,44)*(u.x*1.5+.2),u.y);l F(e,J(t-.03,.02));}P _73(){F e=aj(ai,S);d c=G(S);c.x=C(.5-c.x);h t=G(-U.x),r=K(c-d(0,.4)),v=t*pow(M(0.,1.-r),4.)*e.w;X(t>.75)v+=j(.03,.01,C(G(c.y+c.x*.5+t*2.)-.45))*j(.1,.08,c.x);N=F(e.ae*a3()+n(180,150,5)*v,1);}H(_14){h b=D(c,d(5),.9,3.,4);k e=o(n(44,14,16),n(93,63,63),b*b);l e;}k bO(d c){h I=3e-3,a=0.;d g=d(6),r=bm(c,g).xy;for(W m=0;m<9;++m)a+=aL(C(bm(d(m%3-1,m/3-1)*I+c,g).xy-r));l k(c+r.xy/g,a);}H(_15){h b=D(c,d(3),.9,3.,4);k e=o(n(80,70,72),n(128,120,120),b*b);k u=bO(c);e*=o(.95,1.1,V(u.xy,d(6)));e=o(e,n(168,128,120),j(.5,1.,u.z)*b*.7);l e;}an(_16){h b=D(c,d(3),.9,3.,4);k e=o(n(80,70,72),n(128,120,120),b*b);k u=bO(c);h B=j(.5,1.,u.z);h r=j(.4,.2,K(.5-G(u.xy)));e*=o(.95,1.1,V(u.xy,d(6)))-2.*r*b*b;e=o(e,n(168,128,120),B*b*.7);l F(e,B*r);}P _17(){F e=aj(ai,S);N=F(e.ae*a3()+i(.5,.125,G(S.y*.5+U.x*.5))*e.w*.3,1);}H(_18){h b=D(c,d(5),.9,3.,4);k pt=bI(c,8.,.31);k e=o(n(66,58,55),n(118,107,105),b);h v=1.-.5*j(.034,.036,pt.x);v=o(v,1.4,i(.033,.004,pt.x));l e*v;}h cz(d c){h b=V(c,d(64)),T=0.,f=1e6;for(;T<11.;++T)f=bE(f,C(K(.5-C(c-b8(T)))-o(.36,.29,aI(T+.7)))-o(.015,.03,b),.01);l f*1e2;}k bP(d c){k s,p;for(W m=0;m<3;++m){p=k(c,0);p[m]+=1e-4;s[m]=cz(p.xy);}l k(aE(s.xy-s.z),s.z);}H(_19){h b=D(c,d(5),.9,3.,4),v;k e=o(n(51,46,43),n(165,147,143),b*b),f=bP(c);v=1.-.5*(f.y-f.x)*i(.5,3.,f.z)*j(1.,.0,f.z);l e*v*.8;}H(_20){h b=D(c,d(3),"
"1.1,3.,4),v;k e=o(n(51,46,43),n(165,147,143),b*b),f=bP(c);v=1.-.5*(f.y-f.x)*i(.5,3.,f.z)*j(1.,.0,f.z);l e*v;}H(_21){h b=D(c,d(5),.9,3.,4),v=.18*(.7+b*b);k g;c=Y(c,13.,.007);bh(g,c,E(j(.3+b*.2,.9,D(p[m],d(23),.5,2.,4))));l k(v*(1.-g.y*g.z));}H(_46){h b=D(c,d(5),.9,3.,4);k e=o(n(77,55,53),n(62,48,48),V(c,d(128,13)))*(.7+b*b),g;c=Y(c,13.,.007);bh(g,c,E(j(.4+b*.4,.95,V(p[m],d(63,43)))));e*=j(1.3,.9,g.z);l k(e*(1.+g.y*g.z));}h bQ(d c,d s){c.y=M(c.y,0.);l bc(c,s);}k bn(k e,k b,d c,h R,h s){h y=(c.y-R)/s,p=1.-y*y;e*=1.-i(-1.,1.,y);X(p>0.)e=b*(p*(.8+.2*i(.5,.25,G(c.x/s))))*(.7+E(i(.2,.7,y)));l e;}k c0(d c,h A){l k(A*A*.4);}H(_47){c.x*=.5;h b=D(c*d(2,1),d(3,5),.9,3.,4),A=.75+b*b,t=c.y+.2*O(.4,i(.5,.33,G(c.x*4.))),c1=bQ(c-d(.25,.62),d(3,2)/32.),bf=bQ(c-d(.25,.55),d(3,2)/48.),r;k e=o(n(66,50,51),n(111,66,44),a8(i(.31,.01,c.y))),ao;d p=c,q;p.x=G(p.x*4.);X(c.y>.3)e=bd(e,d(4.*C(p.x-.5)-1.6,G(c.y*16.)-.5),.07);r=C(p.x-.5);e*=1.-.3*j(.31,.32,c.y)*j(.87,.86,c.y)*(j(.035,.03,.5-r)+i(.48,.01,r)-i(.46,.02,r));e=o(e*A,c0(c,b),M(j(.31,.3,c.y),J(bf)));e*=j(1.5,.7,c.y);X(c.y<.306)e*=1.-i(.3,.05,c.y)*J(-bf+10.,20.);e*=1.-i(.316,.004,c.y)*J(-bf);X(c.y<.1)e*=.0;q=c;q.y+=i(.1,.01,mod(q.x,.33))/2e2;e=bn(e,2.*b*n(93,84,79),c,.185,.015);e=bn(e,2.*b*n(138,77,48),c,.13,.025);e=bn(e,2.*b*n(112,71,51),c,.09,.015);e=bn(e,2.*b*n(138,77,48),q,.05,.015);p.x=C(G(c.x*6.-.5)-.5)/6.;e*=1.+.5*j(.04,.03,p.x)*i(.18,.03,p.y);r=aZ(p-d(0,.12),d(.03,.01));r=aC(r,aZ(p-d(0,.11),d(.01)));e*=1.-E(i(.0,.04,r));e=o(e,n(166,99,77)*2.*b*(.75+.5*E(i(.125,.01,c.y))),J(r));q=p;q.y-=.07;r=Z(q,.03);e*=1.-E(i(.0,.07,r));e=o(e,n(127,83,72)*b*2.*j(.01,-.005,r),j(.005,.0,r));q.y-=.004;r=Z(q,.015);e*=E(j(-.01,.01,r));q.y+=.013;r=Z(q,.05);e+=n(67,38,30)*4.*a8(b)*E(i(-.02,.015,r)*i(.023,.02,c.y));r=aC(c1,bf);r=aC(r,(c.y-.3)*3e2);e*=1.-.5*i(-2.,17.,bf)*j(.26,.3,c.y);ao=n(67,39,17)*A;ao=o(ao,k(A*.2),i(0.,4.,r)*b);ao*=1.-.4*pow(i(.0,3.,r),4.);ao+=(ao+.6)*a8(b)*E(i(-6.,8.,r)*i(.66,.04,c.y))*J(r);X(c.y<.56)ao=bd(ao,d(24.*C(c.x-.25)-1.85,G(c.y*24.+.5)-.5),.15);e=o(e,_46(c),j(.85,.9,t)+step(c.y,1./256.));e*=1.+i(.88,.015,t)-E(i(.87,.03,t));l o(e,ao,j(1.,.1,r));}k c2(k e,k L,d c,W w,W R){h b=D(c,d(w,R),.5,2.,2);e*=.9-.3*j(.15,.1,C(b-.5));l o(e,L,i(.5,.1,b));}H(_52){c=Y(c,9.,.005);W m=0,v[]=W[](13,43,17,47,23,59,27,63);h b=D(c,d(19),.7,2.,4);k e=o(n(40,50,60),n(46,33,27),b)*(.5+b);for(;m<8;m+=2)e=c2(e,o(n(145,140,137),n(132,123,116),b),c,v[m],v[m+1]);l e;}k c3(k e,k L,d c,W w,W R){h b=D(c,d(w,R),.5,2.,2);e*=1.-.15*E(j(.15,.1,C(b-.5)));l o(e,L,i(.5,.1,b));}H(_59){h b=D(c,d(13),.9,3.,4),A=D(c,d(7),.9,3.,4);k e=o(n(111,66,55),n(80,55,52),E(j(.8,.2,A)))*(.8+.8*b*b),L=e;c=Y(c,13.,.01);W m=0,v[]=W[](13,43,17,47,23,59,27,63);b=D(c,d(19),.7,2.,4);for(;m<6;m+=2)e=c3(e,L,c,v[m],v[m+1]);l e;}k c4(k e,k L,d c,W w,W R){h b=D(c,d(w,R),.5,2.,1);e*=.9-.3*E(j(.15,.1,C(b-.5)));l o(e,L,i(.5,.1,b));}H(_41){h b=D(c,d(3,29),.9,2.,4),t=.8+.8*b*b,f=C(c.y-.61),ad=j(.25,.24,f),B;k e=n(140,127,127),L=e;d p=c;e*=1.-.1*j(.85,.86,c.y);e=t*o(e,n(110,55,50),j(.33,.32,c.y));p.y+=p.x*.11+b*.007;p.y=G(p.y*9.)-.5;B=j(.0,.1,C(p.y)-.2);W m=0,v[]=W[](3,29,5,37,9,63,27,63);for(;m<6;m+=2)e=o(e,c4(e,L,c,v[m],v[m+1]),B*ad);e*=1.+t*ad*(+.6*i(.1,.1,p.y)-.7*i(-.25,.3,p.y)-.5*i(.2,.1,p.y));e=o(e,n(99,66,51)*t,i(-.15,.1,p.y)*ad);e*=1.+i(.36,.005,c.y)+i(.34,.005,c.y)+i(.865,.005,c.y)+i(.89,.01,c.y)-.5*E(i(.245,.01,f))-.7*E(i(.35,.01,c.y))-.5*E(i(.325,.02,c.y))-.8*E(i(.875,.02,c.y))-.3*E(i(.9,.02,c.y));e*=.3+a8(a6(c.x));l e;}k bR(k e,d p,h s,h B){s=bm(p,d(s)).z/s*1e2;e*=1.+.5*B*j(.9,.2,s)-.5*B*i(2.5,.5,.3,s);l e;}k bS(k e,d c,h c5){h b=D(c,d(4,9),.9,3.,4),t=.8+.8*b*b,a,f,B,s,L,m,u,r,z;k aK=o(n(133,100,88),n(133,100,100),b)*t;d p,q;p=q=c;q.x=C(q.x);f=Z(p,.31);u=aM(q);B=j(.01,.0,f);e=o(e*j(.0,.05,f+c5),k(.13*t),B);e=bR(e,p,37.,j(.04,.02,C(f+.07)));a=u*22.;m=ah(a);s=a-m;L=j(.23,.22,C(u-.25))+aI(m)*j(.0,.1,q.y);f-=r=(f*.3+.005)*L;B=j(.0,.1,q.y)*J(C(f+.015)-.015);e=o(e,aK,i(-.005,.01,f));e=o(e,n(130,75,44)*t,i(-.02,.005,f)*j(.0,.1,q.y));e*=1.-.3*j(.025,.03,-f)-.5*j(.4,.5,C(s-.5))*B+.2*i(.5,.3,C(s-.5))*B-.5*i(-.015,.007,f)-.5*i(-.03,.007,f)-.5*i(-.1,.005,f+r)-.5*i(-.115,.005,f+r)-.5*i(-.125,.015,f+r)-.5*i(-.145,.005,f+r)+.9*i(-.11,.007,f+r)+.5*i(-.14,.005,f+r)-b*i(.225,.005,C(u-.25))*J(C(f+.015)-.015);a=u*72.;m=ah(a);s=a-m;L=step(.7,af(m))*step(q.y,.0)*j(.02,.0,C(f+.02));e=o(e,k(aK*.6),L*j(.4,.3,C(s-.5)));e*=1.-.7*L*i(.4,.1,C(s-.5));l e;}k b1(d c){h b=D(c,d(4,9),.9,3.,4),t=.8+.8*b*b,a,f,B,s,L,m,u,r,z;k aK=o(n(133,100,88),n(133,100,100),b)*t,e=k(.1*t);d p,q;p.x=c.x-.5;p.y=M(c.y-.2,0.)*1.89;u=atan(p.y,C(p.x))/aH;f=Z(p,.48);L=j(.3,.31,u);f*=1.-.2*j(.3,.31,u)-.1*j(.43,.44,u);a=u*(u>.44?2.:u>.3?63.:31.);e=o(e,aK,j(.03,.01,C(f)));B=J(C(f-.01)-.02);m=ah(a);s=a-m;X(u>.33&&u<.44)s=G(s+af(m)*.6-.3);e*=1.-.5*B*i(.307,.01,u)-t*B*i(.5,.1+L*.2,s)+b*B*i(.52,.2+L*.2,s);e*=1.-.9*i(-.015,.015,f)-.5*i(.0,.01,f)-.7*i(.03,.02,f)+i(.01,.015,f);q=p;q.y-=.5;q.x=C(q.x)+.6;f=Z(q,1.13);B=j(.03,.02,C(f))*j(.5,.6,q.y);e=o(e,aK*ac(1.-C(f-.015)/.03),B);e*=1.-.5*B*i(.005,.01,f)+.5*B*i(.017,.005,f);q.x=C(c.x-.5)-.35;q.y=c.y*9./4.-2.1;f=Z(q,.13)*10.;a=aM(q)*49.;m=ah(a);s=a-m;u=j(.85,.9,af(m));for(W aB=0;aB<2;++aB,f+=.3){e=o(e,aK*(b*.5+.2),j(.09,.03,C(f)));e*=1.+.7*E(i(.01,.05,f));}p.y=(c.y-.7)*9./4.;f=Z(p,.43);a=atan(p.y,C(p.x))/aH;a=M(a,-.48);r=a;m=ah(a*=23.);s=a-m;L=aI(m)*.2-.1*j(.0,.1,-f);u=j(.1,.2,C(s-.5)-L);f+=u*.007;r=(1.-u)*E(i(.5,.3,r))*j(.25,.05,C(s-.5));f-=.17*r;B=i(.04,.0,-.4,f);e=o(e,aK*(b*.4+.4),B);e=bR(e,p,31.,j(.1,.05,C(f+.15))*u);e*=o(1.,1.-i(.1,.2,.4,C(s-.5)-L),B*b);e*=1.-.7*E(i(.03,.03,f))-.7*E(i(.03,.03,f+.05))*u-.7*E(i(.0,.02,f+.05))*u-.3*j(.04,.06,-f)*u+.5*i(.02,.0,-.1,f)+i(.0,.01+.07*r,f)+i(.0,.01,f+.03)*u;p.y-=.05;e=bS(e,p,0.);l e;}H(_42){c.y=(c.y+8.)/9.;l b1(c);}H(_43){c.y=(c.y*4.+4.)/9.;l b1(c);}H(_44){X(c.y<.01)++c.y;c.y=c.y*4./9.;l b1(c);}H(_54){h b=D(c,d(5),.9,3.,4),t=bD(Y(c,4.,.01),d(7),.5,3.,5),A=V(Y(c,4.,.05),d(9)),aD,I;k pt=bI(c,4.,.1+A*t*.05),e;d f=bF(pt.x);aD=af(G(pt.yz));e=n(74,65,62)*(.8+.8*b*b);e+=i(.6,.3,A)*j(.3,.9,b*t)*.2;e*=1.-i(.5,.4,A)*j(.5,.7,t)*.1;e=o(e,n(86,74,78),i(.5,.1,b)*i(.7,.3,aD)*.7);e=o(e,n(105,90,70),i(.3,.1,t)*i(.3,.3,aD)*.3);I=i(.015,.005+.015*A,pt.x)+i(.4,.1,A*t)*.4;e*=1.-b*j(.015,.05,pt.x)*.7;e*=1.+I*b*(f.y-.5)*.7;e*=.9+.2*aD;e*=.9+.2*a6(V(c-pt.yx,d(5)));l e;}H(_45){k e=_54(c);c-=.5;e=bS(e,c*.9,.02);l e;}k aR(d c){c*=1.5;h b=D(Y(c,7.,.02),d(9),.7,3.,2),t=b,A=V(c,d(13)),a,s,f;k e=_54(c);d p;p.x=C(c.x-.75);p.y=M(c.y-.58,0.)*1.15;a=atan(p.x,p.y)/aH;s=G(a*7.+.5);f=Z(p,.45);f-=.06*j(.4,.33,c.y);f-=.05*j(.15,.07,C(s-.5))*step(.63,c.y);f=aC(f,c.y-.107);X(c.y<.6)f=aC(f,C(p.x-.493)-.113);f=aC(f,Z(p,.6)+.044*j(.48,.43,c.y));e=o(e,n(144,125,115)*t,J(f-.1,.005));e*=1.-.3*i(.12,.11,.1,f)+.5*i(.1,.005+.015*A*A,f);l e;}H(_32){l aR(d(5,0)/6.+c*d(1,4)/6.);}H(_33){l aR(d(1,0)/6.+c*d(4)/6.);}H(_34){l aR(c*d(1,4)/6.);}H(_35){l aR(d(5,4)/6.+c*d(1,2)/6.);}H(_36){l aR(d(1,4)/6.+c*d(4,2)/6.);}H(_37){l aR(d(0,4)/6.+c*d(1,2)/6.);}H(_55){h b=D(c,d(13,1),.7,2.,3);k e=_54(c)*.7;e*=1.-E(j(.4,1.,b));l e;}k bT(d c,d s){h b=D(c,d(5),.9,3.,4),A=D(c,d(31,3),.5,3.,3),t=.75+b*b;d p=c;k e=_54(c);X(c.y<.38)e=o(n(92,43,15),n(66,44,33),j(.1,.05,c.y))*t*(.5+.5*j(.0,.35,c.y));e+=b*s.y*E(i(.32,s.x*.015,c.y))+.3*b*i(.34,.05,c.y);e*=1.-i(.38,.005+b*b*.03,c.y)+3.*i(.15,.2,c.y)*(A-.5);l e;}H(_56){h b=D(c,d(5),.9,3.,4),t=.75+b*b,f,ad,m;d p=c;k e=bT(c,d(1));p.x=mod(p.x,1./7.)-.07;p.y-=.21;f=Z(d(.75*p.x,bv(p.y,.1)),.033);ad=J(f,.005);f=Z(d(.75*p.x,bv(p.y+.005,.09)),.033);m=J(f+.015);e=o(e,n(83,81,66)*t,(ad-m)*j(.1,.3,c.y));e*=1.-j(.17,.25,c.y)*m;e+=E(i(.0,.015,f))*i(.32,.03,c.y);e*=1.+3.*pow(i(-.01,.03,f),4.)*i(.09,.03,c.y);f=Z(d(.75*p.x,bv(p.y+.03,.1)),.033);e*=1.-J(f+.01,.02)*(1.-ad);X(c.y>.09&&c.y<.3)e=bd(e,d((C(p.x)-.035)*36.,G(c.y*36.)-.5),.1);l e;}d c6(d p){p.x=C(p.x);d q=p,u;q.y-=.5;h f=Z(q,.35),I,e;u=q/.35;q.y+=.25;q.x-=.15;f=O(f,Q(q,d(.09,.05))-.1);I=bc(q,d(.15,.1))/5e1;e=.1+dot(d(u.y,a8(ac(1.-aT(u)))),d(.3,.3));q.y+=.2;q.x=p.x;e=M(e,ac(.4-K(q)));e+=.15*i(.0,.1,I)-.1*J(I+.12,.15);f=O(f,Q(q,d(.15-j(-.15,.15,q.y)*.07,.03))-.09);e*=1.-j(.05,.25,q.x)*j(.2,.1,C(q.y+.12));q.y-=.06;e-=.5*J(bc(q,d(.05-j(-.1,.1,q.y)*.03,.06))/1e3+.03,.05);l d(ac(e),J(f,.02));}H(_57){h b=D(c,d(9),.7,2.,4),t=.75+b"
"*b;k e=bT(c,d(4,.3));d p=c,s;p.x=mod(p.x,.2);p-=.1;s=c6(p*5.);l o(e,o(k(.5,.4,.3),k(.95,.8,.55),t)*t*s.x,s.y);}H(_53){d p=bB(c,d(8)),q=G(p),u=Y(c,31.,.002),aD=p-q;h b=D(Y(c,5.,.02),d(7),1.,3.,4),t=.8+.8*b*b,A=V(c+b8(af(aD)*64.),d(23)),B=D(c,d(9),.7,3.,4),f=D(u,d(63),.7,3.,4),r=D(u-d(0,.002),d(63),.7,3.,4),v=f-r,I=bC(q,.03+.03*a6(A)).z,R=af(aD);k e=o(n(91,61,42),n(70,30,15),I*b);e=o(e,n(70,48,35),j(.5,.6,B))*t;e*=1.+v*(.1+A+i(.6,.1,B))*(1.-I)-t*j(.7,1.,I)*V(c,d(13))+.5*b*i(.3,.3,I);f=D(u,d(23),.5,2.,4);e*=1.-.2*j(.6,.7,f)*R+.3*i(.6,.05,f)*R*A;e*=.9+.2*R*(1.-I);e*=.9+.4*pow(bD(c-af(aD/8.),d(5),.6,2.,4),4.);l e;}an(_28){h b=D(c-=.5,d(5),.9,3.,4),t=.75+b*b,A=V(Y(c,7.,.02),d(17)),r=K(c),L=r>.4?38.:r>.32?28.:16.,a=G(atan(c.y,c.x)/b7),m=ah(a*L),a5=C(C(r-.41-A*.002)*1e2-6.),B=j(1.5,1.4,a5),b2[]=h[](1.,3.,-.145,-1.,2.,.166),f,bU,s;d p=c;k e=n(78,68,63);e*=1.+.5*E(i(.49,.005+.015*A*A+.015*b,r));e=o(e,n(83,52,47)*(.6+.4*A*A),B)*t;e*=1.-.5*i(1.5,.5,a5)+b*i(1.,.5+.5*A,C(r-.418)*1e2-5.)-b*i(.5,.08,G(a*L+.5))*B+b*i(.5,.1,G(a*L+.55))*B;B=j(.34,.33,r);e=o(e*(1.-.5*B),n(83,52,47)*t,A*b*B);e=o(e,n(112,86,31)*t,B*E(i(.1,.15,.45,b)));e=o(e,n(77,66,77)*t,B*j(.5,.8,b)*.5);e*=1.-.7*i(.27,.34,.35,r);a5=r+A*.004;B=r>.21&&r<.31?1.:0.;e*=1.-i(.325,.005,a5)-i(.31,.005,a5)-b*E(i(.29,.005,a5))-b*E(i(.23,.01,a5))-.5*E(i(.21,.02,a5))+E(i(.3,.01,a5))*b+E(i(.22,.01,a5))*b-b*i(.5,.07,G(a*L+.5))*B;X(r<.23)m+=37.;X(r<.31)m+=73.;X(r<.31)m+=91.;e*=o(1.,.9+.2*aI(m),B);B=j(.01,.0,C(r-.411)-.039);m=ah(a*72.);p*=au(m*5.);s=0.;f=1e6;W aB=0;for(;aB<6;aB+=3){f=b9(f,bU=dot(p,aE(d(b2[aB],b2[aB+1])))+b2[aB+2]);s+=s+h(bU>0.);}X(s==3.)++m;else m+=66.*s;m=aI(m);e=o(e,t*n(90,80,75),B);e=o(e,t*n(127,111,88),m*b*B);e*=o(1.,.7+.6*af(m),B);e*=1.-B*E(i(.0,.006,f))*b+B*E(i(.006,.006,C(f)))*b*.5;m=ah(a*4.);p=C(c*au(m*90.+45.));f=1e6;for(aB=0;aB<2;++aB,p=C(p*au(45.)))f=b9(f,C(K(p-d(0,.12))-.16));B=j(.21,.2,r);a5=bu(bu(f,.012),.001);e*=1.-j(.21,.2,r)*J(.012-f)+b*B*E(i(.005,.005,f))-.5*B*E(J(a5-.001,.001));l F(e,(1.-j(.21,.15,r)*J(.028-f,.02))*j(.07,.087,r));}an(_29){d p=G(S)-.5;h b=D(au(U.x*333.)*p/(.8+.2*sin(U.x*61.)),d(53),.7,2.,4);F e=F(1.-b*k(0,.3,1),1),ao=aj(ai,(au(U.x*30.)*p/(.8+.2*sin(U.x*1.26)))+.5);e.ae=o(e.ae,ao.ae,ao.w);ao=aj(ai,S);e.ae=o(e.ae,ao.ae,ao.w)*a3();l e;}h c7(d p){p=cj(p,8.);h f=Z(p,.41);f=O(f,Q((p-d(.34,0))*au(45.),d(.1)));f=M(f,p.x-.45);l f;}H(_30){d p=c-.5,q;h b=D(c,d(9),.7,2.,4),t=.8+.8*b*b,r=Z(p,.41),f=c7(p),v=dFdy(f)/.004,B=J(f+.01,.007),a=aM(p),I,A,x,z;k e=_54(c),L=n(155,135,115)*t;e*=1.-(.5*-v+.5)*j(.03,.0,f);e=o(e,L,B);I=K(p)*9.;q.x=a*ah(I+1.)*3.;q.y=G(I);A=i(.5,.2,V(Y(c,7.,.03),d(41)));A=D(q,d(3,9),.5,2.,4)*i(.5,.5+.5*A,q.y);e=o(e,n(100,85,80)*a6(A)*b,J(r+.15,.02));q=p;q.x=C(p.x);I=M(M(f,cr(q,-d(.08,.4))),C(r+.06)-.09);z=Q(q-d(0,.3),d(.01,.03));I=aC(I,z-.02);x=J(-I,.01);e*=1.+v*B*j(.02,.0,-f)*j(.01,.0,-r)+x*i(.035,.015,-r)+.5*x*i(.13,.01,-r)+.7*i(.08,.007,z)*(1.-x)-.7*x*E(i(.01,.04,-r))-.6*x*E(i(.13,.06,.03,-r))-.5*x*E(i(.12,.02,-r))*B-.9*E(i(.12,.15,.2,-r))*B-.5*E(i(.0,.05,I));e+=k(.8,.8,1)*pow(ac(1.-K(c-d(.41,.59))/.35),8.);q.x=a;q.y=j(.05,.12,-r);I=bm(q,d(37,1)).z;B=i(.085,.035,-r)*x;e*=1.+.5*B*i(.0,.2,I)-.3*B*i(.3,.3,I);I=Q(p+d(0,.33),d(.01,.03))-.03;v=dFdy(I)/.004;e=o(e,L*(.4+.8*j(.25,.41,-p.y)),z=J(I,.01));e*=1.+.7*i(.005,.01,I)*v-.5*i(.0,.01,.05,I);I=Q(q=p+d(0,.35),d(.01,.015))-.01;v=dFdy(I)/.004;e*=1.+.5*i(.005,.01,I)-.5*E(i(.0,.01,I));e+=k(1,.7,.5)*pow(ac(1.-K(q)/.11),8.);l e;}P _31(){k e=aj(ai,S).ae*a3();d c=G(S)-.5,p=c;h t=mod(U.x*2.,7.),m=ah(t),f=1e6;X(m==0.)f=ct(p*2.4+d(0,.05));X(m==1.){p.x=ck(p.x,.1,-1.,1.);f=Q(p,d(.02,.15))*2.;}X(m==2.){f=O(f,Q(bk(p,.0)+d(.13-j(-.3,.3,c.y)*.17,0),d(.02,.15)));f=O(f,Q(p+d(0,.07),d(.07,.02)))*2.;}X(m==4.)f=be(p*1.8,.5);else f=bu(f,.005);N=F(e+J(f,.02)*G(-t)*k(.5,.05,.05),1);}H(_58){h b=D(c,d(13),.9,3.,4),A=D(c,d(7),.9,3.,4);k e=o(n(60,50,50),n(87,47,37),E(j(.7,.25,A)))*(.7+.8*b*b),g;c=Y(c,31.,.003);bh(g,c,a8(j(.0,.9,V(p[m],d(93)))));e*=1.-(g.y+.4)*E(b*g.z)*g.z;l e;}H(_62){h b=D(c,d(5),.9,3.,4);k e=n(67,64,63)*(.6+.5*b),g;c=Y(c,31.,.003);bh(g,c,a8(V(p[m],d(53,93))));e*=1.-.3*g.y*g.z*g.z;l e;}an(_22){c=Y(c,7.,.01);h b=D(c,d(9),.7,2.,4),A=D(c,d(13),.5,2.,4),B=j(.6,.9,D(Y(c,5.,.03),d(11),.6,2.,4));k e=n(127,70,55)*(.85+.3*b);e*=1.-.2*E(j(.3,.0,b*b))-.2*B-.3*j(.6,.77,A)+.3*j(.5,.9,b);e+=.5*E(j(.5,1.,D(c,d(17),1.,2.,3)));l F(e,1.-B);}P _24(){F e=aj(ai,S);d c=G(S);c.y-=.2*U.x;h b=D(Y(c,7.,.02),d(5),.9,2.,4);N=F(o(n(25,10,8)*b,e.ae,e.w)*a3(),1);}H(_23){d p=c-.5;p=Y(p,17.,.007);p.x*=2.-c.y*1.5;h b=D(c,d(9),.7,2.,4),A=V(c,d(7)),f=K(p),s;k e=_22(c).ae;s=G(f*=13.);X(f<=6.){e*=1.-pow(j(6.,.5,f+b*b),6.);A=j(.3,.7,A);e*=1.-A*b*i(.4,.2,s)+A*b*i(.6,.4,s);}l e;}H(_25){c=Y(c,13.,.003);h b=D(c,d(7),.9,3.,4),A=D(c,d(5),.5,2.,4),t=.5+b;k e=n(80,38,34),u=b0(c,d(23));e=o(e,o(n(180,125,118),n(165,78,51),A),b*i(.0,.4+A*.4,u.z))*t;l e;}H(_26){h b=D(c,d(7),.9,3.,4),t=.8+.4*b,m,r=.7,v,B;k e,u=b0(c,d(23));d p=u.xy/r,q=c+u.xy/23.;m=af(G(q)*3.3);e=o(n(155,55,55),n(200,166,155),j(.75,.45,q.y))*t;B=i(.5,.5,K(p));v=dot(d(-p.y,a8(ac(1.-aT(p)))),d(.6+m*.3,.3));e*=1.-b*.8*j(.5,.1,u.z)+b*B*v;e*=t*t*t*t;l e;}H(_27){h b=D(c,d(13),.9,3.,4),t=.4+b*b,A=a6(V(Y(c,12.,.02),d(48))),m,r,v,B;k e=n(60,50,46)*t,u=b0(c,d(17));m=af(G(c+u.xy/17.));r=.4+.3*m;d p=u.xy/r;B=O(j(1.1,1.,K(p)),j(.0,.15,u.z));v=dot(d(-p.y,a8(ac(1.-aT(p)))),d(.1+m*.2,.3));e+=b*B*v*A;A=a6(D(Y(c,13.,.01),d(23,43),.5,2.,3));e*=1.+(1.-B)*i(.4,.4,A);l e;}H(_60){h b=D(c,d(7),.9,3.,4),A=D(c,d(3),.5,3.,4);k e=o(n(103,56,53),n(73,58,71),j(.1,.7,A))*(.75+b*b);l e;}H(_61){h b=D(c,d(13),.9,3.,4),A=V(Y(c,5.,.05),d(9)),f=a6(G(c.x*4.)),B=j(.1,.15,f)*j(1.,.99,c.y);k e=n(51,44,44);e=o(e,n(73,55,52),j(.2,.2,b)*A*B);e=o(e,n(69,60,66),j(.7,.1,b)*b*B);e=o(e,n(99,77,77),j(.1,.5,A)*A*B*b*b*.3);e*=.6+.3*b+.3*b*b;e*=1.+.9*E(i(.21,.02+.1*A,f+b*.05))*B*b;e*=1.-E(j(.49,.5,C(c.y-.5)));e*=1.-j(.05,.2,f)*j(.16,.1,f);e*=1.+i(.99,.007,c.y);l bd(e,d(f-.4,G(c.y*8.)-.5),.07);}k bV(d c,h s){h b=D(c,d(3,1.+s+s),.7,2.,4),f=a6(c.x),B;c.y*=2.;k e=o(n(71,60,58),n(110,88,77),j(.1,.05,f))*(.7+.6*b);e*=1.-j(.05,.0,c.x)*(1.-b*b);e*=1.+.5*i(.05,.02,c.x);d p=d(f-.35,G(c.y*s)-.5);F L=bG(p,.11);B=J(L.w);e*=1.-.7*bH(p,1.1)*(1.-B);e=o(e,(L.y>.0?n(128,105,88):n(200,111,66)*j(-.2,.7,L.z))*(.4+2.*b*pow(ac(aL(L.yz*.7)),4.))*(1.-.6*i(-.1,.4,L.y)),B);l e;}H(_38){l bV(c,4.);}H(_39){l _38(c.yx);}H(_40){l bV(c,1.);}H(_48){h b=D(c,d(40,5),.9,3.,4);k e=n(110,110,98)*(.8+.8*b*b);X(c.y<1./4.)e*=.5;e*=1.-.4*j(.4,.0,b)+.5*j(.02,.0,c.y)+.2*i(.24,.01,c.y);l e;}H(_49){d p=c,q;p.y*=22.;q=G(p);h b=D(c,d(3,23),1.,2.,6),A=D(c,d(3,33),.7,3.,4),aD=af(p.y-q.y);k e=n(92,67,53)*(.8+.8*b*b);e*=1.-E(j(.1,.0,O(q.y,1.-q.y)))*b;e*=1.-.2*smoothstep(.3,.7,A);e*=.8+.3*b*aD;l e;}H(_50){h b=D(c,d(13),.9,3.,4),x=c.x*16./3.;k e=_49(c)*j(.15,.21,c.x);X(x<1.)e=n(59,48,49)*(.7+.6*b);e*=1.+.5*i(.05,.05,a6(x));l bd(e,d(C(c.x-3./32.)-.07,mod(c.y,.1)-.05),.004);}H(_51){d p=bB(c,d(6,4)),q=G(p),a4=q;h b=D(Y(c-=.5,5.,.03),d(13),.9,2.,3),A=V(c,d(73,7)),t=(.75+b*b)*(.8+.4*cf(c.x*93.)),r;k e=k(.25*t);a4.y+=a4.y*2.-.01-.03*A;r=K(a4-=clamp(a4,d(.49,.5),d(.51,3)));e*=1.-.7*b*E(j(.07,.03,C(r-.5)))+.5*b*i(.35,.1,r)*E(j(.2,.1,q.y))-.3*E(j(.8,1.,q.y))-.3*(j(.3,.1,q.y))*j(.4,.6,r)+.2*E(j(.5,.1,q.y))*j(.45,.4,r);l e;}H(_64){h b=D(c,d(5),.9,3.,4),R=G(c.y*10.);k e=o(n(53,48,42),n(38,38,36),b)*(.6+b*.8);e*=1.+.5*E(i(.25,.25,R))+.5*E(i(.65,.35,R))-.6*E(i(.5,.5,R));l e;}H(_63){h b=D(c,d(7,3),.9,3.,4),R=c.y+b*.04;k e=o(n(50,40,34),n(93,92,88),b*b);e*=1.+.9*j(.07,.04,C(c.y-.44))-.4*j(.08,.05,C(c.y-.7))-.5*i(.34,.05,c.y)-.3*i(.7,.04,c.y)+.7*i(.01,.03,c.y)+1.5*i(.89,.1,R)+1.3*i(.5,.04,R)+.9*i(.18,.04,R);l e;}H(_65){h b=D(c,d(5,3),.9,3.,4);k e=o(n(74,66,55),n(99,90,78),b*b);c.x*=2.;d p=bl(c,d(.5,.625),d(1.5,.625));h f=K(p-c),B=j(.22,.20,f),v=1.-.15*B-.5*j(.7,.9,c.y)*B-.3*(bF(f).y-.5)*i(.2,.03,f)-.3*E(i(.17,.03,f))-.5*j(.05,0.,c.y)-.3*i(.33,.05,c.y)+.7*j(.93,1.,c.y)+.7*i(.31,.04,c.y);l e*v;}an(_66){h b=D(c,d(1,5),.4,3.,4);k e=o(n(56,49,43),n(142,136,136),b);c=.5-C(c-.5);c.y*=4.;h a=i(.0,.1,K(c-bl(c,d(.41,.5),d(.42,3.5)))),f=bj(c),v=1.-.7*M(0.,1.-f/.15);v*=1.-.8*j(.24,.31,O(f,c.y-.1));e+=n(80,80,20)*a;l F(e*o(v,2.7,a),a);}an(_69){h b=D(c,d(1,5),.4,3.,4);k e=o(n(56,49,43),n(142,136,136),b);c=.5-C(c-.5);c.y*=8.;h f=K(c-bl(c,"
"d(.27,.3),d(.27,7.7))),a=i(.0,.17,f),v=1.-.5*i(.17,.07,f);e+=n(80,80,20)*a;l F(e*o(v,2.7,a),a);}an(_67){d p=C(c-.5);h b=D(c,d(1),.4,3.,4),r=K(p),a=j(.37,.33,r)*(.5+2.*b),v=1.+.0*j(.08,.03,C(r-.41));k e=o(n(56,49,43),n(142,136,136),b);v=o(v,7.,j(.44,.1*b,r));v*=1.-.5*E(i(.46,.04,r));v*=1.-.4*E(i(.36,.04,r));l F(e*v,a);}an(_68){h b=D(c,d(5),.9,3.,4),f=bE(bN(c-=.5,.35),C(Z(c,.4)),.02),a=pow(J(f-.02,.15),8.),ad=O(M(Q(c,d(.46)),-Z(c,.51)),C(Z(c,.44)));k e=n(76,62,47)*(.8+.8*b*b);e*=1.+(b+.5)*J(C(ad)-.01,.01);e*=1.-j(.1,.05,f)*J(Z(c,.4));l F(e+1.*k(1,1,.3)*a,a);}an(_1){c-=d(.48,.5);h f=be(c,0.),b=K(c)-.47;k e=1.-k(.5,1,1)*J(M(.007-f,b+.04));l F(e,1)*J(b);}H(_2){c-=.5;d r=d(dFdx(c.x),dFdy(c.y));c/=r/aF(r);c*=.8;c.y-=.03;h x=C(c.x),b=D(c,d(31,5),.7,2.,3),t=.8+.8*b*b,f=be(c,1.),I=be(c+d(0,.002),1.),v=(I-f)*5e2+.5;k e=k(.3*t,0,0)*J(f,.004);e*=1.-E(j(.0,.3,x))-.5*j(.1,.3,C(c.y-.1));e+=+t*.2*i(.0,.01-.01*x,f)*i(.1,.2,c.y)*j(.3,.2,x)*v+t*.5*j(.004,.0,f)*j(.07,.1,c.y)*i(.23,.1,x)*k(.9,.9,1)+t*.4*i(.005,.005,f)*j(.2,-.1,c.y)*j(.3,.2,x)*ac(-v);l e;}h bW(d c,h s,h m,h c8){m=(c.y-E(C(c.x-.5))*c8)*s-m;l 2.*i(.5,.4,m)*(G(m)-.5);}an(_78){h b=D(c,d(5,9),.9,3.,4),t=.8+.8*b*b,A=D(c,d(5,9),.9,3.,2),x=C(c.x-.5),f;k e=n(77,60,44)*t;d p=c*d(1,2)-d(.5,.7);e*=1.-.55*J(aC(Z(p,.3),be(au(45.)*p*.8,0.)-.01));e*=1.+E(j(.6,.9,b))+i(.2,.5,c.y)*i(.2,.3,x)*bW(c,4.,.2,4.);for(h T=6.;T<9.;++T)e*=1.+i(.8,.5,c.y)*i(.2,.3,x)*bW(c,12.,T,1.);f=c.y-.81-E(ac(x*4.))*.09;e=o(e,n(82,66,60)*t,j(.0,.01,f));e*=1.-.5*(i(.01,.02,f))+.5*(i(.02,.01,f));f=.15*(1.-c.y);f=Q(c-.5,d(.49)-f)+A*.1*a8(1.-c.y)-f;l F(e*E(J(f+.01,.05)),J(f));}an(_79){F e=aj(ai,c);X(e.w<.5)discard;e.ae*=E(a3()*.5);l e;}F aS(k p,h s){p*=s/h(textureSize(ai,0).x);k A=Nor*Nor;F e=F(0);for(W m=0;m<3;++m,p=p.yzx)e+=aj(ai,p.yz)*A[m];l e/aL(A);}F aS(h s){l aS(Pos,s);}P _80(){k A=Nor*Nor;N=aS(4.)*(Clr+.5*A.z*A.z*sign(Nor.z))*1.5;}P _81(){_80();}P _94(){N=aS(16.)*(aE(WNor).z*.5+.5)*F(1,.95,.9,1);N+=2.*pow(ac(aE(Nor).z),o(2.,8.,N.y))*E(N);}P _95(){N=F((aJ(Ref,15.)*1.4+.3)*U.yzw,1);}P _96(){k r=Ref;h s=17.;X(U.y<1.){s=9.;mat2 a=au(U.x*90.);r.xy*=a;r.yz*=a;}N=F(E(j(.1,.9,aJ(r,s)))*1.5*G(U.yzw),0);}P _97(){k A=aE(Nor),p=Pos;p.z-=24.;h I=i((D(Pos.y/48.+U.x*7.4,13.,.6,2.,4)-.5)*.15,.03,G(j(-8.,32.,Pos.z)-U.x*1.3)-.5);N=F((aS(16.).ae*(A.z*.5+.5)*(.2+.8*j(4.5,6.5,K(p)))+I*I)*U.yzw*2.,1);}h bX(h e,h s,h x){h a=M(fwidth(x)*2./s,1.);l i(e,s*a,x)/a;}P _82(){_80();k p=Pos,q;p.y=C(p.y);q=p-k(7,3.5,-21);q.z*=1.3;h r=K(q);N.ae*=1.+ac(p.z+30.)*j(4.,5.,aF(p.xy))+.5*ac(p.z+15.)+.3*i(3.,.5,r)+.7*bX(-15.,.5,p.z)-bX(-14.5,.5,p.z)-j(3.,2.5,r)*(.6+.4*q.z/r);q=p-k(8,0,-23);q.z*=.7;r=K(q);N.ae*=1.-j(2.,1.,r)*(.6+.4*q.z/r);}an(_83){c*=d(256,64);c.y+=2.;h f=Z(c-d(81,30),11.);f=M(f,c.x-80.);f=M(f,-Z(c-d(84,26),9.));f=O(f,Q(c-d(73,37),d(4,9))-4.);f=M(f,-Q(c-d(73,37),d(0,7))+1.);f=O(f,Q(c-d(91.5,47),d(4,19))-4.);f=M(f,-Q(c-d(91.5,47),d(0,17.5))+1.);f=O(f,Q(bk(c,111.)-d(105.+j(23.,50.,c.y)*3.,43),d(3.5,19)));f=O(f,Q(c-d(111,32),d(4,3)));f=O(f,Q(c-d(126,37),d(3,13)));f=O(f,Q(c-d(125.5+j(23.,50.,c.y)*10.,44),d(3.5,6)));f=O(f,Q(c-d(136.5-j(23.,50.,c.y)*9.,32),d(3.5,8)));f=O(f,Q(c-d(148.5,37),d(7,13)));f=M(f,-Q(c-d(155,33),d(6,3)));f=M(f,-Q(c-d(155,43),d(6,2)));f=O(f,Q(c-d(168,37),d(3.5,13)));f=O(f,Q(c-d(178.,37),d(3.5,13)));f=O(f,Q(c-d(188,37),d(3.5,13)));f=M(f,c.y-50.);l F(J(f,.8),0,0,af(c*511.));}P _84(){k e=aj(ai,S*2.).ae*step(.5,G(U.x*.5));e=o(e*a3(),k(.5,0,0),i(G(U.x*2.),1./64.,G(S.y)));N=F(e+aJ(Ref)*.25+aj(ai,S+af(U.xx)).w*.1,1);}P _85(){d c=G(S);c.x+=U.x/13.;h b=D(c,d(3,7),.9,2.,4),T=c.y;N=F(2.*n(95,85,80)*T*T*T*T*o(1.,b,.5),0.);}P _86(){N=F(aJ(Ref)*n(133,111,111),0);}h star(d p,d e,h s){p-=e;h a=j(.3,.5,C(G(aM(p/=s)*8.+af(e))-.5));l j(.9,.6,pow(aT(p),.0625)-a*a*.006);}P _87(){N=F(2,2,2,0)*pow(star(S,d(.5),1.),2.);}h bY(d c,h s){d p=c;p.y+=p.y-U.x*s;c.x+=(cm(p.y*5.)-.5)*1.5*E(c.y);h A=D(Y(p,7.,.02),d(9),.7,2.,4),R=j(.9,.03,c.y),b=Q(c-d(.5,.15),d(0,.3));l E(J(b+A*E(1.2-R)-.13,.15));}P bZ(h s){d c=G(S);N=(bY(c,s*.6)+bY(c,s))*F(2.5,1,.35,0);}P _88(){bZ(2.5);}P _89(){bZ(1.);}P _0(){h v=dot(Nor,aE(k(2,0,8)));v=v*.4+.7;d c=cv(Pos,cw(Nor));k e=k(.5);e*=cx(k(G(aV*U.w+.25),1.,1.));N=F(e*v,1);}P _6(){F e=aj(ai,S);N=F(e.ae*o(a3(),k(1),e.w),1);}P _71(){F e=aj(ai,S);h r=K(G(S)-.5);h s=o(.4,8.,G(U.x*1.5));N=F(e.ae*a3()+n(240,130,5)*i(.1,.05,r/s)*j(.37,.32,r),1);}P _5(){k e=aj(ai,S).ae;N=F(e*a3(),1);}P _7(){F e=aj(ai,S);e.ae*=1.+e.w*aJ(Ref);N=F(e.ae*a3(),1);}P _74(){N=F(0);}P _75(){k f=aE(Pos-Cam.ae);f.z=f.z*4.+2.;d c=aE(f).xy*2.;h b=j(.2,1.,D(c-U.x*d(.1,.2),d(5),.5,2.,6));c.y*=1.5;h s=j(.3,1.,D(c-U.x*d(.1,.18),d(5),.6,2.,6));N=F(k(b,0,0)+n(80,30,8)*s*s*2.,1);}P _76(){d c=Y(S/8.,U.x*.5,2.,.05);h b=D(c,d(7),.9,2.,4);k e=n(91,22,14)*(.2+1.6*b);e=o(e,n(144,44,0),i(.6,.2,D(c,d(3),.7,3.,4)));e=o(e,n(244,144,66)*b*2.,E(i(.55,.25,D(c,d(11),.5,2.,4))));N=F(e*ac(aF(a3())),1);}P _77(){_76();}P _3(){N=aj(ai,(.5+S*127.)/128.,2.5);N.ae*=.7+.3*V(S,.5/fwidth(S));}P _4(){N=aj(ai,S)*Clr;}P _90(){d c=d(aM(Pos.xy),j(8.,128.,Pos.z));c.x=G(c.x*3.-U.x*2.2);c.y*=4.;h T=star(c,d(0,.3),1.)+star(c,d(1,.3),1.)+star(c,d(.5,.4),1.5);T*=T;N=F(T,T,T,0);}an(_91){h b=D(Y(c,5.,.03),d(7),.5,3.,2);l(b*1.4+.3)*F(1.2,.54,.06,0)+.5*i(.7,.2,b)*F(1,1,1,0);}P _92(){k p=Pos;p-=U.x*6.4*sign(p.z-72.);N=aS(p,8.);}H(_93){c=Y(c,5.,.02);h b=D(c,d(6),.8,2.,4),B=D(c,d(2),.6,2.,4),t=.8+.3*b*b;k e=o(n(36,33,30),n(168,177,168),.3+.7*B)*t;l e;}"
;

// src/demo/data/shaders/vertex_shaders.glsl: 4148 => 2043 (49.3%)
static constexpr char g_vertex_shaders[] =
"#define c void\n"
"#define f vec4\n"
"#define k gl_Position\n"
"#define l location\n"
"#define n normalize\n"
"uniform mat4 MVP,View,World;uniform f Time,Cam;layout(l=0)in f e;layout(l=1)in f j;layout(l=2)in vec3 h;layout(l=3)in f o;out vec3 Pos,Nor,WNor,Ref;out vec2 UV,LUV;out f Clr;c d(){k=e;UV=e.xy*.5+.5;}c m(float v,float A,float B){k+=A*MVP*f(h,0)*sin(6.28*(Time.x*B+dot(e.xyz/v,vec3(1))));}c _0(){k=MVP*e;Pos=e.xyz;Nor=h;UV=j.xy;LUV=j.zw;Clr=o;Ref=n(reflect((e-Cam).xyz,h));}c i(){_0();Pos=floor(h)/4.;Nor=fract(h)*4.-2.;Ref=n(reflect((e-Cam).xyz,Nor));float a=j.z,u=cos(a),s=sin(a);Nor.xy*=mat2(u,s,-s,u);}c _1(){d();}c _2(){d();}c _3(){d();}c _4(){k=f(2.*e.x-1.,1.-2.*e.y,1,1);UV=j.xy;Clr=o;}c _5(){_0();}c _6(){_0();}c _7(){_0();}c _8(){d();}c _9(){d();}c _10(){d();}c _11(){d();}c _12(){d();}c _13(){_0();}c _14(){d();}c _15(){d();}c _16(){d();}c _17(){_0();}c _18(){d();}c _19(){d();}c _20(){d();}c _21(){d();}c _22(){d();}c _23(){d();}c _24(){_0();}c _25(){d();}c _26(){d();}c _27(){d();}c _28(){d();}c _29(){_0();}c _30(){d();}c _31(){_0();}c _32(){d();}c _33(){d();}c _34(){d();}c _35(){d();}c _36(){d();}c _37(){d();}c _38(){d();}c _39(){d();}c _40(){d();}c _41(){d();}c _42(){d();}c _43(){d();}c _44(){d();}c _45(){d();}c _46(){d();}c _47(){d();}c _48(){d();}c _49(){d();}c _50(){d();}c _51(){d();}c _52(){d();}c _53(){d();}c _54(){d();}c _55(){d();}c _56(){d();}c _57(){d();}c _58(){d();}c _59(){d();}c _60(){d();}c _61(){d();}c _62(){d();}c _63(){d();}c _64(){d();}c _65(){d();}c _66(){d();}c _67(){d();}c _68(){d();}c _69(){d();}c _70(){d();}c _71(){_0();}c _72(){d();}c _73(){_0();}c _74(){_0();}c _75(){_0();}c _76(){_0();m(100.,3.,.1);}c _77(){_0();}c _78(){d();}c _79(){_0();m(30.,3.,.2);m(100.,3.,.7);}c _80(){i();}c _81(){i();}c _82(){i();}c _83(){d();}c _84(){_0();}c _85(){_0();}c _86(){i();}c _87(){_0();k+=MVP*f(Nor*mat3(View),0);}c _88(){_0();}c _89(){_0();}c _90(){i();}c _91(){d();}c _92(){i();}c _93(){d();}c _94(){_0();WNor=n(mat3(World)*h);Ref=n(reflect((World*e-Cam).xyz,WNor));}c _95(){_94();}c _96(){_94();}c _97(){_94();}"
;
