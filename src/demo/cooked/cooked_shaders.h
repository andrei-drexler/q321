#pragma once

// auto-generated, do not modify
static_assert(0x325726dbU == Demo::Shader::Version, "Shader definition mismatch, please recompile the shader compiler & shaders");

// src/demo/data/shaders/fragment_shaders.glsl: 71094 => 29201 (41.1%)
static constexpr char g_fragment_shaders[] =
"#define d vec2\n"
"#define h float\n"
"#define k vec3\n"
"#define l return\n"
"#define o mix\n"
"#define B abs\n"
"#define F fract\n"
"#define G vec4\n"
"#define L length\n"
"#define M max\n"
"#define N min\n"
"#define Q UV\n"
"#define U if\n"
"#define V int\n"
"#define X xyz\n"
"#define ad void\n"
"#define ae Time\n"
"#define af floor\n"
"#define ai Texture0\n"
"#define am texture\n"
"#define a7 sqrt\n"
"#define aI normalize\n"
"#define b1(x)((x)*(x)*(3.-2.*(x)))\n"
"#define E(x)((x)*(x))\n"
"#define aV(x)dot(x,x)\n"
"#define Y(x)clamp(x,0.,1.)\n"
"#define u(r,g,b)(k(r,g,b)/255.)\n"
"#define a5(v)i(.5,.5,v)\n"
"#define H(a6)k a6(d);ad a6(){ac=G(a6(Q),1);}k a6(d c)\n"
"#define ao(a6)G a6(d);ad a6(){ac=a6(Q);}G a6(d c)\n"
"#define aW(bY,c,bZ){d p[3];h r[3];p[0]=c;p[1]=c+dFdx(c);p[2]=c+dFdy(c);for(V m=0;m<3;++m)r[m]=bZ;bY=k(ca(d(r[1],r[2])-r[0]),r[0]);}\n"
"#define b2(a6,cb)k a6(d p,d b3){p*=b3;d C=af(p),Z=p-C,b4,g,T,r;h aO=8.,aX=aO,f;for(V m=0;m<9;++m){g=d(m%3-1,m/3-1);T=cc(mod(C+g,b3));r=g+T-Z;f=cb;U(f<aO){aX=aO;aO=f;b4=r;}else U(f<aX){aX=f;}}l k(b4,aX-aO);}\n"
"uniform G ae,Cam;uniform sampler2D ai,Texture1;in k Pos,Nor,Ref;in d Q,LUV;in G Clr;out G ac;h aC=3.1415927,b5=2.*aC,aY=1.618034;d b6(h m){h bj=1.324718;l F(.5+m/d(bj,bj*bj));}h aD(h m){l F(.5+m*aY);}h cd(h Z){h m=af(Z);l o(aD(m),aD(m+1.),b1(Z-m));}h aZ(d v){l N(v.x,v.y);}h aZ(k v){l N(v.x,N(v.y,v.z));}h aZ(G v){l N(N(v.x,v.y),N(v.z,v.w));}h aE(d v){l M(v.x,v.y);}h aE(k v){l M(v.x,M(v.y,v.z));}h aE(G v){l M(M(v.x,v.y),M(v.z,v.w));}h bc(d v){l v.x+v.y;}h b7(h a,h b){l B(a)<B(b)?a:b;}h bk(h x,h s){l B(x)-s;}h bl(h x,h s){l sign(x)*M(0.,B(x)-s);}d ca(d v){h n=dot(v,v);l n>0.?v/a7(n):v;}h i(h ce,h cf,h x){l 1.-Y(B(x-ce)/cf);}h j(h b8,h cg,h x){l Y((x-b8)/(cg-b8));}h i(h a,h b,h e,h x){l N(j(a,b,x),j(e,b,x));}d aj(h x){l d(sin(x),cos(x));}mat2 a3(h x){d v=aj(radians(x));l mat2(v.y,v.x,-v.x,v.y);}h bd(d p){l F(atan(p.y,p.x)/b5);}d ch(d p,h C){l p*a3(360./C*(af(bd(p)*C+1.5)-1.));}d W(d c,h p,h s){l c+sin(c.yx*aC*p)*s;}d W(d c,h t,h p,h s){l c+sin(c.yx*aC*p+t)*s;}h be(h v,h A){l A-B(v-A);}d be(d v,h A){v.x=be(v.x,A);l v;}h ci(h x,h p,h n,h P){l x-p*clamp(af(x/p+.5),n,P);}d b9(d c,d s){c.x+=af(c.y*s.y)*(.5/s.x);l F(c)*s;}k bA(d c,h r){l k(c-=clamp(c,r,1.-r),L(c)/r);}k bA(d c,d s,h r){s=s.yx/aZ(s);c*=s;l k(c-=clamp(c,d(r),s-r),L(c)/r);}h ah(d p){k q=F(p.xyx*.09153);q+=dot(q,q.yzx+19.19);l F((q.x+q.y)*q.z);}h ah(h p){p=F(p*.1031);p*=p+33.33;p*=p+p;l F(p);}k c4(h p){k au=F(k(p)*k(.1031,.1030,.0973));au+=dot(au,au.yzx+33.33);l F((au.xxy+au.yzz)*au.zyx);}d cc(d p){k au=F(k(p.xyx)*k(.1031,.1030,.0973));au+=dot(au,au.yzx+33.33);l F((au.xx+au.yz)*au.zy);}G cj(h p){G av=F(G(p)*G(.1031,.1030,.0973,.1099));av+=dot(av,av.wzxy+33.33);l F((av.xxyz+av.yzzw)*av.zywx);}G cj(d p){G av=F(G(p.xyxy)*G(.1031,.1030,.0973,.1099));av+=dot(av,av.wzxy+33.33);l F((av.xxyz+av.yzzw)*av.zywx);}h ak(h x,h p){l ah(mod(x,p));}h c5(h x){h m;l o(ah(m=af(x)),ah(m+1.),b1(x-m));}h R(h x,h p){h m;l o(ak(m=af(x),p),ak(m+1.,p),x-m);}h ak(d p,d s){l ah(mod(p,s));}h R(d p,d s){p*=s;d m=af(p);p-=m;p*=p*(3.-2.*p);h ck=ak(m+d(0,0),s);h cl=ak(m+d(0,1),s);h cm=ak(m+d(1,1),s);h cn=ak(m+d(1,0),s);l o(o(ck,cn,p.x),o(cl,cm,p.x),p.y);}h D(d p,d aF,h bm,h bn,V bo){h aP=R(p,aF),aG=1.,tw=1.;for(V m=0;m<bo;++m){p=F(p+aY);aF*=bn;aG*=bm;aP+=R(p,aF)*aG;tw+=aG;}l aP/tw;}h bB(d p,d aF,h bm,h bn,V bo){h aP=a5(R(p,aF)),aG=1.,tw=1.;for(V m=0;m<bo;++m){p=F(p+aY);aF*=bn;aG*=bm;aP+=a5(R(p,aF))*aG;tw+=aG;}l aP/tw;}d bf(d p,d a,d b){d ab=b-a,ap=p-a;h t=Y(dot(ap,ab)/dot(ab,ab));l ab*t+a;}h co(d c,d f){l dot(c,a3(90.)*aI(f));}h O(d p,d b){d f=B(p)-b;l N(M(f.x,f.y),0.)+L(M(f,0.));}h aQ(d p,d b){l aE(B(p)-b);}h S(d p,h r){l L(p)-r;}h aR(d p,d r){l S(p/r,1.)/N(r.x,r.y);}h aA(h a,h b){l M(a,-b);}h bC(h a,h b,h K){h P=Y(.5+0.5*(b-a)/K);l o(b,a,P)-K*P*(1.-P);}d bD(h x){d f=d(dFdx(x),dFdy(x));l f/M(L(f),1e-8);}h J(h s,h f){l Y(1.-s/f);}h J(h s){l Y(1.-s/fwidth(s));}G bE(d c,h s){l G(c/=s,a7(Y(1.-aV(c))),L(c)-1.);}h cp(k C){h n=bc(C.yz)*.7;l pow(Y(n),4.)+n;}h bF(d c,h s){c/=s;c.y+=.06;c.x*=2.;l j(.3,.0,L(c));}k aS(k e,d c,h s){G b=bE(c,s);e*=1.+cp(b.X)*J(b.w)*.5;e*=1.-E(bF(c,20.*s))*(1.-J(b.w))*.3;l e;}h aT(d c,h t){c.x=B(c.x);c.y-=.07;h f=aR(c,d(.31,.12-t*.02))/50.;f=M(f,-aR(c-d(0,.01+.01*t),d(.28+t*.01,.07))/75.);f=M(f,-O(c-d(0,.1),d(.22-.02*t,.12)));f=M(f,-O(c-d(0,.1),d(.084-.012*t,.31)));f=N(f,aQ(c-d(0,-.09),d(i(-.09,.32,c.y)*(.04-.015*t),.32)));f=N(f,aQ(c-d(.11-.02*t,-.21+.01*t),d(i(-.07,.3,c.y)*(.03-.01*t),.15)));l f;}h cq(d c){h f=S(c-d(0,.17),.32);f=aA(f,S(c-d(0,.235),.27));f=aA(f,S(c-d(0,.5),.15));c.y-=.09;d s=d(.09,.52);h P=Y(-c.y/s.y);s*=.5;s.x*=j(1.05,.6,P)+E(j(.1,.02,P));c.y+=s.y;f=N(f,O(c,s));l f;}k bG(d p,h aj,h cr){k I=k(-1,0,1),r=k(1e5);d a0=af(p*aj),c6=I.yy;p-=(a0+.5)/aj;h al=.5*ak(a0+I.yy,d(aj)),aJ=.5*ak(a0+I.xy,d(aj)),aK=.5*ak(a0+I.yz,d(aj)),aL=.5*ak(a0+I.zy,d(aj)),aM=.5*ak(a0+I.yx,d(aj)),bH=.5*ak(a0+I.xz,d(aj)),bI=.5*ak(a0+I.zz,d(aj)),bJ=.5*ak(a0+I.zx,d(aj)),bK=.5*ak(a0+I.xx,d(aj));d[4]a8,n;U(mod(a0.x+a0.y,2.)<.5){n[0]=1.+d(aM-aJ,al-bK);n[1]=1.+d(aL-aM,al-bJ);n[2]=1.+d(aK-aJ,bH-al);n[3]=1.+d(aL-aK,bI-al);a8[0]=d(aM,al);a8[1]=d(aM,al);a8[2]=d(aK,al);a8[3]=d(aK,al);}else{n[0]=1.+d(al-bK,aJ-aM);n[1]=1.+d(bJ-al,aL-aM);n[2]=1.+d(al-bH,aK-aJ);n[3]=1.+d(bI-al,aK-aL);a8[0]=d(al,aJ);a8[1]=d(al,aL);a8[2]=d(al,aJ);a8[3]=d(al,aL);}for(V m=0;m<4;m++){a8[m]+=n[m]*(d(m&1,m/2)-.5);n[m]/=aj;h bx=aQ(p-a8[m]/aj,n[m]/2.-cr/aj);U(bx<r.x)r=k(bx,a0+a8[m]);}l r;}b2(bh,bc(B(r)))b2(bu,L(r))h bL(k p){p=aI(p);k a=mod(degrees(atan(p,p.yzx)),360.);l R(a.x/8.,45.)*j(.9,.0,B(p.z))+R(a.y/8.,45.)*j(.7,.0,B(p.x));}d bM(k p,V ax){l(ax==0)?p.yz:(ax==1)?p.xz:p.xy;}V bN(k C){C=B(C)+k(.01,.02,.03);h A=aE(C);l(A==C.x)?0:(A==C.y)?1:2;}k cs(k e){k rgb=Y(B(mod(e.x*6.+k(0,4,2),6.)-3.)-1.);rgb*=rgb*(3.-2.*rgb);l e.z*o(k(1.),rgb,e.y);}k a1(){k f=Cam.X-Pos;h b=D(f.xy/256.*a3(Cam.w),d(3),.7,3.,4),n=1.-j(14.,-6.,L(f.xy)-b*8.)*j(128.,48.,f.z)*step(.1,Nor.z);l am(Texture1,LUV).X*2.*n;}H(_8){h b=D(c,d(5),.9,3.,4);k e=o(u(48,41,33),u(103,101,104),b);l e;}H(_9){h b=D(c,d(3),.9,3.,4);k e=o(u(49,45,43),u(81,75,78),b*b);l e;}H(_10){h b=D(c,d(7),.9,3.,4);c.x*=-1.5;c.y+=c.x*.5;c.x=1.-c.x+c.y;c=F(c*28.);h Z=Y(1.-L(.1-c));Z*=j(.6,.2,L(.6-c));Z*=j(.6,.8,L(.1-c));Z*=j(.2,.6,b)*2.+1.;h n=1.-j(.2,b+2.,aE(B(c-.5)));l k((Z+1.)*o(.21,.29,b*b)*n);}H(_11){h b=D(c,d(7),.9,3.,4),r=L(c-.5);k e=_10(c);e=o(e,e*u(70,61,53),j(.5,.2,r+b*b*b));l e;}h bO(d c,h s){h f=1e6,m=0.;for(;m<5.;++m){d p=d(0,-s)*a3(m*72.);f=N(f,L(c-bf(c,p,p*a3(144.))));}l f;}ao(_12){k e=_11(c);c=F(c)-.5;h b=D(c,d(3),.9,3.,4),f=N(B(L(c)-.4),bO(c,.35));l G(e,J(f-.02+b*.02,.01));}ad _13(){G e=am(ai,Q);ac=G(e.X*a1()+u(111,55,0)*e.w*(sin(ae.x*aC)*.5+.5),1);}H(_70){h b=D(c,d(7),.9,3.,4);k e=_10(c);h r=L(c-.5);h A=j(.46,.45,r);h n=1.5-1.5*j(.0,.3,r*r);n=o(n,2.5,i(.42,.07,r));n=o(n,3.5,i(.44,.05,r));n=o(n,2.6,i(.36,.03,r));h C=.3+.2*j(.35,.30,r);n*=1.-C*j(.3,.7,b);n*=1.-.3*E(j(.13,.05,r));n=o(n,2.5,j(.04,.01,r));n-=n*i(.03,.01,r)*.7;e=o(e,u(68,66,54)*n,A);e*=1.-E(i(.34,.02,r));e*=1.-E(i(.46,.03,r));e*=1.-i(.41,.03,r)*.7;l e;}d ct(d c,h s){l d(1.-L(c)/s,J(L(c)-s));}ao(_72){h b=D(c,d(5),.9,3.,4),t,T,K,r;k e=_10(c);d a2,v;a2.x=B(c.x-.5);a2.y=N(c.y,.4);r=L(a2-d(0,.4))-(.18-.06*j(.4,1.,c.y));K=.25-.15*j(.9,.96,c.y)+.03*E(j(.82,.86,c.y))+.07*j(.8,.2,c.y)+.07*E(j(.35,.22,c.y))-.07*j(.22,.0,c.y);T=O(c-d(.5,.5),d(K,.46));T=M(T,-O(a2,d(.15,.03))+.06);e=o(e,k(.6,.55,.55)-c.y*.3+b*.2,J(T));e*=1.-.7*i(.0,.013,T);e*=1.-(r/.5-.1)*J(T);t=M(r,c.y-.96);T=B(t-.02)-.03;T=M(T,c.y-1.+a2.x*.5);T=M(T,c.y-.96);e=o(e,k(1,1,.9)-c.y*.55,i(-.01,.01,T));e=o(e,k(.2*b+.1),J(t,.01));e*=1.-.2*i(.0,.05,t)*J(T);v=ct(a2=c-d(.5,.4),.02);e*=1.+u(111,80,70)*i(.03,.01,L(a2));e*=1.-.5*i(.02,.01,L(a2));e=o(e,u(111,66,44)*(v.x*1.5+.2),v.y);l G(e,J(t-.03,.02));}ad _73(){G e=am(ai,Q);d c=F(Q);c.x=B(.5-c.x);h t=F(-ae.x),r=L(c-d(0,.4)),n=t*pow(M(0.,1.-r),4.)*e.w;U(t>.75)n+=j(.03,.01,B(F(c.y+c.x*.5+t*2.)-.45))*j(.1,.08,c.x);ac=G(e.X*a1()+u(180,150,5)*n,1);}H(_14){h b=D(c,d(5),.9,3.,4);k e=o(u(44,14,16),u(93,63,63),b*b);l e;}k bP(d c){h I=3e-3,a=0.;d g=d(6),r=bh(c,g).xy;for(V m=0;m<9;++m)a+=bc(B(bh(d(m%3-1,m/3-1)*I+c,g).xy-r));l k(c+r.xy/g,a);}H(_15){h b=D(c,d(3),.9,3.,4);k e=o(u(80,70,72),u(128,120,120),b*b);k v=bP(c);e*=o(.95,1.1,R(v.xy,d(6)));e=o(e,u(168,128,120),j(.5,1.,v.z)*b*.7);l e;}ao(_16){h b=D(c,d(3),.9,3.,4);k e=o(u(80,70,72),u(128,120,120),b*b);k v=bP(c);h A=j(.5,1.,v.z);h r=j(.4,.2,L(.5-F(v.xy)));e*=o(.95,1.1,R(v.xy,d(6)))-2.*r*b*b;e=o(e,u(168,128,120),A*b*.7);l G(e,A*r);}ad _17(){G e=am(ai,Q);ac=G(e.X*a1()+i(.5,.125,F(Q.y*.5+ae.x*.5))*e.w*.3,1);}H(_18){h b=D(c,d(5),.9,3.,4);k pt=bG(c,8.,.31);k e=o(u(66,58,55),u(118,107,105),b);h n=1.-.5*j(.034,.036,pt.x);n=o(n,1.4,i(.033,.004,pt.x));l e*n;}h cu(d c){h b=R(c,d(64)),Z=0.,f=1e6;for(;Z<11.;++Z)f=bC(f,B(L(.5-B(c-b6(Z)))-o(.36,.29,aD(Z+.7)))-o(.015,.03,b),.01);l f*1e2;}k bQ(d c){k s,p;for(V m=0;m<3;++m){p=k(c,0);p[m]+=1e-4;s[m]=cu(p.xy);}l k(aI(s.xy-s.z),s.z);}H(_19){h b=D(c,d(5),.9,3.,4),n;k e=o(u(51,46,43),u(165,147,143),b*b),f=bQ(c);n=1.-.5*(f.y-f.x)*i(.5,3.,f.z)*j(1.,.0,f.z);l e*n*.8;}H(_20){h b=D(c,d(3),1.1,3.,4),n;k e=o(u(51,46,43),u(165,147,143),b*b),f=bQ(c);n=1.-.5*(f.y-f.x)*i(.5,3.,f.z)*j(1.,.0,f.z);l e*n;}H(_21){h b=D(c,d(5),.9,3.,4),n=.18*(.7+b*b);k g;c=W(c,13.,.007);aW(g,c,E(j(.3+b*.2,.9,D(p[m],d("
"23),.5,2.,4))));l k(n*(1.-g.y*g.z));}H(_46){h b=D(c,d(5),.9,3.,4);k e=o(u(77,55,53),u(62,48,48),R(c,d(128,13)))*(.7+b*b),g;c=W(c,13.,.007);aW(g,c,E(j(.4+b*.4,.95,R(p[m],d(63,43)))));e*=j(1.3,.9,g.z);l k(e*(1.+g.y*g.z));}h bR(d c,d s){c.y=M(c.y,0.);l aR(c,s);}k bi(k e,k b,d c,h P,h s){h y=(c.y-P)/s,p=1.-y*y;e*=1.-i(-1.,1.,y);U(p>0.)e=b*(p*(.8+.2*i(.5,.25,F(c.x/s))))*(.7+E(i(.2,.7,y)));l e;}k cv(d c,h C){l k(C*C*.4);}H(_47){c.x*=.5;h b=D(c*d(2,1),d(3,5),.9,3.,4),C=.75+b*b,t=c.y+.2*N(.4,i(.5,.33,F(c.x*4.))),cw=bR(c-d(.25,.62),d(3,2)/32.),aU=bR(c-d(.25,.55),d(3,2)/48.),r;k e=o(u(66,50,51),u(111,66,44),a7(i(.31,.01,c.y))),an;d p=c,q;p.x=F(p.x*4.);U(c.y>.3)e=aS(e,d(4.*B(p.x-.5)-1.6,F(c.y*16.)-.5),.07);r=B(p.x-.5);e*=1.-.3*j(.31,.32,c.y)*j(.87,.86,c.y)*(j(.035,.03,.5-r)+i(.48,.01,r)-i(.46,.02,r));e=o(e*C,cv(c,b),M(j(.31,.3,c.y),J(aU)));e*=j(1.5,.7,c.y);U(c.y<.306)e*=1.-i(.3,.05,c.y)*J(-aU+10.,20.);e*=1.-i(.316,.004,c.y)*J(-aU);U(c.y<.1)e*=.0;q=c;q.y+=i(.1,.01,mod(q.x,.33))/2e2;e=bi(e,2.*b*u(93,84,79),c,.185,.015);e=bi(e,2.*b*u(138,77,48),c,.13,.025);e=bi(e,2.*b*u(112,71,51),c,.09,.015);e=bi(e,2.*b*u(138,77,48),q,.05,.015);p.x=B(F(c.x*6.-.5)-.5)/6.;e*=1.+.5*j(.04,.03,p.x)*i(.18,.03,p.y);r=aQ(p-d(0,.12),d(.03,.01));r=aA(r,aQ(p-d(0,.11),d(.01)));e*=1.-E(i(.0,.04,r));e=o(e,u(166,99,77)*2.*b*(.75+.5*E(i(.125,.01,c.y))),J(r));q=p;q.y-=.07;r=S(q,.03);e*=1.-E(i(.0,.07,r));e=o(e,u(127,83,72)*b*2.*j(.01,-.005,r),j(.005,.0,r));q.y-=.004;r=S(q,.015);e*=E(j(-.01,.01,r));q.y+=.013;r=S(q,.05);e+=u(67,38,30)*4.*a7(b)*E(i(-.02,.015,r)*i(.023,.02,c.y));r=aA(cw,aU);r=aA(r,(c.y-.3)*3e2);e*=1.-.5*i(-2.,17.,aU)*j(.26,.3,c.y);an=u(67,39,17)*C;an=o(an,k(C*.2),i(0.,4.,r)*b);an*=1.-.4*pow(i(.0,3.,r),4.);an+=(an+.6)*a7(b)*E(i(-6.,8.,r)*i(.66,.04,c.y))*J(r);U(c.y<.56)an=aS(an,d(24.*B(c.x-.25)-1.85,F(c.y*24.+.5)-.5),.15);e=o(e,_46(c),j(.85,.9,t)+step(c.y,1./256.));e*=1.+i(.88,.015,t)-E(i(.87,.03,t));l o(e,an,j(1.,.1,r));}k cx(k e,k K,d c,V w,V P){h b=D(c,d(w,P),.5,2.,2);e*=.9-.3*j(.15,.1,B(b-.5));l o(e,K,i(.5,.1,b));}H(_52){c=W(c,9.,.005);V m=0,n[]=V[](13,43,17,47,23,59,27,63);h b=D(c,d(19),.7,2.,4);k e=o(u(40,50,60),u(46,33,27),b)*(.5+b);for(;m<8;m+=2)e=cx(e,o(u(145,140,137),u(132,123,116),b),c,n[m],n[m+1]);l e;}k cy(k e,k K,d c,V w,V P){h b=D(c,d(w,P),.5,2.,2);e*=1.-.15*E(j(.15,.1,B(b-.5)));l o(e,K,i(.5,.1,b));}H(_59){h b=D(c,d(13),.9,3.,4),C=D(c,d(7),.9,3.,4);k e=o(u(111,66,55),u(80,55,52),E(j(.8,.2,C)))*(.8+.8*b*b),K=e;c=W(c,13.,.01);V m=0,n[]=V[](13,43,17,47,23,59,27,63);b=D(c,d(19),.7,2.,4);for(;m<6;m+=2)e=cy(e,K,c,n[m],n[m+1]);l e;}k cz(k e,k K,d c,V w,V P){h b=D(c,d(w,P),.5,2.,1);e*=.9-.3*E(j(.15,.1,B(b-.5)));l o(e,K,i(.5,.1,b));}H(_41){h b=D(c,d(3,29),.9,2.,4),t=.8+.8*b*b,f=B(c.y-.61),T=j(.25,.24,f),A;k e=u(140,127,127),K=e;d p=c;e*=1.-.1*j(.85,.86,c.y);e=t*o(e,u(110,55,50),j(.33,.32,c.y));p.y+=p.x*.11+b*.007;p.y=F(p.y*9.)-.5;A=j(.0,.1,B(p.y)-.2);V m=0,n[]=V[](3,29,5,37,9,63,27,63);for(;m<6;m+=2)e=o(e,cz(e,K,c,n[m],n[m+1]),A*T);e*=1.+t*T*(+.6*i(.1,.1,p.y)-.7*i(-.25,.3,p.y)-.5*i(.2,.1,p.y));e=o(e,u(99,66,51)*t,i(-.15,.1,p.y)*T);e*=1.+i(.36,.005,c.y)+i(.34,.005,c.y)+i(.865,.005,c.y)+i(.89,.01,c.y)-.5*E(i(.245,.01,f))-.7*E(i(.35,.01,c.y))-.5*E(i(.325,.02,c.y))-.8*E(i(.875,.02,c.y))-.3*E(i(.9,.02,c.y));e*=.3+a7(a5(c.x));l e;}k bS(k e,d p,h s,h A){s=bh(p,d(s)).z/s*1e2;e*=1.+.5*A*j(.9,.2,s)-.5*A*i(2.5,.5,.3,s);l e;}k bT(k e,d c,h c0){h b=D(c,d(4,9),.9,3.,4),t=.8+.8*b*b,a,f,A,s,K,m,v,r,z;k aH=o(u(133,100,88),u(133,100,100),b)*t;d p,q;p=q=c;q.x=B(q.x);f=S(p,.31);v=bd(q);A=j(.01,.0,f);e=o(e*j(.0,.05,f+c0),k(.13*t),A);e=bS(e,p,37.,j(.04,.02,B(f+.07)));a=v*22.;m=af(a);s=a-m;K=j(.23,.22,B(v-.25))+aD(m)*j(.0,.1,q.y);f-=r=(f*.3+.005)*K;A=j(.0,.1,q.y)*J(B(f+.015)-.015);e=o(e,aH,i(-.005,.01,f));e=o(e,u(130,75,44)*t,i(-.02,.005,f)*j(.0,.1,q.y));e*=1.-.3*j(.025,.03,-f)-.5*j(.4,.5,B(s-.5))*A+.2*i(.5,.3,B(s-.5))*A-.5*i(-.015,.007,f)-.5*i(-.03,.007,f)-.5*i(-.1,.005,f+r)-.5*i(-.115,.005,f+r)-.5*i(-.125,.015,f+r)-.5*i(-.145,.005,f+r)+.9*i(-.11,.007,f+r)+.5*i(-.14,.005,f+r)-b*i(.225,.005,B(v-.25))*J(B(f+.015)-.015);a=v*72.;m=af(a);s=a-m;K=step(.7,ah(m))*step(q.y,.0)*j(.02,.0,B(f+.02));e=o(e,k(aH*.6),K*j(.4,.3,B(s-.5)));e*=1.-.7*K*i(.4,.1,B(s-.5));l e;}k bv(d c){h b=D(c,d(4,9),.9,3.,4),t=.8+.8*b*b,a,f,A,s,K,m,v,r,z;k aH=o(u(133,100,88),u(133,100,100),b)*t,e=k(.1*t);d p,q;p.x=c.x-.5;p.y=M(c.y-.2,0.)*1.89;v=atan(p.y,B(p.x))/aC;f=S(p,.48);K=j(.3,.31,v);f*=1.-.2*j(.3,.31,v)-.1*j(.43,.44,v);a=v*(v>.44?2.:v>.3?63.:31.);e=o(e,aH,j(.03,.01,B(f)));A=J(B(f-.01)-.02);m=af(a);s=a-m;U(v>.33&&v<.44)s=F(s+ah(m)*.6-.3);e*=1.-.5*A*i(.307,.01,v)-t*A*i(.5,.1+K*.2,s)+b*A*i(.52,.2+K*.2,s);e*=1.-.9*i(-.015,.015,f)-.5*i(.0,.01,f)-.7*i(.03,.02,f)+i(.01,.015,f);q=p;q.y-=.5;q.x=B(q.x)+.6;f=S(q,1.13);A=j(.03,.02,B(f))*j(.5,.6,q.y);e=o(e,aH*Y(1.-B(f-.015)/.03),A);e*=1.-.5*A*i(.005,.01,f)+.5*A*i(.017,.005,f);q.x=B(c.x-.5)-.35;q.y=c.y*9./4.-2.1;f=S(q,.13)*10.;a=bd(q)*49.;m=af(a);s=a-m;v=j(.85,.9,ah(m));for(V a9=0;a9<2;++a9,f+=.3){e=o(e,aH*(b*.5+.2),j(.09,.03,B(f)));e*=1.+.7*E(i(.01,.05,f));}p.y=(c.y-.7)*9./4.;f=S(p,.43);a=atan(p.y,B(p.x))/aC;a=M(a,-.48);r=a;m=af(a*=23.);s=a-m;K=aD(m)*.2-.1*j(.0,.1,-f);v=j(.1,.2,B(s-.5)-K);f+=v*.007;r=(1.-v)*E(i(.5,.3,r))*j(.25,.05,B(s-.5));f-=.17*r;A=i(.04,.0,-.4,f);e=o(e,aH*(b*.4+.4),A);e=bS(e,p,31.,j(.1,.05,B(f+.15))*v);e*=o(1.,1.-i(.1,.2,.4,B(s-.5)-K),A*b);e*=1.-.7*E(i(.03,.03,f))-.7*E(i(.03,.03,f+.05))*v-.7*E(i(.0,.02,f+.05))*v-.3*j(.04,.06,-f)*v+.5*i(.02,.0,-.1,f)+i(.0,.01+.07*r,f)+i(.0,.01,f+.03)*v;p.y-=.05;e=bT(e,p,0.);l e;}H(_42){c.y=(c.y+8.)/9.;l bv(c);}H(_43){c.y=(c.y*4.+4.)/9.;l bv(c);}H(_44){U(c.y<.01)++c.y;c.y=c.y*4./9.;l bv(c);}H(_54){h b=D(c,d(5),.9,3.,4),t=bB(W(c,4.,.01),d(7),.5,3.,5),C=R(W(c,4.,.05),d(9)),aB,I;k pt=bG(c,4.,.1+C*t*.05),e;d f=bD(pt.x);aB=ah(F(pt.yz));e=u(74,65,62)*(.8+.8*b*b);e+=i(.6,.3,C)*j(.3,.9,b*t)*.2;e*=1.-i(.5,.4,C)*j(.5,.7,t)*.1;e=o(e,u(86,74,78),i(.5,.1,b)*i(.7,.3,aB)*.7);e=o(e,u(105,90,70),i(.3,.1,t)*i(.3,.3,aB)*.3);I=i(.015,.005+.015*C,pt.x)+i(.4,.1,C*t)*.4;e*=1.-b*j(.015,.05,pt.x)*.7;e*=1.+I*b*(f.y-.5)*.7;e*=.9+.2*aB;e*=.9+.2*a5(R(c-pt.yx,d(5)));l e;}H(_45){k e=_54(c);c-=.5;e=bT(e,c*.9,.02);l e;}k aN(d c){c*=1.5;h b=D(W(c,7.,.02),d(9),.7,3.,2),t=b,C=R(c,d(13)),a,s,f;k e=_54(c);d p;p.x=B(c.x-.75);p.y=M(c.y-.58,0.)*1.15;a=atan(p.x,p.y)/aC;s=F(a*7.+.5);f=S(p,.45);f-=.06*j(.4,.33,c.y);f-=.05*j(.15,.07,B(s-.5))*step(.63,c.y);f=aA(f,c.y-.107);U(c.y<.6)f=aA(f,B(p.x-.493)-.113);f=aA(f,S(p,.6)+.044*j(.48,.43,c.y));e=o(e,u(144,125,115)*t,J(f-.1,.005));e*=1.-.3*i(.12,.11,.1,f)+.5*i(.1,.005+.015*C*C,f);l e;}H(_32){l aN(d(5,0)/6.+c*d(1,4)/6.);}H(_33){l aN(d(1,0)/6.+c*d(4)/6.);}H(_34){l aN(c*d(1,4)/6.);}H(_35){l aN(d(5,4)/6.+c*d(1,2)/6.);}H(_36){l aN(d(1,4)/6.+c*d(4,2)/6.);}H(_37){l aN(d(0,4)/6.+c*d(1,2)/6.);}H(_55){h b=D(c,d(13,1),.7,2.,3);k e=_54(c)*.7;e*=1.-E(j(.4,1.,b));l e;}k bU(d c,d s){h b=D(c,d(5),.9,3.,4),C=D(c,d(31,3),.5,3.,3),t=.75+b*b;d p=c;k e=_54(c);U(c.y<.38)e=o(u(92,43,15),u(66,44,33),j(.1,.05,c.y))*t*(.5+.5*j(.0,.35,c.y));e+=b*s.y*E(i(.32,s.x*.015,c.y))+.3*b*i(.34,.05,c.y);e*=1.-i(.38,.005+b*b*.03,c.y)+3.*i(.15,.2,c.y)*(C-.5);l e;}H(_56){h b=D(c,d(5),.9,3.,4),t=.75+b*b,f,T,m;d p=c;k e=bU(c,d(1));p.x=mod(p.x,1./7.)-.07;p.y-=.21;f=S(d(.75*p.x,bl(p.y,.1)),.033);T=J(f,.005);f=S(d(.75*p.x,bl(p.y+.005,.09)),.033);m=J(f+.015);e=o(e,u(83,81,66)*t,(T-m)*j(.1,.3,c.y));e*=1.-j(.17,.25,c.y)*m;e+=E(i(.0,.015,f))*i(.32,.03,c.y);e*=1.+3.*pow(i(-.01,.03,f),4.)*i(.09,.03,c.y);f=S(d(.75*p.x,bl(p.y+.03,.1)),.033);e*=1.-J(f+.01,.02)*(1.-T);U(c.y>.09&&c.y<.3)e=aS(e,d((B(p.x)-.035)*36.,F(c.y*36.)-.5),.1);l e;}d c1(d p){p.x=B(p.x);d q=p,v;q.y-=.5;h f=S(q,.35),I,e;v=q/.35;q.y+=.25;q.x-=.15;f=N(f,O(q,d(.09,.05))-.1);I=aR(q,d(.15,.1))/5e1;e=.1+dot(d(v.y,a7(Y(1.-aV(v)))),d(.3,.3));q.y+=.2;q.x=p.x;e=M(e,Y(.4-L(q)));e+=.15*i(.0,.1,I)-.1*J(I+.12,.15);f=N(f,O(q,d(.15-j(-.15,.15,q.y)*.07,.03))-.09);e*=1.-j(.05,.25,q.x)*j(.2,.1,B(q.y+.12));q.y-=.06;e-=.5*J(aR(q,d(.05-j(-.1,.1,q.y)*.03,.06))/1e3+.03,.05);l d(Y(e),J(f,.02));}H(_57){h b=D(c,d(9),.7,2.,4),t=.75+b*b;k e=bU(c,d(4,.3));d p=c,s;p.x=mod(p.x,.2);p-=.1;s=c1(p*5.);l o(e,o(k(.5,.4,.3),k(.95,.8,.55),t)*t*s.x,s.y);}H(_53){d p=b9(c,d(8)),q=F(p),v=W(c,31.,.002),aB=p-q;h b=D(W(c,5.,.02),d(7),1.,3.,4),t=.8+.8*b*b,C=R(c+b6("
"ah(aB)*64.),d(23)),A=D(c,d(9),.7,3.,4),f=D(v,d(63),.7,3.,4),r=D(v-d(0,.002),d(63),.7,3.,4),n=f-r,I=bA(q,.03+.03*a5(C)).z,P=ah(aB);k e=o(u(91,61,42),u(70,30,15),I*b);e=o(e,u(70,48,35),j(.5,.6,A))*t;e*=1.+n*(.1+C+i(.6,.1,A))*(1.-I)-t*j(.7,1.,I)*R(c,d(13))+.5*b*i(.3,.3,I);f=D(v,d(23),.5,2.,4);e*=1.-.2*j(.6,.7,f)*P+.3*i(.6,.05,f)*P*C;e*=.9+.2*P*(1.-I);e*=.9+.4*pow(bB(c-ah(aB/8.),d(5),.6,2.,4),4.);l e;}ao(_28){h b=D(c-=.5,d(5),.9,3.,4),t=.75+b*b,C=R(W(c,7.,.02),d(17)),r=L(c),K=r>.4?38.:r>.32?28.:16.,a=F(atan(c.y,c.x)/b5),m=af(a*K),a4=B(B(r-.41-C*.002)*1e2-6.),A=j(1.5,1.4,a4),b0[]=h[](1.,3.,-.145,-1.,2.,.166),f,bV,s;d p=c;k e=u(78,68,63);e*=1.+.5*E(i(.49,.005+.015*C*C+.015*b,r));e=o(e,u(83,52,47)*(.6+.4*C*C),A)*t;e*=1.-.5*i(1.5,.5,a4)+b*i(1.,.5+.5*C,B(r-.418)*1e2-5.)-b*i(.5,.08,F(a*K+.5))*A+b*i(.5,.1,F(a*K+.55))*A;A=j(.34,.33,r);e=o(e*(1.-.5*A),u(83,52,47)*t,C*b*A);e=o(e,u(112,86,31)*t,A*E(i(.1,.15,.45,b)));e=o(e,u(77,66,77)*t,A*j(.5,.8,b)*.5);e*=1.-.7*i(.27,.34,.35,r);a4=r+C*.004;A=r>.21&&r<.31?1.:0.;e*=1.-i(.325,.005,a4)-i(.31,.005,a4)-b*E(i(.29,.005,a4))-b*E(i(.23,.01,a4))-.5*E(i(.21,.02,a4))+E(i(.3,.01,a4))*b+E(i(.22,.01,a4))*b-b*i(.5,.07,F(a*K+.5))*A;U(r<.23)m+=37.;U(r<.31)m+=73.;U(r<.31)m+=91.;e*=o(1.,.9+.2*aD(m),A);A=j(.01,.0,B(r-.411)-.039);m=af(a*72.);p*=a3(m*5.);s=0.;f=1e6;V a9=0;for(;a9<6;a9+=3){f=b7(f,bV=dot(p,aI(d(b0[a9],b0[a9+1])))+b0[a9+2]);s+=s+h(bV>0.);}U(s==3.)++m;else m+=66.*s;m=aD(m);e=o(e,t*u(90,80,75),A);e=o(e,t*u(127,111,88),m*b*A);e*=o(1.,.7+.6*ah(m),A);e*=1.-A*E(i(.0,.006,f))*b+A*E(i(.006,.006,B(f)))*b*.5;m=af(a*4.);p=B(c*a3(m*90.+45.));f=1e6;for(a9=0;a9<2;++a9,p=B(p*a3(45.)))f=b7(f,B(L(p-d(0,.12))-.16));A=j(.21,.2,r);a4=bk(bk(f,.012),.001);e*=1.-j(.21,.2,r)*J(.012-f)+b*A*E(i(.005,.005,f))-.5*A*E(J(a4-.001,.001));l G(e,(1.-j(.21,.15,r)*J(.028-f,.02))*j(.07,.087,r));}ao(_29){d p=F(Q)-.5;h b=D(a3(ae.x*333.)*p/(.8+.2*sin(ae.x*61.)),d(53),.7,2.,4);G e=G(1.-b*k(0,.3,1),1),an=am(ai,(a3(ae.x*30.)*p/(.8+.2*sin(ae.x*1.26)))+.5);e.X=o(e.X,an.X,an.w);an=am(ai,Q);e.X=o(e.X,an.X,an.w)*a1();l e;}h c2(d p){p=ch(p,8.);h f=S(p,.41);f=N(f,O((p-d(.34,0))*a3(45.),d(.1)));f=M(f,p.x-.45);l f;}H(_30){d p=c-.5,q;h b=D(c,d(9),.7,2.,4),t=.8+.8*b*b,r=S(p,.41),f=c2(p),n=dFdy(f)/.004,A=J(f+.01,.007),a=bd(p),I,C,x,z;k e=_54(c),K=u(155,135,115)*t;e*=1.-(.5*-n+.5)*j(.03,.0,f);e=o(e,K,A);I=L(p)*9.;q.x=a*af(I+1.)*3.;q.y=F(I);C=i(.5,.2,R(W(c,7.,.03),d(41)));C=D(q,d(3,9),.5,2.,4)*i(.5,.5+.5*C,q.y);e=o(e,u(100,85,80)*a5(C)*b,J(r+.15,.02));q=p;q.x=B(p.x);I=M(M(f,co(q,-d(.08,.4))),B(r+.06)-.09);z=O(q-d(0,.3),d(.01,.03));I=aA(I,z-.02);x=J(-I,.01);e*=1.+n*A*j(.02,.0,-f)*j(.01,.0,-r)+x*i(.035,.015,-r)+.5*x*i(.13,.01,-r)+.7*i(.08,.007,z)*(1.-x)-.7*x*E(i(.01,.04,-r))-.6*x*E(i(.13,.06,.03,-r))-.5*x*E(i(.12,.02,-r))*A-.9*E(i(.12,.15,.2,-r))*A-.5*E(i(.0,.05,I));e+=k(.8,.8,1)*pow(Y(1.-L(c-d(.41,.59))/.35),8.);q.x=a;q.y=j(.05,.12,-r);I=bh(q,d(37,1)).z;A=i(.085,.035,-r)*x;e*=1.+.5*A*i(.0,.2,I)-.3*A*i(.3,.3,I);I=O(p+d(0,.33),d(.01,.03))-.03;n=dFdy(I)/.004;e=o(e,K*(.4+.8*j(.25,.41,-p.y)),z=J(I,.01));e*=1.+.7*i(.005,.01,I)*n-.5*i(.0,.01,.05,I);I=O(q=p+d(0,.35),d(.01,.015))-.01;n=dFdy(I)/.004;e*=1.+.5*i(.005,.01,I)-.5*E(i(.0,.01,I));e+=k(1,.7,.5)*pow(Y(1.-L(q)/.11),8.);l e;}ad _31(){k e=am(ai,Q).X*a1();d c=F(Q)-.5,p=c;h t=mod(ae.x*2.,7.),m=af(t),f=1e6;U(m==0.)f=cq(p*2.4+d(0,.05));U(m==1.){p.x=ci(p.x,.1,-1.,1.);f=O(p,d(.02,.15))*2.;}U(m==2.){f=N(f,O(be(p,.0)+d(.13-j(-.3,.3,c.y)*.17,0),d(.02,.15)));f=N(f,O(p+d(0,.07),d(.07,.02)))*2.;}U(m==4.)f=aT(p*1.8,.5);else f=bk(f,.005);ac=G(e+J(f,.02)*F(-t)*k(.5,.05,.05),1);}H(_58){h b=D(c,d(13),.9,3.,4),C=D(c,d(7),.9,3.,4);k e=o(u(60,50,50),u(87,47,37),E(j(.7,.25,C)))*(.7+.8*b*b),g;c=W(c,31.,.003);aW(g,c,a7(j(.0,.9,R(p[m],d(93)))));e*=1.-(g.y+.4)*E(b*g.z)*g.z;l e;}H(_62){h b=D(c,d(5),.9,3.,4);k e=u(67,64,63)*(.6+.5*b),g;c=W(c,31.,.003);aW(g,c,a7(R(p[m],d(53,93))));e*=1.-.3*g.y*g.z*g.z;l e;}ao(_22){c=W(c,7.,.01);h b=D(c,d(9),.7,2.,4),C=D(c,d(13),.5,2.,4),A=j(.6,.9,D(W(c,5.,.03),d(11),.6,2.,4));k e=u(127,70,55)*(.85+.3*b);e*=1.-.2*E(j(.3,.0,b*b))-.2*A-.3*j(.6,.77,C)+.3*j(.5,.9,b);e+=.5*E(j(.5,1.,D(c,d(17),1.,2.,3)));l G(e,1.-A);}ad _24(){G e=am(ai,Q);d c=F(Q);c.y-=.2*ae.x;h b=D(W(c,7.,.02),d(5),.9,2.,4);ac=G(o(u(25,10,8)*b,e.X,e.w)*a1(),1);}H(_23){d p=c-.5;p=W(p,17.,.007);p.x*=2.-c.y*1.5;h b=D(c,d(9),.7,2.,4),C=R(c,d(7)),f=L(p),s;k e=_22(c).X;s=F(f*=13.);U(f<=6.){e*=1.-pow(j(6.,.5,f+b*b),6.);C=j(.3,.7,C);e*=1.-C*b*i(.4,.2,s)+C*b*i(.6,.4,s);}l e;}H(_25){c=W(c,13.,.003);h b=D(c,d(7),.9,3.,4),C=D(c,d(5),.5,2.,4),t=.5+b;k e=u(80,38,34),v=bu(c,d(23));e=o(e,o(u(180,125,118),u(165,78,51),C),b*i(.0,.4+C*.4,v.z))*t;l e;}H(_26){h b=D(c,d(7),.9,3.,4),t=.8+.4*b,m,r=.7,n,A;k e,v=bu(c,d(23));d p=v.xy/r,q=c+v.xy/23.;m=ah(F(q)*3.3);e=o(u(155,55,55),u(200,166,155),j(.75,.45,q.y))*t;A=i(.5,.5,L(p));n=dot(d(-p.y,a7(Y(1.-aV(p)))),d(.6+m*.3,.3));e*=1.-b*.8*j(.5,.1,v.z)+b*A*n;e*=t*t*t*t;l e;}H(_27){h b=D(c,d(13),.9,3.,4),t=.4+b*b,C=a5(R(W(c,12.,.02),d(48))),m,r,n,A;k e=u(60,50,46)*t,v=bu(c,d(17));m=ah(F(c+v.xy/17.));r=.4+.3*m;d p=v.xy/r;A=N(j(1.1,1.,L(p)),j(.0,.15,v.z));n=dot(d(-p.y,a7(Y(1.-aV(p)))),d(.1+m*.2,.3));e+=b*A*n*C;C=a5(D(W(c,13.,.01),d(23,43),.5,2.,3));e*=1.+(1.-A)*i(.4,.4,C);l e;}H(_60){h b=D(c,d(7),.9,3.,4),C=D(c,d(3),.5,3.,4);k e=o(u(103,56,53),u(73,58,71),j(.1,.7,C))*(.75+b*b);l e;}H(_61){h b=D(c,d(13),.9,3.,4),C=R(W(c,5.,.05),d(9)),f=a5(F(c.x*4.)),A=j(.1,.15,f)*j(1.,.99,c.y);k e=u(51,44,44);e=o(e,u(73,55,52),j(.2,.2,b)*C*A);e=o(e,u(69,60,66),j(.7,.1,b)*b*A);e=o(e,u(99,77,77),j(.1,.5,C)*C*A*b*b*.3);e*=.6+.3*b+.3*b*b;e*=1.+.9*E(i(.21,.02+.1*C,f+b*.05))*A*b;e*=1.-E(j(.49,.5,B(c.y-.5)));e*=1.-j(.05,.2,f)*j(.16,.1,f);e*=1.+i(.99,.007,c.y);l aS(e,d(f-.4,F(c.y*8.)-.5),.07);}k bW(d c,h s){h b=D(c,d(3,1.+s+s),.7,2.,4),f=a5(c.x),A;c.y*=2.;k e=o(u(71,60,58),u(110,88,77),j(.1,.05,f))*(.7+.6*b);e*=1.-j(.05,.0,c.x)*(1.-b*b);e*=1.+.5*i(.05,.02,c.x);d p=d(f-.35,F(c.y*s)-.5);G K=bE(p,.11);A=J(K.w);e*=1.-.7*bF(p,1.1)*(1.-A);e=o(e,(K.y>.0?u(128,105,88):u(200,111,66)*j(-.2,.7,K.z))*(.4+2.*b*pow(Y(bc(K.yz*.7)),4.))*(1.-.6*i(-.1,.4,K.y)),A);l e;}H(_38){l bW(c,4.);}H(_39){l _38(c.yx);}H(_40){l bW(c,1.);}H(_48){h b=D(c,d(40,5),.9,3.,4);k e=u(110,110,98)*(.8+.8*b*b);U(c.y<1./4.)e*=.5;e*=1.-.4*j(.4,.0,b)+.5*j(.02,.0,c.y)+.2*i(.24,.01,c.y);l e;}H(_49){d p=c,q;p.y*=22.;q=F(p);h b=D(c,d(3,23),1.,2.,6),C=D(c,d(3,33),.7,3.,4),aB=ah(p.y-q.y);k e=u(92,67,53)*(.8+.8*b*b);e*=1.-E(j(.1,.0,N(q.y,1.-q.y)))*b;e*=1.-.2*smoothstep(.3,.7,C);e*=.8+.3*b*aB;l e;}H(_50){h b=D(c,d(13),.9,3.,4),x=c.x*16./3.;k e=_49(c)*j(.15,.21,c.x);U(x<1.)e=u(59,48,49)*(.7+.6*b);e*=1.+.5*i(.05,.05,a5(x));l aS(e,d(B(c.x-3./32.)-.07,mod(c.y,.1)-.05),.004);}H(_51){d p=b9(c,d(6,4)),q=F(p),a2=q;h b=D(W(c-=.5,5.,.03),d(13),.9,2.,3),C=R(c,d(73,7)),t=(.75+b*b)*(.8+.4*cd(c.x*93.)),r;k e=k(.25*t);a2.y+=a2.y*2.-.01-.03*C;r=L(a2-=clamp(a2,d(.49,.5),d(.51,3)));e*=1.-.7*b*E(j(.07,.03,B(r-.5)))+.5*b*i(.35,.1,r)*E(j(.2,.1,q.y))-.3*E(j(.8,1.,q.y))-.3*(j(.3,.1,q.y))*j(.4,.6,r)+.2*E(j(.5,.1,q.y))*j(.45,.4,r);l e;}H(_64){h b=D(c,d(5),.9,3.,4),P=F(c.y*10.);k e=o(u(53,48,42),u(38,38,36),b);e*=.6+b*.8;e*=1.-.5*E(i(.5,.5,P));e*=1.+.5*E(i(.25,.25,P));e*=1.+.5*E(i(.65,.35,P));l e;}H(_63){h b=D(c,d(7,3),.9,3.,4),P=c.y+b*.04,n=1.-.15;k e=o(u(59,48,40),u(110,108,102),b*b);n=o(n,.5,i(.34,.05,c.y));n=o(n,.5,j(.08,.05,B(c.y-.7)));n=o(n,.3,i(.7,.03,c.y));n=o(n,1.5,i(.01,.03,c.y));n=o(n,2.2,i(.89,.1,P));n=o(n,1.6,j(.07,.04,B(c.y-.44)));n=o(n,2.5,i(.5,.04,P));n=o(n,1.7,i(.18,.04,P));l e*n;}H(_65){h b=D(c,d(5,3),.9,3.,4);k e=o(u(74,66,55),u(99,90,78),b*b);c.x*=2.;d p=bf(c,d(.5,.625),d(1.5,.625));h f=L(p-c),A=j(.22,.20,f),n=1.-.15*A;n=o(n,.5,j(.7,.9,c.y)*A);n=o(n,1.-bD(f).y*.5,i(.22,.04,f));n=o(n,.6,E(i(.19,.05,f)));n=o(n,.5,j(.05,0.,c.y));n=o(n,.5,i(.26,.05,c.y));n=o(n,1.7,j(.93,1.,c.y));n=o(n,1.7,i(.23,.04,c.y));l e*n;}ao(_66){h b=D(c,d(1,5),.4,3.,4);k e=o(u(56,49,43),u(142,136,136),b);c=.5-B(c-.5);c.y*=4.;h a=i(.0,.1,L(c-bf(c,d(.41,.5),d(.42,3.5)))),f=aZ(c),n=1.-.7*M(0.,1.-f/.15);n*=1.-.8*j(.24,.31,N(f,c.y-.1));e+=u(80,80,20)*a;l G(e*o(n,2.7,a),a);}ao(_69){h b=D(c,d(1,5),.4,3.,4);k e=o(u(56,49,43),u(142,136,136),b);c=.5-B(c-.5);c.y*=8.;h f=L(c-bf(c,d(.27,.3),d(.27,7.7))),a=i(.0,.17,f),n=1.-.5*i(.17,.07,f);e+=u(80,80,20)*a;l G(e*o(n,2.7,a"
"),a);}ao(_67){d p=B(c-.5);h b=D(c,d(1),.4,3.,4),r=L(p),a=j(.37,.33,r)*(.5+2.*b),n=1.+.0*j(.08,.03,B(r-.41));k e=o(u(56,49,43),u(142,136,136),b);n=o(n,7.,j(.44,.1*b,r));n*=1.-.5*E(i(.46,.04,r));n*=1.-.4*E(i(.36,.04,r));l G(e*n,a);}ao(_68){h b=D(c,d(5),.9,3.,4),f=bC(bO(c-=.5,.35),B(S(c,.4)),.02),a=pow(J(f-.02,.15),8.),T=N(M(O(c,d(.46)),-S(c,.51)),B(S(c,.44)));k e=u(76,62,47)*(.8+.8*b*b);e*=1.+(b+.5)*J(B(T)-.01,.01);e*=1.-j(.1,.05,f)*J(S(c,.4));l G(e+1.*k(1,1,.3)*a,a);}ao(_1){c-=d(.48,.5);h f=aT(c,0.),b=L(c)-.47;k e=1.-k(.5,1,1)*J(M(.007-f,b+.04));l G(e,1)*J(b);}H(_2){c-=.5;d r=d(dFdx(c.x),dFdy(c.y));c/=r/aE(r);c*=.8;c.y-=.03;h x=B(c.x),b=D(c,d(31,5),.7,2.,3),t=.8+.8*b*b,f=aT(c,1.),I=aT(c+d(0,.002),1.),n=(I-f)*5e2+.5;k e=k(.3*t,0,0)*J(f,.004);e*=1.-E(j(.0,.3,x))-.5*j(.1,.3,B(c.y-.1));e+=+t*.2*i(.0,.01-.01*x,f)*i(.1,.2,c.y)*j(.3,.2,x)*n+t*.5*j(.004,.0,f)*j(.07,.1,c.y)*i(.23,.1,x)*k(.9,.9,1)+t*.4*i(.005,.005,f)*j(.2,-.1,c.y)*j(.3,.2,x)*Y(-n);l e;}h bX(d c,h s,h m,h c3){m=(c.y-E(B(c.x-.5))*c3)*s-m;l 2.*i(.5,.4,m)*(F(m)-.5);}ao(_78){h b=D(c,d(5,9),.9,3.,4),t=.8+.8*b*b,C=D(c,d(5,9),.9,3.,2),x=B(c.x-.5),f;k e=u(77,60,44)*t;d p=c*d(1,2)-d(.5,.7);e*=1.-.55*J(aA(S(p,.3),aT(a3(45.)*p*.8,0.)-.01));e*=1.+E(j(.6,.9,b))+i(.2,.5,c.y)*i(.2,.3,x)*bX(c,4.,.2,4.);for(h Z=6.;Z<9.;++Z)e*=1.+i(.8,.5,c.y)*i(.2,.3,x)*bX(c,12.,Z,1.);f=c.y-.81-E(Y(x*4.))*.09;e=o(e,u(82,66,60)*t,j(.0,.01,f));e*=1.-.5*(i(.01,.02,f))+.5*(i(.02,.01,f));f=.15*(1.-c.y);f=O(c-.5,d(.49)-f)+C*.1*a7(1.-c.y)-f;l G(e*E(J(f+.01,.05)),J(f));}ao(_79){G e=am(ai,c);U(e.w<.5)discard;e.X*=E(a1()*.5);l e;}ao(_80){c*=d(256,64);c.y+=2.;h f=S(c-d(81,30),11.);f=M(f,c.x-80.);f=M(f,-S(c-d(84,26),9.));f=N(f,O(c-d(73,37),d(4,9))-4.);f=M(f,-O(c-d(73,37),d(0,7))+1.);f=N(f,O(c-d(91.5,47),d(4,19))-4.);f=M(f,-O(c-d(91.5,47),d(0,17.5))+1.);f=N(f,O(be(c,111.)-d(105.+j(23.,50.,c.y)*3.,43),d(3.5,19)));f=N(f,O(c-d(111,32),d(4,3)));f=N(f,O(c-d(126,37),d(3,13)));f=N(f,O(c-d(125.5+j(23.,50.,c.y)*10.,44),d(3.5,6)));f=N(f,O(c-d(136.5-j(23.,50.,c.y)*9.,32),d(3.5,8)));f=N(f,O(c-d(148.5,37),d(7,13)));f=M(f,-O(c-d(155,33),d(6,3)));f=M(f,-O(c-d(155,43),d(6,2)));f=N(f,O(c-d(168,37),d(3.5,13)));f=N(f,O(c-d(178.,37),d(3.5,13)));f=N(f,O(c-d(188,37),d(3.5,13)));f=M(f,c.y-50.);l G(J(f,.8),0,0,ah(c*511.));}ad _81(){k e=am(ai,Q*2.).X*step(.5,F(ae.x*.5));e=o(e*a1(),k(.5,0,0),i(F(ae.x*2.),1./64.,F(Q.y)));ac=G(e+bL(Ref)*.25+am(ai,Q+ah(ae.xx)).w*.1,1);}ad _82(){d c=F(bM(Pos.X,bN(Nor))/128.);c.x+=ae.x/33.;h b=D(c,d(7),.9,2.,4),Z=F(Pos.z/128.-.375);ac=G(2.*u(95,85,80)*Z*Z*Z*Z*o(1.,b,.5),0.);}ad _83(){d c=F(Q),p=c;p.y+=p.y-ae.x;c.x+=sin(p.y*7.)*.2*c.y;h C=D(p+sin(p.yx*aC*9.+d(0,ae.x*9.))*.015+R(p,d(5))*.1,d(13),.4,3.,4),b=O(c-d(.5,.25),d(.05*E(j(.4,.2,c.y)),.1)),A=E(J(b+C*.25,.35));ac=j(.0,.4,A)*G(5,2,.7,0);}ad _0(){h n=dot(Nor,aI(k(2,0,8)));n=n*.4+.7;d c=bM(Pos,bN(Nor));k e=k(.5);e*=cs(k(F(aY*ae.w+.25),1.,1.));ac=G(e*n,1);}ad _6(){G e=am(ai,Q);ac=G(e.X*o(a1(),k(1),e.w),1);}ad _71(){G e=am(ai,Q);h r=L(F(Q)-.5);h s=o(.4,8.,F(ae.x*1.5));ac=G(e.X*a1()+u(240,130,5)*i(.1,.05,r/s)*j(.37,.32,r),1);}ad _5(){k e=am(ai,Q).X;ac=G(e*a1(),1);}ad _7(){G e=am(ai,Q);e.X*=1.+e.w*bL(Ref);ac=G(e.X*a1(),1);}ad _74(){ac=G(0);}ad _75(){k f=aI(Pos-Cam.X);f.z=f.z*4.+2.;d c=aI(f).xy*2.;h b=j(.2,1.,D(c-ae.x*d(.1,.2),d(5),.5,2.,6));c.y*=1.5;h s=j(.3,1.,D(c-ae.x*d(.1,.18),d(5),.6,2.,6));ac=G(k(b,0,0)+u(80,30,8)*s*s*2.,1);}ad _76(){d c=W(Q/8.,ae.x*.5,2.,.05);h b=D(c,d(7),.9,2.,4);k e=u(91,22,14)*(.2+1.6*b);e=o(e,u(144,44,0),i(.6,.2,D(c,d(3),.7,3.,4)));e=o(e,u(244,144,66)*b*2.,E(i(.55,.25,D(c,d(11),.5,2.,4))));ac=G(e*Y(aE(a1())),1);}ad _77(){_76();}ad _3(){ac=am(ai,(.5+Q*127.)/128.,2.5);ac.X*=.7+.3*R(Q,.5/fwidth(Q));}ad _4(){ac=am(ai,Q)*Clr;}"
;

// src/demo/data/shaders/vertex_shaders.glsl: 3236 => 1598 (49.4%)
static constexpr char g_vertex_shaders[] =
"#define c void\n"
"#define f vec4\n"
"#define h location\n"
"#define k gl_Position\n"
"uniform mat4 MVP;uniform f Time,Cam;layout(h=0)in f e;layout(h=1)in f i;layout(h=2)in vec3 j;layout(h=3)in f m;out vec3 Pos,Nor,Ref;out vec2 UV,LUV;out f Clr;c d(){k=e;UV=e.xy*.5+.5;}c l(float n,float o,float u){k+=o*MVP*f(j,0)*sin(6.28*(Time.x*u+dot(e.xyz/n,vec3(1))));}c _0(){k=MVP*e;Pos=e.xyz;Nor=j;UV=i.xy;LUV=i.zw;Ref=normalize(reflect((e-Cam).xyz,j));}c _1(){d();}c _2(){d();}c _3(){d();}c _4(){k=f(2.*e.x-1.,1.-2.*e.y,1,1);UV=i.xy;Clr=m;}c _5(){_0();}c _6(){_0();}c _7(){_0();}c _8(){d();}c _9(){d();}c _10(){d();}c _11(){d();}c _12(){d();}c _13(){_0();}c _14(){d();}c _15(){d();}c _16(){d();}c _17(){_0();}c _18(){d();}c _19(){d();}c _20(){d();}c _21(){d();}c _22(){d();}c _23(){d();}c _24(){_0();}c _25(){d();}c _26(){d();}c _27(){d();}c _28(){d();}c _29(){_0();}c _30(){d();}c _31(){_0();}c _32(){d();}c _33(){d();}c _34(){d();}c _35(){d();}c _36(){d();}c _37(){d();}c _38(){d();}c _39(){d();}c _40(){d();}c _41(){d();}c _42(){d();}c _43(){d();}c _44(){d();}c _45(){d();}c _46(){d();}c _47(){d();}c _48(){d();}c _49(){d();}c _50(){d();}c _51(){d();}c _52(){d();}c _53(){d();}c _54(){d();}c _55(){d();}c _56(){d();}c _57(){d();}c _58(){d();}c _59(){d();}c _60(){d();}c _61(){d();}c _62(){d();}c _63(){d();}c _64(){d();}c _65(){d();}c _66(){d();}c _67(){d();}c _68(){d();}c _69(){d();}c _70(){d();}c _71(){_0();}c _72(){d();}c _73(){_0();}c _74(){_0();}c _75(){_0();}c _76(){_0();l(100.,3.,.1);}c _77(){_0();}c _78(){d();}c _79(){_0();l(30.,3.,.2);l(100.,3.,.7);}c _80(){d();}c _81(){_0();}c _82(){_0();}c _83(){_0();}"
;
