#pragma once

// auto-generated, do not modify
static_assert(0xe6b55f00U == Demo::Shader::Version, "Shader definition mismatch, please recompile the shader compiler & shaders");

// src/demo/data/shaders/fragment_shaders.glsl: 84493 => 35723 (42.3%)
static constexpr char g_fragment_shaders[] =
"#define d vec2\n"
"#define h float\n"
"#define k vec3\n"
"#define l return\n"
"#define o mix\n"
"#define ae abs\n"
"#define ah vec4\n"
"#define aj fract\n"
"#define ao void\n"
"#define au length\n"
"#define a1 xyz\n"
"#define a2 min\n"
"#define a3 max\n"
"#define a4 Time\n"
"#define be int\n"
"#define bi floor\n"
"#define bv sqrt\n"
"#define cl normalize\n"
"#define dj(x)((x)*(x)*(3.-2.*(x)))\n"
"#define ai(x)((x)*(x))\n"
"#define cw(x)dot(x,x)\n"
"#define bd(x)clamp(x,0.,1.)\n"
"#define n(r,g,b)(k(r,g,b)/255.)\n"
"#define b6(v)i(.5,.5,v)\n"
"#define am(b8)k b8(d);ao b8(){an=ah(b8(UV),1);}k b8(d c)\n"
"#define bj(b8)ah b8(d);ao b8(){an=b8(UV);}ah b8(d c)\n"
"#define a6(x)texture(Texture0,x)\n"
"#define c5(eg,c,eh){d p[3];h r[3];p[0]=c;p[1]=c+dFdx(c);p[2]=c+dFdy(c);for(be m=0;m<3;++m)r[m]=eh;eg=k(ei(d(r[1],r[2])-r[0]),r[0]);}\n"
"#define dk(b8,ej)k b8(d p,d dl){p*=dl;d u=bi(p),a7=p-u,dm,g,bf,r;h cx=8.,c6=cx,f;for(be m=0;m<9;++m){g=d(m%3-1,m/3-1);bf=dn(mod(u+g,dl));r=g+bf-a7;f=ej;if(f<cx){c6=cx;cx=f;dm=r;}else if(f<c6){c6=f;}}l k(dm,c6-cx);}\n"
"uniform ah a4,Cam,LightDir,LightColor,Ambient;uniform sampler2D Texture0,Texture1;in k Pos,Nor,WNor,Ref;in d UV,LUV;in ah Clr;out ah an;h ch=3.1415927,dp=2.*ch,co=1.618034;d dq(h m){h dc=1.324718;l aj(.5+m/d(dc,dc*dc));}h ci(h m){l aj(.5+m*co);}h ek(h a7){h m=bi(a7);l o(ci(m),ci(m+1.),dj(a7-m));}h c7(d v){l a2(v.x,v.y);}h c7(k v){l a2(v.x,a2(v.y,v.z));}h c7(ah v){l a2(a2(v.x,v.y),a2(v.z,v.w));}h cj(d v){l a3(v.x,v.y);}h cj(k v){l a3(v.x,a3(v.y,v.z));}h cj(ah v){l a3(a3(v.x,v.y),a3(v.z,v.w));}h cp(d v){l v.x+v.y;}h cp(k v){l v.x+v.y+v.z;}h dr(h a,h b){l ae(a)<ae(b)?a:b;}h cy(h x,h s){l ae(x)-s;}h dd(h x,h s){l sign(x)*a3(0.,ae(x)-s);}d ei(d v){h af=dot(v,v);l af>0.?v/bv(af):v;}h i(h el,h em,h x){l 1.-bd(ae(x-el)/em);}h j(h ds,h en,h x){l bd((x-ds)/(en-ds));}h i(h a,h b,h e,h x){l a2(j(a,b,x),j(e,b,x));}d bl(h x){l d(sin(x),cos(x));}mat2 bh(h x){d v=bl(radians(x));l mat2(v.y,v.x,-v.x,v.y);}h cf(d p){l aj(atan(p.y,p.x)/dp);}d c8(d p,h u){l p*bh(360./u*(bi(cf(p)*u+1.5)-1.));}d a9(d c,h p,h s){l c+sin(c.yx*ch*p)*s;}d a9(d c,h t,h p,h s){l c+sin(c.yx*ch*p+t)*s;}h c9(h v,h ac){l ac-ae(v-ac);}d c9(d v,h ac){v.x=c9(v.x,ac);l v;}h eo(h x,h p,h af,h a5){l x-p*clamp(bi(x/p+.5),af,a5);}d dt(d c,d s){c.x+=bi(c.y*s.y)*(.5/s.x);l aj(c)*s;}k du(d c,h r){l k(c-=clamp(c,r,1.-r),au(c)/r);}k du(d c,d s,h r){s=s.yx/c7(s);c*=s;l k(c-=clamp(c,d(r),s-r),au(c)/r);}h bk(d p){k q=aj(p.xyx*.09153);q+=dot(q,q.yzx+19.19);l aj((q.x+q.y)*q.z);}h bk(h p){p=aj(p*.1031);p*=p+33.33;p*=p+p;l aj(p);}k ff(h p){k b1=aj(k(p)*k(.1031,.1030,.0973));b1+=dot(b1,b1.yzx+33.33);l aj((b1.xxy+b1.yzz)*b1.zyx);}d dn(d p){k b1=aj(k(p.xyx)*k(.1031,.1030,.0973));b1+=dot(b1,b1.yzx+33.33);l aj((b1.xx+b1.yz)*b1.zy);}ah ep(h p){ah b2=aj(ah(p)*ah(.1031,.1030,.0973,.1099));b2+=dot(b2,b2.wzxy+33.33);l aj((b2.xxyz+b2.yzzw)*b2.zywx);}ah ep(d p){ah b2=aj(ah(p.xyxy)*ah(.1031,.1030,.0973,.1099));b2+=dot(b2,b2.wzxy+33.33);l aj((b2.xxyz+b2.yzzw)*b2.zywx);}h bm(h x,h p){l bk(mod(x,p));}h eq(h x){h m;l o(bk(m=bi(x)),bk(m+1.),dj(x-m));}h a8(h x,h p){h m;l o(bm(m=bi(x),p),bm(m+1.,p),x-m);}h bm(d p,d s){l bk(mod(p,s));}h a8(d p,d s){p*=s;d m=bi(p);p-=m;p*=p*(3.-2.*p);h er=bm(m+d(0,0),s);h es=bm(m+d(0,1),s);h et=bm(m+d(1,1),s);h eu=bm(m+d(1,0),s);l o(o(er,eu,p.x),o(es,et,p.x),p.y);}h ad(d p,d b3,h cz,h c0,be c1){h cg=a8(p,b3),b9=1.,tw=1.;for(be m=0;m<c1;++m){p=aj(p+co);b3*=c0;b9*=cz;cg+=a8(p,b3)*b9;tw+=b9;}l cg/tw;}h ad(h p,h b3,h cz,h c0,be c1){h cg=a8(p*b3,b3),b9=1.,tw=1.;for(be m=0;m<c1;++m){p=aj(p+co);b3*=c0;b9*=cz;cg+=a8(p*b3,b3)*b9;tw+=b9;}l cg/tw;}h dv(d p,d b3,h cz,h c0,be c1){h cg=b6(a8(p,b3)),b9=1.,tw=1.;for(be m=0;m<c1;++m){p=aj(p+co);b3*=c0;b9*=cz;cg+=b6(a8(p,b3))*b9;tw+=b9;}l cg/tw;}d ck(d p,d a,d b){d ab=b-a,ap=p-a;h t=bd(dot(ap,ab)/dot(ab,ab));l ab*t+a;}h ev(d c,d f){l dot(c,bh(90.)*cl(f));}h av(d p,d b){d f=ae(p)-b;l a2(a3(f.x,f.y),0.)+au(a3(f,0.));}h c2(d p,d b){l cj(ae(p)-b);}h bc(d p,h r){l au(p)-r;}h ck(d p,d a,d b,h r){l bc(p-ck(p,a,b),r);}h c3(d p,d r){l bc(p/r,1.)/a2(r.x,r.y);}h b0(h a,h b){l a3(a,-b);}h de(h a,h b,h a0){h a5=bd(.5+0.5*(b-a)/a0);l o(b,a,a5)-a0*a5*(1.-a5);}d df(h x){d f=d(dFdx(x),dFdy(x));l f/a3(au(f),1e-8);}h ak(h s,h f){l bd(1.-s/f);}h ak(h s){l bd(1.-s/fwidth(s));}ah dw(d c,h s){l ah(c/=s,bv(bd(1.-cw(c))),au(c)-1.);}h ew(k u){h af=cp(u.yz)*.7;l pow(bd(af),4.)+af;}h dx(d c,h s){c/=s;c.y+=.06;c.x*=2.;l j(.3,.0,au(c));}k cm(k e,d c,h s){ah b=dw(c,s);e*=1.+ew(b.a1)*ak(b.w)*.5;e*=1.-ai(dx(c,20.*s))*(1.-ak(b.w))*.3;l e;}h cq(d c,h t){c.x=ae(c.x);c.y-=.07;h f=c3(c,d(.31,.12-t*.02))/50.;f=a3(f,-c3(c-d(0,.01+.01*t),d(.28+t*.01,.07))/75.);f=a3(f,-av(c-d(0,.1),d(.22-.02*t,.12)));f=a3(f,-av(c-d(0,.1),d(.084-.012*t,.31)));f=a2(f,c2(c-d(0,-.09),d(i(-.09,.32,c.y)*(.04-.015*t),.32)));f=a2(f,c2(c-d(.11-.02*t,-.21+.01*t),d(i(-.07,.3,c.y)*(.03-.01*t),.15)));l f;}h ex(d c){h f=bc(c-d(0,.17),.32);f=b0(f,bc(c-d(0,.235),.27));f=b0(f,bc(c-d(0,.5),.15));c.y-=.09;d s=d(.09,.52);h a5=bd(-c.y/s.y);s*=.5;s.x*=j(1.05,.6,a5)+ai(j(.1,.02,a5));c.y+=s.y;f=a2(f,av(c,s));l f;}k dy(d p,h bl,h ey){k al=k(-1,0,1),r=k(1e5);d b4=bi(p*bl),fg=al.yy;p-=(b4+.5)/bl;h bn=.5*bm(b4+al.yy,d(bl)),cr=.5*bm(b4+al.xy,d(bl)),cs=.5*bm(b4+al.yz,d(bl)),ct=.5*bm(b4+al.zy,d(bl)),cu=.5*bm(b4+al.yx,d(bl)),dz=.5*bm(b4+al.xz,d(bl)),d0=.5*bm(b4+al.zz,d(bl)),d1=.5*bm(b4+al.zx,d(bl)),d2=.5*bm(b4+al.xx,d(bl));d[4]ca,af;if(mod(b4.x+b4.y,2.)<.5){af[0]=1.+d(cu-cr,bn-d2);af[1]=1.+d(ct-cu,bn-d1);af[2]=1.+d(cs-cr,dz-bn);af[3]=1.+d(ct-cs,d0-bn);ca[0]=d(cu,bn);ca[1]=d(cu,bn);ca[2]=d(cs,bn);ca[3]=d(cs,bn);}else{af[0]=1.+d(bn-d2,cr-cu);af[1]=1.+d(d1-bn,ct-cu);af[2]=1.+d(bn-dz,cs-cr);af[3]=1.+d(d0-bn,cs-ct);ca[0]=d(bn,cr);ca[1]=d(bn,ct);ca[2]=d(bn,cr);ca[3]=d(bn,ct);}for(be m=0;m<4;m++){ca[m]+=af[m]*(d(m&1,m/2)-.5);af[m]/=bl;h bx=c2(p-ca[m]/bl,af[m]/2.-ey/bl);if(bx<r.x)r=k(bx,b4+ca[m]);}l r;}dk(da,cp(ae(r)))dk(dg,au(r))h cb(k p,h s){p=cl(p);k a=aj(degrees(atan(p,p.yzx))/360.);l a8(a.x*s,s)*j(.9,.0,ae(p.z))+a8(a.y*s,s)*j(.7,.0,ae(p.x));}h cb(k p){l cb(p,45);}d ez(k p,be ax){l(ax==0)?p.yz:(ax==1)?p.xz:p.xy;}be e0(k u){u=ae(u)+k(.01,.02,.03);h ac=cj(u);l(ac==u.x)?0:(ac==u.y)?1:2;}k e1(k e){k rgb=bd(ae(mod(e.x*6.+k(0,4,2),6.)-3.)-1.);rgb*=rgb*(3.-2.*rgb);l e.z*o(k(1.),rgb,e.y);}k bo(){k f=Cam.a1-Pos;h b=ad(f.xy/256.*bh(Cam.w),d(3),.7,3.,4),af=1.-j(14.,-6.,au(f.xy)-b*8.)*j(128.,48.,f.z)*step(.1,Nor.z);l texture(Texture1,LUV).a1*2.*af;}k e2(){l o(Ambient.a1,LightColor.a1,bd(dot(LightDir.a1,WNor)));}am(_8){h b=ad(c,d(5),.9,3.,4);k e=o(n(48,41,33),n(103,101,104),b);l e;}am(_9){h b=ad(c,d(3),.9,3.,4);k e=o(n(49,45,43),n(81,75,78),b*b);l e;}am(_10){h b=ad(c,d(7),.9,3.,4);c.x*=-1.5;c.y+=c.x*.5;c.x=1.-c.x+c.y;c=aj(c*28.);h a7=bd(1.-au(.1-c));a7*=j(.6,.2,au(.6-c));a7*=j(.6,.8,au(.1-c));a7*=j(.2,.6,b)*2.+1.;h af=1.-j(.2,b+2.,cj(ae(c-.5)));l k((a7+1.)*o(.21,.29,b*b)*af);}bj(_11){k e=a6(c).a1;h b=ad(c=a9(c,7.,.01),d(5),.7,3.,4),u=ad(c,d(7),.5,2.,4),r=au(c-=.5),a=cf(c),f=r+u*.8-.33;e=o(e,o(n(21,17,14),n(70,59,51),b),j(.5,.2,r+b*b*b));e*=j(.1,.11,f)+2.*pow(i(.4,.12,.1,f),8.);l ah(e,1.-bv(j(.11,.06,f)));}ao _12(){h b=ad(UV*.5+a4.x*d(9,5),d(7),.6,2.,4);b*=1.+.5*i(.5,.05,b);ah e=a6(UV);an=ah(o(n(66,111,155)*(.8+2.*b*b),e.a1*bo(),e.w),1);}h d3(d c,h s){h f=1e6,m=0.;for(;m<5.;++m){d p=d(0,-s)*bh(m*72.);f=a2(f,au(c-ck(c,p,p*bh(144.))));}l f;}am(_75){h b=ad(c,d(7),.9,3.,4);k e=a6(c).a1;h r=au(c-.5);h ac=j(.46,.45,r);h af=1.5-1.5*j(.0,.3,r*r);af=o(af,2.5,i(.42,.07,r));af=o(af,3.5,i(.44,.05,r));af=o(af,2.6,i(.36,.03,r));h u=.3+.2*j(.35,.30,r);af*=1.-u*j(.3,.7,b);af*=1.-.3*ai(j(.13,.05,r));af=o(af,2.5,j(.04,.01,r));af-=af*i(.03,.01,r)*.7;e=o(e,n(68,66,54)*af,ac);e*=1.-ai(i(.34,.02,r));e*=1.-ai(i(.46,.03,r));e*=1.-i(.41,.03,r)*.7;l e;}bj(_13){k e=_75(c);c=aj(c)-.5;h b=ad(c,d(3),.9,3.,4),f=a2(ae(au(c)-.4),d3(c,.35));l ah(e,ak(f-.02+b*.02,.01));}ao _14(){ah e=a6(UV);an=ah(e.a1*bo()+n(111,55,0)*e.w*(sin(a4.x*ch)*.5+.5),1);}d e3(d c,h s){l d(1.-au(c)/s,ak(au(c)-s));}bj(_77){h b=ad(c,d(5),.9,3.,4),t,bf,a0,r;k e=a6(c).a1;d b5,v;b5.x=ae(c.x-.5);b5.y=a2(c.y,.4);r=au(b5-d(0,.4))-(.18-.06*j(.4,1.,c.y));a0=.25-.15*j(.9,.96,c.y)+.03*ai(j(.82,.86,c.y))+.07*j(.8,.2,c.y)+.07*ai(j(.35,.22,c.y))-.07*j(.22,.0,c.y);bf=av(c-d(.5,.5),d(a0,.46));bf=a3(bf,-av(b5,d(.15,.03))+.06);e=o(e,k(.6,.55,.55)-c.y*.3+b*.2,ak(bf));e*=1.-.7*i(.0,.013,bf);e*=1.-(r/.5-.1)*ak(bf);t=a3(r,c.y-.96);bf=ae(t-.02)-.03;bf=a3(bf,c.y-1.+b5.x*.5);bf=a3(bf,c.y-.96);e=o(e,k(1,1,.9)-c.y*.55,i(-.01,.01,bf));e=o(e,k(.2*b+.1),ak(t,.01));e*=1.-.2*i(.0,.05,t)*ak(bf);v=e3(b5=c-d(.5,.4),.02);e*=1.+n(111,80,70)*i(.03,.01,au(b5));e*=1.-.5*i(.02,.01,au(b5));e=o(e,n(111,66,44)*(v.x*1.5+.2),v.y);l ah(e,ak(t-.03,.02));}ao _78(){ah e=a6(UV);d c=aj(UV);c.x=ae(.5-c.x);h t=aj(-a4.x),r=au(c-d(0,.4)),af=t*pow(a3(0.,1.-r),4.)*e.w;if(t>.75)af+=j(.03,.01,ae(aj(c.y+c.x*.5+t*2.)-.45))*j(.1,.08,c.x);an=ah(e.a1*bo()+n(180,150,5)*af,1);}am(_15){h b=ad(c,d(5),.9,3.,4);k e=o(n(44,14,16),n(93,63,63),b*b);l e;}k d4(d c){h al=3e-3,a=0.;d g=d(6),r=da(c,g).xy;for(be m=0;m<9;++m)a+=cp(ae(da(d(m%3-1,m/3-1)*al+c,g).xy-r));l k(c+r.xy/g,a);}am(_16){h b=ad(c,d(3),.9,3.,4);k e=o(n(80,70,72),n(128,120,120),b*b);k v=d4(c);e*=o(.95,1.1,a8(v.xy,d(6)));e=o(e,n(168,128,120),j(.5,1.,v.z)*b*.7);l e;}bj(_17){h b=ad(c,d(3),.9,3.,4);k e=o(n(80,70,72),n(128,120,120),b*b);k v=d4(c"
");h ac=j(.5,1.,v.z);h r=j(.4,.2,au(.5-aj(v.xy)));e*=o(.95,1.1,a8(v.xy,d(6)))-2.*r*b*b;e=o(e,n(168,128,120),ac*b*.7);l ah(e,ac*r);}ao _18(){ah e=a6(UV);an=ah(e.a1*bo()+i(.5,.125,aj(UV.y*.5+a4.x*.5))*e.w*.3,1);}am(_19){h b=ad(c,d(5),.9,3.,4);k pt=dy(c,8.,.31);k e=o(n(66,58,55),n(118,107,105),b);h af=1.-.5*j(.034,.036,pt.x);af=o(af,1.4,i(.033,.004,pt.x));l e*af;}h e4(d c){h b=a8(c,d(64)),a7=0.,f=1e6;for(;a7<11.;++a7)f=de(f,ae(au(.5-ae(c-dq(a7)))-o(.36,.29,ci(a7+.7)))-o(.015,.03,b),.01);l f*1e2;}k d5(d c){k s,p;for(be m=0;m<3;++m){p=k(c,0);p[m]+=1e-4;s[m]=e4(p.xy);}l k(cl(s.xy-s.z),s.z);}am(_20){h b=ad(c,d(5),.9,3.,4),af;k e=o(n(51,46,43),n(165,147,143),b*b),f=d5(c);af=1.-.5*(f.y-f.x)*i(.5,3.,f.z)*j(1.,.0,f.z);l e*af*.8;}am(_21){h b=ad(c,d(3),1.1,3.,4),af;k e=o(n(51,46,43),n(165,147,143),b*b),f=d5(c);af=1.-.5*(f.y-f.x)*i(.5,3.,f.z)*j(1.,.0,f.z);l e*af;}am(_22){h b=ad(c,d(5),.9,3.,4),af=.18*(.7+b*b);k g;c=a9(c,13.,.007);c5(g,c,ai(j(.3+b*.2,.9,ad(p[m],d(23),.5,2.,4))));l k(af*(1.-g.y*g.z));}am(_47){h b=ad(c,d(5),.9,3.,4);k e=o(n(77,55,53),n(62,48,48),a8(c,d(128,13)))*(.7+b*b),g;c=a9(c,13.,.007);c5(g,c,ai(j(.4+b*.4,.95,a8(p[m],d(63,43)))));e*=j(1.3,.9,g.z);l k(e*(1.+g.y*g.z));}h d6(d c,d s){c.y=a3(c.y,0.);l c3(c,s);}k db(k e,k b,d c,h a5,h s){h y=(c.y-a5)/s,p=1.-y*y;e*=1.-i(-1.,1.,y);if(p>0.)e=b*(p*(.8+.2*i(.5,.25,aj(c.x/s))))*(.7+ai(i(.2,.7,y)));l e;}k e5(d c,h u){l k(u*u*.4);}am(_48){c.x*=.5;h b=ad(c*d(2,1),d(3,5),.9,3.,4),u=.75+b*b,t=c.y+.2*a2(.4,i(.5,.33,aj(c.x*4.))),e6=d6(c-d(.25,.62),d(3,2)/32.),c4=d6(c-d(.25,.55),d(3,2)/48.),r;k e=o(n(66,50,51),n(111,66,44),bv(i(.31,.01,c.y))),bu;d p=c,q;p.x=aj(p.x*4.);if(c.y>.3)e=cm(e,d(4.*ae(p.x-.5)-1.6,aj(c.y*16.)-.5),.07);r=ae(p.x-.5);e*=1.-.3*j(.31,.32,c.y)*j(.87,.86,c.y)*(j(.035,.03,.5-r)+i(.48,.01,r)-i(.46,.02,r));e=o(e*u,e5(c,b),a3(j(.31,.3,c.y),ak(c4)));e*=j(1.5,.7,c.y);if(c.y<.306)e*=1.-i(.3,.05,c.y)*ak(-c4+10.,20.);e*=1.-i(.316,.004,c.y)*ak(-c4);if(c.y<.1)e*=.0;q=c;q.y+=i(.1,.01,mod(q.x,.33))/2e2;e=db(e,2.*b*n(93,84,79),c,.185,.015);e=db(e,2.*b*n(138,77,48),c,.13,.025);e=db(e,2.*b*n(112,71,51),c,.09,.015);e=db(e,2.*b*n(138,77,48),q,.05,.015);p.x=ae(aj(c.x*6.-.5)-.5)/6.;e*=1.+.5*j(.04,.03,p.x)*i(.18,.03,p.y);r=c2(p-d(0,.12),d(.03,.01));r=b0(r,c2(p-d(0,.11),d(.01)));e*=1.-ai(i(.0,.04,r));e=o(e,n(166,99,77)*2.*b*(.75+.5*ai(i(.125,.01,c.y))),ak(r));q=p;q.y-=.07;r=bc(q,.03);e*=1.-ai(i(.0,.07,r));e=o(e,n(127,83,72)*b*2.*j(.01,-.005,r),j(.005,.0,r));q.y-=.004;r=bc(q,.015);e*=ai(j(-.01,.01,r));q.y+=.013;r=bc(q,.05);e+=n(67,38,30)*4.*bv(b)*ai(i(-.02,.015,r)*i(.023,.02,c.y));r=b0(e6,c4);r=b0(r,(c.y-.3)*3e2);e*=1.-.5*i(-2.,17.,c4)*j(.26,.3,c.y);bu=n(67,39,17)*u;bu=o(bu,k(u*.2),i(0.,4.,r)*b);bu*=1.-.4*pow(i(.0,3.,r),4.);bu+=(bu+.6)*bv(b)*ai(i(-6.,8.,r)*i(.66,.04,c.y))*ak(r);if(c.y<.56)bu=cm(bu,d(24.*ae(c.x-.25)-1.85,aj(c.y*24.+.5)-.5),.15);e=o(e,_47(c),j(.85,.9,t)+step(c.y,1./256.));e*=1.+i(.88,.015,t)-ai(i(.87,.03,t));l o(e,bu,j(1.,.1,r));}k e7(k e,k a0,d c,be w,be a5){h b=ad(c,d(w,a5),.5,2.,2);e*=.9-.3*j(.15,.1,ae(b-.5));l o(e,a0,i(.5,.1,b));}am(_53){c=a9(c,9.,.005);be m=0,af[]=be[](13,43,17,47,23,59,27,63);h b=ad(c,d(19),.7,2.,4);k e=o(n(40,50,60),n(46,33,27),b)*(.5+b);for(;m<8;m+=2)e=e7(e,o(n(145,140,137),n(132,123,116),b),c,af[m],af[m+1]);l e;}k e8(k e,k a0,d c,be w,be a5){h b=ad(c,d(w,a5),.5,2.,2);e*=1.-.15*ai(j(.15,.1,ae(b-.5)));l o(e,a0,i(.5,.1,b));}am(_64){h b=ad(c,d(13),.9,3.,4),u=ad(c,d(7),.9,3.,4);k e=o(n(111,66,55),n(80,55,52),ai(j(.8,.2,u)))*(.8+.8*b*b),a0=e;c=a9(c,13.,.01);be m=0,af[]=be[](13,43,17,47,23,59,27,63);b=ad(c,d(19),.7,2.,4);for(;m<6;m+=2)e=e8(e,a0,c,af[m],af[m+1]);l e;}k e9(k e,k a0,d c,be w,be a5){h b=ad(c,d(w,a5),.5,2.,1);e*=.9-.3*ai(j(.15,.1,ae(b-.5)));l o(e,a0,i(.5,.1,b));}am(_42){h b=ad(c,d(3,29),.9,2.,4),t=.8+.8*b*b,f=ae(c.y-.61),bf=j(.25,.24,f),ac;k e=n(140,127,127),a0=e;d p=c;e*=1.-.1*j(.85,.86,c.y);e=t*o(e,n(110,55,50),j(.33,.32,c.y));p.y+=p.x*.11+b*.007;p.y=aj(p.y*9.)-.5;ac=j(.0,.1,ae(p.y)-.2);be m=0,af[]=be[](3,29,5,37,9,63,27,63);for(;m<6;m+=2)e=o(e,e9(e,a0,c,af[m],af[m+1]),ac*bf);e*=1.+t*bf*(+.6*i(.1,.1,p.y)-.7*i(-.25,.3,p.y)-.5*i(.2,.1,p.y));e=o(e,n(99,66,51)*t,i(-.15,.1,p.y)*bf);e*=1.+i(.36,.005,c.y)+i(.34,.005,c.y)+i(.865,.005,c.y)+i(.89,.01,c.y)-.5*ai(i(.245,.01,f))-.7*ai(i(.35,.01,c.y))-.5*ai(i(.325,.02,c.y))-.8*ai(i(.875,.02,c.y))-.3*ai(i(.9,.02,c.y));e*=.3+bv(b6(c.x));l e;}k d7(k e,d p,h s,h ac){s=da(p,d(s)).z/s*1e2;e*=1.+.5*ac*j(.9,.2,s)-.5*ac*i(2.5,.5,.3,s);l e;}k d8(k e,d c,h fa){h b=ad(c,d(4,9),.9,3.,4),t=.8+.8*b*b,a,f,ac,s,a0,m,v,r,z;k cn=o(n(133,100,88),n(133,100,100),b)*t;d p,q;p=q=c;q.x=ae(q.x);f=bc(p,.31);v=cf(q);ac=j(.01,.0,f);e=o(e*j(.0,.05,f+fa),k(.13*t),ac);e=d7(e,p,37.,j(.04,.02,ae(f+.07)));a=v*22.;m=bi(a);s=a-m;a0=j(.23,.22,ae(v-.25))+ci(m)*j(.0,.1,q.y);f-=r=(f*.3+.005)*a0;ac=j(.0,.1,q.y)*ak(ae(f+.015)-.015);e=o(e,cn,i(-.005,.01,f));e=o(e,n(130,75,44)*t,i(-.02,.005,f)*j(.0,.1,q.y));e*=1.-.3*j(.025,.03,-f)-.5*j(.4,.5,ae(s-.5))*ac+.2*i(.5,.3,ae(s-.5))*ac-.5*i(-.015,.007,f)-.5*i(-.03,.007,f)-.5*i(-.1,.005,f+r)-.5*i(-.115,.005,f+r)-.5*i(-.125,.015,f+r)-.5*i(-.145,.005,f+r)+.9*i(-.11,.007,f+r)+.5*i(-.14,.005,f+r)-b*i(.225,.005,ae(v-.25))*ak(ae(f+.015)-.015);a=v*72.;m=bi(a);s=a-m;a0=step(.7,bk(m))*step(q.y,.0)*j(.02,.0,ae(f+.02));e=o(e,k(cn*.6),a0*j(.4,.3,ae(s-.5)));e*=1.-.7*a0*i(.4,.1,ae(s-.5));l e;}k dh(d c){h b=ad(c,d(4,9),.9,3.,4),t=.8+.8*b*b,a,f,ac,s,a0,m,v,r,z;k cn=o(n(133,100,88),n(133,100,100),b)*t,e=k(.1*t);d p,q;p.x=c.x-.5;p.y=a3(c.y-.2,0.)*1.89;v=atan(p.y,ae(p.x))/ch;f=bc(p,.48);a0=j(.3,.31,v);f*=1.-.2*j(.3,.31,v)-.1*j(.43,.44,v);a=v*(v>.44?2.:v>.3?63.:31.);e=o(e,cn,j(.03,.01,ae(f)));ac=ak(ae(f-.01)-.02);m=bi(a);s=a-m;if(v>.33&&v<.44)s=aj(s+bk(m)*.6-.3);e*=1.-.5*ac*i(.307,.01,v)-t*ac*i(.5,.1+a0*.2,s)+b*ac*i(.52,.2+a0*.2,s);e*=1.-.9*i(-.015,.015,f)-.5*i(.0,.01,f)-.7*i(.03,.02,f)+i(.01,.015,f);q=p;q.y-=.5;q.x=ae(q.x)+.6;f=bc(q,1.13);ac=j(.03,.02,ae(f))*j(.5,.6,q.y);e=o(e,cn*bd(1.-ae(f-.015)/.03),ac);e*=1.-.5*ac*i(.005,.01,f)+.5*ac*i(.017,.005,f);q.x=ae(c.x-.5)-.35;q.y=c.y*9./4.-2.1;f=bc(q,.13)*10.;a=cf(q)*49.;m=bi(a);s=a-m;v=j(.85,.9,bk(m));for(be cc=0;cc<2;++cc,f+=.3){e=o(e,cn*(b*.5+.2),j(.09,.03,ae(f)));e*=1.+.7*ai(i(.01,.05,f));}p.y=(c.y-.7)*9./4.;f=bc(p,.43);a=atan(p.y,ae(p.x))/ch;a=a3(a,-.48);r=a;m=bi(a*=23.);s=a-m;a0=ci(m)*.2-.1*j(.0,.1,-f);v=j(.1,.2,ae(s-.5)-a0);f+=v*.007;r=(1.-v)*ai(i(.5,.3,r))*j(.25,.05,ae(s-.5));f-=.17*r;ac=i(.04,.0,-.4,f);e=o(e,cn*(b*.4+.4),ac);e=d7(e,p,31.,j(.1,.05,ae(f+.15))*v);e*=o(1.,1.-i(.1,.2,.4,ae(s-.5)-a0),ac*b);e*=1.-.7*ai(i(.03,.03,f))-.7*ai(i(.03,.03,f+.05))*v-.7*ai(i(.0,.02,f+.05))*v-.3*j(.04,.06,-f)*v+.5*i(.02,.0,-.1,f)+i(.0,.01+.07*r,f)+i(.0,.01,f+.03)*v;p.y-=.05;e=d8(e,p,0.);l e;}am(_43){c.y=(c.y+8.)/9.;l dh(c);}am(_44){c.y=(c.y*4.+4.)/9.;l dh(c);}am(_45){if(c.y<.01)++c.y;c.y=c.y*4./9.;l dh(c);}am(_59){h b=ad(c,d(5),.9,3.,4),t=dv(a9(c,4.,.01),d(7),.5,3.,5),u=a8(a9(c,4.,.05),d(9)),cd,al;k pt=dy(c,4.,.1+u*t*.05),e;d f=df(pt.x);cd=bk(aj(pt.yz));e=n(74,65,62)*(.8+.8*b*b);e+=i(.6,.3,u)*j(.3,.9,b*t)*.2;e*=1.-i(.5,.4,u)*j(.5,.7,t)*.1;e=o(e,n(86,74,78),i(.5,.1,b)*i(.7,.3,cd)*.7);e=o(e,n(105,90,70),i(.3,.1,t)*i(.3,.3,cd)*.3);al=i(.015,.005+.015*u,pt.x)+i(.4,.1,u*t)*.4;e*=1.-b*j(.015,.05,pt.x)*.7;e*=1.+al*b*(f.y-.5)*.7;e*=.9+.2*cd;e*=.9+.2*b6(a8(c-pt.yx,d(5)));l e;}am(_46){k e=a6(c).a1;c-=.5;e=d8(e,c*.9,.02);l e;}k cv(d c){c*=1.5;h b=ad(a9(c,7.,.02),d(9),.7,3.,2),t=b,u=a8(c,d(13)),a,s,f;k e=a6(c).a1;d p;p.x=ae(c.x-.75);p.y=a3(c.y-.58,0.)*1.15;a=atan(p.x,p.y)/ch;s=aj(a*7.+.5);f=bc(p,.45);f-=.06*j(.4,.33,c.y);f-=.05*j(.15,.07,ae(s-.5))*step(.63,c.y);f=b0(f,c.y-.107);if(c.y<.6)f=b0(f,ae(p.x-.493)-.113);f=b0(f,bc(p,.6)+.044*j(.48,.43,c.y));e=o(e,n(144,125,115)*t,ak(f-.1,.005));e*=1.-.3*i(.12,.11,.1,f)+.5*i(.1,.005+.015*u*u,f);l e;}am(_33){l cv(d(5,0)/6.+c*d(1,4)/6.);}am(_34){l cv(d(1,0)/6.+c*d(4)/6.);}am(_35){l cv(c*d(1,4)/6.);}am(_36){l cv(d(5,4)/6.+c*d(1,2)/6.);}am(_37){l cv(d(1,4)/6.+c*d(4,2)/6.);}am(_38){l cv(d(0,4)/6.+c*d(1,2)/6.);}am(_60){h b=ad(c,d(13,1),.7,2.,3);k e=a6(c).a1*.7;e*=1.-ai(j(.4,1.,b));l e;}k d9(d c,d s){h b=ad(c,d(5),.9,3.,4),u=ad(c,d(31,3),.5,3.,3),t=.75+b*b;d p=c;k e=a6(c).a1;if(c.y<.38)e=o(n(92,43,15),n(66,44,33),j(.1,.05,c.y))*t*(.5+.5*j(.0,.35,c.y));e+=b*s.y*ai(i(.32,s.x*.015,c.y))+.3*"
"b*i(.34,.05,c.y);e*=1.-i(.38,.005+b*b*.03,c.y)+3.*i(.15,.2,c.y)*(u-.5);l e;}am(_61){h b=ad(c,d(5),.9,3.,4),t=.75+b*b,f,bf,m;d p=c;k e=d9(c,d(1));p.x=mod(p.x,1./7.)-.07;p.y-=.21;f=bc(d(.75*p.x,dd(p.y,.1)),.033);bf=ak(f,.005);f=bc(d(.75*p.x,dd(p.y+.005,.09)),.033);m=ak(f+.015);e=o(e,n(83,81,66)*t,(bf-m)*j(.1,.3,c.y));e*=1.-j(.17,.25,c.y)*m;e+=ai(i(.0,.015,f))*i(.32,.03,c.y);e*=1.+3.*pow(i(-.01,.03,f),4.)*i(.09,.03,c.y);f=bc(d(.75*p.x,dd(p.y+.03,.1)),.033);e*=1.-ak(f+.01,.02)*(1.-bf);if(c.y>.09&&c.y<.3)e=cm(e,d((ae(p.x)-.035)*36.,aj(c.y*36.)-.5),.1);l e;}d fb(d p){p.x=ae(p.x);d q=p,v;q.y-=.5;h f=bc(q,.35),al,e;v=q/.35;q.y+=.25;q.x-=.15;f=a2(f,av(q,d(.09,.05))-.1);al=c3(q,d(.15,.1))/5e1;e=.1+dot(d(v.y,bv(bd(1.-cw(v)))),d(.3,.3));q.y+=.2;q.x=p.x;e=a3(e,bd(.4-au(q)));e+=.15*i(.0,.1,al)-.1*ak(al+.12,.15);f=a2(f,av(q,d(.15-j(-.15,.15,q.y)*.07,.03))-.09);e*=1.-j(.05,.25,q.x)*j(.2,.1,ae(q.y+.12));q.y-=.06;e-=.5*ak(c3(q,d(.05-j(-.1,.1,q.y)*.03,.06))/1e3+.03,.05);l d(bd(e),ak(f,.02));}am(_62){h b=ad(c,d(9),.7,2.,4),t=.75+b*b;k e=d9(c,d(4,.3));d p=c,s;p.x=mod(p.x,.2);p-=.1;s=fb(p*5.);l o(e,o(k(.5,.4,.3),k(.95,.8,.55),t)*t*s.x,s.y);}am(_54){c.y-=1./32.;d p=dt(c.yx,d(8)),q=aj(p),v=a9(c,31.,.002),cd=p-q;h b=ad(a9(c,5.,.02),d(7),1.,3.,4),t=.8+.8*b*b,u=a8(c+dq(bk(cd)*64.),d(23)),ac=ad(c,d(9),.7,3.,4),f=ad(v,d(63),.7,3.,4),r=ad(v-d(0,.002),d(63),.7,3.,4),af=f-r,al=du(q,.03+.03*b6(u)).z,a5=bk(cd);k e=o(n(91,61,42),n(70,30,15),al*b);e=o(e,n(70,48,35),j(.5,.6,ac))*t;e*=1.+af*(.1+u+i(.6,.1,ac))*(1.-al)-t*j(.7,1.,al)*a8(c,d(13))+.5*b*i(.3,.3,al);f=ad(v,d(23),.5,2.,4);e*=1.-.2*j(.6,.7,f)*a5+.3*i(.6,.05,f)*a5*u;e*=.9+.2*a5*(1.-al);e*=.9+.4*pow(dv(c-bk(cd/8.),d(5),.6,2.,4),4.);l e;}bj(_55){k e=a6(c).a1;c=aj(c*2.+d(8,3)/32.);h u=a8(c,d(19)),ac,r;d p=c-.5,q=d(3,17)*d(cf(p),au(p)+(u-.5)*.03),al;q.x+=bi(q.y)*co;q.x*=bi(q.y);r=au(al=aj(q)-.5);ac=i(.5,1.,4.5,q.y)*u;e*=1.-ac*ai(i(.3,.2,r))*5.*al.y-ac*j(.4,.5,r);e*=j(.9,1.2,q.y);l j(.5,1.1,q.y)*ah(e,1);}ao _56(){d p=UV-dn(a4.x*d(3,5));h b=ad(p,d(13),.6,2.,4);ah e=a6(UV);an=ah(o(b*b*k(3,.4,0),e.a1,e.w)*bo(),1);}bj(_57){k e=a6(c).a1;c=aj(c*2.+7./32.);h b=ad(c,d(5),.6,3.,4),r=au(c-.5)+b*b,ac=j(.2,.6,r);l ah(o(b*k(.5,.2,.0),e,ac),ac);}ao _58(){ah e=a6(UV);h b=ad(a9(UV-a4.x*d(1,3)/2e2,a4.x*.2,4.,.04),d(7),.6,3.,4);e.a1=o(b*b*n(199,19,9)+j(.78,1.5,b),e.a1,e.w);an=ah((e.a1+(1.-e.w)*cb(Ref)*.2)*bo(),1);}bj(_29){h b=ad(c-=.5,d(5),.9,3.,4),t=.75+b*b,u=a8(a9(c,7.,.02),d(17)),r=au(c),a0=r>.4?38.:r>.32?28.:16.,a=aj(atan(c.y,c.x)/dp),m=bi(a*a0),b7=ae(ae(r-.41-u*.002)*1e2-6.),ac=j(1.5,1.4,b7),di[]=h[](1.,3.,-.145,-1.,2.,.166),f,ea,s;d p=c;k e=n(78,68,63);e*=1.+.5*ai(i(.49,.005+.015*u*u+.015*b,r));e=o(e,n(83,52,47)*(.6+.4*u*u),ac)*t;e*=1.-.5*i(1.5,.5,b7)+b*i(1.,.5+.5*u,ae(r-.418)*1e2-5.)-b*i(.5,.08,aj(a*a0+.5))*ac+b*i(.5,.1,aj(a*a0+.55))*ac;ac=j(.34,.33,r);e=o(e*(1.-.5*ac),n(83,52,47)*t,u*b*ac);e=o(e,n(112,86,31)*t,ac*ai(i(.1,.15,.45,b)));e=o(e,n(77,66,77)*t,ac*j(.5,.8,b)*.5);e*=1.-.7*i(.27,.34,.35,r);b7=r+u*.004;ac=r>.21&&r<.31?1.:0.;e*=1.-i(.325,.005,b7)-i(.31,.005,b7)-b*ai(i(.29,.005,b7))-b*ai(i(.23,.01,b7))-.5*ai(i(.21,.02,b7))+ai(i(.3,.01,b7))*b+ai(i(.22,.01,b7))*b-b*i(.5,.07,aj(a*a0+.5))*ac;if(r<.23)m+=37.;if(r<.31)m+=73.;if(r<.31)m+=91.;e*=o(1.,.9+.2*ci(m),ac);ac=j(.01,.0,ae(r-.411)-.039);m=bi(a*72.);p*=bh(m*5.);s=0.;f=1e6;be cc=0;for(;cc<6;cc+=3){f=dr(f,ea=dot(p,cl(d(di[cc],di[cc+1])))+di[cc+2]);s+=s+h(ea>0.);}if(s==3.)++m;else m+=66.*s;m=ci(m);e=o(e,t*n(90,80,75),ac);e=o(e,t*n(127,111,88),m*b*ac);e*=o(1.,.7+.6*bk(m),ac);e*=1.-ac*ai(i(.0,.006,f))*b+ac*ai(i(.006,.006,ae(f)))*b*.5;m=bi(a*4.);p=ae(c*bh(m*90.+45.));f=1e6;for(cc=0;cc<2;++cc,p=ae(p*bh(45.)))f=dr(f,ae(au(p-d(0,.12))-.16));ac=j(.21,.2,r);b7=cy(cy(f,.012),.001);e*=1.-j(.21,.2,r)*ak(.012-f)+b*ac*ai(i(.005,.005,f))-.5*ac*ai(ak(b7-.001,.001));l ah(e,(1.-j(.21,.15,r)*ak(.028-f,.02))*j(.07,.087,r));}bj(_30){d p=aj(UV)-.5;h b=ad(bh(a4.x*333.)*p/(.8+.2*sin(a4.x*61.)),d(53),.7,2.,4);ah e=ah(1.-b*k(0,.3,1),1),bu=a6((bh(a4.x*30.)*p/(.8+.2*sin(a4.x*1.26)))+.5);e.a1=o(e.a1,bu.a1,bu.w);bu=a6(UV);e.a1=o(e.a1,bu.a1,bu.w)*bo();l e;}h fc(d p){p=c8(p,8.);h f=bc(p,.41);f=a2(f,av((p-d(.34,0))*bh(45.),d(.1)));f=a3(f,p.x-.45);l f;}am(_31){d p=c-.5,q;h b=ad(c,d(9),.7,2.,4),t=.8+.8*b*b,r=bc(p,.41),f=fc(p),af=dFdy(f)/.004,ac=ak(f+.01,.007),a=cf(p),al,u,x,z;k e=a6(c).a1,a0=n(155,135,115)*t;e*=1.-(.5*-af+.5)*j(.03,.0,f);e=o(e,a0,ac);al=au(p)*9.;q.x=a*bi(al+1.)*3.;q.y=aj(al);u=i(.5,.2,a8(a9(c,7.,.03),d(41)));u=ad(q,d(3,9),.5,2.,4)*i(.5,.5+.5*u,q.y);e=o(e,n(100,85,80)*b6(u)*b,ak(r+.15,.02));q=p;q.x=ae(p.x);al=a3(a3(f,ev(q,-d(.08,.4))),ae(r+.06)-.09);z=av(q-d(0,.3),d(.01,.03));al=b0(al,z-.02);x=ak(-al,.01);e*=1.+af*ac*j(.02,.0,-f)*j(.01,.0,-r)+x*i(.035,.015,-r)+.5*x*i(.13,.01,-r)+.7*i(.08,.007,z)*(1.-x)-.7*x*ai(i(.01,.04,-r))-.6*x*ai(i(.13,.06,.03,-r))-.5*x*ai(i(.12,.02,-r))*ac-.9*ai(i(.12,.15,.2,-r))*ac-.5*ai(i(.0,.05,al));e+=k(.8,.8,1)*pow(bd(1.-au(c-d(.41,.59))/.35),8.);q.x=a;q.y=j(.05,.12,-r);al=da(q,d(37,1)).z;ac=i(.085,.035,-r)*x;e*=1.+.5*ac*i(.0,.2,al)-.3*ac*i(.3,.3,al);al=av(p+d(0,.33),d(.01,.03))-.03;af=dFdy(al)/.004;e=o(e,a0*(.4+.8*j(.25,.41,-p.y)),z=ak(al,.01));e*=1.+.7*i(.005,.01,al)*af-.5*i(.0,.01,.05,al);al=av(q=p+d(0,.35),d(.01,.015))-.01;af=dFdy(al)/.004;e*=1.+.5*i(.005,.01,al)-.5*ai(i(.0,.01,al));e+=k(1,.7,.5)*pow(bd(1.-au(q)/.11),8.);l e;}ao _32(){k e=a6(UV).a1*bo();d c=aj(UV)-.5,p=c;h t=mod(a4.x*2.,7.),m=bi(t),f=1e6;if(m==0.)f=ex(p*2.4+d(0,.05));if(m==1.){p.x=eo(p.x,.1,-1.,1.);f=av(p,d(.02,.15))*2.;}if(m==2.){f=a2(f,av(c9(p,.0)+d(.13-j(-.3,.3,c.y)*.17,0),d(.02,.15)));f=a2(f,av(p+d(0,.07),d(.07,.02)))*2.;}if(m==4.)f=cq(p*1.8,.5);else f=cy(f,.005);an=ah(e+ak(f,.02)*aj(-t)*k(.5,.05,.05),1);}am(_63){h b=ad(c,d(13),.9,3.,4),u=ad(c,d(7),.9,3.,4);k e=o(n(60,50,50),n(87,47,37),ai(j(.7,.25,u)))*(.7+.8*b*b),g;c=a9(c,31.,.003);c5(g,c,bv(j(.0,.9,a8(p[m],d(93)))));e*=1.-(g.y+.4)*ai(b*g.z)*g.z;l e;}am(_67){h b=ad(c,d(5),.9,3.,4);k e=n(67,64,63)*(.6+.5*b),g;c=a9(c,31.,.003);c5(g,c,bv(a8(p[m],d(53,93))));e*=1.-.3*g.y*g.z*g.z;l e;}bj(_23){c=a9(c,7.,.01);h b=ad(c,d(9),.7,2.,4),u=ad(c,d(13),.5,2.,4),ac=j(.6,.9,ad(a9(c,5.,.03),d(11),.6,2.,4));k e=n(127,70,55)*(.85+.3*b);e*=1.-.2*ai(j(.3,.0,b*b))-.2*ac-.3*j(.6,.77,u)+.3*j(.5,.9,b);e+=.5*ai(j(.5,1.,ad(c,d(17),1.,2.,3)));l ah(e,1.-ac);}ao _25(){ah e=a6(UV);d c=aj(UV);c.y-=.2*a4.x;h b=ad(a9(c,7.,.02),d(5),.9,2.,4);an=ah(o(n(25,10,8)*b,e.a1,e.w)*bo(),1);}am(_24){d p=c-.5;p=a9(p,17.,.007);p.x*=2.-c.y*1.5;h b=ad(c,d(9),.7,2.,4),u=a8(c,d(7)),f=au(p),s;k e=_23(c).a1;s=aj(f*=13.);if(f<=6.){e*=1.-pow(j(6.,.5,f+b*b),6.);u=j(.3,.7,u);e*=1.-u*b*i(.4,.2,s)+u*b*i(.6,.4,s);}l e;}am(_26){c=a9(c,13.,.003);h b=ad(c,d(7),.9,3.,4),u=ad(c,d(5),.5,2.,4),t=.5+b;k e=n(80,38,34),v=dg(c,d(23));e=o(e,o(n(180,125,118),n(165,78,51),u),b*i(.0,.4+u*.4,v.z))*t;l e;}am(_27){h b=ad(c,d(7),.9,3.,4),t=.8+.4*b,m,r=.7,af,ac;k e,v=dg(c,d(23));d p=v.xy/r,q=c+v.xy/23.;m=bk(aj(q)*3.3);e=o(n(155,55,55),n(200,166,155),j(.75,.45,q.y))*t;ac=i(.5,.5,au(p));af=dot(d(-p.y,bv(bd(1.-cw(p)))),d(.6+m*.3,.3));e*=1.-b*.8*j(.5,.1,v.z)+b*ac*af;e*=t*t*t*t;l e;}am(_28){h b=ad(c,d(13),.9,3.,4),t=.4+b*b,u=b6(a8(a9(c,12.,.02),d(48))),m,r,af,ac;k e=n(60,50,46)*t,v=dg(c,d(17));m=bk(aj(c+v.xy/17.));r=.4+.3*m;d p=v.xy/r;ac=a2(j(1.1,1.,au(p)),j(.0,.15,v.z));af=dot(d(-p.y,bv(bd(1.-cw(p)))),d(.1+m*.2,.3));e+=b*ac*af*u;u=b6(ad(a9(c,13.,.01),d(23,43),.5,2.,3));e*=1.+(1.-ac)*i(.4,.4,u);l e;}am(_65){h b=ad(c,d(7),.9,3.,4),u=ad(c,d(3),.5,3.,4);k e=o(n(103,56,53),n(73,58,71),j(.1,.7,u))*(.75+b*b);l e;}am(_66){h b=ad(c,d(13),.9,3.,4),u=a8(a9(c,5.,.05),d(9)),f=b6(aj(c.x*4.)),ac=j(.1,.15,f)*j(1.,.99,c.y);k e=n(51,44,44);e=o(e,n(73,55,52),j(.2,.2,b)*u*ac);e=o(e,n(69,60,66),j(.7,.1,b)*b*ac);e=o(e,n(99,77,77),j(.1,.5,u)*u*ac*b*b*.3);e*=.6+.3*b+.3*b*b;e*=1.+.9*ai(i(.21,.02+.1*u,f+b*.05))*ac*b;e*=1.-ai(j(.49,.5,ae(c.y-.5)));e*=1.-j(.05,.2,f)*j(.16,.1,f);e*=1.+i(.99,.007,c.y);l cm(e,d(f-.4,aj(c.y*8.)-.5),.07);}k eb(d c,h s){h b=ad(c,d(3,1.+s+s),.7,2.,4),f=b6(c.x),ac;c.y*=2.;k e=o(n(71,60,58),n(110,88,77),j(.1,.05,f))*(.7+.6*b);e*=1.-j(.05,.0,c.x)*(1.-b*b);e*=1.+.5*i(.05,.02,c.x);d p=d(f-.35,aj(c.y*s)-.5);ah a0=dw(p,.11);ac=ak(a0.w);e*=1.-.7*dx(p,1.1)*(1.-ac);e=o(e,(a0.y>.0?n(128,105,88):n(200,111,66)*j(-.2,.7,a0.z))*(.4+2.*b*pow(bd(cp("
"a0.yz*.7)),4.))*(1.-.6*i(-.1,.4,a0.y)),ac);l e;}am(_39){l eb(c,4.);}am(_40){l _39(c.yx);}am(_41){l eb(c,1.);}am(_49){h b=ad(c,d(40,5),.9,3.,4);k e=n(110,110,98)*(.8+.8*b*b);if(c.y<1./4.)e*=.5;e*=1.-.4*j(.4,.0,b)+.5*j(.02,.0,c.y)+.2*i(.24,.01,c.y);l e;}am(_50){d p=c,q;p.y*=22.;q=aj(p);h b=ad(c,d(3,23),1.,2.,6),u=ad(c,d(3,33),.7,3.,4),cd=bk(p.y-q.y);k e=n(92,67,53)*(.8+.8*b*b);e*=1.-ai(j(.1,.0,a2(q.y,1.-q.y)))*b;e*=1.-.2*smoothstep(.3,.7,u);e*=.8+.3*b*cd;l e;}am(_51){h b=ad(c,d(13),.9,3.,4),x=c.x*16./3.;k e=a6(c).a1*j(.15,.21,c.x);if(x<1.)e=n(59,48,49)*(.7+.6*b);e*=1.+.5*i(.05,.05,b6(x));l cm(e,d(ae(c.x-3./32.)-.07,mod(c.y,.1)-.05),.004);}am(_52){d p=dt(c,d(6,4)),q=aj(p),b5=q;h b=ad(a9(c-=.5,5.,.03),d(13),.9,2.,3),u=a8(c,d(73,7)),t=(.75+b*b)*(.8+.4*ek(c.x*93.)),r;k e=k(.25*t);b5.y+=b5.y*2.-.01-.03*u;r=au(b5-=clamp(b5,d(.49,.5),d(.51,3)));e*=1.-.7*b*ai(j(.07,.03,ae(r-.5)))+.5*b*i(.35,.1,r)*ai(j(.2,.1,q.y))-.3*ai(j(.8,1.,q.y))-.3*(j(.3,.1,q.y))*j(.4,.6,r)+.2*ai(j(.5,.1,q.y))*j(.45,.4,r);l e;}am(_69){h b=ad(c,d(5),.9,3.,4),a5=aj(c.y*10.);k e=o(n(53,48,42),n(38,38,36),b)*(.6+b*.8);e*=1.+.5*ai(i(.25,.25,a5))+.5*ai(i(.65,.35,a5))-.6*ai(i(.5,.5,a5));l e;}am(_68){h b=ad(c,d(7,3),.9,3.,4),a5=c.y+b*.04;k e=o(n(50,40,34),n(93,92,88),b*b);e*=1.+.9*j(.07,.04,ae(c.y-.44))-.4*j(.08,.05,ae(c.y-.7))-.5*i(.34,.05,c.y)-.3*i(.7,.04,c.y)+.7*i(.01,.03,c.y)+1.5*i(.89,.1,a5)+1.3*i(.5,.04,a5)+.9*i(.18,.04,a5);l e;}am(_70){h b=ad(c,d(5,3),.9,3.,4);k e=o(n(74,66,55),n(99,90,78),b*b);c.x*=2.;d p=ck(c,d(.5,.625),d(1.5,.625));h f=au(p-c),ac=j(.22,.20,f),af=1.-.15*ac-.5*j(.7,.9,c.y)*ac-.3*(df(f).y-.5)*i(.2,.03,f)-.3*ai(i(.17,.03,f))-.5*j(.05,0.,c.y)-.3*i(.33,.05,c.y)+.7*j(.93,1.,c.y)+.7*i(.31,.04,c.y);l e*af;}bj(_71){h b=ad(c,d(1,5),.4,3.,4);k e=o(n(56,49,43),n(142,136,136),b);c=.5-ae(c-.5);c.y*=4.;h a=i(.0,.1,au(c-ck(c,d(.41,.5),d(.42,3.5)))),f=c7(c),af=1.-.7*a3(0.,1.-f/.15);af*=1.-.8*j(.24,.31,a2(f,c.y-.1));e+=n(80,80,20)*a;l ah(e*o(af,2.7,a),a);}bj(_74){h b=ad(c,d(1,5),.4,3.,4);k e=o(n(56,49,43),n(142,136,136),b);c=.5-ae(c-.5);c.y*=8.;h f=au(c-ck(c,d(.27,.3),d(.27,7.7))),a=i(.0,.17,f),af=1.-.5*i(.17,.07,f);e+=n(80,80,20)*a;l ah(e*o(af,2.7,a),a);}bj(_72){d p=ae(c-.5);h b=ad(c,d(1),.4,3.,4),r=au(p),a=j(.37,.33,r)*(.5+2.*b),af=1.+.0*j(.08,.03,ae(r-.41));k e=o(n(56,49,43),n(142,136,136),b);af=o(af,7.,j(.44,.1*b,r));af*=1.-.5*ai(i(.46,.04,r));af*=1.-.4*ai(i(.36,.04,r));l ah(e*af,a);}bj(_73){h b=ad(c,d(5),.9,3.,4),f=de(d3(c-=.5,.35),ae(bc(c,.4)),.02),a=pow(ak(f-.02,.15),8.),bf=a2(a3(av(c,d(.46)),-bc(c,.51)),ae(bc(c,.44)));k e=n(76,62,47)*(.8+.8*b*b);e*=1.+(b+.5)*ak(ae(bf)-.01,.01);e*=1.-j(.1,.05,f)*ak(bc(c,.4));l ah(e+1.*k(1,1,.3)*a,a);}bj(_1){c-=d(.48,.5);h f=cq(c,0.),b=au(c)-.47;k e=1.-k(.5,1,1)*ak(a3(.007-f,b+.04));l ah(e,1)*ak(b);}am(_2){c-=.5;d r=d(dFdx(c.x),dFdy(c.y));c/=r/cj(r);c*=.8;c.y-=.03;h x=ae(c.x),b=ad(c,d(31,5),.7,2.,3),t=.8+.8*b*b,f=cq(c,1.),al=cq(c+d(0,.002),1.),af=(al-f)*5e2+.5;k e=k(.3*t,0,0)*ak(f,.004);e*=1.-ai(j(.0,.3,x))-.5*j(.1,.3,ae(c.y-.1));e+=+t*.2*i(.0,.01-.01*x,f)*i(.1,.2,c.y)*j(.3,.2,x)*af+t*.5*j(.004,.0,f)*j(.07,.1,c.y)*i(.23,.1,x)*k(.9,.9,1)+t*.4*i(.005,.005,f)*j(.2,-.1,c.y)*j(.3,.2,x)*bd(-af);l e;}h ec(d c,h s,h m,h fd){m=(c.y-ai(ae(c.x-.5))*fd)*s-m;l 2.*i(.5,.4,m)*(aj(m)-.5);}bj(_83){h b=ad(c,d(5,9),.9,3.,4),t=.8+.8*b*b,u=ad(c,d(5,9),.9,3.,2),x=ae(c.x-.5),f;k e=n(77,60,44)*t;d p=c*d(1,2)-d(.5,.7);e*=1.-.55*ak(b0(bc(p,.3),cq(bh(45.)*p*.8,0.)-.01));e*=1.+ai(j(.6,.9,b))+i(.2,.5,c.y)*i(.2,.3,x)*ec(c,4.,.2,4.);for(h a7=6.;a7<9.;++a7)e*=1.+i(.8,.5,c.y)*i(.2,.3,x)*ec(c,12.,a7,1.);f=c.y-.81-ai(bd(x*4.))*.09;e=o(e,n(82,66,60)*t,j(.0,.01,f));e*=1.-.5*(i(.01,.02,f))+.5*(i(.02,.01,f));f=.15*(1.-c.y);f=av(c-.5,d(.49)-f)+u*.1*bv(1.-c.y)-f;l ah(e*ai(ak(f+.01,.05)),ak(f));}bj(_84){ah e=a6(c);if(e.w<.5)discard;e.a1*=ai(bo()*.5);l e;}ah ce(k p,k u,h s){p*=s/h(textureSize(Texture0,0).x);u*=u;ah e=ah(0);for(be m=0;m<3;++m,p=p.yzx)e+=a6(p.yz)*u[m];l e/cp(u);}ah ce(k p,h s){l ce(p,Nor,s);}ah ce(h s){l ce(Pos,s);}ao _85(){k u=Nor*Nor;an=ce(4.)*(Clr+.5*u.z*u.z*sign(Nor.z))*1.5;}ao _86(){_85();}ao _101(){an=ce(16.);an.a1*=e2()*k(1,.95,.9);}ao _102(){an=ah((cb(Ref,15.)*1.4+.3)*a4.yzw,1);}ao _103(){k r=Ref;h s=17.;if(a4.y<1.){s=9.;mat2 a=bh(a4.x*90.);r.xy*=a;r.yz*=a;}s=cb(r,s);an=ah(ai(j(.1,.9,s))*1.5*aj(a4.yzw)+.5*ai(j(.9,1.,s)),0);}ao _104(){k r=Ref;r.xz*=bh(a4.x*9.);an=ah(cb(r));}ao _105(){d c=d(Pos.x/24.-.5,au(Pos.yz));an=a8(c,d(3))*j(1.,.9,c.x)*ah(.25,0,0,0);}h fe(h b,h a,h a7,h p){l(ae(aj(a4.x*a7+p-.25)-.5)*4.-1.)*a+b;}ao _106(){k r=Ref;mat2 a=bh(a4.x*90.);r.xy*=a;r.yz*=a;an=cb(r,22.)*bd(fe(-.3,1.3,.3,0.))*ah(.3,.55,.25,0);}am(_107){h b=ad(c,d(7),.5,2.,4),t=.8+.8*b*b,u=a8(c,d(2));l o(n(5,77,55),n(8,122,188),j(.5,.1,u))*ai(1.-u)*t*2.*(1.+i(.2,.05,b*b));}ao _108(){k p=Pos-k(4.5,0,2),r=Ref,u=Nor;r.xz*=bh(a4.x*9.);p.xy*=bh(a4.x*33.);u.xy*=bh(a4.x*33.);an=ce(p-a4.x*16.,u,16.)+cb(r);}ao _109(){k u=cl(Nor),p=Pos;p.z-=24.;h al=i((ad(Pos.y/48.+a4.x*7.4,13.,.6,2.,4)-.5)*.15,.03,aj(j(-8.,32.,Pos.z)-a4.x*1.3)-.5);an=ah((ce(16.).a1*(u.z*.5+.5)*(.2+.8*j(4.5,6.5,au(p)))+al*al)*a4.yzw*2.,1);}h ed(h e,h s,h x){h a=a3(fwidth(x)*2./s,1.);l i(e,s*a,x)/a;}ao _87(){_85();k p=Pos,q;p.y=ae(p.y);q=p-k(7,3.5,-21);q.z*=1.3;h r=au(q);an.a1*=1.+bd(p.z+30.)*j(4.,5.,au(p.xy))+.5*bd(p.z+15.)+.3*i(3.,.5,r)+.7*ed(-15.,.5,p.z)-ed(-14.5,.5,p.z)-j(3.,2.5,r)*(.6+.4*q.z/r);q=p-k(8,0,-23);q.z*=.7;r=au(q);an.a1*=1.-j(2.,1.,r)*(.6+.4*q.z/r);}bj(_88){c*=d(256,64);c.y+=2.;h f=bc(c-d(81,30),11.);f=a3(f,c.x-80.);f=a3(f,-bc(c-d(84,26),9.));f=a2(f,av(c-d(73,37),d(4,9))-4.);f=a3(f,-av(c-d(73,37),d(0,7))+1.);f=a2(f,av(c-d(91.5,47),d(4,19))-4.);f=a3(f,-av(c-d(91.5,47),d(0,17.5))+1.);f=a2(f,av(c9(c,111.)-d(105.+j(23.,50.,c.y)*3.,43),d(3.5,19)));f=a2(f,av(c-d(111,32),d(4,3)));f=a2(f,av(c-d(126,37),d(3,13)));f=a2(f,av(c-d(125.5+j(23.,50.,c.y)*10.,44),d(3.5,6)));f=a2(f,av(c-d(136.5-j(23.,50.,c.y)*9.,32),d(3.5,8)));f=a2(f,av(c-d(148.5,37),d(7,13)));f=a3(f,-av(c-d(155,33),d(6,3)));f=a3(f,-av(c-d(155,43),d(6,2)));f=a2(f,av(c-d(168,37),d(3.5,13)));f=a2(f,av(c-d(178.,37),d(3.5,13)));f=a2(f,av(c-d(188,37),d(3.5,13)));f=a3(f,c.y-50.);l ah(ak(f,.8),0,0,bk(c*511.));}ao _89(){k e=a6(UV*2.).a1*step(.5,aj(a4.x*.5));e=o(e*bo(),k(.5,0,0),i(aj(a4.x*2.),1./64.,aj(UV.y)));an=ah(e+cb(Ref)*.25+a6(UV+bk(a4.xx)).w*.1,1);}ao _90(){d c=aj(UV);c.x+=a4.x/13.;h b=ad(c,d(3,7),.9,2.,4),a7=c.y;an=ah(2.*n(95,85,80)*a7*a7*a7*a7*o(1.,b,.5),0.);}ao _91(){an=ah(cb(Ref)*n(133,111,111),0);}h star(d p,d e,h s){p-=e;h a=j(.3,.5,ae(aj(cf(p/=s)*8.+bk(e))-.5));l j(.9,.6,pow(cw(p),.0625)-a*a*.006);}ao _92(){an=ah(2,2,2,0)*pow(star(UV,d(.5),1.),2.);}h ee(d c,h s){d p=c;p.y+=p.y-a4.x*s;c.x+=(eq(p.y*5.)-.5)*1.5*ai(c.y);h u=ad(a9(p,7.,.02),d(9),.7,2.,4),a5=j(.9,.03,c.y),b=av(c-d(.5,.15),d(0,.3));l ai(ak(b+u*ai(1.2-a5)-.13,.15));}ao ef(h s){d c=aj(UV);an=(ee(c,s*.6)+ee(c,s))*ah(2.5,1,.35,0);}ao _93(){ef(2.5);}ao _94(){ef(1.);}ao _0(){h af=dot(Nor,cl(k(2,0,8)));af=af*.4+.7;d c=ez(Pos,e0(Nor));k e=k(.5);e*=e1(k(aj(co*a4.w+.25),1.,1.));an=ah(e*af,1);}ao _6(){ah e=a6(UV);an=ah(e.a1*o(bo(),k(1),e.w),1);}ao _76(){ah e=a6(UV);h r=au(aj(UV)-.5);h s=o(.4,8.,aj(a4.x*1.5));an=ah(e.a1*bo()+n(240,130,5)*i(.1,.05,r/s)*j(.37,.32,r),1);}ao _5(){k e=a6(UV).a1;an=ah(e*bo(),1);}ao _7(){ah e=a6(UV);e.a1*=1.+e.w*cb(Ref);an=ah(e.a1*bo(),1);}ao _79(){an=ah(0);}ao _80(){k f=cl(Pos-Cam.a1);f.z=f.z*4.+2.;d c=cl(f).xy*2.;h b=j(.2,1.,ad(c-a4.x*d(.1,.2),d(5),.5,2.,6));c.y*=1.5;h s=j(.3,1.,ad(c-a4.x*d(.1,.18),d(5),.6,2.,6));an=ah(k(b,0,0)+n(80,30,8)*s*s*2.,1);}ao _81(){d c=a9(UV/8.,a4.x*.5,2.,.05);h b=ad(c,d(7),.9,2.,4);k e=n(91,22,14)*(.2+1.6*b);e=o(e,n(144,44,0),i(.6,.2,ad(c,d(3),.7,3.,4)));e=o(e,n(244,144,66)*b*2.,ai(i(.55,.25,ad(c,d(11),.5,2.,4))));an=ah(e*bd(cj(bo())),1);}ao _82(){_81();}ao _3(){an=texture(Texture0,(.5+UV*127.)/128.,2.5);an.a1*=.7+.3*a8(UV,.5/fwidth(UV));}ao _4(){an=a6(UV)*Clr;}bj(_95){c.y*=2.;h b=ad(c,d(5),.9,2.,4),a0=.5+b,t=.8+b*.4,f=c.y-1.05;k e=n(55,44,37)*t;d p=c;f=de(f,av((c-d(.5,1.5))*bh(45.),d(.2))-.1,.3);f=a3(f,.03-p.y);e=o(e,cm(e,c-d(.5,1.5),.1),8.*b);p.x=a2(p.x,1.-p.x);if(p.y>.5)p.y-=.5;e=o(e,cm(e,p-d(.22,.31),.04),2.*t);e*=1.+ai(j(.5,1.,b))+df(f).y*i(.0,.05,f)*(1.+11.*j(1.5,2.,c.y))*b+a0*i(.93,.05,c.y)+a0*i(.4,.05,c.y)+a0*i(.1,.05,c.y)-.5*i(.97,.05,c.y)-.5"
"*ai(i(.7,.05,c.y))-.5*ai(i(.2,.05,c.y))-.5*i(.66,.1,c.y)-.5*i(.45,.03,c.y)-.5*i(.15,.1,c.y)-i(.01,.03,c.y);l ah(e,ak(f,.03));}ao _96(){an=a6(UV);if(an.w<0.5)discard;an*=Clr*1.5+.3;}ao _97(){d c=d(cf(Pos.xy),j(8.,128.,Pos.z));c.x=aj(c.x*3.-a4.x*2.2);c.y*=4.;h a7=star(c,d(0,.3),1.)+star(c,d(1,.3),1.)+star(c,d(.5,.4),1.5);a7*=a7;an=ah(a7,a7,a7,0);}bj(_98){h b=ad(a9(c,5.,.03),d(7),.5,3.,2);l(b*1.4+.3)*ah(1.2,.54,.06,0)+.5*i(.7,.2,b)*ah(1,1,1,0);}ao _99(){k p=Pos;p-=a4.x*6.4*sign(p.z-72.);an=ce(p,8.);}am(_100){c=a9(c,5.,.02);h b=ad(c,d(6),.8,2.,4),ac=ad(c,d(2),.6,2.,4),t=.8+.6*b*b;k e=o(n(36,33,30),n(168,177,168),.3+.7*ac)*t;l e;}ao _110(){d c=c8(UV-.5,4.);an=ah(1,1,0,1)*ak(a2(cy(au(c)-.4,.04),av(c,d(.25,.06))),.01);}ao _111(){an=ah(.4,.7,.3,1)*ak(dot(ae(UV-.5),d(4,3)/5.)-.2,.01);}ao _112(){d c=UV-.5;c.x=ae(c.x);h w=.15*bv(j(-.32,-.29,c.y))+.05*j(-.3,-.12,c.y)+.1*bv(j(-.12,.07,c.y))+.14*step(.07,c.y)-.1*ai(j(.07,.31,c.y)),f=b0(av(c,d(w,.3)),bc(c-d(0,.777),.5));an=ah(1,1,0,1)*ak(f-.01,.01);}ao _113(){d c=UV;h f=av(c-d(.35,.5),d(.1,.4));c.y=mod(c.y-.06,.28)-.16;f=a2(f,av(c-d(.51,0),d(.34,.04*j(.85,.7,c.x))));f=b0(f,av(c-d(.65,0),d(.02,1)));an=ah(1,1,0,1)*ak(f,.01);}ao _114(){d c=UV;c.x=mod(c.x,.33)-.16;h f=av(c-d(0,.52),d(.1,.4));f=b0(f,av(c-d(0,.65),d(.05,.23)));f=b0(f,ae(c.y-.2)-.02);an=ah(1,.5,0,1)*ak(f,.01);}ao _115(){d c=(UV-d(.55,.45))*bh(-45.);h f=ck(c,d(-.4,0),d(.2,0),.1);f=a3(f,-.3-c.x);c.y=ae(c.y);f=a2(f,av(c+d(.37,0),d(.05,.005)));f=a2(f,av(c+d(c.y+.1,-.18),d(.1,.05)));an=ah(1,0,0,1)*ak(f,.01);}ao _116(){d c=(UV-.5)*bh(45.);h f=av(c,d(.6,.03)),p=c.x*12.6+1.55,a5=sin(p)*.15,q=b6(aj(a5));f=a2(f,a3(cy(c.y-a5,.05-.1*q)*(1.+q+q),ae(c.x)-.6));an=ah(0,1,0,1)*ak(f,.01);}ao _117(){d c=c8(UV-.5,5.);h f=au(c)-.18,r=c.x-.33,a5=.04*i(.0,.3,r);c.x-=.33;c*=bh(-15.);f=a2(f,av(c,d(.04,2.*a5)));f=a2(f,av(c-d(0,a5*sign(r)),d(.14,a5)));an=ah(.77,0,1,1)*ak(f+.01,.01);}ao _118(){d c=UV-.5;h f=cq(c+d(0,.05),0.)-.01;f=a2(f,av(c8(c*bh(45.),4.)-d(.45,.0),d(.11,.02)));an=ah(0,.88,.95,1)*ak(f,.01);}"
;

// src/demo/data/shaders/vertex_shaders.glsl: 4853 => 2375 (48.9%)
static constexpr char g_vertex_shaders[] =
"#define c void\n"
"#define f vec4\n"
"#define j gl_Position\n"
"#define l location\n"
"#define n normalize\n"
"uniform mat4 MVP,View,World;uniform f Time,Cam;layout(l=0)in f e;layout(l=1)in f k;layout(l=2)in vec3 h;layout(l=3)in f o;out vec3 Pos,Nor,WNor,Ref;out vec2 UV,LUV;out f Clr;c d(){j=e;UV=e.xy*.5+.5;}c m(float v,float ac,float ad){j+=ac*MVP*f(h,0)*sin(6.28*(Time.x*ad+dot(e.xyz/v,vec3(1))));}c _0(){j=MVP*e;Pos=e.xyz;Nor=h;UV=k.xy;LUV=k.zw;Clr=o;Ref=n(reflect((e-Cam).xyz,h));}c i(){_0();Pos=floor(h)/4.;Nor=fract(h)*4.-2.;Ref=n(reflect((e-Cam).xyz,Nor));float a=k.z,u=cos(a),s=sin(a);Nor.xy*=mat2(u,s,-s,u);}c _1(){d();}c _2(){d();}c _3(){d();}c _4(){j=f(2.*e.x-1.,1.-2.*e.y,1,1);UV=k.xy;Clr=o;}c _5(){_0();}c _6(){_0();}c _7(){_0();}c _8(){d();}c _9(){d();}c _10(){d();}c _11(){d();}c _12(){_0();}c _13(){d();}c _14(){_0();}c _15(){d();}c _16(){d();}c _17(){d();}c _18(){_0();}c _19(){d();}c _20(){d();}c _21(){d();}c _22(){d();}c _23(){d();}c _24(){d();}c _25(){_0();}c _26(){d();}c _27(){d();}c _28(){d();}c _29(){d();}c _30(){_0();}c _31(){d();}c _32(){_0();}c _33(){d();}c _34(){d();}c _35(){d();}c _36(){d();}c _37(){d();}c _38(){d();}c _39(){d();}c _40(){d();}c _41(){d();}c _42(){d();}c _43(){d();}c _44(){d();}c _45(){d();}c _46(){d();}c _47(){d();}c _48(){d();}c _49(){d();}c _50(){d();}c _51(){d();}c _52(){d();}c _53(){d();}c _54(){d();}c _55(){d();}c _56(){_0();}c _57(){d();}c _58(){_0();}c _59(){d();}c _60(){d();}c _61(){d();}c _62(){d();}c _63(){d();}c _64(){d();}c _65(){d();}c _66(){d();}c _67(){d();}c _68(){d();}c _69(){d();}c _70(){d();}c _71(){d();}c _72(){d();}c _73(){d();}c _74(){d();}c _75(){d();}c _76(){_0();}c _77(){d();}c _78(){_0();}c _79(){_0();}c _80(){_0();}c _81(){_0();m(100.,3.,.1);}c _82(){_0();}c _83(){d();}c _84(){_0();m(30.,3.,.2);m(100.,3.,.7);}c _85(){i();}c _86(){i();}c _87(){i();}c _88(){d();}c _89(){_0();}c _90(){_0();}c _91(){i();}c _92(){_0();j+=MVP*f(Nor*mat3(View),0);}c _93(){_0();}c _94(){_0();}c _95(){d();}c _96(){_85();}c _97(){i();}c _98(){d();}c _99(){i();}c _100(){d();}c _101(){_0();WNor=n(mat3(World)*h);Ref=n(reflect((World*e-Cam).xyz,WNor));}c _102(){_101();}c _103(){_101();}c _104(){_101();}c _105(){_101();}c _106(){_101();j+=MVP*f(h+h,0);}c _107(){d();}c _108(){_101();}c _109(){_101();}c _110(){d();}c _111(){d();}c _112(){d();}c _113(){d();}c _114(){d();}c _115(){d();}c _116(){d();}c _117(){d();}c _118(){d();}"
;
