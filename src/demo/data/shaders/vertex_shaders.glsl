uniform mat4 MVP;
uniform vec4 Time, Cam;

layout(location=0) in vec4 P;
layout(location=1) in vec4 T;
layout(location=2) in vec3 N;
layout(location=3) in vec4 C;

out vec3 Pos, Nor, Ref;
out vec2 UV, LUV;
out vec4 Clr;

////////////////////////////////////////////////////////////////

void Generic() {
	 gl_Position = MVP * P;
	 Pos = P.xyz;
	 Nor = N;
	 UV = T.xy;
	 LUV = T.zw;
	 Ref = normalize(reflect((P - Cam).xyz, N));
}

void FS() {
	gl_Position = P; UV = P.xy * .5 + .5;
}

void UI() {
	gl_Position = vec4(2. * P.x - 1., 1. - 2. * P.y, 1, 1);
	UV = T.xy;
	Clr = C;
}

////////////////////////////////////////////////////////////////

// $protect void[ \t]+([a-zGLU][_a-zA-Z0-9]*)\(\)

void cmet52() { FS(); }
void dmnd2c() { FS(); }
void dmnd2cow() { FS(); }
void dmnd2cjp() { FS(); }
void dmnd2pnt() { FS(); }
void lpdmnd() { FS(); }
void ptrshn() { FS(); }
void mtlfw10() { FS(); }
void mtlfw15() { FS(); }
void mtlfw15ow() { FS(); }
void mtlfb3() { FS(); }
void mtlt12f() { FS(); }
void mtlt6f() { FS(); }
void mtlbk03() { FS(); }
void gmtlbg6() { FS(); }
void glrgbk3b() { FS(); }
void gblks15() { FS(); }
void gtprst3() { FS(); }
void skcpthrt() { FS(); }
void gskull4() { FS(); }
void gmtlspsld() { FS(); }
void gmtlsp4b() { FS(); }
void gspbdrbb() { FS(); }
void gkarntwr4a() { FS(); }
void gkarntwrst() { FS(); }
void giron01e() { FS(); }
void gxstrtop4() { FS(); }
void gwdclg1a() { FS(); }
void gwdclg1bd() { FS(); }
void bmtsprt() { FS(); }
void cable() { FS(); }
void brdr11b() { FS(); }
void blt414k() { FS(); }
void light5() { FS(); }
void lt2() { FS(); }
void gpntgmlt1k() { FS(); }
void icon() { FS(); }
void q3bnr() { FS(); }
void Loading() { FS(); }

void Lmapped() { Generic(); }
void mtlfw15ow_m() { Generic(); }
void dmnd2cjp_m() { Generic(); }
void dmnd2pnt_m() { Generic(); }
void lpdmnd_m() { Generic(); }
void fixture() { Generic(); }
void beam() { Generic(); }
void flame() { Generic(); }
void q3bnr_m() { Generic(); }
void shiny() { Generic(); }
void timhel() { Generic(); }
void lavaf() { Generic(); }

void lava() {
	Generic();
	// assuming N = (0 0 1)
	gl_Position += MVP[2] * sin(Time.x*.5 + dot(P.xyz/1e2, vec3(1))) * 4.;
}