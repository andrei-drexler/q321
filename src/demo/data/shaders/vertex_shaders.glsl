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

void FS() {
	gl_Position = P; UV = P.xy * .5 + .5;
}

void wave(float div, float amp, float freq) {
	gl_Position += amp * MVP * vec4(N, 0) * sin(6.28 * (Time.x * freq + dot(P.xyz/div, vec3(1))));
}

////////////////////////////////////////////////////////////////

// $protect void[ \t]+([a-zGLU][_a-zA-Z0-9]*)\(\)

void Generic() {
	 gl_Position = MVP * P;
	 Pos = P.xyz;
	 Nor = N;
	 UV = T.xy;
	 LUV = T.zw;
	 Clr = C;
	 Ref = normalize(reflect((P - Cam).xyz, N));
}
void icon() { FS(); }
void bglogo() { FS(); }
void Loading() { FS(); }
void UI() {
	gl_Position = vec4(2. * P.x - 1., 1. - 2. * P.y, 1, 1);
	UV = T.xy;
	Clr = C;
}
void Lmapped() { Generic(); }
void fixture() { Generic(); }
void shiny() { Generic(); }
void cmet52() { FS(); }
void ptrshn() { FS(); }
void dmnd2c() { FS(); }
void dmnd2cow() { FS(); }
void dmnd2pnt() { FS(); }
void dmnd2pnt_m() { Generic(); }
void mtlfw10() { FS(); }
void mtlfw15() { FS(); }
void mtlfw15ow() { FS(); }
void mtlfw15ow_m() { Generic(); }
void mtlfb3() { FS(); }
void mtlt12f() { FS(); }
void mtlt6f() { FS(); }
void mtlbk03() { FS(); }
void skcpthrt() { FS(); }
void skcpthrt2() { FS(); }
void skcpthrtooz() { Generic(); }
void sktongue() { FS(); }
void sksurf8()  { FS(); }
void gskull4() { FS(); }
void gcntr2trn() { FS(); }
void gcntr2trn_m() { Generic(); }
void scmpblk17() { FS(); }
void scmpblk17_m() { Generic(); }
void gskdr_a() { FS(); }
void gskdr_b() { FS(); }
void gskdr_c() { FS(); }
void gskdr_d() { FS(); }
void gskdr_e() { FS(); }
void gskdr_f() { FS(); }
void gspbdrbb() { FS(); }
void gkarntwr4a() { FS(); }
void gkarntwrst() { FS(); }
void gkarnclma2r() { FS(); }
void gkarnarcfnltp() { FS(); }
void gkarnarcfnlmd() { FS(); }
void gkarnarcfnlbt() { FS(); }
void gkblkgmtrn() { FS(); }
void giron01e() { FS(); }
void giron01nt3() { FS(); }
void gxstrtop4() { FS(); }
void gwdclg1a() { FS(); }
void gwdclg1bd() { FS(); }
void gsltrfc() { FS(); }
void gmtlbg6() { FS(); }
void glrgbk3b() { FS(); }
void gblks15() { FS(); }
void gblks18c() { FS(); }
void gklblki() { FS(); }
void gklblki4() { FS(); }
void gtprst3() { FS(); }
void gblks17f2() { FS(); }
void gmtlspsld() { FS(); }
void gmtlsp4b() { FS(); }
void gsklvtg02b() { FS(); }
void bmtsprt() { FS(); }
void cable() { FS(); }
void brdr11b() { FS(); }
void blt414k() { FS(); }
void lt2() { FS(); }
void gpntgmlt1k() { FS(); }
void light5() { FS(); }
void dmnd2cjp() { FS(); }
void dmnd2cjp_m() { Generic(); }
void lpdmnd() { FS(); }
void lpdmnd_m() { Generic(); }
void blacksky() { Generic(); }
void timhel() { Generic(); }
void lava() {
	Generic();
	wave(100., 3., .1);
}
void lavaf() { Generic(); }
void bwprtbnr() { FS(); }
void bwprtbnr_m() {
	Generic();
	wave(30., 3., .2);
	wave(100., 3., .7);
}
void statue() { Generic(); }
void q3bnr() { FS(); }
void q3bnr_m() { Generic(); }
void beam() { Generic(); }
void flame() { Generic(); }
