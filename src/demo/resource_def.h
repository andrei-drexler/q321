#pragma once

////////////////////////////////////////////////////////////////
//
// Resources are defined here so that the map compiler can include
// this file without dragging in demo/engine stuff
//
// Downside is that some of these fields don't make much sense on
// their own, you also need to look in gfx_resources.h / material.h
// 
////////////////////////////////////////////////////////////////

#define DEMO_MAPS(x)\
	/*Name,				Source*/\
	x(dm17,				q3dm17sample.map)\
	x(dm1,				q3dm1sample.map)\

#define DEMO_VERTEX_ATTRIBS(x)\
	/*Name,				Type*/\
	x(Position,			vec3)\
	x(TexCoord,			vec4)\
	x(Normal,			vec3)\
	x(Color,			vec4)\

#define DEMO_SHADERS(x)\
	/*Name,					VertexBits*/\
	x(Generic,				MapVertexBits)\
	x(icon,					FSVertexBits)\
	x(Loading,				FSVertexBits)\
	x(UI,					UIVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::ZTestAlways|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(Lmapped,				MapVertexBits)\
	x(fixture,				MapVertexBits)\
	x(shiny,				MapVertexBits)\
	x(cmet52,				FSVertexBits)\
	x(ptrshn,				FSVertexBits)\
	x(dmnd2c,				FSVertexBits)\
	x(dmnd2cow,				FSVertexBits)\
	x(dmnd2pnt,				FSVertexBits)\
	x(dmnd2pnt_m,			MapVertexBits)\
	x(mtlfw10,				FSVertexBits)\
	x(mtlfw15,				FSVertexBits)\
	x(mtlfw15ow,			FSVertexBits)\
	x(mtlfw15ow_m,			MapVertexBits)\
	x(mtlfb3,				FSVertexBits)\
	x(mtlt12f,				FSVertexBits)\
	x(mtlt6f,				FSVertexBits)\
	x(mtlbk03,				FSVertexBits)\
	x(gmtlbg6,				FSVertexBits)\
	x(glrgbk3b,				FSVertexBits)\
	x(gblks15,				FSVertexBits)\
	x(gtprst3,				FSVertexBits)\
	x(bmtsprt,				FSVertexBits)\
	x(cable,				FSVertexBits)\
	x(brdr11b,				FSVertexBits)\
	x(blt414k,				FSVertexBits)\
	x(lt2,					FSVertexBits)\
	x(light5,				FSVertexBits)\
	x(dmnd2cjp,				FSVertexBits)\
	x(dmnd2cjp_m,			MapVertexBits)\
	x(lpdmnd,				FSVertexBits)\
	x(lpdmnd_m,				MapVertexBits)\
	x(timhel,				MapVertexBits)\
	x(q3bnr,				FSVertexBits)\
	x(q3bnr_m,				MapVertexBits)\
	x(beam,					Demo::Attrib::PositionBit|Demo::Attrib::TexCoordBit|Demo::Attrib::NormalBit|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(flame,				Demo::Attrib::PositionBit|Demo::Attrib::TexCoordBit|Demo::Attrib::NormalBit|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\

////////////////////////////////////////////////////////////////

#define DEMO_TEXTURES(x)\
	/*Name,			ProcGenShader,		Width,	Height,	Format,		Flags*/\
	x(White,		Gfx::InvalidID,		16,		16,		BGRA8,		Gfx::Texture::Flags::Default)\
	x(Grey,			Gfx::InvalidID,		16,		16,		BGRA8,		Gfx::Texture::Flags::Default)\
	x(icon,			Shader::icon,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(Lightmap,		Gfx::InvalidID,		1024,	256,	BGRA8,		Gfx::Texture::Flags::Default|Gfx::Texture::Flags::NoMips)\
	x(LevelshotZ,	Gfx::InvalidID,		512,	512,	Z32F,		Gfx::Texture::Flags::ZBuffer)\
	x(Levelshot,	Gfx::InvalidID,		512,	512,	BGRA8,		Gfx::Texture::Flags::ZBuffer|Gfx::Texture::Flags::RenderTarget)\
	x(Font,			Gfx::InvalidID,		512,	512,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2c,		Shader::dmnd2c,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2cow,		Shader::dmnd2cow,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2pnt,		Shader::dmnd2pnt,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(cmet52,		Shader::cmet52,		128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(ptrshn,		Shader::ptrshn,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2cjp,		Shader::dmnd2cjp,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(lpdmnd,		Shader::lpdmnd,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfw10,		Shader::mtlfw10,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfw15,		Shader::mtlfw15,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfw15ow,	Shader::mtlfw15ow,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfb3,		Shader::mtlfb3,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlt12f,		Shader::mtlt12f,	128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlt6f,		Shader::mtlt6f,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlbk03,		Shader::mtlbk03,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gmtlbg6,		Shader::gmtlbg6,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(glrgbk3b,		Shader::glrgbk3b,	512,	512,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gblks15,		Shader::gblks15,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gtprst3,		Shader::gtprst3,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(bmtsprt,		Shader::bmtsprt,	256,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(cable,		Shader::cable,		128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(brdr11b,		Shader::brdr11b,	64,		32,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(blt414k,		Shader::blt414k,	64,		256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(lt2,			Shader::lt2,		64,		64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(light5,		Shader::light5,		16,		128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(q3bnr,		Shader::q3bnr,		512,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	/* HACK: dummy texture needed by the flame material to get proper UV mapping...*/\
	x(_128x256,		Gfx::InvalidID,		128,	256,	BGRA8,		Gfx::Texture::Flags::NoMips)\

////////////////////////////////////////////////////////////////

#ifdef DEVxxx
	#define DEMO_FALLBACK_MATERIAL(x)\
		x("",								Generic,		White,		Solid,		Opaque,					(0))
#else
	#define DEMO_FALLBACK_MATERIAL(x)\
		x("",								Lmapped,		cmet52,		Solid,		Opaque,					(0))
#endif

#define DEMO_MATERIALS(x)\
	/*Path									Shader			Texture		Contents	Draw					Light*/\
	DEMO_FALLBACK_MATERIAL(x)\
	x("common/caulk",						Generic,		White,		Solid,		Invisible|BlocksLight,	(0))\
	x("base_wall/c_met5_2",					Lmapped,		cmet52,		Solid,		Opaque,					(0))\
	x("base_trim/pewter_shiney",			shiny,			ptrshn,		Solid,		Opaque,					(0))\
	x("base_floor/diamond2c",				Lmapped,		dmnd2c,		Solid,		Opaque,					(0))\
	x("base_floor/diamond2c_ow",			Lmapped,		dmnd2cow,	Solid,		Opaque|NeedsUV,			(0))\
	x("sfx/pentfloor_diamond2c",			dmnd2pnt_m,		dmnd2pnt,	Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/metalfloor_wall_10",		Lmapped,		mtlfw10,	Solid,		Opaque,					(0))\
	x("base_wall/metalfloor_wall_15",		Lmapped,		mtlfw15,	Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/metalfloor_wall_15ow",		mtlfw15ow_m,	mtlfw15ow,	Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/metfloor_block_3",			Lmapped,		mtlfb3,		Solid,		Opaque,					(0))\
	x("base_wall/metaltech12final",			Lmapped,		mtlt12f,	Solid,		Opaque,					(0))\
	x("base_wall/metaltech06final",			Lmapped,		mtlt6f,		Solid,		Opaque,					(0))\
	x("base_wall/metalblack03",				Lmapped,		mtlbk03,	Solid,		Opaque,					(0))\
	x("base_trim/basemetalsupport",			Lmapped,		bmtsprt,	Solid,		Opaque,					(0))\
	x("base_trim/border11b",				Lmapped,		brdr11b,	Solid,		Opaque|NeedsUV,			(0))\
	x("base_support/cable",					Lmapped,		cable,		Solid,		Opaque,					(0))\
	x("base_light/baslt4_1_4k",				fixture,		blt414k,	Solid,		Opaque|NeedsUV,			(4000,31,29,26))\
	x("base_light/lt2_2000",				fixture,		lt2,		Solid,		Opaque|NeedsUV,			(2000,31,22,24))\
	x("base_light/lt2_8000",				fixture,		lt2,		Solid,		Opaque|NeedsUV,			(8000,31,22,24))\
	x("base_light/light5_5k",				fixture,		light5,		Solid,		Opaque|NeedsUV,			(5000))\
	x("sfx/diamond2cjumppad",				dmnd2cjp_m,		dmnd2cjp,	Solid,		Opaque|NeedsUV,			(200,100,0))\
	x("sfx/launchpad_diamond",				lpdmnd_m,		lpdmnd,		Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/main_q3abanner",			q3bnr_m,		q3bnr,		Solid,		Opaque|NeedsUV,			(20,2,0,0))\
	x("gothic_floor/metalbridge06",			Lmapped,		gmtlbg6,	Solid,		Opaque,					(0))\
	x("gothic_floor/largerblock3b",			Lmapped,		glrgbk3b,	Solid,		Opaque,					(0))\
	x("gothic_block/blocks15",				Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_trim/pitted_rust3",			Lmapped,		gtprst3,	Solid,		Opaque,					(0))\
	x("liquids/lavahellflat_400",			Lmapped,		cmet52,		Solid,		Opaque,					(400,16,5,2))\
	x("gothic_trim/pitted_rust2",			Lmapped,		gtprst3,	Solid,		Opaque,					(0))\
	x("gothic_trim/pitted_rust",			Lmapped,		gtprst3,	Solid,		Opaque,					(0))\
	x("gothic_block/blocks18c_3",			Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_block/killblock",				Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_block/blocks18b",				Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_block/blocks11b",				Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_block/blocks18c",				Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_block/killblock_i4",			Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_block/killblock_i",			Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("gothic_block/blocks1",				Lmapped,		gblks15,	Solid,		Opaque,					(0))\
	x("liquids/lavahell_750",				Lmapped,		cmet52,		Solid,		Opaque,					(500,16,5,2))\
	x("skies/blacksky",						Generic,		White,		Solid,		Invisible,				(0))\
	x("skies/tim_hell",						timhel,			White,		Solid,		Sky,					(0))\
	x("sfx/beam",							beam,			White,		NonSolid,	Translucent|NeedsUV,	(0))\
	x("sfx/flame1_hell",					flame,			_128x256,	NonSolid,	Translucent|NeedsUV,	(600,8,6,1))\
	x("common/weapclip",					Generic,		White,		Solid,		Invisible,				(0))\
	x("common/clip",						Generic,		White,		Solid,		Invisible,				(0))\
	x("common/nodraw",						Generic,		White,		Solid,		Invisible,				(0))\
	x("common/nodrawnonsolid",				Generic,		White,		NonSolid,	Invisible,				(0))\
	x("common/trigger",						Generic,		White,		NonSolid,	Invisible,				(0))\
	x("common/nodrop",						Generic,		White,		NoDrop,		Invisible,				(0))\

////////////////////////////////////////////////////////////////

#define DEMO_MODELS(x)\
	x(mega_cross)\
	x(small_cross)\
	x(ammo)\
	x(shard)\
	x(quad)\
	x(armor_red)\
	x(machinegun_barrel)\
	x(railgun)\
	x(rocketl)\
	x(shotgun)\

////////////////////////////////////////////////////////////////

#define DEMO_UNIFORMS(x)\
	/*Name,					Type*/\
	x(Texture0,				Gfx::Texture::ID)\
	x(Texture1,				Gfx::Texture::ID)\
	x(MVP,					mat4)\
	x(Cam,					vec4)\
	x(Time,					vec4)\

////////////////////////////////////////////////////////////////
