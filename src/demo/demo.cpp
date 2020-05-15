//#define ENABLE_RENDERDOC
#define USE_RAW_INPUT
//#define DISABLE_PARTITION
//#define DRAW_LIGHTS
//#define DRAW_POINT_ENTITIES
//#define DRAW_CLIPPING
//#define SAVE_ICON
//#define SAVE_LIGHTMAP
//#define SHOW_LIGHTMAP
//#define START_NOCLIP

#define START_MAP					dm1

////////////////////////////////////////////////////////////////

#include "../engine/demo.h"
#include "console.h"
#include "resource_def.h"
#include "gfx_resources.h"
#include "map.h"
#include "cooked/cooked_maps.h"
#include "cooked/cooked_models.h"
#include "player.h"

////////////////////////////////////////////////////////////////

namespace Demo {
	Sys::Thread				g_loading_thread;
	bool					g_updated_lightmap;
	Sys::Time				g_load_time;
	Sys::Time				g_time;

	FORCEINLINE bool IsLoading() {
		return !Sys::IsThreadReady(g_loading_thread);
	}

	void GenerateLightmap(void*) {
		g_updated_lightmap = false;
		Map::ComputeLighting(Map::LightMode::Shadows);
		if (r_bounce.integer)
			Map::ComputeLighting(Map::LightMode::Bounce);
	}

	void RenderSprite(const vec3& point, float size) {
		using namespace Demo;

		vec3 pos[4];
		vec3 nor[4];
		vec2 uv[4];

		Gfx::Mesh mesh;
		memset(&mesh, 0, sizeof(mesh));

		mat4 rotation = MakeRotation(g_player.angles * Math::DEG2RAD);
		vec3 x = rotation.x.xyz * size;
		vec3 y = rotation.z.xyz * size;
		vec3 z = rotation.y.xyz;

		nor[0] = nor[1] = nor[2] = nor[3] = -z;

		pos[0] = point - x - y;
		pos[1] = point + x - y;
		pos[2] = point + x + y;
		pos[3] = point - x + y;

		uv[0] = vec2{0.f, 0.f};
		uv[1] = vec2{1.f, 0.f};
		uv[2] = vec2{1.f, 1.f};
		uv[3] = vec2{0.f, 1.f};

		u32 idx[6] = { 0, 1, 2, 0, 2, 3 };

		mesh.vertices[Attrib::Position	].SetData(pos, 4);
		mesh.vertices[Attrib::TexCoord	].SetData(uv, 4);
		mesh.vertices[Attrib::Normal	].SetData(nor, 4);
		mesh.num_vertices = 4;
		mesh.SetIndices(idx, 6);

		Gfx::SetShader(Demo::Shader::Generic);
		
		Uniform::Time.w = 0;
		Uniform::Texture0 = Demo::Texture::White;

		Gfx::UpdateUniforms();
		Gfx::Draw(mesh);
	}

	////////////////////////////////////////////////////////////////

	void DrawLights() {
		for (u16 i = 0; i < Map::num_lights; ++i) {
			auto& light = Map::lights[i];
			vec3 pos;
			pos[0] = light.position[0];
			pos[1] = light.position[1];
			pos[2] = light.position[2];
			RenderSprite(light.position, max(light.intensity / 25.f, 2.f));
		}
	}

	void DrawPointEntities() {
		for (u16 i = Map::num_brush_entities; i < Map::num_entities; ++i) {
			auto& e = Map::entities[i];
			vec3 pos;
			pos[0] = e.origin[0];
			pos[1] = e.origin[1];
			pos[2] = e.origin[2];
			RenderSprite(pos, 8.f);
		}
	}

	void RenderDebug() {
#ifdef DRAW_LIGHTS
		DrawLights();
#endif

#ifdef DRAW_POINT_ENTITIES
		DrawPointEntities();
#endif
	}

	////////////////////////////////////////////////////////////////

	struct Frame {
		vec3				pos;
		vec3				angles;
		float				fov;
		float				time;
		float				shadow_angle;
		Gfx::Texture::ID	render_target;
	};

	static constexpr mat4 ToYUp = {
			1.f,  0.f,  0.f,  0.f,
			0.f,  0.f, -1.f,  0.f,
			0.f,  1.f,  0.f,  0.f,
			0.f,  0.f,  0.f,  1.f,
	};

	NOINLINE void RenderWorld(const Frame& frame) {
		using namespace Math::constants;

		Uniform::Time.x = frame.time;

		mat4 rotation, translation, projection;
		transpose(MakeRotation(frame.angles * DEG2RAD), rotation);
		translation = i4x4;
		translation.SetPosition(-frame.pos);

		vec2 res = Gfx::GetResolution();
		vec2 fov(frame.fov * DEG2RAD);
		fov.y = ScaleFov(fov.x, res.y/res.x);

		MakePerspective(fov, 2.f, 8192.f, projection);
		Uniform::MVP = projection * ToYUp * rotation * translation;
		Uniform::Cam.xyz = frame.pos;
		Uniform::Cam.w = frame.shadow_angle * RAD2DEG;

		Gfx::SetRenderTarget(frame.render_target);
		Gfx::Clear(Gfx::ClearBit::Depth);
		Map::Render();
	}

	FORCEINLINE void GenerateLevelShot() {
		const auto& levelshot = Map::source->levelshot;

		Frame frame;
		MemSet(&frame);

		frame.pos[0] = levelshot.position[0];
		frame.pos[1] = levelshot.position[1];
		frame.pos[2] = levelshot.position[2];
		frame.angles[0] = levelshot.angles[0];
		frame.angles[1] = levelshot.angles[1];
		frame.fov = 90.f;
		frame.render_target = Demo::Texture::Levelshot;

		RenderWorld(frame);

		Gfx::GenerateMipMaps(Demo::Texture::Levelshot);
	}

	////////////////////////////////////////////////////////////////

	static constexpr char LoadingText[] = 
		"LOADING Q3DM17"				"\0"
		"CONNECTING TO LOCALHOST"		"\0"
		"PRESS ESC TO ABORT"			"\0"
		" "								"\0"
		"LOADING... MAPS/Q3DM17.BSP"	"\0"
		" "								"\0"
		" "/* MAP MESSAGE */			"\0"
		"CHEATS ARE ENABLED"			"\0"
		"FREE FOR ALL"					"\0"
		"FRAGLIMIT 20"					"\0"
	;
	static constexpr int LoadingScreenMessageLine = 6;

	FORCEINLINE void RenderLoadingScreen() {
		Gfx::SetRenderTarget(Gfx::Backbuffer);
		Gfx::SetShader(Demo::Shader::Loading);
		Demo::Uniform::Texture0 = Demo::Texture::Levelshot;
		Demo::Uniform::Time.x = g_time;
		Gfx::UpdateUniforms();
		Gfx::Clear(Gfx::ClearBit::ColorAndDepth);
		Gfx::DrawFullScreen();

		vec2 pos = Gfx::GetResolution() * vec2{0.5f, 0.125f};
		vec2 ui_scale = UI::GetScale();
		vec2 font_scale = UI::FontScale[UI::LargeFont] * ui_scale.y;
		int line_number = 0;
		for (const char* line = LoadingText; *line; line = NextAfter(line), ++line_number) {
			const char* text = line;
			if (line_number == LoadingScreenMessageLine)
				text = Map::source->message;
			UI::PrintShadowed(text, pos, font_scale, -1, 0.5f, UI::LargeFont);
			pos.y += ui_scale.y * 80.f;
		}
		UI::FlushGeometry();
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void RenderFrame() {
		if (IsLoading()) {
			RenderLoadingScreen();
			return;
		}

		if (!g_updated_lightmap) {
			Map::UpdateLightmapTexture();
			g_updated_lightmap = true;
			g_load_time = g_time;
		}

		Frame frame;
		frame.pos			= g_player.position;
		frame.pos.z			-= g_player.step;

		if (g_player.land_time > 0.f) {
			float land_frac;
			if (g_player.land_time > g_player.LandReturnTime) {
				land_frac = 1.f - (g_player.land_time - g_player.LandReturnTime) / g_player.LandDeflectTime;
			} else {
				land_frac = g_player.land_time / g_player.LandReturnTime;
			}
			frame.pos.z -= land_frac * g_player.land_change;
		}

		float speed = length(g_player.velocity.xy);
		float bob = speed / 640.f * sin(g_player.walk_cycle * 10.f);
		float bob_pitch = bob;

		frame.angles		= g_player.angles;
		frame.angles.y		-= abs(bob);
		frame.angles.z		+= bob;
		frame.fov			= mix(cg_fov.value, cg_zoomfov.value, g_player.zoom);
		frame.time			= g_time - g_load_time;
		frame.shadow_angle	= g_player.shadow_angle;
		frame.render_target	= Gfx::Backbuffer;

		Gfx::Sync();
		RenderWorld(frame);
		RenderDebug();
	}

	////////////////////////////////////////////////////////////////

	u32 g_screenshot_index;

	constexpr char ScreenshotFolder[] = "screenshots";
	constexpr char ScreenshotPrefix[] = "screenshots/screenshot_";
	constexpr char ScreenshotSuffix[] = ".tga";

	void TakeScreenshot() {
		if (!Sys::CreateFolder(ScreenshotFolder))
			return;

		char file_name[64];
		MemCopy(file_name, ScreenshotPrefix, size(ScreenshotPrefix) - 1);
		do {
			MemCopy(IntToString(++g_screenshot_index, file_name + (size(ScreenshotPrefix) - 1)), ScreenshotSuffix, size(ScreenshotSuffix));
			if (g_screenshot_index == 10000) {
				Sys::DebugStream << "Error: couldn't generate screenshot name.\n";
				return;
			}
		} while (Sys::FileExists(file_name));

		i32 width = Sys::g_window.width;
		i32 height = Sys::g_window.height;
		u32* pixels = Sys::Alloc<u32>(width * height);
		Gfx::ReadBack(Gfx::Backbuffer, pixels);
		Gfx::SaveTGA(file_name, pixels, width, height);
		Sys::Free(pixels);
	}

	////////////////////////////////////////////////////////////////

	float g_delta_time;
	u8 g_key_state[256];

	FORCEINLINE void Tick(float dt) {
		assign_min(dt, 0.25f);
		if (g_delta_time == 0.f)
			g_delta_time = dt;
		dt = mix(g_delta_time, dt, 0.125f);
		g_delta_time = dt;

		Sys::UpdateKeyboardState();
		if (Sys::IsKeyReleased(Key::Escape))
			Sys::Exit();
		if (Sys::IsKeyReleased(Key::PrintScreen))
			TakeScreenshot();
		if (Sys::IsKeyReleased(Key::Backspace))
			g_player.Spawn();

		vec2 mouse;
		Sys::UpdateMouseState(mouse, dt);

		if (!IsLoading()) {
			float fov = mix(cg_fov.value, cg_zoomfov.value, g_player.zoom);
			float zoom_scale = tan(fov * (0.5f * Math::DEG2RAD)) / tan(cg_fov.value * (0.5f * Math::DEG2RAD));
			float mouse_scale = zoom_scale * sensitivity.value * -90.f / sqrt(Sys::g_window.width * Sys::g_window.height);
			g_player.angles.x += mouse.x * mouse_scale;
			g_player.angles.y += mouse.y * mouse_scale;
			g_player.angles.x = mod(g_player.angles.x, 360.f);
			g_player.angles.y = clamp(g_player.angles.y, -85.f, 85.f);

			g_player.Update(dt);
		}
	}

	////////////////////////////////////////////////////////////////

	static constexpr auto IconDescriptor = Texture::Descriptors[Texture::icon];
	static_assert(IconDescriptor.width == IconDescriptor.height, "Icon texture must be square");

	FORCEINLINE void UpdateWindowIcon() {
		u32* pixels = Sys::Alloc<u32>(IconDescriptor.width * IconDescriptor.height);
		Gfx::ReadBack(Texture::icon, pixels);
		Sys::SetWindowIcon(&Sys::g_window, pixels, IconDescriptor.width);
#ifdef SAVE_ICON
		Gfx::SaveTGA("icon.tga", pixels, IconDescriptor.width, IconDescriptor.height);
#endif
		Sys::Free(pixels);
	}
}

////////////////////////////////////////////////////////////////

int FORCEINLINE demo_main() {
	Sys::Printf("%s", "Starting up...\n");

	Mem::Init();
	Demo::Console::Init();
	Sys::InitWindow(&Sys::g_window, nullptr, "Q320");
	Sys::SetFPSMode(&Sys::g_window);
	Gfx::InitMemory(16 * Mem::MB, 16 * Mem::MB);
	Demo::RegisterGfxResources();
	Map::AllocLightmap();
	Demo::Texture::GenerateAll();
	Demo::UpdateWindowIcon();

#ifdef SHOW_LIGHTMAP
	Demo::r_lightmap.Set(1);
#endif

	auto touch = [](auto& src) { MemCopy(Mem::Alloc(sizeof(src)), &src, sizeof(src)); };
	#define PP_DEMO_MODEL_TOUCH(name)	touch(name::vertices); touch(name::uvs); touch(name::indices);
	DEMO_MODELS(PP_DEMO_MODEL_TOUCH)
	#undef PP_DEMO_MODEL_TOUCH

	Map::Load(START_MAP::map);
	Map::UpdateLightmapTexture();
	Demo::GenerateLevelShot();
	
	Demo::g_updated_lightmap = false;
	Demo::g_loading_thread.work = &Demo::GenerateLightmap;
	Sys::SpawnThread(Demo::g_loading_thread);

	Demo::g_player.Spawn();

	auto last_tick = Sys::GetTime();
	auto next_time = last_tick;

	int code = 0;
	
	while (Sys::PumpMessages(&code)) {
		Demo::g_time = Sys::GetTime();
		Demo::Tick(Demo::g_time - last_tick);
		last_tick = Demo::g_time;

		Demo::RenderFrame();
		Gfx::Present();

		float fps = Demo::com_maxFps.value;
		if (fps == 0.f)
			fps = Sys::g_window.refresh;
		float interval = fps > 0.f ? 1.f / fps : 0.f;

		float wait = 0.f;
		if (interval > 0.f) {
			auto now = Sys::GetTime();
			wait = next_time - now;
			if (wait <= 0.f) {
				next_time = now + interval;
				wait = 0.f;
			} else {
				next_time += interval;
			}
		}
		Sys::PreciseSleep(wait);
	}

	return code;
}
