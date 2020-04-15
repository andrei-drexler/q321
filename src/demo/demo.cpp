#include "../engine/demo.h"
#include "console.h"
#include "resource_def.h"
#include "gfx_resources.h"
#include "map.h"
#include "cooked/maps.h"
#include "cooked/models.h"
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
		Map::ComputeLighting();
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
	
		mesh.vertices[Attrib::Position	].SetData(pos);
		mesh.vertices[Attrib::TexCoord	].SetData(uv);
		mesh.vertices[Attrib::Normal	].SetData(nor);
		
		mesh.indices			= idx;
		mesh.num_vertices		= 4;
		mesh.num_indices		= 6;

		Gfx::SetShader(Demo::Shader::Generic);
		
		Uniform::Time.w = 0;
		Uniform::Texture0 = Demo::Texture::White;

		Gfx::UpdateUniforms();
		Gfx::Draw(mesh);
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
		Gfx::Clear(Gfx::ClearBit::ColorAndDepth);
		Map::Render();
	}

	FORCEINLINE void GenerateLevelShot() {
		static constexpr Frame levelshot = {
			{-920.f, -424.f, 1104.f},
			{296.f, -22.f, 0.f},
			90.f,
			3.f,
			0.f,
			Demo::Texture::Levelshot
		};
		RenderWorld(levelshot);
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
		"THE LONGEST YARD"				"\0"
		"CHEATS ARE ENABLED"			"\0"
		"FREE FOR ALL"					"\0"
		"FRAGLIMIT 20"					"\0"
	;

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
		for (const char* line = LoadingText; *line; line = NextAfter(line)) {
			UI::PrintShadowed(line, pos, font_scale, -1, 0.5f, UI::LargeFont);
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
		frame.fov			= cl_fov.value;
		frame.time			= g_time - g_load_time;
		frame.shadow_angle	= g_player.shadow_angle;
		frame.render_target	= Gfx::Backbuffer;

		RenderWorld(frame);

		if constexpr (0) {
			for (u16 i = Map::num_brush_entities; i < Map::num_entities; ++i) {
				auto& e = Map::entities[i];
				vec3 pos;
				pos[0] = e.origin[0];
				pos[1] = e.origin[1];
				pos[2] = e.origin[2];
				RenderSprite(pos, 16.f);
			}
		}
	}

	////////////////////////////////////////////////////////////////

	float g_delta_time;
	FORCEINLINE void Tick(float dt) {
		if (g_delta_time == 0.f)
			g_delta_time = dt;
		dt = mix(g_delta_time, dt, 0.125f);
		g_delta_time = dt;

		u8 key_state[256];
		Sys::GetKeyboardState(key_state);

		if (!IsLoading())
			g_player.Update(key_state, dt);
	}

	////////////////////////////////////////////////////////////////

	u32 g_screenshot_index;

	constexpr char ScreenshotPrefix[] = "screenshot_";
	constexpr char ScreenshotSuffix[] = ".tga";

	void TakeScreenshot() {
		char file_name[32];
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
		u32* pixels = (u32*)Sys::Alloc(width * height * sizeof(u32));
		Gfx::ReadBack(Gfx::Backbuffer, pixels);
		Gfx::SaveTGA(file_name, pixels, width, height);
		Sys::Free(pixels);
	}

	////////////////////////////////////////////////////////////////

	void HandleEvent(Sys::Window::Event& event) {
		using Event = Sys::Window::Event;

		switch (event.type) {
			case Event::Paint:
				RenderFrame();
				return;

			case Event::KeyUp:
				if (event.data.key_down.code == Key::Escape)
					Sys::Exit();
				else if (event.data.key_down.code == Key::PrintScreen)
					TakeScreenshot();
				else if (event.data.key_down.code == Key::Backspace)
					g_player.Spawn();
				return;

			case Event::MouseMove: {
				if (IsLoading())
					return;
				float scale = (sensitivity.value * -90.f) / event.window->height;
				g_player.angles.x += event.data.mouse_move.pt.x * scale;
				g_player.angles.y += event.data.mouse_move.pt.y * scale;
				g_player.angles.x = mod(g_player.angles.x, 360.f);
				g_player.angles.y = clamp(g_player.angles.y, -85.f, 85.f);
				return;
			}

			default:
				return;
		}
	}

	static constexpr auto IconDescriptor = Texture::Descriptors[Texture::icon];
	static_assert(IconDescriptor.width == IconDescriptor.height, "Icon texture must be square");

	FORCEINLINE void UpdateWindowIcon() {
		u32* pixels = Sys::Alloc<u32>(IconDescriptor.width * IconDescriptor.height);
		Gfx::ReadBack(Texture::icon, pixels);
		Sys::SetWindowIcon(&Sys::g_window, pixels, IconDescriptor.width);
		if constexpr (0) {
			Gfx::SaveTGA("icon.tga", pixels, IconDescriptor.width, IconDescriptor.height);
		}
		Sys::Free(pixels);
	}
}

////////////////////////////////////////////////////////////////

int FORCEINLINE demo_main() {
	Sys::Printf("%s", "Starting up...\n");

	Mem::Init();
	Demo::Console::Init();
	Sys::InitWindow(&Sys::g_window, Demo::HandleEvent, "Q320");
	Sys::SetFPSMode(&Sys::g_window);
	Demo::RegisterGfxResources();
	Demo::Texture::GenerateAll();
	Demo::UpdateWindowIcon();

	auto touch = [](auto& src) { MemCopy(Mem::Alloc(sizeof(src)), &src, sizeof(src)); };
	#define PP_DEMO_MODEL_TOUCH(name)	touch(name::vertices); touch(name::uvs); touch(name::indices);
	DEMO_MODELS(PP_DEMO_MODEL_TOUCH)
	#undef PP_DEMO_MODEL_TOUCH

	Map::Load(dm1::map);
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

		Sys::RedrawWindow(&Sys::g_window);

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
		Sys::Sleep(wait);
	}

	return code;
}
