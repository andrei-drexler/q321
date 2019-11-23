#include "../engine/demo.h"
#include "console.h"
#include "resource_def.h"
#include "gfx_resources.h"
#include "map.h"
#include "cooked/dm17.h"
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
		g_map.ComputeLighting();
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

		u16 idx[6] = { 0, 1, 2, 0, 2, 3 };
	
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

	struct Frame {
		vec3				pos;
		vec3				angles;
		float				fov;
		float				time;
		Gfx::Texture::ID	render_target;
	};

	NOINLINE void RenderFrame(const Frame& frame) {
		using namespace Math::constants;

		Uniform::Time.x = frame.time;

		mat4 rotation, translation(1.f), projection;
		transpose(MakeRotation(frame.angles * DEG2RAD), rotation);
		translation.SetPosition(-frame.pos);

		vec2 res = Gfx::GetResolution();
		vec2 fov(frame.fov * DEG2RAD);
		fov.y = ScaleFov(fov.x, res.y/res.x);

		static constexpr mat4 ToYUp = {
			 1.f,  0.f,  0.f,  0.f,
			 0.f,  0.f, -1.f,  0.f,
			 0.f,  1.f,  0.f,  0.f,
			 0.f,  0.f,  0.f,  1.f,
		};

		MakePerspective(fov, 2.f, 8192.f, projection);
		Uniform::MVP = projection * ToYUp * rotation * translation;
		Uniform::Cam.xyz = frame.pos;
		Uniform::Cam.w = 1.f;

		Gfx::SetRenderTarget(frame.render_target);
		Gfx::Clear(Gfx::ClearBit::ColorAndDepth);
		g_map.Render();
	}

	FORCEINLINE void GenerateLevelShot() {
		static constexpr Frame levelshot = {
			{-920.f, -424.f, 1104.f},
			{296.f, -22.f, 0.f},
			90.f,
			3.f,
			Demo::Texture::Levelshot
		};
		RenderFrame(levelshot);
		Gfx::GenerateMipMaps(Demo::Texture::Levelshot);
	}

	FORCEINLINE void RenderLoadingScreen() {
		Gfx::SetRenderTarget(Gfx::Backbuffer);
		Gfx::SetShader(Demo::Shader::Loading);
		Demo::Uniform::Texture0 = Demo::Texture::Levelshot;
		Demo::Uniform::Time.x = g_time;
		Gfx::UpdateUniforms();
		Gfx::Clear(Gfx::ClearBit::ColorAndDepth);
		Gfx::DrawFullScreen();

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

		vec2 pos = Gfx::GetResolution() * vec2{0.5f, 0.125f};
		vec2 ui_scale = UI::GetScale();
		vec2 font_scale = UI::FontScale[UI::LargeFont] * ui_scale.y;
		for (const char* line = LoadingText; *line; line = NextAfter(line)) {
			UI::PrintShadowed(line, pos, font_scale, -1, 0.5f, UI::LargeFont);
			pos.y += ui_scale.y * 80.f;
		}
		UI::FlushGeometry();
	}

	FORCEINLINE void RenderFrame() {
		if (IsLoading()) {
			RenderLoadingScreen();
			return;
		}

		if (!g_updated_lightmap) {
			g_map.UpdateLightmapTexture();
			g_updated_lightmap = true;
			g_load_time = g_time;
		}

		Frame frame;
		frame.pos			= g_player.position;
		frame.pos.z			-= g_player.step;
		frame.angles		= g_player.angles;
		frame.fov			= cl_fov.f;
		frame.time			= g_time - g_load_time;
		frame.render_target	= Gfx::Backbuffer;

		RenderFrame(frame);

		//for (u16 li = 0; li < g_map.num_lights; ++li)
		//	RenderSprite(g_map.lights[li].position, 16.f);
	}

	float g_delta_time;

	FORCEINLINE void Tick(float dt) {
		if (g_delta_time == 0.f)
			g_delta_time = dt;
		dt = mix(g_delta_time, dt, 0.125f);
		g_delta_time = dt;

		if (!IsLoading())
			g_player.Update(dt);
	}

	constexpr NOINLINE Player::Input GetKeyBinding(int key) {
		switch (key) {
			case Key::W:		return Player::Input::MoveForward;
			case Key::S:		return Player::Input::MoveBack;
			case Key::A:		return Player::Input::MoveLeft;
			case Key::D:		return Player::Input::MoveRight;
			
			case Key::Up:		return Player::Input::MoveForward;
			case Key::Down:		return Player::Input::MoveBack;
			case Key::Left:		return Player::Input::LookLeft;
			case Key::Right:	return Player::Input::LookRight;

			case Key::Space:	return Player::Input::MoveUp;
			case Key::C:		return Player::Input::MoveDown;
			case Key::Ctrl:		return Player::Input::MoveDown;
			
			case Key::PageDown:	return Player::Input::LookUp;
			case Key::Del:		return Player::Input::LookDown;
			case Key::End:		return Player::Input::LookCenter;

			case Key::Shift:	return Player::Input::Run;

			default:			return Player::Input::Invalid;
		}
	}

	constexpr auto KeyBindings =
		MakeLookupTable<int, Player::Input, 0, 255>(GetKeyBinding);

	////////////////////////////////////////////////////////////////

	u32 g_screenshot_index;

	void TakeScreenshot() {
		const char Prefix[] = "screenshot_";
		const char Suffix[] = ".tga";
		char file_name[32];
		MemCopy(file_name, Prefix, size(Prefix) - 1);
		do {
			MemCopy(IntToString(++g_screenshot_index, file_name + (size(Prefix) - 1)), Suffix, size(Suffix));
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

			case Event::KeyDown:
				g_player.Set(KeyBindings[event.data.key_down.code]);
				return;
			
			case Event::KeyUp:
				if (event.data.key_down.code == Key::Escape) {
					Sys::Exit();
				}
				if (event.data.key_down.code == Key::PrintScreen) {
					TakeScreenshot();
					return;
				}
				g_player.Clear(KeyBindings[event.data.key_up.code]);
				return;

			case Event::MouseMove: {
				if (IsLoading())
					return;
				vec2 delta(event.data.mouse_move.pt.x, event.data.mouse_move.pt.y);
				delta *= (sensitivity.f * -90.f) / event.window->height;
				g_player.angles.xy += delta;
				g_player.angles.y = clamp(g_player.angles.y, -85.f, 85.f);
				return;
			}

			default:
				return;
		}
	}
}

////////////////////////////////////////////////////////////////

int FORCEINLINE demo_main() {
	Mem::Init();
	Demo::Console::Init();
	Sys::InitWindow(&Sys::g_window, Demo::HandleEvent, "Q320 - The Shortest Yard");
	Demo::RegisterGfxResources();
	Demo::Texture::GenerateAll();

	auto touch = [](auto& src) { memcpy(Mem::Alloc(sizeof(src)), src, sizeof(src)); };
	//touch(dm17::lightmap_offsets);

	static_assert(dm17::material_version == Demo::Material::Version, "Material definition mismatch, please recompile the map");

	g_map.Load(dm17::map);
	g_map.UpdateLightmapTexture();
	Demo::GenerateLevelShot();
	
	Demo::g_updated_lightmap = false;
	Demo::g_loading_thread.work = &Demo::GenerateLightmap;
	Sys::SpawnThread(Demo::g_loading_thread);

	// levelshot
	//Demo::g_player.position		= vec3{-948.f, -396.f, 1220.f};
	//Demo::g_player.angles		= vec3{296.f, -28.5f, 0.f};

	//Demo::g_player.position	= vec3{488.f, -376.f, 352.f};
	//Demo::g_player.angles		= vec3(-225.f, 0.f, 0.f);
	//Demo::g_player.position		= {-712.f, -840.f, 1172.f};
	//Demo::g_player.angles		= {320.f, -24.f, 0.f};
	//Demo::g_player.angles		= {199.406784f, -2.4375f, 0.f}; // ray benchmark
	
	// lower-left side
	Demo::g_player.position		= {42.f, 608.f, 80.f};
	Demo::g_player.angles		= {222.f, 2.25f, 0.f};

	Sys::SetFPSMode(&Sys::g_window);

	auto last_tick = Sys::GetTime();
	auto next_time = last_tick;

	int code = 0;
	
	while (Sys::PumpMessages(&code)) {
		Demo::g_time = Sys::GetTime();
		Demo::Tick(Demo::g_time - last_tick);
		last_tick = Demo::g_time;

		Sys::RedrawWindow(&Sys::g_window);

		float fps = Demo::com_maxFps.f;
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
