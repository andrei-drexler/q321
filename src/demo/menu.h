#pragma once

////////////////////////////////////////////////////////////////

#define DEMO_MENUS(begin, item, end)\
	begin(InGame)\
		item("RESUME GAME",		ResumeGame)\
		item("SETUP",			Options)\
		item("NEXT ARENA",		NextMap)\
		item("LEAVE ARENA",		QuitMap)\
		item("EXIT GAME",		ExitGame)\
	end(InGame)\

////////////////////////////////////////////////////////////////

namespace Demo::Menu {
	void Init();
	bool Update(float dt);
	void Draw();

	enum class Action : u8 {
		ResumeGame,
		CloseMenu,
		Options,
		NextMap,
		QuitMap,
		ExitGame,

		Count,
	};

	namespace Details {
		static constexpr u32
			ItemColor  = 0xFF'B2'00'00,
			FocusFolor = 0xFF'FF'00'00
		;

		enum {
			#define PP_COUNT_ITEM(caption, action) +1
			ItemCount = DEMO_MENUS(PP_IGNORE_ARGS, PP_COUNT_ITEM, PP_IGNORE_ARGS),
			#undef PP_COUNT_ITEM

			#define PP_COUNT_MENU(name) +1
			MenuCount = DEMO_MENUS(PP_COUNT_MENU, PP_IGNORE_ARGS, PP_IGNORE_ARGS),
			#undef PP_COUNT_MENU
		};

		static constexpr char ItemStringList[] =
			#define PP_ADD_ITEM_STRING(caption, action) caption "\0"
			DEMO_MENUS(PP_IGNORE_ARGS, PP_ADD_ITEM_STRING, PP_IGNORE_ARGS)
			#undef PP_ADD_ITEM_STRING
		;

		static constexpr Action ItemActions[ItemCount] = {
			#define PP_ADD_ITEM_ACTION(caption, action) Action::action,
			DEMO_MENUS(PP_IGNORE_ARGS, PP_ADD_ITEM_ACTION, PP_IGNORE_ARGS)
			#undef PP_ADD_ITEM_ACTION
		};

		static constexpr u8 MenuItemCounts[MenuCount] = {
			#define PP_COUNT_ITEM(caption, action) +1
			#define PP_ADD_COMMA(...) ,
			DEMO_MENUS(PP_IGNORE_ARGS, PP_COUNT_ITEM, PP_ADD_COMMA)
			#undef PP_COUNT_ITEM
			#undef PP_ADD_COMMA
		};
	}

	struct ItemState {
		const char*				text;
		Action					action;
	};

	struct MenuState {
		ItemState*				items;
		u32						num_items;
		u32						focus;
	};

	ItemState					items[Details::ItemCount];
	static union {
		MenuState				list[Details::MenuCount];
		struct {
			#define PP_ADD_MENU(name) MenuState name;
			DEMO_MENUS(PP_ADD_MENU, PP_IGNORE_ARGS, PP_IGNORE_ARGS)
			#undef PP_ADD_MENU
		};
	};

	MenuState*					g_active;
}

////////////////////////////////////////////////////////////////

FORCEINLINE void Demo::Menu::Init() {
	u32 item_index = 0;
	for (const char* text = Details::ItemStringList; *text; text = NextAfter(text), ++item_index) {
		ItemState& item = items[item_index];
		item.text = text;
		item.action = Details::ItemActions[item_index];
	}

	item_index = 0;
	for (u32 menu_index = 0; menu_index < Details::MenuCount; ++menu_index) {
		MenuState& menu = list[menu_index];
		u32 num_items = Details::MenuItemCounts[menu_index];
		menu.items = &items[item_index];
		menu.num_items = num_items;
		item_index += num_items;
	}
}

FORCEINLINE bool Demo::Menu::Update(float dt) {
	if (Sys::IsKeyRepeating(Key::Escape)) {
		if (g_active) {
			g_active = nullptr;
		} else {
			g_active = &InGame;
			g_active->focus = 0;
		}
	}

	if (g_active) {
		if (Sys::IsKeyRepeating(Key::Up)) {
			if (--g_active->focus > g_active->num_items)
				g_active->focus = g_active->num_items - 1;
		}

		if (Sys::IsKeyRepeating(Key::Down)) {
			if (++g_active->focus == g_active->num_items)
				g_active->focus = 0;
		}

		if (Sys::IsKeyFirstDown(Key::Enter)) {
			assert(g_active->focus < g_active->num_items);

			switch (g_active->items[g_active->focus].action) {
				case Action::ResumeGame:
					g_active = nullptr;
					break;

				case Action::CloseMenu:
					g_active = nullptr;
					break;

				case Action::Options:
					break;

				case Action::NextMap:
					LoadNextMap();
					g_active = nullptr;
					break;

				case Action::QuitMap:
					break;

				case Action::ExitGame:
					Sys::Exit();
					break;

				default:
					break;
			}
		}
	}

	return g_active;
}

FORCEINLINE void Demo::Menu::Draw() {
	if (!g_active)
		return;

	Gfx::SetShader(Shader::uiframe);
	Uniform::Time.y = 7.f / 8.f;
	Uniform::Time.z = 5.f / 8.f;
	Gfx::DrawFullScreen();

	vec2 pos = Gfx::GetResolution() * 0.5f;
	vec2 ui_scale = UI::GetScale();
	vec2 font_scale = UI::FontScale[UI::LargeFont] * ui_scale.y;

	float line_height = ui_scale.y * 80.f;
	pos.y -= line_height * 0.5f * float(g_active->num_items - 1);

	for (u32 item_index = 0; item_index < g_active->num_items; ++item_index) {
		const char* text = g_active->items[item_index].text;
		bool focused = g_active->focus == item_index;
		u32 color = focused ? Details::FocusFolor : Details::ItemColor;
		
		UI::PrintShadowed(text, pos, font_scale, color, 0.5f, UI::LargeFont);

		if (focused) {
			u32 glow_alpha = u32(255.f * 2.f * abs(fract(float(g_time) * 2.f) - 0.5f)) << 24;
			color = (color & 0xFF'FF'FFu) | glow_alpha;
			UI::Print(text, pos, font_scale, color, 0.5f, UI::LargeFontBlurry);
		}

		pos.y += line_height;
	}

	UI::FlushGeometry();
}